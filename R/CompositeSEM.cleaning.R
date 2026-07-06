# =====================================================================================
# Data Cleaning Module for CompositeSEM
# -------------------------------------------------------------------------------------
# Optional pre-processing step applied to the raw jamovi data before it is handed to
# cSEM. Base R only - no extra package dependencies.
#
# .cleanSEM_router(data, method, vars)
#   data   - raw data frame (self$data from jamovi)
#   method - one of "listwise", "mean", "regression", "knn"
#   vars   - column names to clean (indicators used by the active constructs)
#
# Returns list(clean_data, summary), where summary contains:
#   method, rows_deleted, values_imputed, per_variable (named counts),
#   notes (warnings, e.g. automatic fallback to mean), status, error_message
#
# Adding a new method: write `.cleanSEM_<name>()` with the same return shape, add it
# to the switch() below, and register it in cleaningMethod (a.yaml / h.R).
# =====================================================================================

# Dispatches to the requested cleaning method and standardises the result. Wrapped in
# tryCatch so a failure is reported as an "Error" status instead of crashing jamovi.
.cleanSEM_router <- function(data, method, vars) {

  summary_info <- list(
    method         = method,
    rows_deleted   = 0L,
    values_imputed = 0L,
    per_variable   = list(),
    notes          = character(0),
    status         = "Success",
    error_message  = NULL
  )

  if (is.null(data) || nrow(data) == 0) {
    summary_info$status        <- "Error"
    summary_info$error_message <- "The data set is empty or unavailable."
    return(list(clean_data = data, summary = summary_info))
  }

  valid_vars <- intersect(vars, colnames(data))
  if (length(valid_vars) == 0) {
    summary_info$status        <- "Error"
    summary_info$error_message <- "None of the selected variables were found in the data set."
    return(list(clean_data = data, summary = summary_info))
  }

  result <- tryCatch({
    switch(method,
      "listwise"   = .cleanSEM_listwise(data, valid_vars),
      "mean"       = .cleanSEM_mean(data, valid_vars),
      "regression" = .cleanSEM_regression(data, valid_vars),
      "knn"        = .cleanSEM_knn(data, valid_vars),
      stop("The selected cleaning method is not supported.")
    )
  }, error = function(e) {
    list(
      data           = data,
      rows_deleted   = 0L,
      values_imputed = 0L,
      per_variable   = list(),
      notes          = character(0),
      error_message  = paste("Data cleaning failed:", e$message)
    )
  })

  summary_info$rows_deleted   <- result$rows_deleted
  summary_info$values_imputed <- result$values_imputed
  summary_info$per_variable   <- result$per_variable
  summary_info$notes          <- result$notes
  if (!is.null(result$error_message)) {
    summary_info$status        <- "Error"
    summary_info$error_message <- result$error_message
  }

  list(clean_data = result$data, summary = summary_info)
}

# Drops any row with a missing value in one of the selected variables.
.cleanSEM_listwise <- function(data, vars) {
  original_rows <- nrow(data)
  data_clean    <- data[stats::complete.cases(data[, vars, drop = FALSE]), , drop = FALSE]

  list(
    data           = data_clean,
    rows_deleted   = original_rows - nrow(data_clean),
    values_imputed = 0L,
    per_variable   = list(),
    notes          = character(0),
    error_message  = NULL
  )
}

# Mean imputation for numeric variables; categorical variables are skipped.
.cleanSEM_mean <- function(data, vars) {
  imputed_total <- 0L
  per_variable  <- list()
  notes         <- character(0)

  for (v in vars) {
    nas <- is.na(data[[v]])
    if (any(nas)) {
      if (!is.numeric(data[[v]])) {
        notes <- c(notes, paste0("'", v, "' is categorical and was skipped by mean imputation."))
        next
      }
      fill_val <- mean(data[[v]], na.rm = TRUE)

      if (!is.na(fill_val)) {
        data[[v]][nas]    <- fill_val
        per_variable[[v]] <- sum(nas)
        imputed_total     <- imputed_total + sum(nas)
      }
    }
  }

  list(data = data, rows_deleted = 0L, values_imputed = imputed_total,
       per_variable = per_variable, notes = notes, error_message = NULL)
}

# Predicts each missing value from the other numeric indicators via a simple lm().
#
# Implementation notes (these fix the two failure modes of the naive approach):
#   1. Predictor columns are pre-filled with their own means before fitting and
#      predicting. Otherwise any row that is missing on the target AND on one of
#      the predictors gets an NA prediction, leaving the value un-imputed.
#   2. The model is fit with `.y ~ .` on a prepared data frame instead of building
#      a formula string from the raw column names, which breaks (and aborts the
#      whole cleaning step) when jamovi variable names contain spaces or symbols.
# Restricted to numeric variables; falls back to mean imputation when too few
# observed cases are available to fit a stable model.
.cleanSEM_regression <- function(data, vars) {
  imputed_total <- 0L
  per_variable  <- list()
  notes         <- character(0)

  num_vars <- vars[sapply(data[, vars, drop = FALSE], is.numeric)]
  if (length(num_vars) < 2)
    stop("Regression imputation requires at least two numeric variables among the selected indicators.")

  non_numeric <- setdiff(vars, num_vars)
  if (length(non_numeric) > 0)
    notes <- c(notes, paste0("Categorical variable(s) skipped by regression imputation: ",
                              paste(non_numeric, collapse = ", ")))

  # Mean-filled copy of the predictors so every row can receive a prediction
  pred_base <- data[, num_vars, drop = FALSE]
  for (v in num_vars) {
    v_mean <- mean(pred_base[[v]], na.rm = TRUE)
    pred_base[[v]][is.na(pred_base[[v]])] <- v_mean
  }

  for (v in num_vars) {
    nas <- is.na(data[[v]])
    if (any(nas)) {
      predictors <- setdiff(num_vars, v)
      v_mean     <- mean(data[[v]], na.rm = TRUE)

      if (sum(!nas) > max(5, length(predictors) + 1)) {
        fit_data        <- pred_base[!nas, predictors, drop = FALSE]
        fit_data[[".y"]] <- data[[v]][!nas]

        preds <- tryCatch({
          model <- stats::lm(.y ~ ., data = fit_data)
          as.numeric(stats::predict(model, newdata = pred_base[nas, predictors, drop = FALSE]))
        }, error = function(e) rep(NA_real_, sum(nas)))

        # Any prediction that could not be computed falls back to the mean
        bad <- !is.finite(preds)
        if (all(bad)) {
          notes <- c(notes, paste0("'", v, "' could not be predicted by regression; mean imputation was used instead."))
          preds <- rep(v_mean, sum(nas))
        } else if (any(bad)) {
          notes <- c(notes, paste0("Some values of '", v, "' could not be predicted by regression; mean imputation was used for those."))
          preds[bad] <- v_mean
        }
        data[[v]][nas] <- preds
      } else {
        notes <- c(notes, paste0("'", v, "' had too few complete cases for regression; mean imputation was used instead."))
        data[[v]][nas] <- v_mean
      }
      per_variable[[v]] <- sum(nas)
      imputed_total     <- imputed_total + sum(nas)
    }
  }

  list(data = data, rows_deleted = 0L, values_imputed = imputed_total,
       per_variable = per_variable, notes = notes, error_message = NULL)
}

# 1-Nearest-Neighbour imputation by squared Euclidean distance over the numeric
# indicators. Restricted to numeric variables; Base R only.
.cleanSEM_knn <- function(data, vars) {
  imputed_total <- 0L
  per_variable  <- list()
  notes         <- character(0)

  num_vars <- vars[sapply(data[, vars, drop = FALSE], is.numeric)]
  if (length(num_vars) < 2)
    stop("KNN imputation requires at least two numeric variables among the selected indicators.")

  non_numeric <- setdiff(vars, num_vars)
  if (length(non_numeric) > 0)
    notes <- c(notes, paste0("Categorical variable(s) skipped by KNN imputation: ",
                              paste(non_numeric, collapse = ", ")))

  complete_idx <- which(stats::complete.cases(data[, num_vars, drop = FALSE]))
  if (length(complete_idx) == 0)
    stop("No fully observed rows are available to use as KNN reference points.")

  for (v in num_vars) {
    na_idx <- which(is.na(data[[v]]))
    if (length(na_idx) > 0) {
      for (i in na_idx) {
        target <- data[i, num_vars, drop = FALSE]
        dists  <- apply(data[complete_idx, num_vars, drop = FALSE], 1, function(row) {
          sum((row - target)^2, na.rm = TRUE)
        })
        nearest      <- complete_idx[which.min(dists)]
        data[[v]][i] <- data[[v]][nearest]
      }
      per_variable[[v]] <- length(na_idx)
      imputed_total      <- imputed_total + length(na_idx)
    }
  }

  list(data = data, rows_deleted = 0L, values_imputed = imputed_total,
       per_variable = per_variable, notes = notes, error_message = NULL)
}
