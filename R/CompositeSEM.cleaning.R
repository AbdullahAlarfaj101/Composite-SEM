# =====================================================================================
# Data Cleaning Module for CompositeSEM
# -------------------------------------------------------------------------------------
# Optional pre-processing step applied to the raw jamovi data before it is handed to
# cSEM. Base R only - no extra package dependencies.
#
# .cleanSEM_router(data, method, vars)
#   data   - raw data frame (self$data from jamovi)
#   method - one of "listwise", "mean", "median", "mode", "regression", "knn"
#   vars   - column names to clean (indicators used by the active constructs)
#
# Returns list(clean_data, summary), where summary contains:
#   method, rows_deleted, values_imputed, per_variable (named counts),
#   notes (warnings, e.g. automatic fallback to mode), status, error_message
#
# Adding a new method: write `.cleanSEM_<name>()` with the same return shape, add it
# to the switch() below, and register it in cleaningMethod (a.yaml / h.R).
# =====================================================================================

# R has no built-in mode function; used for mode imputation and as the fallback when
# mean/median is requested on a non-numeric (categorical) variable.
.cleanSEM_getMode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  if (length(uniqv) == 0)
    return(NA)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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
      "median"     = .cleanSEM_median(data, valid_vars),
      "mode"       = .cleanSEM_mode(data, valid_vars),
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

# Mean imputation for numeric variables; falls back to mode for categorical ones.
.cleanSEM_mean <- function(data, vars) {
  imputed_total <- 0L
  per_variable  <- list()
  notes         <- character(0)

  for (v in vars) {
    nas <- is.na(data[[v]])
    if (any(nas)) {
      if (is.numeric(data[[v]])) {
        fill_val <- mean(data[[v]], na.rm = TRUE)
      } else {
        fill_val <- .cleanSEM_getMode(data[[v]])
        notes <- c(notes, paste0("'", v, "' is categorical; mode imputation was used instead of mean."))
      }

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

# Median imputation for numeric variables; falls back to mode for categorical ones.
.cleanSEM_median <- function(data, vars) {
  imputed_total <- 0L
  per_variable  <- list()
  notes         <- character(0)

  for (v in vars) {
    nas <- is.na(data[[v]])
    if (any(nas)) {
      if (is.numeric(data[[v]])) {
        fill_val <- stats::median(data[[v]], na.rm = TRUE)
      } else {
        fill_val <- .cleanSEM_getMode(data[[v]])
        notes <- c(notes, paste0("'", v, "' is categorical; mode imputation was used instead of median."))
      }

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

# Mode imputation: works natively for both numeric and categorical variables.
.cleanSEM_mode <- function(data, vars) {
  imputed_total <- 0L
  per_variable  <- list()

  for (v in vars) {
    nas <- is.na(data[[v]])
    if (any(nas)) {
      fill_val <- .cleanSEM_getMode(data[[v]])
      if (!is.na(fill_val)) {
        data[[v]][nas]    <- fill_val
        per_variable[[v]] <- sum(nas)
        imputed_total     <- imputed_total + sum(nas)
      }
    }
  }

  list(data = data, rows_deleted = 0L, values_imputed = imputed_total,
       per_variable = per_variable, notes = character(0), error_message = NULL)
}

# Predicts each missing value from the other numeric indicators via a simple lm().
# Restricted to numeric variables; falls back to mean imputation when too few
# complete rows are available to fit a stable model.
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

  for (v in num_vars) {
    nas <- is.na(data[[v]])
    if (any(nas)) {
      predictors <- setdiff(num_vars, v)
      form       <- stats::as.formula(paste(v, "~", paste(predictors, collapse = " + ")))
      fit_data   <- data[!nas, num_vars, drop = FALSE]

      if (nrow(fit_data) > 5) {
        model <- stats::lm(form, data = fit_data)
        preds <- stats::predict(model, newdata = data[nas, num_vars, drop = FALSE])
        data[[v]][nas] <- preds
      } else {
        notes <- c(notes, paste0("'", v, "' had too few complete cases for regression; mean imputation was used instead."))
        data[[v]][nas] <- mean(data[[v]], na.rm = TRUE)
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
