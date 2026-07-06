# This file is a generated template, your changes will not be overwritten

#' @rdname jamovi
#' @export
CompositeSEMClass <- R6::R6Class("CompositeSEMClass",
                                 inherit = CompositeSEMBase,
                                 private = list(
                                   
                                   # =================================================================
                                   # Helper: build structural_input and is_auto_mode from drag-and-drop blocks
                                   # =================================================================
                                   .buildStructural = function() {
                                     
                                     keep_labels <- function(items) {
                                       out <- character(0)
                                       for (item in items) {
                                         label <- item$label %||% ""
                                         vars  <- item$vars %||% list()
                                         if (nzchar(label) && length(vars) > 0)
                                           out <- c(out, label)
                                       }
                                       out
                                     }
                                     
                                     all_labels <- c(
                                       keep_labels(self$options$latent),
                                       keep_labels(self$options$composite)
                                     )
                                     
                                     normalize_terms <- function(x) {
                                       if (is.null(x) || length(x) == 0)
                                         return(character(0))
                                       vals <- sapply(x, function(term) {
                                         nm <- if (is.list(term)) unlist(term) else as.character(term)
                                         paste(nm, collapse = ":")
                                       })
                                       vals[nzchar(vals)]
                                     }
                                     
                                     endogenous_selected <- unique(normalize_terms(self$options$endogenousClass))
                                     exogenous_selected  <- unique(normalize_terms(self$options$exogenousClass))
                                     
                                     endogenous_labels <- all_labels[all_labels %in% endogenous_selected]
                                     exogenous_labels  <- all_labels[all_labels %in% exogenous_selected]
                                     
                                     endo_terms <- self$options$endogenousTerms
                                     structural_parts <- character(0)
                                     used_labels <- unique(c(endogenous_labels, exogenous_labels))
                                     
                                     for (i in seq_along(endogenous_labels)) {
                                       if (i > length(endo_terms)) break
                                       
                                       block_list <- endo_terms[[i]]
                                       if (is.null(block_list) || length(block_list) == 0) next
                                       
                                       predictor_names <- sapply(block_list, function(t) {
                                         nm <- if (is.list(t)) unlist(t) else as.character(t)
                                         paste(nm, collapse = ":")
                                       })
                                       predictor_names <- predictor_names[nzchar(predictor_names)]
                                       predictor_names <- predictor_names[predictor_names %in% all_labels]
                                       predictor_names <- predictor_names[predictor_names != endogenous_labels[i]]
                                       if (length(predictor_names) == 0) next
                                       
                                       used_labels <- unique(c(used_labels, predictor_names))
                                       
                                       lhs <- jmvcore::composeTerm(endogenous_labels[i])
                                       rhs <- paste(
                                         sapply(predictor_names, jmvcore::composeTerm),
                                         collapse = " + "
                                       )
                                       structural_parts <- c(structural_parts, paste0(lhs, " ~ ", rhs))
                                     }
                                     
                                     is_auto_mode <- (length(structural_parts) == 0)
                                     if (is_auto_mode)
                                       used_labels <- all_labels
                                     
                                     list(
                                       all_labels       = all_labels,
                                       endogenous_labels = endogenous_labels,
                                       used_labels      = used_labels,
                                       structural_input = paste(structural_parts, collapse = "\n"),
                                       is_auto_mode     = is_auto_mode
                                     )
                                   },
                                   
                                   # =================================================================
                                   # .init(): Runs instantly to prepare UI tables
                                   # =================================================================
                                   .init = function() {
                                     
                                     summaryTable <- self$results$constructsTable
                                     si           <- private$.buildStructural()
                                     
                                     is_used <- function(label) {
                                       if (si$is_auto_mode) return(TRUE)
                                       label %in% si$used_labels
                                     }
                                     
                                     for (item in self$options$latent) {
                                       if (length(item$vars) > 0 && nzchar(item$label) && is_used(item$label)) {
                                         summaryTable$addRow(rowKey=item$label, values=list(
                                           type       = 'Latent (Reflective)',
                                           construct  = item$label,
                                           indicators = paste(item$vars, collapse=', ')
                                         ))
                                       }
                                     }
                                     
                                     for (item in self$options$composite) {
                                       if (length(item$vars) > 0 && nzchar(item$label) && is_used(item$label)) {
                                         summaryTable$addRow(rowKey=item$label, values=list(
                                           type       = 'Composite (Formative)',
                                           construct  = item$label,
                                           indicators = paste(item$vars, collapse=', ')
                                         ))
                                       }
                                     }
                                   },
                                   
                                   # =================================================================
                                   # .run(): Runs the heavy statistical computations
                                   # =================================================================
                                   .run = function() {
                                     
                                     # Get structural components
                                     si               <- private$.buildStructural()
                                     is_auto_mode     <- si$is_auto_mode
                                     structural_input <- si$structural_input
                                     
                                     is_used <- function(label) {
                                       if (is_auto_mode) return(TRUE)
                                       label %in% si$used_labels
                                     }
                                     
                                     clean_list <- function(l) {
                                       l[sapply(l, function(x) !is.null(x) && !is.na(x) && !is.nan(x))]
                                     }
                                     
                                     measurement_parts <- list()
                                     hasCommonFactors  <- FALSE
                                     active_constructs <- c()
                                     ignored_vars      <- c()
                                     cleaning_vars     <- c() # indicators of active constructs, used for data cleaning

                                     # A) Latent Variables
                                     for (item in self$options$latent) {
                                       if (length(item$vars) > 0 && nzchar(item$label)) {
                                         if (is_used(item$label)) {
                                           safe_label <- jmvcore::composeTerm(item$label)
                                           safe_vars  <- sapply(item$vars, jmvcore::composeTerm)
                                           measurement_parts <- c(measurement_parts,
                                                                  paste0(safe_label, " =~ ", paste(safe_vars, collapse=" + ")))
                                           hasCommonFactors  <- TRUE
                                           active_constructs <- c(active_constructs, item$label)
                                           cleaning_vars      <- c(cleaning_vars, item$vars)
                                         } else {
                                           ignored_vars <- c(ignored_vars, item$label)
                                         }
                                       }
                                     }

                                     # B) Composite Variables
                                     for (item in self$options$composite) {
                                       if (length(item$vars) > 0 && nzchar(item$label)) {
                                         if (is_used(item$label)) {
                                           safe_label <- jmvcore::composeTerm(item$label)
                                           safe_vars  <- sapply(item$vars, jmvcore::composeTerm)
                                           measurement_parts <- c(measurement_parts,
                                                                  paste0(safe_label, " <~ ", paste(safe_vars, collapse=" + ")))
                                           active_constructs <- c(active_constructs, item$label)
                                           cleaning_vars      <- c(cleaning_vars, item$vars)
                                         } else {
                                           ignored_vars <- c(ignored_vars, item$label)
                                         }
                                       }
                                     }
                                     cleaning_vars <- unique(cleaning_vars)

                                     cleaningSummaryTable    <- self$results$cleaningSummaryTable
                                     infoTable               <- self$results$infoTable
                                     fitTable                <- self$results$fitTable
                                     exactFitTable           <- self$results$exactFitTable
                                     outerCompositesTable    <- self$results$outerCompositesTable
                                     outerCommonFactorsTable <- self$results$outerCommonFactorsTable
                                     structuralTable         <- self$results$structuralTable
                                     mediationTable          <- self$results$mediationTable
                                     vcvTable                <- self$results$vcvTable
                                     htmtTable               <- self$results$htmtTable
                                     vifModeBTable           <- self$results$vifModeBTable
                                     reliabilityTable        <- self$results$reliabilityTable
                                     csemOutput              <- self$results$csemOutput
                                     
                                     # Clear existing data from tables
                                     cleaningSummaryTable$deleteRows()
                                     infoTable$deleteRows()
                                     fitTable$deleteRows()
                                     exactFitTable$deleteRows()
                                     outerCompositesTable$deleteRows()
                                     outerCommonFactorsTable$deleteRows()
                                     structuralTable$deleteRows()
                                     mediationTable$deleteRows()
                                     vcvTable$deleteRows()
                                     htmtTable$deleteRows()
                                     vifModeBTable$deleteRows()
                                     reliabilityTable$deleteRows()
                                     
                                     # Set structural table visibility
                                     if (is_auto_mode) {
                                       structuralTable$setVisible(FALSE)
                                     } else {
                                       structuralTable$setVisible(TRUE)
                                     }
                                     
                                     summaryTable <- self$results$constructsTable
                                     summaryTable$setNote("maxvar_error", NULL)
                                     summaryTable$setNote("gsca_error", NULL)
                                     
                                     # Identify if we have any active composite constructs
                                     hasComposites <- FALSE
                                     for (item in self$options$composite) {
                                       if (length(item$vars) > 0 && nzchar(item$label) && is_used(item$label)) {
                                         hasComposites <- TRUE
                                         break
                                       }
                                     }
                                     
                                     # Check validation rules
                                     estimationModel <- self$options$alt
                                     if (estimationModel == "MAXVAR" && !isTRUE(self$options$disattenuate) && hasComposites) {
                                       summaryTable$setNote("maxvar_error", "MAXVAR only works with disattenuation if all constructs are common factors (latent variables). Please enable 'No disattenuation' or choose another estimation method.")
                                       csemOutput$setVisible(FALSE)
                                       return()
                                     }
                                     
                                     
                                     
                                     if (length(measurement_parts) == 0) {
                                       infoTable$addRow(rowKey="msg", values=list(property="Message", value="No constructs defined or matched."))
                                       csemOutput$setContent("No constructs defined or matched.")
                                       return()
                                     }
                                     
                                     measurement_string <- paste(measurement_parts, collapse="\n")
                                     
                                     if (is_auto_mode) {
                                       if (length(active_constructs) > 1) {
                                         safe_active <- sapply(active_constructs, jmvcore::composeTerm)
                                         pairs <- utils::combn(safe_active, 2,
                                                               function(x) paste(x, collapse=" ~~ "))
                                         final_structural_part <- paste(pairs, collapse="\n")
                                       } else {
                                         final_structural_part <- ""
                                       }
                                     } else {
                                       final_structural_part <- structural_input
                                     }
                                     
                                     model <- paste(measurement_string, final_structural_part, sep="\n\n")

                                     # Data Cleaning: runs after the model syntax is built but before cSEM sees
                                     # the data. `working_data` is what gets sent to cSEM::csem() below - either
                                     # the untouched jamovi data, or the cleaned version from `.cleanSEM_router()`
                                     # (see CompositeSEM.cleaning.R). Every detail is written to
                                     # `cleaningSummaryTable` so the user can see exactly what was done.
                                     working_data <- self$data
                                     if (isTRUE(self$options$dataCleaningEnabled)) {
                                       if (length(cleaning_vars) == 0) {
                                         cleaningSummaryTable$addRow(rowKey="msg", values=list(
                                           property = "Message",
                                           value    = "No indicator variables available to clean."
                                         ))
                                       } else {
                                         cleaning_method  <- self$options$cleaningMethod
                                         cleaning_results <- .cleanSEM_router(self$data, cleaning_method, cleaning_vars)
                                         cleaning_summary <- cleaning_results$summary

                                         if (cleaning_summary$status == "Error") {
                                           cleaningSummaryTable$addRow(rowKey="status", values=list(
                                             property = "Status",
                                             value    = paste("Error -", cleaning_summary$error_message)
                                           ))
                                           # Keep going with the uncleaned data rather than failing the whole analysis
                                         } else {
                                           working_data <- cleaning_results$clean_data

                                           cleaningSummaryTable$addRow(rowKey="method", values=list(
                                             property = "Cleaning method used",
                                             value    = cleaning_method
                                           ))
                                           # Only shown when rows were actually removed; a permanent
                                           # "0 rows deleted" line is just noise for imputation methods
                                           if (isTRUE(cleaning_summary$rows_deleted > 0)) {
                                             cleaningSummaryTable$addRow(rowKey="rows_deleted", values=list(
                                               property = "Rows deleted (listwise deletion)",
                                               value    = as.character(cleaning_summary$rows_deleted)
                                             ))
                                           }
                                           if (isTRUE(cleaning_summary$values_imputed > 0)) {
                                             cleaningSummaryTable$addRow(rowKey="values_imputed", values=list(
                                               property = "Values imputed (total)",
                                               value    = as.character(cleaning_summary$values_imputed)
                                             ))
                                           }

                                           # Per-variable breakdown of imputed values
                                           for (v in names(cleaning_summary$per_variable)) {
                                             cleaningSummaryTable$addRow(rowKey=paste0("var_", v), values=list(
                                               property = paste0("Values imputed in '", v, "'"),
                                               value    = as.character(cleaning_summary$per_variable[[v]])
                                             ))
                                           }

                                           # Any automatic fallbacks or warnings (e.g. mean requested on a
                                           # categorical variable) are surfaced to the user as separate rows
                                           if (length(cleaning_summary$notes) > 0) {
                                             for (i in seq_along(cleaning_summary$notes)) {
                                               cleaningSummaryTable$addRow(rowKey=paste0("note_", i), values=list(
                                                 property = "Note",
                                                 value    = cleaning_summary$notes[i]
                                               ))
                                             }
                                           }
                                         }
                                       }
                                     }

                                     # --- Setup ---
                                     multGroupVar     <- self$options$multg
                                     estimationModel  <- self$options$alt
                                     useBootstrap     <- self$options$useBootstrap
                                     bootstrapSamples <- self$options$bootR
                                     runLinearBench   <- self$options$LinearBench
                                     
                                     # Initialize these here so other blocks can access them
                                     groups   <- character(0)
                                     summs    <- list()
                                     is_multi <- FALSE
                                     
                                     # Capture the method's local environment so tryCatch closures can
                                     # write back to it (<<- in R6 bypasses local scope to the object env)
                                     .run_env <- environment()
                                     
                                     # --- Run cSEM ---
                                     tryCatch({
                                       
                                      csem_args <- list(.data=working_data, .model=model)
                                       
                                       if (!is.null(multGroupVar) && multGroupVar != "")
                                         csem_args$.id <- multGroupVar
                                       
                                       if (estimationModel == 'PLS') {
                                         csem_args$.PLS_weight_scheme_inner <-
                                           if (is_auto_mode) "factorial" else "path"
                                       } else if (estimationModel == 'GSCA') {
                                         csem_args$.approach_weights <- 'GSCA'
                                       } else {
                                         csem_args$.approach_weights <- estimationModel
                                       }
                                       
                                       if (isTRUE(useBootstrap)) {
                                         csem_args$.resample_method <- "bootstrap"
                                         csem_args$.R <- bootstrapSamples
                                       }
                                       
                                       csem_args$.disattenuate <- !isTRUE(self$options$disattenuate)
                                       
                                       out <- do.call(cSEM::csem, csem_args)
                                       summ <- cSEM::summarize(out)
                                       
                                       # Check if it is multi-group
                                       # Assign into the method's captured env so the plot block (outside
                                       # tryCatch) can read these variables
                                       .run_env$is_multi <- inherits(out, "cSEMResults_multi")
                                       if (.run_env$is_multi) {
                                         .run_env$groups <- names(out)
                                         .run_env$summs  <- summ
                                       } else {
                                         .run_env$groups <- c("")
                                         .run_env$summs  <- list(summ)
                                         names(.run_env$summs) <- ""
                                       }
                                       
                                       # Retrieve assess results if PLS
                                       has_assess <- TRUE
                                       asses <- NULL
                                       if (has_assess) {
                                         ass <- tryCatch({
                                           cSEM::assess(out)
                                         }, error = function(e) {
                                           NULL
                                         })
                                         if (!is.null(ass)) {
                                           if (is_multi) {
                                             asses <- ass
                                           } else {
                                             asses <- list(ass)
                                             names(asses) <- ""
                                           }
                                         }
                                       }
                                       
                                       # 1. Model Info Table
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         n_obs <- nrow(s$Information$Data)
                                         conv <- s$Information$Weight_info$Convergence_status
                                         iters <- s$Information$Weight_info$Number_iterations
                                         
                                         infoTable$addRow(rowKey=paste0(g, "_obs"), values=list(
                                           group = g,
                                           property = "Number of observations",
                                           value = as.character(n_obs)
                                         ))
                                         
                                         infoTable$addRow(rowKey=paste0(g, "_conv"), values=list(
                                           group = g,
                                           property = "Algorithm converged",
                                           value = if (isTRUE(conv)) "Yes" else "No"
                                         ))
                                         
                                         if (!is.null(iters)) {
                                           infoTable$addRow(rowKey=paste0(g, "_iters"), values=list(
                                             group = g,
                                             property = "Number of iterations",
                                             value = as.character(iters)
                                           ))
                                         }
                                       }
                                       
                                       # Add general model info
                                       infoTable$addRow(rowKey="est_method", values=list(
                                         group = "",
                                         property = "Estimation method",
                                         value = estimationModel
                                       ))
                                       if (estimationModel == "PLS") {
                                         infoTable$addRow(rowKey="inner_scheme", values=list(
                                           group = "",
                                           property = "Inner weighting scheme",
                                           value = if (is_auto_mode) "factorial" else "path"
                                         ))
                                       }
                                       
                                       # Add disattenuation info
                                       infoTable$addRow(rowKey="disattenuate", values=list(
                                         group = "",
                                         property = "Disattenuate reflective measures",
                                         value = if (!isTRUE(self$options$disattenuate)) "Yes" else "No"
                                       ))
                                       
                                       # Get construct types
                                       c_types <- if (is_multi) out[[1]]$Information$Arguments$.model$construct_type else out$Information$Arguments$.model$construct_type
                                       all_constructs <- names(c_types)
                                       
                                       # 2. Fit Indices Table
                                       if (!is.null(asses)) {
                                         for (g in groups) {
                                           a <- if (is_multi) asses[[g]] else asses[[1]]
                                           if (is.null(a)) next
                                           
                                           metrics <- list(
                                             "Chi-square" = a$Chi_square,
                                             "Degrees of freedom (df)" = a$Df,
                                             "CFI" = a$CFI,
                                             "TLI (NNFI)" = a$NNFI,
                                             "IFI" = a$IFI,
                                             "RMSEA" = a$RMSEA,
                                             "SRMR" = a$SRMR,
                                             "RMS<sub>\u03B8</sub>" = a$RMS_theta
                                           )
                                           
                                           for (m_name in names(metrics)) {
                                             val <- metrics[[m_name]]
                                             if (!is.null(val) && !is.nan(val) && length(val) > 0) {
                                               fitTable$addRow(rowKey=paste0(g, "_", m_name), values=list(
                                                 group = g,
                                                 metric = m_name,
                                                 value = as.numeric(val)
                                               ))
                                             }
                                           }
                                         }
                                       }
                                       
                                       
                                       # 2.5 Exact Fit Test Table
                                       exactFit <- self$options$exactFit
                                       if (isTRUE(exactFit)) {
                                         exactFitTable$setVisible(TRUE)
                                         if (isTRUE(useBootstrap)) {
                                           omf_res <- tryCatch({
                                             cSEM::testOMF(out)
                                           }, error = function(e) {
                                             NULL
                                           })
                                           
                                           if (!is.null(omf_res)) {
                                             for (g in groups) {
                                               o <- if (is_multi) omf_res[[g]] else omf_res
                                               if (is.null(o)) next
                                               
                                               measures <- c("dG", "SRMR", "dL", "dML")
                                               for (m in measures) {
                                                 stat <- o$Test_statistic[m]
                                                 crit <- if (m %in% rownames(o$Critical_value)) o$Critical_value[m, "95%"] else NA
                                                 dec_val <- if (m %in% rownames(o$Decision)) o$Decision[m, "95%"] else NA
                                                 decision <- if (is.na(dec_val)) "" else (if (isTRUE(dec_val)) "Do not reject" else "Reject")
                                                 
                                                 exactFitTable$addRow(rowKey=paste0(g, "_", m), values=list(
                                                   group = g,
                                                   measure = m,
                                                   stat = as.numeric(stat),
                                                   crit = as.numeric(crit),
                                                   decision = decision
                                                 ))
                                               }
                                             }
                                           }
                                         } else {
                                           exactFitTable$setNote("bootNote", "The exact fit test requires bootstrapping. Please enable Bootstrapping.")
                                         }
                                       } else {
                                         exactFitTable$setVisible(FALSE)
                                       }
                                       
                                       # 3. Outer Model Tables (Composites & Common Factors)
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         loadings_df <- as.data.frame(s$Estimates$Loading_estimates)
                                         weights_df  <- as.data.frame(s$Estimates$Weight_estimates)
                                         
                                         showCompositeLoadings <- self$options$showCompositeLoadings
                                         
                                         if (nrow(loadings_df) > 0) {
                                           for (i in 1:nrow(loadings_df)) {
                                             row <- loadings_df[i, ]
                                             parts <- strsplit(as.character(row$Name), " =~ | <~ ")[[1]]
                                             construct <- parts[1]
                                             indicator <- parts[2]
                                             
                                             c_type <- c_types[construct]
                                             if (is.null(c_type) || as.character(c_type) != "Common factor") {
                                               # If composite and showCompositeLoadings is checked, add loadings to composites
                                               if (as.character(c_type) == "Composite" && isTRUE(showCompositeLoadings)) {
                                                 se <- if ("Std_err" %in% names(row)) row$Std_err else NA
                                                 t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                                 p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                                 cil <- if ("CI_percentile.95%L" %in% names(row)) row$`CI_percentile.95%L` else NA
                                                 ciu <- if ("CI_percentile.95%U" %in% names(row)) row$`CI_percentile.95%U` else NA
                                                 
                                                 outerCompositesTable$addRow(rowKey=paste0(g, "_loading_", row$Name), values=list(
                                                   group = g,
                                                   construct = construct,
                                                   indicator = indicator,
                                                   relation = "Loading",
                                                   estimate = as.numeric(row$Estimate),
                                                   se = as.numeric(se),
                                                   t = as.numeric(t_val),
                                                   p = as.numeric(p_val),
                                                   cil = as.numeric(cil),
                                                   ciu = as.numeric(ciu)
                                                 ))
                                               }
                                               next
                                             }
                                             
                                             se <- if ("Std_err" %in% names(row)) row$Std_err else NA
                                             t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                             p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                             cil <- if ("CI_percentile.95%L" %in% names(row)) row$`CI_percentile.95%L` else NA
                                             ciu <- if ("CI_percentile.95%U" %in% names(row)) row$`CI_percentile.95%U` else NA
                                             
                                             outerCommonFactorsTable$addRow(rowKey=paste0(g, "_loading_", row$Name), values=list(
                                               group = g,
                                               construct = construct,
                                               indicator = indicator,
                                               estimate = as.numeric(row$Estimate),
                                               se = as.numeric(se),
                                               t = as.numeric(t_val),
                                               p = as.numeric(p_val),
                                               cil = as.numeric(cil),
                                               ciu = as.numeric(ciu)
                                             ))
                                           }
                                         }
                                         
                                         if (nrow(weights_df) > 0) {
                                           for (i in 1:nrow(weights_df)) {
                                             row <- weights_df[i, ]
                                             parts <- strsplit(as.character(row$Name), " =~ | <~ ")[[1]]
                                             construct <- parts[1]
                                             indicator <- parts[2]
                                             
                                             # Only include constructs defined as "Composite"
                                             c_type <- c_types[construct]
                                             if (is.null(c_type) || as.character(c_type) != "Composite") next
                                             
                                             se <- if ("Std_err" %in% names(row)) row$Std_err else NA
                                             t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                             p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                             cil <- if ("CI_percentile.95%L" %in% names(row)) row$`CI_percentile.95%L` else NA
                                             ciu <- if ("CI_percentile.95%U" %in% names(row)) row$`CI_percentile.95%U` else NA
                                             
                                             outerCompositesTable$addRow(rowKey=paste0(g, "_weight_", row$Name), values=list(
                                               group = g,
                                               construct = construct,
                                               indicator = indicator,
                                               relation = "Weight",
                                               estimate = as.numeric(row$Estimate),
                                               se = as.numeric(se),
                                               t = as.numeric(t_val),
                                               p = as.numeric(p_val),
                                               cil = as.numeric(cil),
                                               ciu = as.numeric(ciu)
                                             ))
                                           }
                                         }
                                       }
                                       
                                       any_composite <- FALSE
                                       any_common_factor <- FALSE
                                       if (!is.null(c_types) && length(c_types) > 0) {
                                         any_composite <- any(as.character(c_types) == "Composite")
                                         any_common_factor <- any(as.character(c_types) == "Common factor")
                                       }
                                       outerCompositesTable$setVisible(any_composite)
                                       outerCommonFactorsTable$setVisible(any_common_factor)
                                       
                                       # 4. Structural Path Estimates Table
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         paths_df <- as.data.frame(s$Estimates$Path_estimates)
                                         r2_vec <- s$Estimates$R2
                                         r2adj_vec <- s$Estimates$R2adj
                                         
                                         if (nrow(paths_df) > 0) {
                                           for (i in 1:nrow(paths_df)) {
                                             row <- paths_df[i, ]
                                             parts <- strsplit(as.character(row$Name), " ~ ")[[1]]
                                             lhs <- parts[1]
                                             rhs <- parts[2]
                                             
                                             se <- if ("Std_err" %in% names(row)) row$Std_err else NA
                                             t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                             p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                             cil <- if ("CI_percentile.95%L" %in% names(row)) row$`CI_percentile.95%L` else NA
                                             ciu <- if ("CI_percentile.95%U" %in% names(row)) row$`CI_percentile.95%U` else NA
                                             
                                             r2 <- if (!is.null(r2_vec) && lhs %in% names(r2_vec)) r2_vec[lhs] else NA
                                             r2adj <- if (!is.null(r2adj_vec) && lhs %in% names(r2adj_vec)) r2adj_vec[lhs] else NA
                                             
                                             structuralTable$addRow(rowKey=paste0(g, "_path_", row$Name), values=list(
                                               group = g,
                                               rhs = rhs,
                                               lhs = lhs,
                                               estimate = as.numeric(row$Estimate),
                                               se = as.numeric(se),
                                               t = as.numeric(t_val),
                                               p = as.numeric(p_val),
                                               cil = as.numeric(cil),
                                               ciu = as.numeric(ciu),
                                               r2 = as.numeric(r2),
                                               r2adj = as.numeric(r2adj)
                                             ))
                                           }
                                         }
                                       }
                                       
                                       # 5. Construct Correlations Table
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         vcv <- s$Estimates$Construct_VCV
                                         if (is.null(vcv) || length(vcv) == 0) next
                                         
                                         constructs <- colnames(vcv)
                                         if (length(constructs) < 2) next
                                         
                                         path_names <- character(0)
                                         if (!is.null(s$Estimates$Path_estimates) && nrow(as.data.frame(s$Estimates$Path_estimates)) > 0) {
                                           path_names <- as.character(s$Estimates$Path_estimates$Name)
                                         }
                                         
                                         exo_df <- if (!is.null(s$Estimates$Exo_construct_correlation)) as.data.frame(s$Estimates$Exo_construct_correlation) else NULL
                                         
                                         for (i in 1:(length(constructs)-1)) {
                                           for (j in (i+1):length(constructs)) {
                                             c1 <- constructs[i]
                                             c2 <- constructs[j]
                                             
                                             # Skip if there is a directional regression path between c1 and c2
                                             path_key1 <- paste0(c1, " ~ ", c2)
                                             path_key2 <- paste0(c2, " ~ ", c1)
                                             if (path_key1 %in% path_names || path_key2 %in% path_names) {
                                               next
                                             }
                                             
                                             val <- vcv[c1, c2]
                                             
                                             se <- NA
                                             t_val <- NA
                                             p_val <- NA
                                             cil <- NA
                                             ciu <- NA
                                             
                                             if (!is.null(exo_df) && nrow(exo_df) > 0) {
                                               key1 <- paste0(c1, " ~~ ", c2)
                                               key2 <- paste0(c2, " ~~ ", c1)
                                               idx <- which(exo_df$Name == key1 | exo_df$Name == key2)
                                               if (length(idx) > 0) {
                                                 match_row <- exo_df[idx[1], ]
                                                 se <- if ("Std_err" %in% names(match_row)) match_row$Std_err else NA
                                                 t_val <- if ("t_stat" %in% names(match_row)) match_row$t_stat else NA
                                                 p_val <- if ("p_value" %in% names(match_row)) match_row$p_value else NA
                                                 cil <- if ("CI_percentile.95%L" %in% names(match_row)) match_row$`CI_percentile.95%L` else NA
                                                 ciu <- if ("CI_percentile.95%U" %in% names(match_row)) match_row$`CI_percentile.95%U` else NA
                                               }
                                             }
                                             
                                             vcvTable$addRow(rowKey=paste0(g, "_cor_", c1, "_", c2), values=clean_list(list(
                                               group = g,
                                               c1 = c1,
                                               c2 = c2,
                                               estimate = as.numeric(val),
                                               se = as.numeric(se),
                                               t = as.numeric(t_val),
                                               p = as.numeric(p_val),
                                               cil = as.numeric(cil),
                                               ciu = as.numeric(ciu)
                                             )))
                                           }
                                         }
                                       }
                                       
                                       # Set construct correlations visibility
                                       vcvTable$setVisible(TRUE)
                                       
                                       # Dynamically set column visibility (shrunk for structural, expanded for correlated)
                                       vcvTable$getColumn("se")$setVisible(is_auto_mode && useBootstrap)
                                       vcvTable$getColumn("cil")$setVisible(is_auto_mode && useBootstrap)
                                       vcvTable$getColumn("ciu")$setVisible(is_auto_mode && useBootstrap)
                                       vcvTable$getColumn("p")$setVisible(is_auto_mode && useBootstrap)
                                       vcvTable$getColumn("t")$setVisible(is_auto_mode && useBootstrap)
                                       
                                       
                                       
                                       # 5.5 VIF and HTMT Tables
                                       has_vif <- FALSE
                                       has_htmt <- FALSE
                                       if (!is.null(asses)) {
                                         for (g in groups) {
                                           a <- if (is_multi) asses[[g]] else asses[[1]]
                                           if (is.null(a)) next
                                           
                                           # VIF for mode B Weights
                                           vif_mat <- a$VIF_modeB
                                           if (!is.null(vif_mat) && is.matrix(vif_mat)) {
                                             for (construct in rownames(vif_mat)) {
                                               for (indicator in colnames(vif_mat)) {
                                                 val <- vif_mat[construct, indicator]
                                                 if (!is.null(val) && !is.na(val) && !is.nan(val) && val > 0) {
                                                   has_vif <- TRUE
                                                   vifModeBTable$addRow(rowKey=paste0(g, "_vif_", construct, "_", indicator), values=list(
                                                     group = g,
                                                     construct = construct,
                                                     indicator = indicator,
                                                     vif = as.numeric(val)
                                                   ))
                                                 }
                                               }
                                             }
                                           }
                                           
                                           # Discriminant Validity (HTMT & HTMT2)
                                           htmts <- a$HTMT$htmts
                                           htmt2s <- a$HTMT2$htmts
                                           if (!is.null(htmts) && is.matrix(htmts) && nrow(htmts) >= 2) {
                                             constructs <- colnames(htmts)
                                             for (i in 1:(length(constructs)-1)) {
                                               for (j in (i+1):length(constructs)) {
                                                 c1 <- constructs[i]
                                                 c2 <- constructs[j]
                                                 val_htmt <- htmts[j, i]
                                                 val_htmt2 <- if (!is.null(htmt2s) && is.matrix(htmt2s) && all(dim(htmt2s) == dim(htmts))) htmt2s[j, i] else NA
                                                 
                                                 has_htmt <- TRUE
                                                 htmtTable$addRow(rowKey=paste0(g, "_htmt_", c1, "_", c2), values=list(
                                                   group = g,
                                                   c1 = c1,
                                                   c2 = c2,
                                                   htmt = as.numeric(val_htmt),
                                                   htmt2 = as.numeric(val_htmt2)
                                                 ))
                                               }
                                             }
                                           }
                                         }
                                       }
                                       vifModeBTable$setVisible(has_vif)
                                       htmtTable$setVisible(has_htmt)
                                       
                                       # 5.6 Mediation / Indirect and Total Effects Table
                                       has_mediation <- FALSE
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         ind_df <- s$Estimates$Effect_estimates$Indirect_effect
                                         if (!is.null(ind_df) && is.data.frame(ind_df) && nrow(ind_df) > 0) {
                                           has_mediation <- TRUE
                                           break
                                         }
                                       }
                                       
                                       if (has_mediation) {
                                         mediationTable$setVisible(TRUE)
                                         
                                         # Helper function to find all indirect paths in structural model
                                         find_all_indirect_paths <- function(structural) {
                                           constructs <- colnames(structural)
                                           paths <- list()
                                           
                                           dfs <- function(current_path) {
                                             last_node <- current_path[length(current_path)]
                                             # descendants of last_node are rows where structural[, last_node] == 1
                                             descendants <- rownames(structural)[structural[, last_node] == 1]
                                             for (d in descendants) {
                                               if (!(d %in% current_path)) {
                                                 new_path <- c(current_path, d)
                                                 if (length(new_path) >= 3) {
                                                   paths[[length(paths) + 1]] <<- new_path
                                                 }
                                                 dfs(new_path)
                                               }
                                             }
                                           }
                                           
                                           for (c in constructs) {
                                             dfs(c)
                                           }
                                           return(paths)
                                         }
                                         
                                         for (g in groups) {
                                           s <- if (is_multi) summs[[g]] else summs[[1]]
                                           res_group <- if (is_multi) out[[g]] else out
                                           structural <- res_group$Information$Model$structural
                                           
                                           paths <- find_all_indirect_paths(structural)
                                           tot_df <- as.data.frame(s$Estimates$Effect_estimates$Total_effect)
                                           
                                           # Keep track of unique (lhs, rhs) pairs that have indirect paths
                                           indirect_pairs <- list()
                                           
                                           # Populate Specific Indirect Paths
                                           if (length(paths) > 0) {
                                             for (p_idx in seq_along(paths)) {
                                               path <- paths[[p_idx]]
                                               pred <- path[1]
                                               outc <- path[length(path)]
                                               
                                               # Add to unique indirect pairs
                                               pair_key <- paste0(outc, " ~ ", pred)
                                               indirect_pairs[[pair_key]] <- TRUE
                                               
                                               # Compute estimate
                                               estimate <- 1
                                               for (i in 1:(length(path)-1)) {
                                                 estimate <- estimate * res_group$Estimates$Path_estimates[path[i+1], path[i]]
                                               }
                                               
                                               se <- NA
                                               t_val <- NA
                                               p_val <- NA
                                               cil <- NA
                                               ciu <- NA
                                               
                                               if (isTRUE(useBootstrap)) {
                                                 resampled_paths <- NULL
                                                 tryCatch({
                                                   resampled_paths <- res_group$Estimates$Estimates_resample$Estimates1$Path_estimates$Resampled
                                                 }, error = function(e) NULL)
                                                 
                                                 if (!is.null(resampled_paths) && is.matrix(resampled_paths) && nrow(resampled_paths) > 0) {
                                                   boot_vals <- rep(1, nrow(resampled_paths))
                                                   valid_path <- TRUE
                                                   for (i in 1:(length(path)-1)) {
                                                     col_name <- paste0(path[i+1], " ~ ", path[i])
                                                     if (col_name %in% colnames(resampled_paths)) {
                                                       boot_vals <- boot_vals * resampled_paths[, col_name]
                                                     } else {
                                                       valid_path <- FALSE
                                                       break
                                                     }
                                                   }
                                                   if (valid_path) {
                                                     se <- sd(boot_vals)
                                                     cil <- quantile(boot_vals, probs = 0.025, na.rm = TRUE)
                                                     ciu <- quantile(boot_vals, probs = 0.975, na.rm = TRUE)
                                                     t_val <- estimate / se
                                                     p_val <- 2 * pnorm(abs(t_val), lower.tail = FALSE)
                                                   }
                                                 }
                                               }
                                               
                                               path_label <- paste(path, collapse = " -> ")
                                               mediationTable$addRow(rowKey=paste0(g, "_indir_path_", path_label), values=list(
                                                 group = g,
                                                 type = path_label,
                                                 rhs = pred,
                                                 lhs = outc,
                                                 estimate = as.numeric(estimate),
                                                 se = as.numeric(se),
                                                 t = as.numeric(t_val),
                                                 p = as.numeric(p_val),
                                                 cil = as.numeric(cil),
                                                 ciu = as.numeric(ciu)
                                               ))
                                             }
                                           }
                                           
                                           # Populate Total Effects (only those with corresponding indirect effects)
                                           if (!is.null(tot_df) && nrow(tot_df) > 0) {
                                             for (i in 1:nrow(tot_df)) {
                                               row <- tot_df[i, ]
                                               if (!isTRUE(indirect_pairs[[as.character(row$Name)]])) next
                                               
                                               parts <- strsplit(as.character(row$Name), " ~ ")[[1]]
                                               lhs <- parts[1]
                                               rhs <- parts[2]
                                               
                                               se <- if ("Std_err" %in% names(row)) row$Std_err else NA
                                               t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                               p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                               cil <- if ("CI_percentile.95%L" %in% names(row)) row$`CI_percentile.95%L` else NA
                                               ciu <- if ("CI_percentile.95%U" %in% names(row)) row$`CI_percentile.95%U` else NA
                                               
                                               mediationTable$addRow(rowKey=paste0(g, "_tot_", row$Name), values=list(
                                                 group = g,
                                                 type = "Total effect",
                                                 rhs = rhs,
                                                 lhs = lhs,
                                                 estimate = as.numeric(row$Estimate),
                                                 se = as.numeric(se),
                                                 t = as.numeric(t_val),
                                                 p = as.numeric(p_val),
                                                 cil = as.numeric(cil),
                                                 ciu = as.numeric(ciu)
                                               ))
                                             }
                                           }
                                         }
                                       } else {
                                         mediationTable$setVisible(FALSE)
                                       }
                                       
                                       # 6. Construct Reliability Table
                                       has_rel <- FALSE
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         a <- if (!is.null(asses)) (if (is_multi) asses[[g]] else asses[[1]]) else NULL
                                         
                                         rhoA_vec <- s$Estimates$Reliabilities
                                         alpha_vec <- if (!is.null(a)) a$Reliability$Cronbachs_alpha else NULL
                                         rhoC_vec <- if (!is.null(a)) a$Reliability$Joereskogs_rho else NULL
                                         ave_vec <- if (!is.null(a)) a$AVE else NULL
                                         
                                         for (construct in all_constructs) {
                                           c_type <- c_types[construct]
                                           if (is.null(c_type) || as.character(c_type) != "Common factor") next
                                           
                                           has_rel <- TRUE
                                           alpha <- if (!is.null(alpha_vec) && construct %in% names(alpha_vec)) alpha_vec[construct] else NA
                                           rhoC <- if (!is.null(rhoC_vec) && construct %in% names(rhoC_vec)) rhoC_vec[construct] else NA
                                           rhoA <- if (!is.null(rhoA_vec) && construct %in% names(rhoA_vec)) rhoA_vec[construct] else NA
                                           ave <- if (!is.null(ave_vec) && construct %in% names(ave_vec)) ave_vec[construct] else NA
                                           
                                           reliabilityTable$addRow(rowKey=paste0(g, "_rel_", construct), values=list(
                                             group = g,
                                             construct = construct,
                                             alpha = as.numeric(alpha),
                                             rhoC = as.numeric(rhoC),
                                             rhoA = as.numeric(rhoA),
                                             ave = as.numeric(ave)
                                           ))
                                         }
                                       }
                                       reliabilityTable$setVisible(has_rel)
                                       
                                       # 7. Linear Prediction Benchmark Table
                                       if (isTRUE(runLinearBench)) {
                                         predictTable <- self$results$predictTable
                                         predictTable$deleteRows()
                                         
                                         folds <- self$options$predictFolds
                                         if (is.null(folds)) folds <- 10
                                         
                                         runif(1)
                                         pred_res <- tryCatch({
                                           cSEM::predict(.object=out, .benchmark="lm", .cv_folds=folds, .r=1, .seed=123)
                                         }, error=function(e) {
                                           NULL
                                         })
                                         
                                         if (!is.null(pred_res)) {
                                           if (inherits(pred_res, "cSEMPredict_multi")) {
                                             pred_groups <- names(pred_res)
                                           } else {
                                             pred_groups <- c("")
                                           }
                                           
                                           for (g in pred_groups) {
                                             sub_pred <- if (g == "") pred_res else pred_res[[g]]
                                             metrics_df <- sub_pred$Prediction_metrics
                                             
                                             if (!is.null(metrics_df) && nrow(metrics_df) > 0) {
                                               for (i in 1:nrow(metrics_df)) {
                                                 row <- metrics_df[i, ]
                                                 predictTable$addRow(rowKey=paste0(g, "_pred_", row$Name), values=list(
                                                   group = g,
                                                   indicator = as.character(row$Name),
                                                   maeTarget = as.numeric(row$MAE_target),
                                                   maeBench = as.numeric(row$MAE_benchmark),
                                                   rmseTarget = as.numeric(row$RMSE_target),
                                                   rmseBench = as.numeric(row$RMSE_benchmark),
                                                   q2 = as.numeric(row$Q2_predict)
                                                 ))
                                               }
                                             }
                                           }
                                         }
                                       }
                                       
                                       # 8. Multigroup Analysis Output
                                       mga_output <- ""
                                       mga_header <- ""
                                       if (!is.null(multGroupVar) && multGroupVar != "") {
                                         mga_header <- paste0("\n(Multigroup Analysis by: ", multGroupVar, ")\n")
                                         perm_R <- if (is.numeric(bootstrapSamples) && bootstrapSamples > 0)
                                           bootstrapSamples else 50
                                         mga_output <- tryCatch({
                                           mga_res   <- cSEM::testMGD(.object=out, .R_permutation=perm_R)
                                           mga_print <- capture.output(print(mga_res))
                                           paste("\n--- Multi-Group Difference Tests ---\n",
                                                 "Permutations:", perm_R, "\n\n",
                                                 paste(mga_print, collapse="\n"))
                                         }, error=function(e) {
                                           paste("\n--- Multi-Group Difference Error ---\n", e$message)
                                         })
                                         
                                         csemOutput$setContent(paste0(mga_header, "\n", mga_output))
                                         csemOutput$setVisible(TRUE)
                                       } else {
                                         csemOutput$setContent("")
                                         csemOutput$setVisible(FALSE)
                                       }
                                       
                                     }, error=function(e) {
                                       # If there's an error, show it in the output table and make it visible
                                       msg <- e$message
                                       if (estimationModel == "GSCA") {
                                         # Shown as a table note (like the MAXVAR message) rather than a
                                         # thrown error, so jamovi does not dump a debug stack trace
                                         summaryTable$setNote("gsca_error", "Currently, GSCA does not work with a correlated model (CCA/CFA).")
                                         csemOutput$setContent("")
                                         csemOutput$setVisible(FALSE)
                                       } else if (grepl("do not appear in the structural model", msg)) {
                                         missing_constructs <- gsub(".*structural model:\\s*", "", msg)
                                         clean_msg <- paste0(
                                           "Please use all constructs defined in the 'Structural Roles' window. ",
                                           "The following construct(s) are missing from the structural model: ", missing_constructs
                                         )
                                         jmvcore::reject(clean_msg)
                                       } else {
                                         csemOutput$setContent(paste0(
                                           "An error occurred during cSEM analysis (", estimationModel, "):\n\n",
                                           e$message, "\n\nModel sent to cSEM:\n", model
                                         ))
                                         csemOutput$setVisible(TRUE)
                                       }
                                     })
                                     
                                     # Plot generation removed in light version
                                   }
                                 )
)

# Null-coalescing operator (available in R >= 4.4, polyfill for older R)
`%||%` <- function(a, b) if (!is.null(a) && !identical(a, "")) a else b
