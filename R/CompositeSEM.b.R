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
                                     
                                     # NEW in 1.7: Higher-order constructs participate in the structural
                                     # model exactly like ordinary constructs, so their labels have to be
                                     # part of the construct pool. A HOC only counts as defined once it has
                                     # both a name and at least one lower-order component assigned.
                                     keep_hoc_labels <- function(items) {
                                       out <- character(0)
                                       if (!is.null(items)) {
                                         for (item in items) {
                                           label <- item$label %||% ""
                                           comps <- item$components %||% list()
                                           if (nzchar(label) && length(comps) > 0)
                                             out <- c(out, label)
                                         }
                                       }
                                       out
                                     }
                                     
                                     all_labels <- c(
                                       keep_labels(self$options$latent),
                                       keep_labels(self$options$composite),
                                       keep_hoc_labels(self$options$hoc)
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
                                     
                                     # UPDATED in 1.7: The role vectors are filtered in the order in which
                                     # the user dropped the constructs into the role boxes, not in the order
                                     # in which the constructs happen to be defined. This keeps each
                                     # endogenous construct aligned with its own block of directional path
                                     # terms in 'endogenousTerms', which is indexed positionally.
                                     endogenous_labels <- endogenous_selected[endogenous_selected %in% all_labels]
                                     exogenous_labels  <- exogenous_selected[exogenous_selected %in% all_labels]
                                     
                                     endo_terms <- self$options$endogenousTerms
                                     structural_parts <- character(0)
                                     used_labels <- unique(c(endogenous_labels, exogenous_labels))
                                     
                                     for (i in seq_along(endogenous_labels)) {
                                       if (i > length(endo_terms)) break
                                       
                                       block_list <- endo_terms[[i]]
                                       if (is.null(block_list) || length(block_list) == 0) next
                                       
                                       # NEW in 1.7: Interaction support in the directional path blocks. A
                                       # multi-element term whose members are all defined constructs is
                                       # rendered as a cSEM interaction term ("X.M"); every other
                                       # multi-element term keeps the classic ":" separator. Validation and
                                       # bookkeeping therefore have to be performed on the individual
                                       # components of a term rather than on the term as a whole.
                                       predictor_names <- sapply(block_list, function(t) {
                                         nm <- if (is.list(t)) unlist(t) else as.character(t)
                                         if (length(nm) > 1 && all(nm %in% all_labels)) {
                                           paste(nm, collapse = ".")
                                         } else {
                                           paste(nm, collapse = ":")
                                         }
                                       })
                                       predictor_names <- predictor_names[nzchar(predictor_names)]

                                       valid_predictors <- sapply(predictor_names, function(pn) {
                                         sub_parts <- unlist(strsplit(pn, "\\."))
                                         all(sub_parts %in% all_labels)
                                       })
                                       predictor_names <- predictor_names[valid_predictors]
                                       predictor_names <- predictor_names[predictor_names != endogenous_labels[i]]
                                       if (length(predictor_names) == 0) next

                                       # Each component of an interaction term counts as a used construct,
                                       # so its measurement model is not dropped as unused.
                                       for (pn in predictor_names) {
                                         used_labels <- unique(c(used_labels, unlist(strsplit(pn, "\\."))))
                                       }
                                       
                                       lhs <- jmvcore::composeTerm(endogenous_labels[i])
                                       rhs <- paste(
                                         sapply(predictor_names, jmvcore::composeTerm),
                                         collapse = " + "
                                       )
                                       structural_parts <- c(structural_parts, paste0(lhs, " ~ ", rhs))
                                     }
                                     
                                     # NEW in 1.7: Moderation analysis. When the user has selected a valid
                                     # (Y, X, M) triplet, the main effects and the product term X.M are
                                     # injected into the structural equation of Y. If Y already has an
                                     # equation, the missing terms are appended to it; otherwise a new
                                     # equation is created. Because the terms are added here rather than in
                                     # the interface, the interaction is guaranteed to be part of the model
                                     # that is actually estimated.
                                     if (isTRUE(self$options$moderationEnabled)) {
                                       mod_dep <- self$options$modDependent %||% ""
                                       mod_ind <- self$options$modIndependent %||% ""
                                       mod_mod <- self$options$modModerator %||% ""
                                       
                                       if (nzchar(mod_dep) && nzchar(mod_ind) && nzchar(mod_mod) &&
                                           mod_dep %in% all_labels && mod_ind %in% all_labels && mod_mod %in% all_labels) {
                                         
                                         inter_term <- paste0(mod_ind, ".", mod_mod)
                                         used_labels <- unique(c(used_labels, mod_dep, mod_ind, mod_mod))
                                         
                                         dep_idx <- grep(paste0("^", jmvcore::composeTerm(mod_dep), " ~ "), structural_parts)
                                         if (length(dep_idx) > 0) {
                                           curr_eq <- structural_parts[dep_idx]
                                           for (term_to_add in c(mod_ind, mod_mod, inter_term)) {
                                             if (!grepl(term_to_add, curr_eq, fixed = TRUE)) {
                                               curr_eq <- paste0(curr_eq, " + ", term_to_add)
                                             }
                                           }
                                           structural_parts[dep_idx] <- curr_eq
                                         } else {
                                           new_eq <- paste0(jmvcore::composeTerm(mod_dep), " ~ ", mod_ind, " + ", mod_mod, " + ", inter_term)
                                           structural_parts <- c(structural_parts, new_eq)
                                         }
                                       }
                                     }
                                     
                                     is_auto_mode <- (length(structural_parts) == 0)
                                     if (is_auto_mode)
                                       used_labels <- all_labels

                                     # NEW in 1.7: Whenever a higher-order construct is part of the model,
                                     # its lower-order components are implicitly used as well and must be
                                     # kept in the measurement model even if they were never assigned a
                                     # structural role of their own.
                                     if (!is.null(self$options$hoc)) {
                                       for (item in self$options$hoc) {
                                         label <- item$label %||% ""
                                         comps <- item$components %||% list()
                                         if (label %in% used_labels && length(comps) > 0) {
                                           used_labels <- unique(c(used_labels, unlist(comps)))
                                         }
                                       }
                                     }
                                     
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
                                     # NEW in 1.7: Show the required citation as a note underneath the first
                                     # output table, so it remains visible independently of the References
                                     # section of the jamovi results.
                                     summaryTable$setNote("citation", "If you use Composite-SEM you must cite it as follows: Al Arfaj, A. A., & Alamer, A. A. (2026). Composite-SEM: A Jamovi Module (Software). https://github.com/AbdullahAlarfaj101/Composite-SEM")
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
                                     
                                     # NEW in 1.7: List the higher-order constructs in the Model Structure
                                     # Summary alongside the ordinary constructs, showing their measurement
                                     # mode and the lower-order components that make them up.
                                     if (!is.null(self$options$hoc)) {
                                       for (item in self$options$hoc) {
                                         if (length(item$components) > 0 && nzchar(item$label) && is_used(item$label)) {
                                            c_type <- if (item$type == "composite" || item$type == "<~") "Higher-Order (Composite)" else "Higher-Order (Latent)"
                                           summaryTable$addRow(rowKey=item$label, values=list(
                                             type       = c_type,
                                             construct  = item$label,
                                             indicators = paste(item$components, collapse=', ')
                                           ))
                                         }
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

                                     # NEW in 1.7: Detect whether a hierarchical component model has been
                                     # requested. A higher-order construct only counts once it carries both
                                     # a name and at least one lower-order component.
                                     has_hoc <- FALSE
                                     if (!is.null(self$options$hoc) && length(self$options$hoc) > 0) {
                                       for (item in self$options$hoc) {
                                         if (length(item$components) > 0 && nzchar(item$label %||% "")) {
                                           has_hoc <- TRUE
                                           break
                                         }
                                       }
                                     }

                                     # cSEM's second-order routines require an inner model, so a
                                     # higher-order construct cannot be estimated in the automatic
                                     # correlated-model (CFA/CCA) mode.
                                     if (has_hoc && is_auto_mode) {
                                       stop("Higher-Order Constructs (HOC) require structural relationships. Please specify the structural paths for your HOC in 'Structural Roles' or 'Directional Paths'.")
                                     }
                                     
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

                                      # C) Higher-Order Constructs
                                      # NEW in 1.7: Append the second-order part of the measurement model.
                                      # A reflective ("Latent") higher-order construct is written with the
                                      # "=~" operator and therefore introduces a common factor, whereas a
                                      # formative ("Composite") one is written with "<~". The name/type
                                      # lookup built here is reused later to route the estimates into the
                                      # dedicated HOC output table and to draw the correct node shape in
                                      # the SEM diagram. ('has_hoc' has already been determined above.)
                                      hoc_names <- character(0)
                                      hoc_types <- character(0)
                                      if (!is.null(self$options$hoc) && length(self$options$hoc) > 0) {
                                        for (item in self$options$hoc) {
                                          if (length(item$components) > 0 && nzchar(item$label %||% "")) {
                                            hoc_names <- c(hoc_names, item$label)
                                            hoc_types[item$label] <- if (item$type %in% c("composite", "<~")) "composite" else "latent"
                                          }
                                        }
                                      }
                                      if (!is.null(self$options$hoc)) {
                                        for (item in self$options$hoc) {
                                          if (length(item$components) > 0 && nzchar(item$label)) {
                                            if (is_used(item$label)) {
                                              safe_label <- jmvcore::composeTerm(item$label)
                                              safe_comps <- sapply(item$components, jmvcore::composeTerm)
                                              op <- if (item$type == "composite" || item$type == "<~") " <~ " else " =~ "
                                              measurement_parts <- c(measurement_parts,
                                                                     paste0(safe_label, op, paste(safe_comps, collapse=" + ")))
                                              active_constructs <- c(active_constructs, item$label)
                                              if (op == " =~ ") hasCommonFactors <- TRUE
                                            } else {
                                              ignored_vars <- c(ignored_vars, item$label)
                                            }
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
                                     outerHocTable           <- self$results$outerHocTable
                                     structuralTable         <- self$results$structuralTable
                                     mediationTable          <- self$results$mediationTable
                                     vcvTable                <- self$results$vcvTable
                                     htmtTable               <- self$results$htmtTable
                                     vifModeBTable           <- self$results$vifModeBTable
                                     reliabilityTable        <- self$results$reliabilityTable
                                     
                                     # Clear existing data from tables
                                     cleaningSummaryTable$deleteRows()
                                     infoTable$deleteRows()
                                     fitTable$deleteRows()
                                     exactFitTable$deleteRows()
                                     outerCompositesTable$deleteRows()
                                     outerCommonFactorsTable$deleteRows()
                                     if (!is.null(outerHocTable)) outerHocTable$deleteRows()
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
                                     # NEW in 1.7: Show the required citation as a note underneath the first
                                     # output table, so it remains visible independently of the References
                                     # section of the jamovi results.
                                     summaryTable$setNote("citation", "If you use Composite-SEM you must cite it as follows: Al Arfaj, A. A., & Alamer, A. A. (2026). Composite-SEM: A Jamovi Module (Software). https://github.com/AbdullahAlarfaj101/Composite-SEM")
                                     summaryTable$setNote("maxvar_error", NULL)
                                     summaryTable$setNote("gsca_error", NULL)
                                     summaryTable$setNote("min_constructs_note", NULL)
                                     
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
                                       return()
                                     }
                                     
                                     # UPDATED in 1.7: A model with fewer than two constructs is now stopped
                                     # before cSEM is invoked and reported as a note on the Model Structure
                                     # Summary. Up to 1.5 this produced a "No constructs defined or matched."
                                     # row in the Model Information table, which was easy to miss and left
                                     # an otherwise empty results panel behind.
                                     if (length(measurement_parts) < 2) {
                                       summaryTable$setNote("min_constructs_note", "At least two constructs with defined indicators are required to run model estimation.")
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

                                       # NEW in 1.7: Hierarchical component models. Passing
                                       # .approach_2ndorder switches cSEM into its two-stage machinery and
                                       # makes csem() return a 'cSEMResults_2ndorder' object whose
                                       # First_stage/Second_stage results are unwrapped further below.
                                       if (has_hoc) {
                                         csem_args$.approach_2ndorder <- self$options$hocApproach %||% "2stage"
                                       }

                                       if (!is.null(multGroupVar) && multGroupVar != "")
                                         csem_args$.id <- multGroupVar
                                       
                                       if (estimationModel == 'PLS') {
                                         csem_args$.PLS_weight_scheme_inner <-
                                           if (is_auto_mode) "factorial" else "path"

                                         # NEW in 1.5: Per-construct PLS weighting modes. The 'modes' option
                                         # (a list of {construct, mode} pairs maintained by the UI) is
                                         # converted into a named list and passed to cSEM via .PLS_modes,
                                         # allowing each composite to be estimated with Mode A (correlation
                                         # weights) or Mode B (regression weights) individually.
                                         user_modes <- self$options$modes
                                         if (!is.null(user_modes) && length(user_modes) > 0) {
                                           pls_modes <- list()
                                           for (m_item in user_modes) {
                                             c_name <- m_item$construct
                                             c_mode <- m_item$mode
                                             if (!is.null(c_name) && c_name != "" && !is.null(c_mode) && c_mode != "") {
                                               pls_modes[[c_name]] <- c_mode
                                             }
                                           }
                                           if (length(pls_modes) > 0) {
                                             csem_args$.PLS_modes <- pls_modes
                                           }
                                         }
                                       } else if (estimationModel == 'GSCA') {
                                         csem_args$.approach_weights <- 'GSCA'
                                       } else {
                                         csem_args$.approach_weights <- estimationModel
                                       }
                                       
                                       if (isTRUE(useBootstrap)) {
                                         csem_args$.resample_method <- "bootstrap"
                                         csem_args$.R <- bootstrapSamples
                                       }

                                       # NEW in 1.5: Robust estimation. When enabled, indicator correlations
                                       # are computed with the Spearman rank correlation instead of Pearson,
                                       # making the estimation robust against nonnormal data and outliers.
                                       if (isTRUE(self$options$robustEst)) {
                                         csem_args$.approach_cor_robust <- "spearman"
                                       }
                                       
                                       csem_args$.disattenuate <- !isTRUE(self$options$disattenuate)
                                       
                                       out <- do.call(cSEM::csem, csem_args)

                                       # NEW in 1.7: Two-stage result handling. For a hierarchical component
                                       # model cSEM returns a 'cSEMResults_2ndorder' container holding a
                                       # First_stage and a Second_stage result. The estimates that describe
                                       # the model as the user specified it live in the second stage, so
                                       # from here on 'out' refers to the stage that carries the estimates,
                                       # while 'full_out' keeps the complete container for the places that
                                       # need information from both stages (e.g. the construct types).
                                       # For an ordinary first-order model both objects are identical.
                                       full_out <- out
                                       out      <- if (inherits(out, "cSEMResults_2ndorder")) out$Second_stage else out
                                       eval_out <- out

                                       # NEW in 1.5: User-selectable bootstrap confidence interval type.
                                       # The chosen CI construction method (Percentile, Basic, BC or BCa)
                                       # is forwarded to cSEM::summarize() via its .ci argument.
                                       boot_ci_type <- self$options$bootCI
                                       summ <- cSEM::summarize(out, .ci = boot_ci_type)

                                       # Check if it is multi-group
                                       # Assign into the method's captured env so the plot block (outside
                                       # tryCatch) can read these variables
                                       is_multi <- inherits(eval_out, "cSEMResults_multi")
                                       if (is_multi) {
                                         groups <- names(eval_out)
                                         summs  <- summ
                                       } else {
                                         groups <- c("")
                                         summs  <- list(summ)
                                         names(summs) <- ""
                                       }
                                       .run_env$is_multi <- is_multi
                                       .run_env$groups   <- groups
                                       .run_env$summs    <- summs

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
                                       
                                       inf_res <- if (isTRUE(useBootstrap)) tryCatch(cSEM::infer(out), error = function(e) NULL) else NULL
                                       
                                       # 1. Model Info Table
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         eval_s <- if (inherits(s, "cSEMSummarize_2ndorder")) s$Second_stage else s
                                         n_obs <- nrow(eval_s$Information$Data)
                                         conv <- eval_s$Information$Weight_info$Convergence_status
                                         iters <- eval_s$Information$Weight_info$Number_iterations
                                         
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
                                       
                                       # NEW in 1.5: Extended Estimation Information table. The rows below
                                       # document the robust estimation setting and the full bootstrap
                                       # configuration (enabled/disabled, number of samples, CI type) so the
                                       # analysis setup is fully transparent and reproducible from the output.
                                       infoTable$addRow(rowKey="robust_est", values=list(
                                         group = "",
                                         property = "Spearman rank correlation (robust estimation)",
                                         value = if (isTRUE(self$options$robustEst)) "Yes" else "No"
                                       ))
                                       
                                       # Add bootstrap info
                                       use_boot <- isTRUE(self$options$useBootstrap)
                                       infoTable$addRow(rowKey="use_bootstrap", values=list(
                                         group = "",
                                         property = "Bootstrapping",
                                         value = if (use_boot) "Yes" else "No"
                                       ))
                                       
                                       if (use_boot) {
                                         infoTable$addRow(rowKey="boot_samples", values=list(
                                           group = "",
                                           property = "Number of bootstrap samples",
                                           value = as.character(self$options$bootR)
                                         ))
                                         
                                         ci_map <- list(
                                           CI_percentile = "Percentile",
                                           CI_basic = "Basic",
                                           CI_bc = "Bias-corrected (BC)",
                                           CI_bca = "Bias-corrected and accelerated (BCa)"
                                         )
                                         ci_title <- ci_map[[self$options$bootCI]]
                                         if (is.null(ci_title)) ci_title <- self$options$bootCI
                                         
                                         infoTable$addRow(rowKey="boot_ci_type", values=list(
                                           group = "",
                                           property = "Bootstrap confidence interval type",
                                           value = ci_title
                                         ))
                                       }
                                       
                                       # UPDATED in 1.7: Construct types are now resolved from the interface
                                       # definitions first and only then overridden with what cSEM actually
                                       # estimated. Up to 1.5 the types were read from the cSEM result
                                       # alone, which is not sufficient for hierarchical models: their
                                       # construct types are split over the first and the second stage, and
                                       # the second stage renames the carried-over constructs with a
                                       # '_temp' suffix. Both stages are therefore merged and the suffix is
                                       # stripped before the lookup table is built.
                                        # Get construct types from UI definitions first
                                        ui_c_types <- character(0)
                                        for (item in self$options$latent) {
                                          if (nzchar(item$label %||% "")) ui_c_types[item$label] <- "Common factor"
                                        }
                                        for (item in self$options$composite) {
                                          if (nzchar(item$label %||% "")) ui_c_types[item$label] <- "Composite"
                                        }
                                        for (item in self$options$hoc) {
                                          if (nzchar(item$label %||% "")) {
                                            ui_c_types[item$label] <- if (item$type == "composite" || item$type == "<~") "Composite" else "Common factor"
                                          }
                                        }

                                        # 'full_out' is the untouched cSEM container, so for a hierarchical
                                        # model both stages remain reachable here.
                                        full_out_obj <- if (exists("full_out", envir = .run_env)) .run_env$full_out else NULL
                                        csem_obj <- if (!is.null(full_out_obj)) full_out_obj else eval_out
                                        if (!is.null(csem_obj) && inherits(csem_obj, "cSEMResults_multi")) {
                                          csem_obj <- csem_obj[[1]]
                                        }

                                        csem_c_types <- character(0)
                                        if (!is.null(csem_obj)) {
                                          if (inherits(csem_obj, "cSEMResults_2ndorder")) {
                                            m1 <- unclass(csem_obj$First_stage$Information$Model)
                                            m2 <- unclass(csem_obj$Second_stage$Information$Model)
                                            csem_c_types <- c(m1$construct_type, m2$construct_type)
                                          } else {
                                            m1 <- unclass(csem_obj$Information$Model)
                                            csem_c_types <- m1$construct_type
                                          }
                                        }

                                        if (!is.null(csem_c_types) && length(csem_c_types) > 0) {
                                          names(csem_c_types) <- gsub("_temp", "", names(csem_c_types), fixed = TRUE)
                                          csem_c_types <- csem_c_types[!duplicated(names(csem_c_types))]
                                        } else {
                                          csem_c_types <- character(0)
                                        }

                                        # Merge csem_c_types into ui_c_types (cSEM takes precedence)
                                        c_types <- ui_c_types
                                        if (length(csem_c_types) > 0) {
                                          for (nm in names(csem_c_types)) {
                                            c_types[nm] <- csem_c_types[nm]
                                          }
                                        }
                                        all_constructs <- unique(names(c_types))

                                        # NEW in 1.7: Small accessors shared by the reporting sections below.
                                        # get_c_type()  - construct type lookup that tolerates '_temp' names.
                                        # safe_num()    - returns NULL instead of NA/NaN, so jamovi renders
                                        #                 an empty cell rather than a missing-value marker.
                                        get_c_type <- function(c_name) {
                                          if (is.null(c_types) || length(c_types) == 0) return("")
                                          c_clean <- gsub("_temp", "", c_name, fixed = TRUE)
                                          val <- unname(c_types[c_clean])
                                          if (is.null(val) || is.na(val) || length(val) == 0) return("")
                                          as.character(val)
                                        }

                                        safe_num <- function(val) {
                                          if (is.null(val) || is.na(val) || is.nan(val)) return(NULL)
                                          as.numeric(val)
                                        }

                                        # Helper to unwrap cSEMSummarize_2ndorder to its Second_stage
                                        # for sections that only need stage-2 estimates (paths, VCV, effects, reliability)
                                        get_eval_s <- function(s) {
                                          if (inherits(s, "cSEMSummarize_2ndorder")) {
                                            if (inherits(s, "cSEMSummarize_resampled")) s else s$Second_stage
                                          } else {
                                            s
                                          }
                                        }

                                        # NEW in 1.7: Bootstrap inference fallback. cSEM::summarize() does
                                        # not attach standard errors and confidence intervals to every
                                        # quantity (in particular not to the second stage of a hierarchical
                                        # model). Whenever a value is missing from the summary, it is looked
                                        # up in the cSEM::infer() result instead. The lookup tolerates the
                                        # '_temp' suffix used by the two-stage approach and, for two-sided
                                        # relations, also tries the reversed "A ~ B" / "B ~ A" spelling.
                                        get_inf_stat <- function(inf_obj, section, metric, target_name, ci_bound = "L") {
                                          if (is.null(inf_obj) || is.null(inf_obj[[section]])) return(NA)
                                          mat <- inf_obj[[section]][[metric]]
                                          if (is.null(mat)) return(NA)
                                          
                                          clean_target <- gsub("_temp", "", target_name, fixed = TRUE)
                                          
                                          if (is.matrix(mat) || is.data.frame(mat)) {
                                            cols <- colnames(mat)
                                            clean_cols <- gsub("_temp", "", cols, fixed = TRUE)
                                            idx <- which(clean_cols == clean_target)
                                            if (length(idx) == 0) {
                                              parts <- unlist(strsplit(clean_target, " ~ "))
                                              if (length(parts) == 2) {
                                                rev_target <- paste0(parts[2], " ~ ", parts[1])
                                                idx <- which(clean_cols == rev_target)
                                              }
                                            }
                                            if (length(idx) > 0) {
                                              row_name <- if (ci_bound == "L") "95%L" else "95%U"
                                              if (row_name %in% rownames(mat)) return(mat[row_name, idx[1]])
                                              return(mat[1, idx[1]])
                                            }
                                          } else if (is.vector(mat)) {
                                            nms <- names(mat)
                                            clean_nms <- gsub("_temp", "", nms, fixed = TRUE)
                                            idx <- which(clean_nms == clean_target)
                                            if (length(idx) == 0) {
                                              parts <- unlist(strsplit(clean_target, " ~ "))
                                              if (length(parts) == 2) {
                                                rev_target <- paste0(parts[2], " ~ ", parts[1])
                                                idx <- which(clean_nms == rev_target)
                                              }
                                            }
                                            if (length(idx) > 0) return(mat[idx[1]])
                                          }
                                          return(NA)
                                        }

                                        # NEW in 1.7: Construct-indexed lookup that is tolerant of the
                                        # '_temp' suffix introduced by the two-stage approach. The second
                                        # argument holds the corresponding first-stage vector and is
                                        # consulted whenever the second stage does not report the quantity
                                        # (for instance the reliability of a lower-order construct).
                                        get_stat <- function(vec, vec_first, c_name) {
                                          if (!is.null(vec)) {
                                            nms <- names(vec)
                                            clean_nms <- gsub("_temp", "", nms, fixed = TRUE)
                                            if (c_name %in% clean_nms) {
                                              val <- vec[which(clean_nms == c_name)[1]]
                                              if (!is.null(val) && !is.na(val) && !is.nan(val)) return(val)
                                            }
                                          }
                                          if (!is.null(vec_first)) {
                                            nms <- names(vec_first)
                                            clean_nms <- gsub("_temp", "", nms, fixed = TRUE)
                                            if (c_name %in% clean_nms) {
                                              val <- vec_first[which(clean_nms == c_name)[1]]
                                              if (!is.null(val) && !is.na(val) && !is.nan(val)) return(val)
                                            }
                                          }
                                          return(NA)
                                        }
                                       
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
                                        # UPDATED in 1.7: Rows are routed to their table by the construct
                                        # definition made in the interface instead of by the construct type
                                        # reported by cSEM. This keeps the split stable for hierarchical
                                        # models, where the second stage renames constructs and can report
                                        # a type that differs from the one the user specified. Loadings and
                                        # weights of higher-order constructs are diverted to the dedicated
                                        # outerHocTable, and any standard error, confidence interval, t and
                                        # p value that summarize() left empty is recovered from the
                                        # cSEM::infer() results.
                                        ui_latent_names <- character(0)
                                        for (item in self$options$latent) {
                                          if (length(item$vars) > 0 && nzchar(item$label %||% "")) ui_latent_names <- c(ui_latent_names, item$label)
                                        }

                                        ui_composite_names <- character(0)
                                        for (item in self$options$composite) {
                                          if (length(item$vars) > 0 && nzchar(item$label %||% "")) ui_composite_names <- c(ui_composite_names, item$label)
                                        }
                                        
                                        for (g in groups) {
                                          s <- if (is_multi) summs[[g]] else summs[[1]]
                                          if (inherits(s, "cSEMSummarize_2ndorder")) {
                                            loadings_df <- rbind(
                                              as.data.frame(s$First_stage$Estimates$Loading_estimates),
                                              as.data.frame(s$Second_stage$Estimates$Loading_estimates)
                                            )
                                            weights_df <- rbind(
                                              as.data.frame(s$First_stage$Estimates$Weight_estimates),
                                              as.data.frame(s$Second_stage$Estimates$Weight_estimates)
                                            )
                                          } else {
                                            loadings_df <- as.data.frame(s$Estimates$Loading_estimates)
                                            weights_df  <- as.data.frame(s$Estimates$Weight_estimates)
                                          }
                                          
                                          showCompositeLoadings <- self$options$showCompositeLoadings
                                          
                                          has_hoc_rows <- FALSE
                                          if (nrow(loadings_df) > 0) {
                                            for (i in 1:nrow(loadings_df)) {
                                              row <- loadings_df[i, ]
                                              parts <- strsplit(as.character(row$Name), " =~ | <~ ")[[1]]
                                              raw_construct <- parts[1]
                                              indicator <- gsub("_temp", "", parts[2], fixed = TRUE)
                                              construct <- gsub("_temp", "", raw_construct, fixed = TRUE)
                                              
                                              # Skip stage-2 internal identity loading rows created by cSEM (e.g. Int_temp =~ Int)
                                              if (construct == indicator) next
                                              
                                              inf_obj <- if (!is.null(inf_res)) (if (is_multi) inf_res[[g]] else inf_res) else NULL
                                              boot_ci_type <- self$options$bootCI
                                              if (is.null(boot_ci_type)) boot_ci_type <- "CI_percentile"
                                              
                                              se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Loading_estimates", "sd", row$Name)
                                              cil <- if ("CI_percentile.95%L" %in% names(row) && !is.na(row$`CI_percentile.95%L`)) row$`CI_percentile.95%L` else get_inf_stat(inf_obj, "Loading_estimates", boot_ci_type, row$Name, "L")
                                              ciu <- if ("CI_percentile.95%U" %in% names(row) && !is.na(row$`CI_percentile.95%U`)) row$`CI_percentile.95%U` else get_inf_stat(inf_obj, "Loading_estimates", boot_ci_type, row$Name, "U")
                                              
                                              if (!is.na(se) && !is.null(se) && se > 0) {
                                                t_val <- as.numeric(row$Estimate) / se
                                                p_val <- 2 * pnorm(-abs(t_val))
                                              } else {
                                                t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                                p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                              }

                                              # Route HOC loadings/weights to dedicated outerHocTable based on HOC type
                                               if (construct %in% hoc_names) {
                                                 h_type <- unname(hoc_types[construct])
                                                 if (is.null(h_type) || is.na(h_type)) h_type <- "latent"
                                                 if (h_type == "latent") {
                                                   has_hoc_rows <- TRUE
                                                   if (!is.null(outerHocTable)) {
                                                     outerHocTable$addRow(rowKey=paste0(g, "_hoc_loading_", construct, "_", indicator), values=clean_list(list(
                                                       group = g,
                                                       construct = construct,
                                                       indicator = indicator,
                                                       estimate = as.numeric(row$Estimate),
                                                       se = as.numeric(se),
                                                       t = as.numeric(t_val),
                                                       p = as.numeric(p_val),
                                                       cil = as.numeric(cil),
                                                       ciu = as.numeric(ciu)
                                                     )))
                                                   }
                                                 }
                                                 next
                                               }
                                              
                                              if (construct %in% ui_composite_names) {
                                                # If composite and showCompositeLoadings is checked, add loadings to composites
                                                if (isTRUE(showCompositeLoadings)) {
                                                  outerCompositesTable$addRow(rowKey=paste0(g, "_loading_", construct, "_", indicator), values=clean_list(list(
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
                                                  )))
                                                }
                                                next
                                              }
                                              
                                              outerCommonFactorsTable$addRow(rowKey=paste0(g, "_loading_", construct, "_", indicator), values=clean_list(list(
                                                group = g,
                                                construct = construct,
                                                indicator = indicator,
                                                estimate = as.numeric(row$Estimate),
                                                se = as.numeric(se),
                                                t = as.numeric(t_val),
                                                p = as.numeric(p_val),
                                                cil = as.numeric(cil),
                                                ciu = as.numeric(ciu)
                                              )))
                                            }
                                          }
                                          
                                          if (nrow(weights_df) > 0) {
                                            for (i in 1:nrow(weights_df)) {
                                              row <- weights_df[i, ]
                                              parts <- strsplit(as.character(row$Name), " =~ | <~ ")[[1]]
                                              construct <- parts[1]
                                              indicator <- parts[2]
                                              
                                              inf_obj <- if (!is.null(inf_res)) (if (is_multi) inf_res[[g]] else inf_res) else NULL
                                              boot_ci_type <- self$options$bootCI
                                              if (is.null(boot_ci_type)) boot_ci_type <- "CI_percentile"
                                              
                                              se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Weight_estimates", "sd", row$Name)
                                              cil <- if ("CI_percentile.95%L" %in% names(row) && !is.na(row$`CI_percentile.95%L`)) row$`CI_percentile.95%L` else get_inf_stat(inf_obj, "Weight_estimates", boot_ci_type, row$Name, "L")
                                              ciu <- if ("CI_percentile.95%U" %in% names(row) && !is.na(row$`CI_percentile.95%U`)) row$`CI_percentile.95%U` else get_inf_stat(inf_obj, "Weight_estimates", boot_ci_type, row$Name, "U")
                                              
                                              if (!is.na(se) && !is.null(se) && se > 0) {
                                                t_val <- as.numeric(row$Estimate) / se
                                                p_val <- 2 * pnorm(-abs(t_val))
                                              } else {
                                                t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                                p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                              }
                                              
                                              if (construct %in% hoc_names) {
                                                h_type <- unname(hoc_types[construct])
                                                if (is.null(h_type) || is.na(h_type)) h_type <- "latent"
                                                if (h_type == "composite") {
                                                  has_hoc_rows <- TRUE
                                                  if (!is.null(outerHocTable)) {
                                                    outerHocTable$addRow(rowKey=paste0(g, "_hoc_weight_", construct, "_", indicator), values=clean_list(list(
                                                      group = g,
                                                      construct = construct,
                                                      indicator = indicator,
                                                      estimate = as.numeric(row$Estimate),
                                                      se = as.numeric(se),
                                                      t = as.numeric(t_val),
                                                      p = as.numeric(p_val),
                                                      cil = as.numeric(cil),
                                                      ciu = as.numeric(ciu)
                                                    )))
                                                  }
                                                }
                                                next
                                              }

                                              # Only include constructs defined as "Composite"
                                              # (UPDATED in 1.7: the decision is based on the construct
                                              # definitions made in the interface; any construct that is
                                              # not declared reflective is treated as a composite, which
                                              # also covers the auxiliary constructs cSEM creates for
                                              # interaction terms.)
                                              if (construct %in% ui_composite_names || !(construct %in% ui_latent_names)) {
                                                outerCompositesTable$addRow(rowKey=paste0(g, "_weight_", construct, "_", indicator), values=clean_list(list(
                                                  group = g,
                                                  construct = construct,
                                                  indicator = indicator,
                                                  relation = "Weight",
                                                  estimate = safe_num(row$Estimate),
                                                  se = safe_num(se),
                                                  t = safe_num(t_val),
                                                  p = safe_num(p_val),
                                                  cil = safe_num(cil),
                                                  ciu = safe_num(ciu)
                                                )))
                                              }
                                            }
                                          }
                                        }
                                        
                                        # UPDATED in 1.7: Outer model table visibility. In a hierarchical
                                        # model the second stage no longer produces meaningful first-order
                                        # loadings and weights, so the ordinary outer model tables are
                                        # hidden and only the dedicated HOC table is shown. Otherwise each
                                        # table is shown when the interface actually defines a construct of
                                        # the corresponding kind.
                                        if (has_hoc || length(hoc_names) > 0) {
                                          outerCompositesTable$setVisible(FALSE)
                                          outerCommonFactorsTable$setVisible(FALSE)
                                          if (!is.null(outerHocTable)) {
                                            outerHocTable$setVisible(has_hoc_rows)
                                            outerHocTable$getColumn("se")$setVisible(useBootstrap)
                                            outerHocTable$getColumn("t")$setVisible(useBootstrap)
                                            outerHocTable$getColumn("p")$setVisible(useBootstrap)
                                            outerHocTable$getColumn("cil")$setVisible(useBootstrap)
                                            outerHocTable$getColumn("ciu")$setVisible(useBootstrap)
                                          }
                                        } else {
                                          outerCompositesTable$setVisible(length(ui_composite_names) > 0)
                                          outerCommonFactorsTable$setVisible(length(ui_latent_names) > 0)
                                          if (!is.null(outerHocTable)) {
                                            outerHocTable$setVisible(FALSE)
                                          }
                                        }

                                        # Inference columns of the common factor table are only meaningful
                                        # when bootstrapping is enabled.
                                        outerCommonFactorsTable$getColumn("se")$setVisible(useBootstrap)


                                       # 4. Structural Path Estimates Table
                                       # UPDATED in 1.7: R2 and adjusted R2 are taken from the cSEM::assess()
                                       # result rather than from the summary object, because the summary of
                                       # a two-stage model does not carry them. Missing standard errors and
                                       # confidence intervals are again recovered from cSEM::infer().
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         es <- get_eval_s(s)
                                         paths_df <- as.data.frame(es$Estimates$Path_estimates)
                                         a <- if (!is.null(asses)) (if (is_multi) asses[[g]] else asses[[1]]) else NULL
                                         r2_vec <- if (!is.null(a)) a$R2 else NULL
                                         r2adj_vec <- if (!is.null(a)) a$R2_adj else NULL

                                         if (nrow(paths_df) > 0) {
                                           for (i in 1:nrow(paths_df)) {
                                             row <- paths_df[i, ]
                                             parts <- strsplit(as.character(row$Name), " ~ ")[[1]]
                                             lhs <- gsub("_temp", "", parts[1], fixed = TRUE)
                                             rhs <- gsub("_temp", "", parts[2], fixed = TRUE)
                                             
                                              inf_obj <- if (!is.null(inf_res)) (if (is_multi) inf_res[[g]] else inf_res) else NULL
                                              boot_ci_type <- self$options$bootCI
                                              if (is.null(boot_ci_type)) boot_ci_type <- "CI_percentile"
                                              
                                              se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Path_estimates", "sd", row$Name)
                                              cil <- if ("CI_percentile.95%L" %in% names(row) && !is.na(row$`CI_percentile.95%L`)) row$`CI_percentile.95%L` else get_inf_stat(inf_obj, "Path_estimates", boot_ci_type, row$Name, "L")
                                              ciu <- if ("CI_percentile.95%U" %in% names(row) && !is.na(row$`CI_percentile.95%U`)) row$`CI_percentile.95%U` else get_inf_stat(inf_obj, "Path_estimates", boot_ci_type, row$Name, "U")
                                              
                                              if (!is.na(se) && !is.null(se) && se > 0) {
                                                t_val <- as.numeric(row$Estimate) / se
                                                p_val <- 2 * pnorm(-abs(t_val))
                                              } else {
                                                t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                                p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                              }
                                              
                                              r2 <- get_stat(r2_vec, NULL, lhs)
                                              r2adj <- get_stat(r2adj_vec, NULL, lhs)
                                             
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
                                       
                                       # 5. Construct Correlations Table (Construct VCV Matrix)
                                       # UPDATED in 1.7: The construct names are only cleaned of the '_temp'
                                       # suffix for display; the directional-path lookup still uses the raw
                                       # cSEM names, so a pair that is connected by a regression path is
                                       # reliably excluded from the correlation table. The table is now
                                       # hidden when no correlation pair survives that filter, instead of
                                       # being shown empty.
                                       has_correlations <- FALSE
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         es <- get_eval_s(s)
                                         vcv <- es$Estimates$Construct_VCV
                                         if (is.null(vcv) || length(vcv) == 0) next
                                         
                                         constructs <- colnames(vcv)
                                         if (length(constructs) < 2) next
                                         
                                         path_names <- character(0)
                                         if (!is.null(es$Estimates$Path_estimates) && nrow(as.data.frame(es$Estimates$Path_estimates)) > 0) {
                                           path_names <- as.character(es$Estimates$Path_estimates$Name)
                                         }
                                         
                                         exo_df <- if (!is.null(es$Estimates$Exo_construct_correlation)) as.data.frame(es$Estimates$Exo_construct_correlation) else NULL
                                         
                                         for (i in 1:(length(constructs)-1)) {
                                           for (j in (i+1):length(constructs)) {
                                             c1 <- gsub("_temp", "", constructs[i], fixed = TRUE)
                                             c2 <- gsub("_temp", "", constructs[j], fixed = TRUE)
                                             
                                             # Skip if there is a directional regression path between c1 and c2
                                             path_key1 <- paste0(constructs[i], " ~ ", constructs[j])
                                             path_key2 <- paste0(constructs[j], " ~ ", constructs[i])
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
                                             has_correlations <- TRUE
                                           }
                                         }
                                       }
                                       
                                       # Set construct correlations visibility
                                       vcvTable$setVisible(has_correlations)
                                       
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
                                                     construct = gsub("_temp", "", construct, fixed = TRUE),
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
                                                 c1 <- gsub("_temp", "", constructs[i], fixed = TRUE)
                                                 c2 <- gsub("_temp", "", constructs[j], fixed = TRUE)
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
                                         es <- get_eval_s(s)
                                         ind_df <- es$Estimates$Effect_estimates$Indirect_effect
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
                                           # UPDATED in 1.7: The indirect paths are read from the structural
                                           # matrix of the estimated (second) stage, while the total effects
                                           # are taken from the summary of the very same group. Both are
                                           # resolved per group so that a multigroup analysis reports each
                                           # group's own effects.
                                           s <- if (is_multi) summs[[g]] else summs[[1]]
                                           es <- get_eval_s(s)
                                           a <- if (!is.null(asses)) (if (is_multi) asses[[g]] else asses[[1]]) else NULL
                                           res_group <- if (is_multi) out[[g]] else out
                                           eval_res <- if (inherits(res_group, "cSEMResults_2ndorder")) res_group$Second_stage else res_group
                                           structural <- eval_res$Information$Model$structural

                                           paths <- find_all_indirect_paths(structural)
                                           tot_df <- as.data.frame(es$Estimates$Effect_estimates$Total_effect)
                                           
                                           # Keep track of unique (lhs, rhs) pairs that have indirect paths
                                           indirect_pairs <- list()
                                           
                                           # Populate Specific Indirect Paths
                                           if (length(paths) > 0) {
                                             for (p_idx in seq_along(paths)) {
                                               path <- paths[[p_idx]]
                                               pred <- gsub("_temp", "", path[1], fixed = TRUE)
                                               outc <- gsub("_temp", "", path[length(path)], fixed = TRUE)
                                               
                                               # Add to unique indirect pairs
                                               pair_key <- paste0(path[length(path)], " ~ ", path[1])
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
                                               
                                                if (is.na(se) && isTRUE(useBootstrap)) {
                                                  inf_obj <- if (!is.null(inf_res)) (if (is_multi) inf_res[[g]] else inf_res) else NULL
                                                  boot_ci_type <- self$options$bootCI
                                                  if (is.null(boot_ci_type)) boot_ci_type <- "CI_percentile"
                                                  
                                                  se <- get_inf_stat(inf_obj, "Indirect_effect", "sd", pair_key)
                                                  cil <- get_inf_stat(inf_obj, "Indirect_effect", boot_ci_type, pair_key, "L")
                                                  ciu <- get_inf_stat(inf_obj, "Indirect_effect", boot_ci_type, pair_key, "U")
                                                  if (!is.na(se) && !is.null(se) && se > 0) {
                                                    t_val <- as.numeric(estimate) / se
                                                    p_val <- 2 * pnorm(-abs(t_val))
                                                  }
                                                }
                                                
                                                clean_path <- sapply(path, function(p) gsub("_temp", "", p, fixed = TRUE))
                                                path_label <- paste(clean_path, collapse = " -> ")
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
                                                lhs <- gsub("_temp", "", parts[1], fixed = TRUE)
                                                rhs <- gsub("_temp", "", parts[2], fixed = TRUE)
                                                
                                                inf_obj <- if (!is.null(inf_res)) (if (is_multi) inf_res[[g]] else inf_res) else NULL
                                                boot_ci_type <- self$options$bootCI
                                                if (is.null(boot_ci_type)) boot_ci_type <- "CI_percentile"
                                                
                                                se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Total_effect", "sd", row$Name)
                                                cil <- if ("CI_percentile.95%L" %in% names(row) && !is.na(row$`CI_percentile.95%L`)) row$`CI_percentile.95%L` else get_inf_stat(inf_obj, "Total_effect", boot_ci_type, row$Name, "L")
                                                ciu <- if ("CI_percentile.95%U" %in% names(row) && !is.na(row$`CI_percentile.95%U`)) row$`CI_percentile.95%U` else get_inf_stat(inf_obj, "Total_effect", boot_ci_type, row$Name, "U")
                                               
                                               if (!is.na(se) && !is.null(se) && se > 0) {
                                                 t_val <- as.numeric(row$Estimate) / se
                                                 p_val <- 2 * pnorm(-abs(t_val))
                                               } else {
                                                 t_val <- if ("t_stat" %in% names(row)) row$t_stat else NA
                                                 p_val <- if ("p_value" %in% names(row)) row$p_value else NA
                                               }
                                               
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

                                       # NEW in 1.7: The inference columns of the structural table only
                                       # carry information when bootstrapping is enabled, so they are
                                       # hidden otherwise. This applies to every model, with or without
                                       # mediation paths.
                                       structuralTable$getColumn("se")$setVisible(useBootstrap)
                                       structuralTable$getColumn("t")$setVisible(useBootstrap)
                                       structuralTable$getColumn("p")$setVisible(useBootstrap)
                                       structuralTable$getColumn("cil")$setVisible(useBootstrap)
                                       structuralTable$getColumn("ciu")$setVisible(useBootstrap)


                                       # 6. Construct Reliability Table
                                       # UPDATED in 1.7: For a hierarchical model the reliability of the
                                       # lower-order constructs is only available from the first stage, so
                                       # cSEM::assess() is additionally run on the first stage and used as a
                                       # fallback for every coefficient the second stage does not report.
                                       has_rel <- FALSE
                                       for (g in groups) {
                                         s <- if (is_multi) summs[[g]] else summs[[1]]
                                         es <- get_eval_s(s)
                                         res_group <- if (is_multi) out[[g]] else out
                                         a <- if (!is.null(asses)) (if (is_multi) asses[[g]] else asses[[1]]) else NULL
                                         a_first <- if (inherits(res_group, "cSEMResults_2ndorder")) tryCatch(cSEM::assess(res_group$First_stage), error = function(e) NULL) else a
                                         
                                         rhoA_vec <- if (inherits(s, "cSEMSummarize_2ndorder")) {
                                            c(s$First_stage$Estimates$Reliabilities, s$Second_stage$Estimates$Reliabilities)
                                          } else {
                                            es$Estimates$Reliabilities
                                          }
                                         alpha_vec       <- if (!is.null(a)) a$Reliability$Cronbachs_alpha else NULL
                                         alpha_first_vec <- if (!is.null(a_first)) a_first$Reliability$Cronbachs_alpha else NULL
                                         
                                         rhoC_vec       <- if (!is.null(a)) a$Reliability$Joereskogs_rho else NULL
                                         rhoC_first_vec <- if (!is.null(a_first)) a_first$Reliability$Joereskogs_rho else NULL
                                         
                                         ave_vec       <- if (!is.null(a)) a$AVE else NULL
                                         ave_first_vec <- if (!is.null(a_first)) a_first$AVE else NULL
                                         
                                         for (construct in all_constructs) {
                                           clean_c <- gsub("_temp", "", construct, fixed = TRUE)
                                           c_type_str <- get_c_type(clean_c)
                                           if (c_type_str != "Common factor") next
                                           
                                           # Reliability coefficients are looked up with the shared
                                           # get_stat() helper defined above: it matches the construct name
                                           # against the '_temp'-stripped names of the second-stage vector
                                           # and falls back to the first-stage vector when the second stage
                                           # does not report the coefficient.
                                           alpha <- get_stat(alpha_vec, alpha_first_vec, clean_c)
                                           rhoC  <- get_stat(rhoC_vec, rhoC_first_vec, clean_c)
                                           rhoA  <- get_stat(rhoA_vec, NULL, clean_c)
                                           ave   <- get_stat(ave_vec, ave_first_vec, clean_c)
                                           
                                           has_rel <- TRUE
                                           reliabilityTable$addRow(rowKey=paste0(g, "_rel_", clean_c), values=list(
                                             group = g,
                                             construct = clean_c,
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
                                         # NEW in 1.5: Structured Multi-Group Analysis. Group differences are
                                         # tested with cSEM::testMGD() using the user-selected test methods
                                         # (Henseler PLS-MGA, Sarstedt, Chin, Keil, Nitzl) and the results are
                                         # presented in three native jamovi tables (overall decision, run
                                         # overview/metadata, per-parameter comparisons) instead of the raw
                                         # preformatted text dump used in earlier versions.
                                         mgaDecisionTable <- self$results$mgaDecisionTable
                                         mgaOverviewTable <- self$results$mgaOverviewTable
                                         mgaTable         <- self$results$mgaTable
                                         
                                         mgaDecisionTable$deleteRows()
                                         mgaOverviewTable$deleteRows()
                                         mgaTable$deleteRows()
                                         
                                         if (!is.null(multGroupVar) && multGroupVar != "") {
                                           perm_R <- if (is.numeric(bootstrapSamples) && bootstrapSamples > 0)
                                             bootstrapSamples else 50
                                           
                                           
                                           # Patch cSEM::testMGD dynamically to support models without structural paths (correlated models)
                                           testMGD_patched <- cSEM::testMGD
                                           body_str <- deparse(body(testMGD_patched))
                                           target_idx <- grep("n <- nrow\\(path_resamples\\)", body_str)
                                           if (length(target_idx) > 0) {
                                             body_str[target_idx] <- "n <- if (!is.null(path_resamples)) nrow(path_resamples) else if (!is.null(loading_resamples)) nrow(loading_resamples) else nrow(weight_resamples)"
                                             body(testMGD_patched) <- parse(text = paste(body_str, collapse = "\n"))
                                           }

                                            # Determine MGA test methods to run
                                            mga_methods <- character(0)
                                            if (isTRUE(self$options$mgaHenseler)) mga_methods <- c(mga_methods, "Henseler")
                                            if (isTRUE(self$options$mgaSarstedt)) mga_methods <- c(mga_methods, "Sarstedt")
                                            if (isTRUE(self$options$mgaChin))     mga_methods <- c(mga_methods, "Chin")
                                            if (isTRUE(self$options$mgaKeil))     mga_methods <- c(mga_methods, "Keil")
                                            if (isTRUE(self$options$mgaNitzl))    mga_methods <- c(mga_methods, "Nitzl")

                                            mga_res   <- NULL
                                            mga_error <- NULL
                                            if (length(mga_methods) > 0) {
                                              tryCatch({
                                                mga_res <- testMGD_patched(.object=out, .R_permutation=perm_R, .approach_mgd=mga_methods)
                                              }, error=function(e) {
                                                mga_error <<- e$message
                                              })
                                            }
                                           
                                           if (!is.null(mga_error)) {
                                             clean_msg <- paste0(
                                               "Multi-Group Analysis failed: ", mga_error, 
                                               ". Please verify your grouping variable has adequate sample size per group, check for missing values, or choose a different missing data handling method."
                                             )
                                             jmvcore::reject(clean_msg)
                                           } else if (!is.null(mga_res)) {
                                             
                                             mgaDecisionTable$setVisible(TRUE)
                                             mgaOverviewTable$setVisible(TRUE)
                                             mgaTable$setVisible(TRUE)
                                             
                                             # A) Populate Overall Decision Table
                                             methods_to_check <- mga_methods
                                             for (m in methods_to_check) {
                                               res_m <- mga_res[[m]]
                                               if (is.null(res_m)) next
                                               
                                               overall_dec <- res_m$Decision_overall
                                               if (!is.null(overall_dec) && length(overall_dec) > 0) {
                                                 dec_val <- overall_dec[[1]][[1]]
                                                 dec_str <- if (isTRUE(dec_val)) "Do not reject" else "Reject"
                                                 
                                                 mgaDecisionTable$addRow(rowKey=m, values=list(
                                                   test     = m,
                                                   decision = dec_str
                                                 ))
                                               }
                                             }
                                             
                                             # B) Populate Overview Table
                                             info <- mga_res$Information
                                             
                                             # Total permutation runs / admissibility
                                             mgaOverviewTable$addRow(rowKey="tot_perm", values=list(
                                               property = "Total Permutation Runs",
                                               value    = as.character(info$Information_permutation$Total_runs)
                                             ))
                                             mgaOverviewTable$addRow(rowKey="admiss_perm", values=list(
                                               property = "Admissible Permutation Results",
                                               value    = as.character(info$Information_permutation$Number_admissibles)
                                             ))
                                             mgaOverviewTable$addRow(rowKey="perm_seed", values=list(
                                               property = "Permutation Seed",
                                               value    = as.character(info$Information_permutation$Permutation_seed)
                                             ))
                                             
                                             # Groups info
                                             g_names <- info$Group_names
                                             g_obs   <- info$Number_of_observations
                                             
                                             for (g_idx in seq_along(g_names)) {
                                               g_name <- g_names[g_idx]
                                               # Group N
                                               mgaOverviewTable$addRow(rowKey=paste0("obs_", g_name), values=list(
                                                 property = paste0("Observations per group - '", g_name, "'"),
                                                 value    = as.character(g_obs[g_name])
                                               ))
                                               
                                               # Bootstrap admissibility
                                               g_admiss <- info$Information_bootstrap$Number_admissibles[[g_name]]
                                               if (!is.null(g_admiss)) {
                                                 mgaOverviewTable$addRow(rowKey=paste0("admiss_boot_", g_name), values=list(
                                                   property = paste0("Admissible bootstrap results - '", g_name, "'"),
                                                   value    = as.character(g_admiss)
                                                 ))
                                               }
                                               
                                               # Bootstrap seed
                                               g_seed <- info$Information_bootstrap$Bootstrap_seed[[g_name]]
                                               if (!is.null(g_seed)) {
                                                 mgaOverviewTable$addRow(rowKey=paste0("seed_boot_", g_name), values=list(
                                                   property = paste0("Bootstrap seed - '", g_name, "'"),
                                                   value    = as.character(g_seed)
                                                 ))
                                               }
                                             }
                                             
                                             # C) Populate Comparison Results Table
                                             row_counter <- 1
                                             for (m in methods_to_check) {
                                               res_m <- mga_res[[m]]
                                               if (is.null(res_m)) next
                                               
                                               stat_obj <- res_m$Test_statistic
                                               p_obj    <- res_m$P_value$none
                                               dec_obj  <- res_m$Decision$none$`5%`
                                               
                                               if (is.list(stat_obj)) {
                                                 # Nested by comparison pair (e.g. Chin, Keil, Henseler, Nitzl)
                                                 for (pair in names(stat_obj)) {
                                                   stats  <- stat_obj[[pair]]
                                                   p_vals <- p_obj[[pair]]
                                                   decs   <- dec_obj[[pair]]
                                                   
                                                   for (param in names(stats)) {
                                                     stat_val <- as.numeric(stats[param])
                                                     p_val    <- as.numeric(p_vals[param])
                                                     dec_bool <- decs[param]
                                                     dec_str  <- if (isTRUE(dec_bool)) "Do not reject" else "Reject"
                                                     
                                                     mgaTable$addRow(rowKey=as.character(row_counter), values=list(
                                                       comparison = pair,
                                                       parameter  = param,
                                                       test       = m,
                                                       stat       = stat_val,
                                                       p          = p_val,
                                                       decision   = dec_str
                                                     ))
                                                     row_counter <- row_counter + 1
                                                   }
                                                 }
                                               } else if (is.numeric(stat_obj)) {
                                                 # Direct vector (e.g. Sarstedt)
                                                 for (param in names(stat_obj)) {
                                                   stat_val <- as.numeric(stat_obj[param])
                                                   p_val    <- as.numeric(p_obj[param])
                                                   dec_bool <- dec_obj[param]
                                                   dec_str  <- if (isTRUE(dec_bool)) "Do not reject" else "Reject"
                                                   
                                                   mgaTable$addRow(rowKey=as.character(row_counter), values=list(
                                                     comparison = "Overall",
                                                     parameter  = param,
                                                     test       = m,
                                                     stat       = stat_val,
                                                     p          = p_val,
                                                     decision   = dec_str
                                                   ))
                                                   row_counter <- row_counter + 1
                                                 }
                                               }
                                             }
                                           } else {
                                             mgaDecisionTable$setVisible(FALSE)
                                             mgaOverviewTable$setVisible(FALSE)
                                             mgaTable$setVisible(FALSE)
                                           }
                                         } else {
                                           mgaDecisionTable$setVisible(FALSE)
                                           mgaOverviewTable$setVisible(FALSE)
                                           mgaTable$setVisible(FALSE)
                                         }
                                       
                                     }, error=function(e) {
                                       # If there's an error, report it as a clean, user-friendly message
                                       # (UPDATED in 1.5: errors are now raised via jmvcore::reject() with
                                       # actionable guidance instead of being dumped into a raw text output)
                                       msg <- e$message
                                       if (estimationModel == "GSCA" && is_auto_mode) {
                                         # Shown as a table note (like the MAXVAR message) rather than a
                                         # thrown error, so jamovi does not dump a debug stack trace
                                         # (UPDATED in 1.5: the note is now limited to auto-mode/correlated
                                         # models; GSCA failures in structural models are reported through
                                         # the standard error path below instead of being masked)
                                         summaryTable$setNote("gsca_error", "Currently, GSCA does not work with a correlated model (CCA/CFA).")
                                       } else if (grepl("At least two constructs required", msg, ignore.case = TRUE)) {
                                         # NEW in 1.7: An under-specified model is reported as a table note
                                         # rather than as an estimation error, so the output stays clean
                                         # while the user is still assembling the measurement model.
                                         if (length(measurement_parts) < 2) {
                                           summaryTable$setNote("min_constructs_note", "At least two constructs with defined indicators are required to run model estimation.")
                                           return()
                                         }
                                       } else if (grepl("do not appear in the structural model", msg)) {
                                         missing_constructs <- gsub(".*structural model:\\s*", "", msg)
                                         clean_msg <- paste0(
                                           "Please use all constructs defined in the 'Structural Roles' window. ",
                                         "The following construct(s) are missing from the structural model: ", missing_constructs
                                         )
                                         jmvcore::reject(clean_msg)
                                       } else {
                                         clean_msg <- paste0(
                                            "An error occurred during estimation (", estimationModel, "): ",
                                            e$message, ". Please check your model specification, ensure indicator variables exist in the data set, or verify that your grouping variable has adequate sample size."
                                          )
                                          jmvcore::reject(clean_msg)
                                       }
                                     })
                                     
                                     # Retrieve variables populated inside tryCatch
                                     if (exists("groups", envir = .run_env) && length(.run_env$groups) > 0) {
                                       groups   <- .run_env$groups
                                       summs    <- .run_env$summs
                                       is_multi <- .run_env$is_multi
                                     }
                                     
                                      # --- Moderation Analysis ---
                                      # NEW in 1.7: Probing of the interaction effect that .buildStructural()
                                      # added to the model. Because cSEM works with standardised construct
                                      # scores, the conditional (simple) effect of X on Y at a moderator
                                      # value z is
                                      #     slope(z)  = b1 + b3 * z
                                      #     se(z)     = sqrt(se1^2 + (z * se3)^2)
                                      # where b1/se1 belong to the X -> Y path and b3/se3 to the
                                      # interaction path X.M -> Y. The same expression evaluated over a
                                      # grid of moderator values yields the Johnson-Neyman (floodlight)
                                      # band. The block runs outside the estimation tryCatch, so a
                                      # moderation failure can never suppress the main results.
                                      if (isTRUE(self$options$moderationEnabled)) {
                                        modGroup <- self$results$moderationGroup
                                        simpleTable <- modGroup$simpleEffectsTable
                                        simpleTable$deleteRows()
                                        
                                        modDep <- self$options$modDependent %||% ""
                                        modInd <- self$options$modIndependent %||% ""
                                        modMod <- self$options$modModerator %||% ""
                                        modLevels <- self$options$modLevels %||% "sd"
                                        
                                        # If the selectors have not been filled in yet, recover the triplet
                                        # from the estimated model by looking for the first path whose
                                        # predictor is a product term ("X.M").
                                        if (!nzchar(modDep) || !nzchar(modInd) || !nzchar(modMod)) {
                                          s_first <- if (is_multi) summs[[groups[1]]] else summs[[1]]
                                          if (!is.null(s_first$Estimates$Path_estimates)) {
                                            pe <- as.data.frame(s_first$Estimates$Path_estimates)
                                            for (r_idx in seq_len(nrow(pe))) {
                                              pname <- as.character(pe$Name[r_idx])
                                              parts <- unlist(strsplit(pname, " ~ "))
                                              if (length(parts) == 2 && grepl(".", parts[2], fixed = TRUE)) {
                                                modDep <- parts[1]
                                                terms <- unlist(strsplit(parts[2], "\\."))
                                                if (length(terms) == 2) {
                                                  modInd <- terms[1]
                                                  modMod <- terms[2]
                                                  break
                                                }
                                              }
                                            }
                                          }
                                        }
                                        
                                        if (nzchar(modDep) && nzchar(modInd) && nzchar(modMod)) {
                                          s_first <- if (is_multi) summs[[groups[1]]] else summs[[1]]
                                          pe <- as.data.frame(s_first$Estimates$Path_estimates)
                                          
                                          # cSEM may report a product term in either order ("X.M" or
                                          # "M.X"), so both spellings are accepted when looking up a path.
                                          get_path_val <- function(y, x) {
                                            p1 <- paste0(y, " ~ ", x)
                                            p2 <- paste0(y, " ~ ", paste(rev(unlist(strsplit(x, ".", fixed = TRUE))), collapse = "."))
                                            row_idx <- which(pe$Name == p1 | pe$Name == p2)
                                            if (length(row_idx) > 0) pe[row_idx[1], ] else NULL
                                          }
                                          
                                          r_ind <- get_path_val(modDep, modInd)
                                          r_mod <- get_path_val(modDep, modMod)
                                          r_int <- get_path_val(modDep, paste0(modInd, ".", modMod))
                                          if (is.null(r_int)) {
                                            r_int <- get_path_val(modDep, paste0(modMod, ".", modInd))
                                          }
                                          
                                          if (!is.null(r_ind) && !is.null(r_int)) {
                                            modGroup$setVisible(TRUE)
                                            simpleTable$setVisible(isTRUE(self$options$showSimpleEffectsTable))
                                            
                                            b1 <- as.numeric(r_ind$Estimate)
                                            b2 <- if (!is.null(r_mod)) as.numeric(r_mod$Estimate) else 0
                                            b3 <- as.numeric(r_int$Estimate)
                                            
                                            se1 <- if ("Std_err" %in% names(r_ind) && !is.na(r_ind$Std_err)) as.numeric(r_ind$Std_err) else if ("Std. error" %in% names(r_ind) && !is.na(r_ind$`Std. error`)) as.numeric(r_ind$`Std. error`) else 0.1
                                            se3 <- if ("Std_err" %in% names(r_int) && !is.na(r_int$Std_err)) as.numeric(r_int$Std_err) else if ("Std. error" %in% names(r_int) && !is.na(r_int$`Std. error`)) as.numeric(r_int$`Std. error`) else 0.1
                                            
                                            # Probing points on the standardised moderator scale. The
                                            # percentile option uses the z-scores of the 16th/50th/84th
                                            # percentile of the standard normal distribution.
                                            z_vals <- if (modLevels == "percentile") c(-0.994, 0, 0.994) else c(-1, 0, 1)
                                            z_labels <- if (modLevels == "percentile") c("16th Percentile (Low)", "50th Percentile (Mean)", "84th Percentile (High)") else c("-1 SD (Low)", "Mean (0)", "+1 SD (High)")
                                            
                                            for (k in seq_along(z_vals)) {
                                              zv <- z_vals[k]
                                              lbl <- z_labels[k]
                                              
                                              slope <- b1 + b3 * zv
                                              se_slope <- sqrt(se1^2 + (zv * se3)^2)
                                              t_val <- slope / se_slope
                                              p_val <- 2 * pnorm(abs(t_val), lower.tail = FALSE)
                                              cil <- slope - 1.96 * se_slope
                                              ciu <- slope + 1.96 * se_slope
                                              
                                              simpleTable$addRow(rowKey = paste0("mod_lvl_", k), values = list(
                                                level = lbl,
                                                modValue = zv,
                                                slope = slope,
                                                se = se_slope,
                                                t = t_val,
                                                p = p_val,
                                                cil = cil,
                                                ciu = ciu
                                              ))
                                            }
                                            
                                            # Simple slopes state: one predicted line per moderator level,
                                            # evaluated over +/- 2 SD of the (standardised) predictor.
                                            x_seq <- seq(-2, 2, length.out = 50)
                                            df_lines_list <- list()
                                            for (k in seq_along(z_vals)) {
                                              zv <- z_vals[k]
                                              lbl <- z_labels[k]
                                              y_pred <- b1 * x_seq + b2 * zv + b3 * (x_seq * zv)
                                              df_lines_list[[k]] <- data.frame(x = x_seq, y = y_pred, Level = factor(lbl, levels = z_labels))
                                            }
                                            df_lines <- do.call(rbind, df_lines_list)
                                            modGroup$simpleEffectsPlot$setState(list(
                                              df_lines = df_lines,
                                              dep_name = modDep,
                                              ind_name = modInd,
                                              mod_name = modMod
                                            ))
                                            
                                            # Floodlight state: the conditional slope and its 95% confidence
                                            # band over +/- 3 SD of the (standardised) moderator. The points
                                            # at which the band crosses zero are the Johnson-Neyman
                                            # boundaries of the region of significance.
                                            m_seq <- seq(-3, 3, length.out = 100)
                                            slopes_flood <- b1 + b3 * m_seq
                                            se_flood <- sqrt(se1^2 + (m_seq * se3)^2)
                                            df_flood <- data.frame(
                                              m = m_seq,
                                              slope = slopes_flood,
                                              lower = slopes_flood - 1.96 * se_flood,
                                              upper = slopes_flood + 1.96 * se_flood
                                            )
                                            modGroup$floodlightPlot$setState(list(
                                              df_flood = df_flood,
                                              dep_name = modDep,
                                              ind_name = modInd,
                                              mod_name = modMod
                                            ))
                                          }
                                        }
                                      } else {
                                        if (!is.null(self$results$moderationGroup)) {
                                          self$results$moderationGroup$setVisible(FALSE)
                                          self$results$moderationGroup$simpleEffectsTable$setVisible(FALSE)
                                        }
                                      }

                                      # --- Plot Generation ---
                                      # NEW in 1.5: Path diagram support. This block collects the estimates
                                      # produced above (loadings, weights, paths, construct correlations and
                                      # their p-values) into a lightweight state object that is handed to
                                      # the .plotPathDiagram render function. The image is automatically
                                      # resized to accommodate multigroup analyses (one panel per group).
                                      # UPDATED in 1.7: The state now also carries the higher-order
                                      # constructs, their measurement types and the dummy manifest variables
                                      # generated for interaction terms, and every estimate table is
                                      # augmented with a p-value column derived from the bootstrap
                                      # inference results when cSEM does not supply one directly.
                                      if (isTRUE(self$options$showPlot) && length(groups) > 0) {
                                        image <- self$results$pathPlot

                                        # Get lists of constructs
                                        latent_names <- character(0)
                                        for (item in self$options$latent) {
                                          if (length(item$vars) > 0 && nzchar(item$label) && is_used(item$label)) {
                                            latent_names <- c(latent_names, item$label)
                                          }
                                        }

                                        composite_names <- character(0)
                                        for (item in self$options$composite) {
                                          if (length(item$vars) > 0 && nzchar(item$label) && is_used(item$label)) {
                                            composite_names <- c(composite_names, item$label)
                                          }
                                        }

                                        # NEW in 1.7: The renderer needs to know which nodes are higher-order
                                        # constructs and how they are measured, so that a formative HOC is
                                        # drawn as a hexagon and a reflective one as an ellipse, and so that
                                        # its outer relations are read from the correct estimate table.
                                        hoc_names <- character(0)
                                        hoc_types <- character(0)
                                        if (!is.null(self$options$hoc)) {
                                          for (item in self$options$hoc) {
                                            if (length(item$components) > 0 && nzchar(item$label) && is_used(item$label)) {
                                              hoc_names <- c(hoc_names, item$label)
                                              hoc_types[item$label] <- if (item$type %in% c("composite", "<~")) "composite" else "latent"
                                            }
                                          }
                                        }

                                        # Build lightweight state: just the data the render function needs
                                        plot_state <- tryCatch({
                                            group_data <- list()
                                            for (g in groups) {
                                              s <- if (is_multi) summs[[g]] else summs[[1]]
                                              es <- get_eval_s(s)
                                              inf_obj <- if (!is.null(inf_res)) (if (is_multi) inf_res[[g]] else inf_res) else NULL
                                              
                                              l_df <- if (inherits(s, "cSEMSummarize_2ndorder")) {
                                                rbind(as.data.frame(s$First_stage$Estimates$Loading_estimates),
                                                      as.data.frame(s$Second_stage$Estimates$Loading_estimates))
                                              } else {
                                                as.data.frame(es$Estimates$Loading_estimates)
                                              }
                                              if (nrow(l_df) > 0) {
                                                l_df$p_value <- sapply(1:nrow(l_df), function(i_l) {
                                                  row <- l_df[i_l, ]
                                                  se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Loading_estimates", "sd", row$Name)
                                                  if (!is.na(se) && !is.null(se) && se > 0) {
                                                    t_val <- as.numeric(row$Estimate) / se
                                                    2 * pnorm(-abs(t_val))
                                                  } else if ("p_value" %in% names(row) && !is.na(row$p_value)) {
                                                    row$p_value
                                                  } else {
                                                    NA_real_
                                                  }
                                                })
                                              }
                                              
                                              w_df <- if (inherits(s, "cSEMSummarize_2ndorder")) {
                                                rbind(as.data.frame(s$First_stage$Estimates$Weight_estimates),
                                                      as.data.frame(s$Second_stage$Estimates$Weight_estimates))
                                              } else {
                                                as.data.frame(es$Estimates$Weight_estimates)
                                              }
                                              if (nrow(w_df) > 0) {
                                                w_df$p_value <- sapply(1:nrow(w_df), function(i_w) {
                                                  row <- w_df[i_w, ]
                                                  se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Weight_estimates", "sd", row$Name)
                                                  if (!is.na(se) && !is.null(se) && se > 0) {
                                                    t_val <- as.numeric(row$Estimate) / se
                                                    2 * pnorm(-abs(t_val))
                                                  } else if ("p_value" %in% names(row) && !is.na(row$p_value)) {
                                                    row$p_value
                                                  } else {
                                                    NA_real_
                                                  }
                                                })
                                              }
                                              
                                              p_df <- if (inherits(s, "cSEMSummarize_2ndorder")) {
                                                rbind(as.data.frame(s$First_stage$Estimates$Path_estimates),
                                                      as.data.frame(s$Second_stage$Estimates$Path_estimates))
                                              } else {
                                                as.data.frame(es$Estimates$Path_estimates)
                                              }
                                              if (nrow(p_df) > 0) {
                                                p_df$p_value <- sapply(1:nrow(p_df), function(i_p) {
                                                  row <- p_df[i_p, ]
                                                  se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Path_estimates", "sd", row$Name)
                                                  if (!is.na(se) && !is.null(se) && se > 0) {
                                                    t_val <- as.numeric(row$Estimate) / se
                                                    2 * pnorm(-abs(t_val))
                                                  } else if ("p_value" %in% names(row) && !is.na(row$p_value)) {
                                                    row$p_value
                                                  } else {
                                                    NA_real_
                                                  }
                                                })
                                              }

                                              exo_df <- if (!is.null(es$Estimates$Exo_construct_correlation)) as.data.frame(es$Estimates$Exo_construct_correlation) else NULL
                                              if (!is.null(exo_df) && nrow(exo_df) > 0) {
                                                exo_df$p_value <- sapply(1:nrow(exo_df), function(i_e) {
                                                  row <- exo_df[i_e, ]
                                                  se <- if ("Std_err" %in% names(row) && !is.na(row$Std_err)) row$Std_err else get_inf_stat(inf_obj, "Exo_construct_correlation", "sd", row$Name)
                                                  if (!is.na(se) && !is.null(se) && se > 0) {
                                                    t_val <- as.numeric(row$Estimate) / se
                                                    2 * pnorm(-abs(t_val))
                                                  } else if ("p_value" %in% names(row) && !is.na(row$p_value)) {
                                                    row$p_value
                                                  } else {
                                                    NA_real_
                                                  }
                                                })
                                              }
                                              
                                              group_data[[if (nzchar(g)) g else "single"]] <- list(
                                                loading_estimates = l_df,
                                                weight_estimates  = w_df,
                                                path_estimates    = p_df,
                                                construct_vcv     = es$Estimates$Construct_VCV,
                                                exo_construct_correlation = exo_df
                                              )
                                            }

                                           # Build lavaan-compatible model syntax
                                           model_lavaan <- gsub("<~", "=~", model, fixed = TRUE)

                                           # NEW in 1.7: Interaction terms have no indicators of their own, so
                                           # lavaan would refuse to parse them. Every product term ("X.M")
                                           # therefore receives a single synthetic indicator that turns it
                                           # into a well-formed latent variable for layout purposes only.
                                           # The synthetic indicators are recorded in 'dummy_manifests' and
                                           # are removed again from the parameter table before the diagram
                                           # is drawn, so they never appear in the figure.
                                           plot_df <- as.data.frame(working_data)
                                           model_terms <- unique(unlist(strsplit(model_lavaan, "[\n~+ \t\r]+")))
                                           dot_terms <- model_terms[grepl(".", model_terms, fixed = TRUE)]
                                           dummy_manifests <- character(0)

                                           for (dt in dot_terms) {
                                             if (nzchar(dt)) {
                                               dummy_var <- paste0("d_", gsub(".", "_", dt, fixed = TRUE))
                                               plot_df[[dummy_var]] <- stats::rnorm(nrow(plot_df))
                                               dummy_manifests <- c(dummy_manifests, dummy_var)
                                               model_lavaan <- paste0(dt, " =~ ", dummy_var, "\n", model_lavaan)
                                               if (!(dt %in% latent_names) && !(dt %in% composite_names)) {
                                                 latent_names <- c(latent_names, dt)
                                               }
                                             }
                                           }

                                           list(
                                             model_lavaan       = model_lavaan,
                                             plot_data          = plot_df,
                                             latent_names       = latent_names,
                                             composite_names    = composite_names,
                                             hoc_names          = hoc_names,
                                             hoc_types          = hoc_types,
                                             dummy_manifests    = dummy_manifests,
                                             groups             = groups,
                                             is_multi           = is_multi,
                                             group_data         = group_data
                                           )
                                        }, error = function(e) {
                                          NULL
                                        })

                                         # UPDATED in 1.7: The canvas grows with the number of indicators as
                                         # well as with the number of groups, so densely measured models
                                         # stay legible instead of being squeezed into a fixed frame.
                                         if (!is.null(plot_state)) {
                                           n_manifests <- 0
                                           for (item in self$options$latent) n_manifests <- n_manifests + length(item$vars)
                                           for (item in self$options$composite) n_manifests <- n_manifests + length(item$vars)

                                           base_h <- max(550, min(800, 450 + n_manifests * 15))
                                           n_grps <- length(groups)
                                           if (n_grps <= 1) {
                                             image$setSize(750, base_h)
                                           } else if (n_grps == 2) {
                                             image$setSize(1000, base_h)
                                           } else {
                                             image$setSize(1100, max(650, base_h))
                                           }
                                           image$setState(plot_state)
                                         }
                                      }
                                    },
                                    # NEW in 1.5: Path diagram render function. Rebuilds a lavaan model
                                    # skeleton from the cSEM syntax (composites are mapped "<~" -> "=~" so
                                    # lavaan can parse them), draws it with semPlot::semPaths(), overlays
                                    # the cSEM estimates as edge labels (with optional significance stars),
                                    # renders composite constructs as hexagons to visually distinguish them
                                    # from latent variables (ellipses), and honours all user plot options
                                    # (layout, rotation, residuals, font size, label abbreviation). For
                                    # multigroup models each group is drawn in its own panel.
                                    # UPDATED in 1.7: The renderer additionally (a) supports higher-order
                                    # constructs and interaction terms, (b) matches edge labels by node name
                                    # instead of by position so labels can no longer be attached to the
                                    # wrong edge, (c) applies construct-grouped indicator stacking so
                                    # indicator blocks never interleave or overlap, and (d) scales the
                                    # canvas with the number of indicators and groups.
                                    .plotPathDiagram = function(image, ggtheme, theme, ...) {
                                      plot_state <- image$state
                                      if (is.null(plot_state))
                                        return(FALSE)

                                      # NEW in 1.7: Dynamically calculate the image size from the number of
                                      # indicators and the number of groups. The same calculation is applied
                                      # in .run() when the state is stored; it is repeated here so the
                                      # canvas is also correct when jamovi re-renders a saved analysis
                                      # without re-running the estimation.
                                      n_manifests <- 0
                                      for (item in self$options$latent) n_manifests <- n_manifests + length(item$vars)
                                      for (item in self$options$composite) n_manifests <- n_manifests + length(item$vars)
                                      base_h <- max(550, min(800, 450 + n_manifests * 15))
                                      n_grps <- if (!is.null(plot_state$groups)) length(plot_state$groups) else 1
                                      if (n_grps <= 1) {
                                        image$setSize(750, base_h)
                                      } else if (n_grps == 2) {
                                        image$setSize(1000, base_h)
                                      } else {
                                        image$setSize(1100, max(650, base_h))
                                      }

                                      # Retrieve UI options
                                      plot_layout    <- self$options$plotLayout
                                      # UPDATED in 1.7: The Direction option now stores self-describing
                                      # identifiers. They are mapped back onto the numeric 'rotation'
                                      # argument of semPlot::semPaths() here; the legacy numeric values
                                      # ('1'-'4') used up to 1.5 are still accepted so analyses saved with
                                      # an earlier version keep their orientation.
                                      rot_opt        <- self$options$plotRotation
                                      plot_rotation  <- if (rot_opt == "left_right" || rot_opt == "2") 2L else if (rot_opt == "top_down" || rot_opt == "1") 1L else if (rot_opt == "bottom_up" || rot_opt == "3") 3L else if (rot_opt == "right_left" || rot_opt == "4") 4L else 2L
                                      show_estimates <- isTRUE(self$options$showEstimates)
                                      show_residuals <- isTRUE(self$options$showResiduals)
                                      font_size_opt  <- self$options$plotFontSize
                                      show_sig_stars <- isTRUE(self$options$showSigStars)
                                      abbreviate     <- isTRUE(self$options$abbreviate)
                                      abbrev_length  <- as.integer(self$options$abbrevLength %||% 4)
                                      n_char_nodes   <- if (abbreviate) abbrev_length else 0

                                      # Map font size
                                      cex_val <- 0.8
                                      if (font_size_opt == "small") {
                                        cex_val <- 0.6
                                      } else if (font_size_opt == "large") {
                                        cex_val <- 1.0
                                      }

                                      res <- tryCatch({
                                        # Unpack state
                                        model_lavaan        <- plot_state$model_lavaan
                                        plot_data           <- plot_state$plot_data
                                        latent_names        <- plot_state$latent_names
                                        composite_names     <- plot_state$composite_names
                                        hoc_names           <- plot_state$hoc_names %||% character(0)
                                        hoc_types           <- plot_state$hoc_types %||% character(0)
                                        groups              <- plot_state$groups
                                        is_multi            <- plot_state$is_multi
                                        group_data          <- plot_state$group_data
                                        all_construct_names <- c(latent_names, composite_names, hoc_names)

                                        # Build dummy lavaan fit from the model syntax
                                        fit_lavaan <- tryCatch(
                                          lavaan::sem(model_lavaan, data = plot_data, do.fit = FALSE),
                                          error = function(e) NULL
                                        )
                                        if (is.null(fit_lavaan))
                                          return(FALSE)

                                        m_base <- tryCatch(
                                          semPlot::semPlotModel(fit_lavaan),
                                          error = function(e) NULL
                                        )
                                        if (is.null(m_base))
                                          return(FALSE)

                                        # NEW in 1.7: Drop the synthetic indicators that were only added so
                                        # lavaan could parse the interaction terms. The product-term nodes
                                        # themselves are kept and drawn; only their placeholder indicators
                                        # disappear from the diagram.
                                        dummy_manifests <- plot_state$dummy_manifests %||% character(0)
                                        if (length(dummy_manifests) > 0) {
                                          m_base@Pars <- m_base@Pars[!(m_base@Pars$lhs %in% dummy_manifests | m_base@Pars$rhs %in% dummy_manifests), ]
                                        }

                                        hexagon_shape <- list(
                                          x = c(1, 0.5, -0.5, -1, -0.5, 0.5),
                                          y = c(0, 0.866, 0.866, 0, -0.866, -0.866)
                                        )

                                        plots_list <- list()
                                        for (g in groups) {
                                          m <- m_base
                                          gkey <- if (nzchar(g)) g else "single"
                                          gd <- group_data[[gkey]]
                                          if (is.null(gd))
                                            next

                                          # Map estimates
                                          pars <- m@Pars
                                          custom_labels <- character(nrow(pars))

                                          for (i in 1:nrow(pars)) {
                                            lhs  <- pars$lhs[i]
                                            rhs  <- pars$rhs[i]
                                            edge <- pars$edge[i]

                                            est_val <- NA
                                            p_val <- NA

                                            # UPDATED in 1.7: Resolving an edge to its cSEM estimate now
                                            # distinguishes four cases - formative composite -> indicator
                                            # (weight), formative HOC -> component (weight or second-stage
                                            # path), reflective latent/HOC -> indicator or component
                                            # (loading), and structural path. Every lookup also tries the
                                            # '_temp' spelling produced by the two-stage approach, and
                                            # formative arrows are reversed so they point from the
                                            # indicator to the construct, as the direction of a formative
                                            # measurement relation requires.
                                            if (edge == "->") {
                                              if (lhs %in% composite_names && !(rhs %in% all_construct_names)) {
                                                idx <- which(gd$weight_estimates$Name == paste0(lhs, " <~ ", rhs) |
                                                             gd$weight_estimates$Name == paste0(lhs, " =~ ", rhs) |
                                                             gd$weight_estimates$Name == paste0(lhs, " <~ ", rhs, "_temp"))
                                                if (length(idx) > 0) {
                                                  est_val <- gd$weight_estimates$Estimate[idx[1]]
                                                  if ("p_value" %in% names(gd$weight_estimates))
                                                    p_val <- gd$weight_estimates$p_value[idx[1]]
                                                }
                                                pars$lhs[i] <- rhs
                                                pars$rhs[i] <- lhs
                                              } else if (lhs %in% hoc_names && identical(unname(hoc_types[lhs]), "composite")) {
                                                idx <- which(gd$weight_estimates$Name == paste0(lhs, " <~ ", rhs) |
                                                             gd$weight_estimates$Name == paste0(lhs, " <~ ", rhs, "_temp") |
                                                             gd$weight_estimates$Name == paste0(lhs, " =~ ", rhs) |
                                                             gd$path_estimates$Name == paste0(lhs, " ~ ", rhs) |
                                                             gd$path_estimates$Name == paste0(lhs, " ~ ", rhs, "_temp"))
                                                if (length(idx) > 0) {
                                                  w_or_p <- if (!is.null(gd$weight_estimates) && nrow(gd$weight_estimates) > 0) gd$weight_estimates else gd$path_estimates
                                                  est_val <- w_or_p$Estimate[idx[1]]
                                                  if ("p_value" %in% names(w_or_p))
                                                    p_val <- w_or_p$p_value[idx[1]]
                                                }
                                                pars$lhs[i] <- rhs
                                                pars$rhs[i] <- lhs
                                              } else if (lhs %in% latent_names || lhs %in% hoc_names) {
                                                idx <- which(gd$loading_estimates$Name == paste0(lhs, " =~ ", rhs) |
                                                             gd$loading_estimates$Name == paste0(lhs, " =~ ", rhs, "_temp"))
                                                if (length(idx) == 0) {
                                                  idx <- which(gd$loading_estimates$Name == paste0(lhs, " <~ ", rhs) |
                                                               gd$loading_estimates$Name == paste0(lhs, " <~ ", rhs, "_temp"))
                                                }
                                                if (length(idx) == 0) {
                                                  idx <- which(gd$weight_estimates$Name == paste0(lhs, " <~ ", rhs) |
                                                               gd$weight_estimates$Name == paste0(lhs, " <~ ", rhs, "_temp"))
                                                }
                                                if (length(idx) > 0) {
                                                  est_val <- gd$loading_estimates$Estimate[idx[1]]
                                                  if ("p_value" %in% names(gd$loading_estimates))
                                                    p_val <- gd$loading_estimates$p_value[idx[1]]
                                                }
                                              }
                                            } else if (edge == "~>" || edge == "~" || edge == "->") {
                                                clean_path_names <- gsub("_temp", "", gd$path_estimates$Name, fixed = TRUE)
                                                p_patterns <- c(
                                                  paste0(rhs, " ~ ", lhs),
                                                  paste0(lhs, " ~ ", rhs),
                                                  paste0(rhs, " ~ ", lhs, "_temp"),
                                                  paste0(lhs, " ~ ", rhs, "_temp"),
                                                  paste0(rhs, "_temp ~ ", lhs),
                                                  paste0(lhs, "_temp ~ ", rhs),
                                                  paste0(rhs, "_temp ~ ", lhs, "_temp"),
                                                  paste0(lhs, "_temp ~ ", rhs, "_temp"),
                                                  paste0(rhs, " ~ ", paste(rev(unlist(strsplit(lhs, ".", fixed = TRUE))), collapse = "."))
                                                )
                                                idx <- which(clean_path_names %in% p_patterns | gd$path_estimates$Name %in% p_patterns)
                                                if (length(idx) > 0) {
                                                  est_val <- gd$path_estimates$Estimate[idx[1]]
                                                  if ("p_value" %in% names(gd$path_estimates))
                                                    p_val <- gd$path_estimates$p_value[idx[1]]
                                                } else if (length(idx) == 0 && !is.null(gd$loading_estimates)) {
                                                 l_patterns <- c(
                                                   paste0(lhs, " =~ ", rhs), paste0(lhs, " =~ ", rhs, "_temp"),
                                                   paste0(rhs, " =~ ", lhs), paste0(rhs, " =~ ", lhs, "_temp"),
                                                   paste0(lhs, " <~ ", rhs), paste0(lhs, " <~ ", rhs, "_temp"),
                                                   paste0(rhs, " <~ ", lhs), paste0(rhs, " <~ ", lhs, "_temp")
                                                 )
                                                 idx_l <- which(gd$loading_estimates$Name %in% l_patterns)
                                                 if (length(idx_l) > 0) {
                                                   est_val <- gd$loading_estimates$Estimate[idx_l[1]]
                                                   if ("p_value" %in% names(gd$loading_estimates))
                                                     p_val <- gd$loading_estimates$p_value[idx_l[1]]
                                                 }
                                               }
                                            } else if (edge == "<->") {
                                              vcv <- gd$construct_vcv
                                              if (lhs %in% all_construct_names && rhs %in% all_construct_names) {
                                                if (!is.null(vcv) && lhs %in% rownames(vcv) && rhs %in% colnames(vcv) && lhs %in% rownames(vcv) && rhs %in% colnames(vcv))
                                                  est_val <- vcv[lhs, rhs]
                                              }
                                              exo_df <- gd$exo_construct_correlation
                                              if (!is.null(exo_df) && nrow(exo_df) > 0) {
                                                key1 <- paste0(lhs, " ~~ ", rhs)
                                                key2 <- paste0(rhs, " ~~ ", lhs)
                                                idx <- which(exo_df$Name == key1 | exo_df$Name == key2)
                                                if (length(idx) > 0) {
                                                  p_val <- exo_df$p_value[idx[1]]
                                                }
                                              }
                                            }

                                            if (!is.na(est_val)) {
                                              pars$est[i] <- est_val
                                              # Create custom label
                                              lbl <- sprintf("%.2f", est_val)
                                              if (show_sig_stars && !is.na(p_val)) {
                                                stars <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else ""
                                                lbl <- paste0(lbl, stars)
                                              }
                                              custom_labels[i] <- lbl
                                            } else {
                                              custom_labels[i] <- ""
                                            }
                                          }

                                          m@Pars <- pars

                                          plot_obj <- tryCatch(
                                            semPlot::semPaths(
                                              m,
                                              whatLabels = if (show_estimates) "est" else "name",
                                              nCharNodes = n_char_nodes,
                                              residuals = show_residuals,
                                              layout = plot_layout,
                                              rotation = plot_rotation,
                                              theme = "classic",
                                              sizeMan = 5,
                                              sizeMan2 = 3,
                                              sizeLat = 7,
                                              mar = c(3, 3, 3, 3),
                                              edge.label.cex = cex_val,
                                              label.cex = cex_val,
                                              DoNotPlot = TRUE,
                                              polygonList = list(hexagon = hexagon_shape)
                                            ),
                                            error = function(e) NULL
                                          )

                                          if (!is.null(plot_obj)) {
                                            # NEW in 1.7: Draw every edge as a solid line. semPlot marks
                                            # fixed parameters with a dashed line, which is misleading here
                                            # because all edges carry freely estimated cSEM coefficients.
                                            if (!is.null(plot_obj$graphAttributes$Edges$lty)) {
                                              plot_obj$graphAttributes$Edges$lty[] <- 1
                                            }

                                            # UPDATED in 1.7: Estimate labels are matched to edges by the
                                            # names of the two nodes they connect instead of by position in
                                            # the parameter table. semPlot may reorder, merge or drop edges
                                            # while laying out the graph, which previously could attach a
                                            # label to the wrong path.
                                            # Add custom labels with significance stars matched by node names
                                            if (show_estimates && !is.null(plot_obj$graphAttributes$Edges)) {
                                              nodes <- plot_obj$graphAttributes$Nodes$names
                                              orig_nodes <- names(nodes)
                                              if (is.null(orig_nodes) || all(!nzchar(orig_nodes))) orig_nodes <- nodes
                                              
                                              edge_from <- if (!is.null(plot_obj$Edgelist$from)) plot_obj$Edgelist$from else if (!is.null(plot_obj$plotOptions$input)) plot_obj$plotOptions$input[, "lhs"] else integer(0)
                                              edge_to   <- if (!is.null(plot_obj$Edgelist$to)) plot_obj$Edgelist$to else if (!is.null(plot_obj$plotOptions$input)) plot_obj$plotOptions$input[, "rhs"] else integer(0)
                                              edge_labels <- plot_obj$graphAttributes$Edges$labels
                                              
                                              for (k in seq_along(edge_from)) {
                                                f_node <- orig_nodes[edge_from[k]]
                                                t_node <- orig_nodes[edge_to[k]]
                                                
                                                matched_idx <- which((pars$lhs == f_node & pars$rhs == t_node) |
                                                                    (pars$lhs == t_node & pars$rhs == f_node))
                                                if (length(matched_idx) > 0) {
                                                  lbl_val <- custom_labels[matched_idx[1]]
                                                  if (nzchar(lbl_val)) {
                                                    edge_labels[k] <- lbl_val
                                                  }
                                                }
                                              }
                                              plot_obj$graphAttributes$Edges$labels <- edge_labels
                                            }

                                            # UPDATED in 1.7: Formative higher-order constructs are drawn
                                            # with the same hexagonal node shape as ordinary composites,
                                            # while reflective ones keep the ellipse used for latent
                                            # variables.
                                            all_composite_shapes <- composite_names
                                            if (length(hoc_names) > 0 && length(hoc_types) > 0) {
                                              for (h in hoc_names) {
                                                if (identical(unname(hoc_types[h]), "composite")) {
                                                  all_composite_shapes <- c(all_composite_shapes, h)
                                                }
                                              }
                                            }

                                            node_names <- plot_obj$graphAttributes$Nodes$names
                                            orig_names <- names(node_names)
                                            if (is.null(orig_names)) orig_names <- node_names
                                            for (comp in all_composite_shapes) {
                                              idx <- which(orig_names == comp | node_names == comp)
                                              if (length(idx) > 0)
                                                plot_obj$graphAttributes$Nodes$shape[idx] <- "hexagon"
                                            }

                                            # NEW in 1.7: Render interaction nodes in the conventional
                                            # notation, i.e. the internal "X.M" term is displayed as
                                            # "X x M".
                                            # Clean up interaction node labels (e.g. task.int -> task × int)
                                            if (!is.null(plot_obj$graphAttributes$Nodes$labels)) {
                                              n_lbls <- plot_obj$graphAttributes$Nodes$labels
                                              for (nl_i in seq_along(n_lbls)) {
                                                if (grepl(".", n_lbls[nl_i], fixed = TRUE)) {
                                                  n_lbls[nl_i] <- gsub(".", "×", n_lbls[nl_i], fixed = TRUE)
                                                }
                                              }
                                              plot_obj$graphAttributes$Nodes$labels <- n_lbls
                                            }
                                            # NEW in 1.7: Construct-grouped indicator stacking. semPlot lays
                                            # indicators out column by column without regard to which
                                            # construct they belong to, so blocks interleave and overlap in
                                            # densely measured models. Each column is therefore re-stacked
                                            # here: indicators are grouped under their parent construct, the
                                            # groups are ordered by the vertical position of that construct,
                                            # and a uniform step (scaled by the Vertical Spacing Gap option)
                                            # plus a margin between groups is applied. A column that serves
                                            # a single construct is centred on that construct instead.
                                             if (!is.null(plot_obj$layout)) {
                                               lay <- plot_obj$layout
                                               node_names <- plot_obj$graphAttributes$Nodes$names
                                               orig_names <- names(node_names)
                                               if (is.null(orig_names)) orig_names <- node_names
                                               x_coords <- unique(round(lay[, 1], 2))
                                                
                                                # Determine max manifests in any single column to set uniform step size
                                                max_manifests_in_any_col <- max(1, max(sapply(x_coords, function(xc) {
                                                  sum(round(lay[, 1], 2) == xc & !(orig_names %in% all_construct_names))
                                                })))
                                                
                                                spacing_opt <- self$options$plotIndicatorSpacing %||% "normal"
                                                scale_factor <- if (spacing_opt == "compact") 0.78 else if (spacing_opt == "large") 1.25 else 1.0
                                                max_target_h <- 1.45
                                                step <- scale_factor * (max_target_h / max_manifests_in_any_col)
                                                group_margin <- step * 1.5

                                                for (xc in x_coords) {
                                                  manifest_idx <- which(round(lay[, 1], 2) == xc & !(orig_names %in% all_construct_names))
                                                  if (length(manifest_idx) == 0) next
                                                  
                                                  # Map each indicator to its parent construct
                                                  comp_of_manifest <- character(length(manifest_idx))
                                                  for (i_m in seq_along(manifest_idx)) {
                                                    m_node <- orig_names[manifest_idx[i_m]]
                                                    ind_r <- which((m@Pars$lhs == m_node | m@Pars$rhs == m_node) & m@Pars$edge == "->")
                                                    if (length(ind_r) > 0) {
                                                      c_name <- if (m@Pars$lhs[ind_r[1]] %in% all_construct_names) m@Pars$lhs[ind_r[1]] else m@Pars$rhs[ind_r[1]]
                                                      comp_of_manifest[i_m] <- c_name
                                                    }
                                                  }
                                                  
                                                  # Unique parent constructs in this column, ordered top-to-bottom
                                                  unique_comps <- unique(comp_of_manifest[nzchar(comp_of_manifest)])
                                                  comp_y <- sapply(unique_comps, function(c_name) {
                                                    c_i <- which(orig_names == c_name | node_names == c_name)
                                                    if (length(c_i) > 0) lay[c_i[1], 2] else 0
                                                  })
                                                  names(comp_y) <- unique_comps
                                                  unique_comps <- unique_comps[order(comp_y, decreasing = TRUE)]
                                                  
                                                  if (length(unique_comps) > 1) {
                                                    # Multiple constructs in column (e.g. left column): center overall block
                                                    total_h <- 0
                                                    for (c_name in unique_comps) {
                                                      n_i <- sum(comp_of_manifest == c_name)
                                                      total_h <- total_h + (n_i - 1) * step + group_margin
                                                    }
                                                    total_h <- total_h - group_margin
                                                    curr_pos <- total_h / 2
                                                    
                                                    for (c_name in unique_comps) {
                                                      c_m_indices <- manifest_idx[comp_of_manifest == c_name]
                                                      c_m_names <- orig_names[c_m_indices]
                                                      c_m_indices <- c_m_indices[order(c_m_names)]
                                                      
                                                      n_i <- length(c_m_indices)
                                                      for (j in seq_len(n_i)) {
                                                        lay[c_m_indices[j], 2] <- curr_pos
                                                        curr_pos <- curr_pos - step
                                                      }
                                                      curr_pos <- curr_pos + step - group_margin
                                                    }
                                                  } else if (length(unique_comps) == 1) {
                                                    # Single construct in column (e.g. Well outcome construct on right): center around construct circle Y
                                                    c_name <- unique_comps[1]
                                                    c_m_indices <- manifest_idx[comp_of_manifest == c_name]
                                                    c_m_names <- orig_names[c_m_indices]
                                                    c_m_indices <- c_m_indices[order(c_m_names)]
                                                    
                                                    n_i <- length(c_m_indices)
                                                    c_group_h <- (n_i - 1) * step
                                                    c_center_y <- comp_y[c_name]
                                                    curr_pos <- c_center_y + c_group_h / 2
                                                    
                                                    for (j in seq_len(n_i)) {
                                                      lay[c_m_indices[j], 2] <- curr_pos
                                                      curr_pos <- curr_pos - step
                                                    }
                                                  }
                                                }

                                                plot_obj$layout <- lay
                                             }

                                            plots_list[[g]] <- plot_obj
                                          }
                                        }

                                        if (length(plots_list) == 0)
                                          return(FALSE)

                                        n_plots <- length(plots_list)
                                        if (is_multi && n_plots > 1) {
                                          if (n_plots == 2) {
                                            par(mfrow = c(1, 2))
                                          } else if (n_plots <= 4) {
                                            par(mfrow = c(2, 2))
                                          } else {
                                            par(mfrow = c(ceiling(n_plots / 3), 3))
                                          }
                                          for (g in names(plots_list)) {
                                            plot(plots_list[[g]])
                                            title(g, line = 0.5)
                                          }
                                        } else {
                                          plot(plots_list[[1]])
                                        }
                                        TRUE
                                      }, error = function(e) {
                                        FALSE
                                      })
                                      return(res)
                                    },

                                    # NEW in 1.7: Simple slopes render function. Draws one predicted
                                    # regression line of the dependent construct on the independent
                                    # construct for every probing level of the moderator, using the state
                                    # prepared by the moderation block in .run(). A visible fan of lines
                                    # indicates an interaction; parallel lines indicate its absence.
                                    .plotSimpleEffects = function(image, ggtheme, theme, ...) {
                                      plot_state <- image$state
                                      if (is.null(plot_state))
                                        return(FALSE)
                                      
                                      df_lines  <- plot_state$df_lines
                                      dep_name  <- plot_state$dep_name
                                      ind_name  <- plot_state$ind_name
                                      mod_name  <- plot_state$mod_name
                                      
                                      if (is.null(df_lines) || nrow(df_lines) == 0)
                                        return(FALSE)
                                      
                                      p <- ggplot2::ggplot(df_lines, ggplot2::aes(x = x, y = y, color = Level, group = Level)) +
                                        ggplot2::geom_line(linewidth = 1.2) +
                                        ggplot2::labs(
                                          title = paste0("Simple Effects of ", ind_name, " on ", dep_name),
                                          subtitle = paste0("Moderator: ", mod_name),
                                          x = paste0(ind_name, " (Z-score)"),
                                          y = paste0(dep_name, " (Predicted)"),
                                          color = paste0(mod_name, " Level")
                                        ) +
                                        ggplot2::scale_color_manual(values = c("-1 SD (Low)" = "#2b5c8f", "Mean (0)" = "#d95f02", "+1 SD (High)" = "#7570b3",
                                                                               "16th Percentile (Low)" = "#2b5c8f", "50th Percentile (Mean)" = "#d95f02", "84th Percentile (High)" = "#7570b3")) +
                                        ggplot2::theme_minimal(base_size = 13) +
                                        ggplot2::theme(
                                          plot.title = ggplot2::element_text(face = "bold", size = 14),
                                          legend.position = "bottom"
                                        )
                                      
                                      print(p)
                                      TRUE
                                    },
                                    
                                    # NEW in 1.7: Floodlight (Johnson-Neyman) render function. Draws the
                                    # conditional effect of the independent construct on the dependent
                                    # construct across the whole range of the moderator, together with its
                                    # 95% confidence band and a dashed reference line at zero. The moderator
                                    # values at which the band crosses that line delimit the region of
                                    # significance.
                                    .plotFloodlight = function(image, ggtheme, theme, ...) {
                                      plot_state <- image$state
                                      if (is.null(plot_state))
                                        return(FALSE)
                                      
                                      df_flood <- plot_state$df_flood
                                      dep_name <- plot_state$dep_name
                                      ind_name <- plot_state$ind_name
                                      mod_name <- plot_state$mod_name
                                      
                                      if (is.null(df_flood) || nrow(df_flood) == 0)
                                        return(FALSE)
                                      
                                      p <- ggplot2::ggplot(df_flood, ggplot2::aes(x = m, y = slope)) +
                                        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#4a90e2", alpha = 0.25) +
                                        ggplot2::geom_line(color = "#1f4e79", linewidth = 1.2) +
                                        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#d9534f", linewidth = 0.8) +
                                        ggplot2::labs(
                                          title = paste0("Floodlight Analysis (Johnson-Neyman)"),
                                          subtitle = paste0("Conditional Effect of ", ind_name, " on ", dep_name, " across ", mod_name),
                                          x = paste0(mod_name, " (Z-score)"),
                                          y = paste0("Conditional Slope of ", ind_name)
                                        ) +
                                        ggplot2::theme_minimal(base_size = 13) +
                                        ggplot2::theme(
                                          plot.title = ggplot2::element_text(face = "bold", size = 14)
                                        )
                                      
                                      print(p)
                                      TRUE
                                    }
                                  )
)

# Null-coalescing operator (available in R >= 4.4, polyfill for older R)
`%||%` <- function(a, b) if (!is.null(a) && !identical(a, "")) a else b
