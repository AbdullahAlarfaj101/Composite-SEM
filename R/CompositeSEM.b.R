# This file is a generated template, your changes will not be overwritten

#' @rdname jamovi
#' @export
CompositeSEMClass <- R6::R6Class("CompositeSEMClass",
                         inherit = CompositeSEMBase,
                         private = list(
                           
                           # =================================================================
                           # .init() method: Runs instantly to prepare UI tables
                           # =================================================================
                           .init = function() {
                             
                             summaryTable <- self$results$constructsTable
                             
                             raw_relations <- c(
                               self$options$ModelRelation,
                               self$options$ModelRelation2,
                               self$options$ModelRelation3,
                               self$options$ModelRelation4,
                               self$options$ModelRelation5
                             )
                             
                             valid_relations <- raw_relations[nzchar(raw_relations)]
                             structural_input <- paste(valid_relations, collapse = '\n')
                             is_auto_mode <- (nchar(structural_input) == 0)
                             
                             # FIX: Safe Regex for label matching (Option 1 from audit)
                             is_used_in_structure <- function(label, text) {
                               safe_label <- gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", label, perl = TRUE)
                               pattern <- paste0("\\b", safe_label, "\\b")
                               return(grepl(pattern, text, perl = TRUE))
                             }
                             
                             # Populate table for Latent Variables
                             if (length(self$options$latent) > 0) {
                               for (item in self$options$latent) {
                                 if (length(item$vars) > 0 && nzchar(item$label)) {
                                   if (is_auto_mode || is_used_in_structure(item$label, structural_input)) {
                                     summaryTable$addRow(rowKey = item$label, values = list(
                                       type = 'Latent (Reflective)', 
                                       construct = item$label, 
                                       indicators = paste(item$vars, collapse=', ')
                                     ))
                                   }
                                 }
                               }
                             }
                             
                             # Populate table for Composite Variables
                             if (length(self$options$composite) > 0) {
                               for (item in self$options$composite) {
                                 if (length(item$vars) > 0 && nzchar(item$label)) {
                                   if (is_auto_mode || is_used_in_structure(item$label, structural_input)) {
                                     summaryTable$addRow(rowKey = item$label, values = list(
                                       type = 'Composite (Formative)', 
                                       construct = item$label, 
                                       indicators = paste(item$vars, collapse=', ')
                                     ))
                                   }
                                 }
                               }
                             }
                           },
                           
                           # =================================================================
                           # .run() method: Runs the heavy statistical computations
                           # =================================================================
                           .run = function() {
                             
                             output_table <- self$results$csemOutput
                             
                             raw_relations <- c(
                               self$options$ModelRelation,
                               self$options$ModelRelation2,
                               self$options$ModelRelation3,
                               self$options$ModelRelation4,
                               self$options$ModelRelation5
                             )
                             
                             valid_relations <- raw_relations[nzchar(raw_relations)]
                             structural_input <- paste(valid_relations, collapse = '\n')
                             is_auto_mode <- (nchar(structural_input) == 0)
                             
                             measurement_parts <- list()
                             hasCommonFactors <- FALSE 
                             active_constructs <- c() 
                             ignored_vars <- c()
                             
                             # FIX: Safe Regex for label matching (Option 1 from audit)
                             is_used_in_structure <- function(label, text) {
                               safe_label <- gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", label, perl = TRUE)
                               pattern <- paste0("\\b", safe_label, "\\b")
                               return(grepl(pattern, text, perl = TRUE))
                             }
                             
                             # A) Process Latent Variables
                             if (length(self$options$latent) > 0) {
                               for (item in self$options$latent) {
                                 if (length(item$vars) > 0 && nzchar(item$label)) {
                                   if (is_auto_mode || is_used_in_structure(item$label, structural_input)) {
                                     
                                     # FIX: Handling spaces in variables using composeTerm()
                                     safe_label <- jmvcore::composeTerm(item$label)
                                     safe_vars <- sapply(item$vars, jmvcore::composeTerm)
                                     formula <- paste0(safe_label, " =~ ", paste(safe_vars, collapse = " + "))
                                     
                                     measurement_parts <- c(measurement_parts, formula)
                                     hasCommonFactors <- TRUE
                                     active_constructs <- c(active_constructs, item$label)
                                   } else {
                                     ignored_vars <- c(ignored_vars, item$label)
                                   }
                                 }
                               }
                             }
                             
                             # B) Process Composite Variables
                             if (length(self$options$composite) > 0) {
                               for (item in self$options$composite) {
                                 if (length(item$vars) > 0 && nzchar(item$label)) {
                                   if (is_auto_mode || is_used_in_structure(item$label, structural_input)) {
                                     
                                     # FIX: Handling spaces in variables using composeTerm()
                                     safe_label <- jmvcore::composeTerm(item$label)
                                     safe_vars <- sapply(item$vars, jmvcore::composeTerm)
                                     formula <- paste0(safe_label, " <~ ", paste(safe_vars, collapse = " + "))
                                     
                                     measurement_parts <- c(measurement_parts, formula)
                                     active_constructs <- c(active_constructs, item$label)
                                   } else {
                                     ignored_vars <- c(ignored_vars, item$label)
                                   }
                                 }
                               }
                             }
                             
                             if (length(measurement_parts) == 0) {
                               output_table$setContent("No constructs defined or matched.")
                               return()
                             }
                             
                             measurement_string <- paste(measurement_parts, collapse = '\n')
                             
                             final_structural_part <- ""
                             
                             if (is_auto_mode) {
                               if (length(active_constructs) > 1) {
                                 # Escape constructs for pairs too, just in case they have spaces
                                 safe_active <- sapply(active_constructs, jmvcore::composeTerm)
                                 pairs <- utils::combn(safe_active, 2, function(x) paste(x, collapse = " ~~ "))
                                 final_structural_part <- paste(pairs, collapse = "\n")
                               } else {
                                 final_structural_part <- ""
                               }
                             } else {
                               final_structural_part <- structural_input
                             }
                             
                             model <- paste(measurement_string, final_structural_part, sep = '\n\n')
                             
                             # --- Setup Analysis Variables ---
                             multGroupVar <- self$options$multg
                             estimationModel <- self$options$alt
                             
                             # FIX: Updated variable name to useBootstrap
                             useBootstrap <- self$options$useBootstrap
                             bootstrapSamples <- self$options$bootR
                             runLinearBench <- self$options$LinearBench
                             
                             # --- Execution ---
                             tryCatch({
                               
                               csem_args <- list(
                                 .data = self$data,
                                 .model = model
                               )
                               
                               if (!is.null(multGroupVar) && multGroupVar != "") {
                                 csem_args$.id <- multGroupVar
                               }
                               
                               if (estimationModel == 'PLS') {
                                 if (is_auto_mode) {
                                   csem_args$.PLS_weight_scheme_inner <- "factorial"
                                 } else {
                                   csem_args$.PLS_weight_scheme_inner <- "path"
                                 }
                               } else {
                                 csem_args$.approach_weights <- estimationModel
                               }
                               
                               if (useBootstrap == TRUE) {
                                 csem_args$.resample_method <- "bootstrap"
                                 csem_args$.R <- bootstrapSamples
                               }
                               
                               out <- do.call(cSEM::csem, csem_args)
                               
                               # --- Results Visualization ---
                               summary_text <- capture.output(print(cSEM::summarize(out)))
                               summary_string <- paste(summary_text, collapse = '\n')
                               
                               assess_string <- "" 
                               if (estimationModel == 'PLS' && hasCommonFactors == TRUE) {
                                 assess_text <- capture.output(print(cSEM::assess(out)))
                                 assess_string <- paste("\n\n\n--- Assess Results (PLS Only) ---\n\n", paste(assess_text, collapse = '\n'))
                               }
                               
                               mga_output <- ""
                               mga_header <- ""
                               if (!is.null(multGroupVar) && multGroupVar != "") {
                                 mga_header <- paste("\n(Multigroup Analysis performed by grouping variable: ", multGroupVar, ")\n")
                                 perm_R <- if (is.numeric(bootstrapSamples) && bootstrapSamples > 0) bootstrapSamples else 50
                                 
                                 mga_output <- tryCatch({
                                   mga_res <- cSEM::testMGD(.object = out, .R_permutation = perm_R)
                                   mga_print <- capture.output(print(mga_res))
                                   paste("\n\n\n--- Multi-Group Difference Tests (Comparison Results) ---\n",
                                         "Number of Permutations: ", perm_R, "\n\n",
                                         paste(mga_print, collapse = '\n'))
                                 }, error = function(e_mga) {
                                   paste("\n\n--- Multi-Group Difference Error ---\n",
                                         "Reason:\n", e_mga$message)
                                 })
                               }
                               
                               linear_bench_output <- ""
                               if (runLinearBench == TRUE) {
                                 linear_bench_output <- tryCatch({
                                   pred_res <- cSEM::predict(.object = out, .benchmark = "lm", .R = 10, .seed = 123)
                                   pred_text <- capture.output(print(pred_res))
                                   paste(
                                     "\n\n\n--- Linear Model Benchmark (Prediction Assessment) ---\n",
                                     "Comparing PLS Model vs Linear Model (LM) Benchmark (Reps = 10, Seed = 123):\n\n",
                                     paste(pred_text, collapse = '\n')
                                   )
                                 }, error = function(e_bench) {
                                   paste("\n\n--- Linear Model Benchmark Error ---\n",
                                         "Reason:\n", e_bench$message)
                                 })
                               }
                               
                               mode_msg <- if (is_auto_mode) {
                                 "\n Auto-Correlation model. No structural relationships defined.\n"
                               } else {
                                 "\n Mode: Structural relationships defined by the user\n"
                               }
                               
                               ignored_msg <- ""
                               if (!is_auto_mode && length(ignored_vars) > 0) {
                                 ignored_msg <- paste0("\n(Excluded constructs (unused in relations): ", paste(ignored_vars, collapse = ", "), ")\n")
                               }
                               
                               final_output <- paste(
                                 "--- Summarize Results ---",
                                 mode_msg,
                                 mga_header,
                                 ignored_msg,
                                 "\n",
                                 summary_string,
                                 assess_string,
                                 mga_output,
                                 linear_bench_output 
                               )
                               output_table$setContent(final_output)
                               
                             }, error = function(e) {
                               error_message <- paste(
                                 "An error occurred during cSEM analysis (Model: ", estimationModel, "):\n\n",
                                 "R Error Message:\n",
                                 e$message,
                                 "\n\nModel sent to cSEM:\n",
                                 model
                               )
                               output_table$setContent(error_message)
                             })
                           })
)
