**Composite-SEM for jamovi**

Composite-SEM is a comprehensive module for jamovi designed to estimate, analyze, and study composites and latent variables simultensouly using structural eqquation modeling (SEM). It mainly relies on the cSEM R package while utlizing user-friendly graphical interface if jamovi.

**Core Features**

Advanced estimation techniques: Supports Partial Least Squares (PLS), Corrected PLS (PLSc) for latent variables, Generalized Structured Component Analysis (GSCA), and MAXVAR.

Permit two types of models: Easily define reflective (latent) and formative (composite) constructs.

Advanced bootstrap options: By default, the user gets 500 samples, and this can be changed along with different interval types available (e.g., percentile, bias-corrected and basic).

Accounts for nonnormality: in "Model Estimation", you can select "Robust estimation," which allows you to appropriately estimate models with data that deviate from a normal distribution.

Weight Models: You can can select between Mode B (default) and Mode A when using PLS.

Comprehensive diagnostics: Produces model fit indices (CFI, TLI, SRMR, RMSEA), exact fit (critical value 95% of SRMR, dG, and dL), confidence intervals via bootstrapping, and out-of-sample prediction measures.

Model flexibility: Testing the measurement (outer) model (such as CFA and CCA) and structural (inner) model in a very intutive way.

Extended report: When structural (inner) model is specified, user automatically gets direct, indirect, and total effects.

Missing data: Several options avaiable to deal with missing data including, listwise deletion, mean imputation, regression imputation, and K-nearest neighbours (KNN) imputation.

Multigroup analysis (MGD): Perform difference tests across groups using differnt methods like permutation-based test (among other types).

PLS predcit (out-of-sample prediction): Compare PLS model performance against standard linear model (LM) benchmarks for predictive assessment.

Plot the model: You can get a visual presentation of your model with a punch of options (e.g., abbreviation, showing stars for sig value, showing estimates on paths, type of layout, font size, and more!).

-----------------------------------------------------------------------------------------------------------------
**Composite-SEM 1.7 Update**
**What is included in 1.7**
Refinements:
-General Enhancements: SEM diagram now has more options including spacing between the indicators.
-Bootstrapping has more options now

New Features:
-Hierarchical Models: Added full support for First-Order and Second-Order models. They can be modeled as latent or composite.

-Moderation Analysis: New feature to test moderating effects. Including Slope plot and Floodlight plot

-Menu Restructuring: The module is now placed under the "sem" menu group.

**Composite-SEM 1.5 Update**
**What is included in 1.5**

- Path diagram: automatic visualization of the estimated model using semPlot, with composites drawn as hexagons and latent variables as ellipses, estimates displayed on paths, optional significance stars (* p < .05, ** p < .01, *** p < .001), and separate panels for each group in multigroup analyses.
- Full plot customization: layout (tree or spring-embedded), direction/rotation, node label abbreviation, indicator residuals, font size, and estimate label toggles.
- Structured Multi-Group Analysis (MGA): group difference tests are now presented in native jamovi tables (overall decision, run overview/metadata, and per-parameter comparison results) instead of a raw text printout.
- Selectable MGA test methods: Henseler (PLS-MGA), Sarstedt, Chin, Keil, and Nitzl (suitable for unequal variances) can be run individually or combined in a single pass.
- MGA support for correlated models: testMGD is dynamically patched so group comparisons also work for models without structural paths (CFA/CCA-style correlated models).
- Per-construct PLS weighting modes: each composite can individually be estimated with Mode A (correlation weights) or Mode B (regression weights), automatically synchronized with the defined composites in the interface.
- Bootstrap confidence interval types: Percentile, Basic, Bias-corrected (BC), and Bias-corrected and accelerated (BCa).
- Robust estimation via Spearman rank correlation for nonnormal data and outliers.
- Cleaner error reporting: estimation and MGA failures now produce concise, actionable messages instead of raw debug output.

**Main outputs**

- Path Diagram (customizable model plot).
- Multi-Group Analysis - Overall Decision table.
- Multi-Group Analysis - Run Overview table (permutation runs, admissible results, seeds, and observations per group).
- Multi-Group Comparison Test Results table (test statistic, p-value, and decision per parameter and comparison pair).
- Extended Estimation Information table now also reports robust estimation, bootstrapping status, number of bootstrap samples, and the confidence interval type.

**Interface overview**

- A new "Plot" menu controls the path diagram and all of its display options.
- A new "Multigroup Analysis Options" menu allows selecting which MGA test methods to run.
- A new "Weights Modes" menu lists the defined composites with a Mode A / Mode B selector for each (PLS estimation only).
- The "Bootstrapping Options" menu now includes the bootstrap confidence interval type in addition to the number of samples.
- The "Model Estimation" menu now includes the robust (Spearman) estimation option.
- The "Data Cleaning Options" labels were renamed to "Enable handling missing data" / "Handling method" for clarity.


**Composite-SEM 1.4 Update**
**What is included in 1.4**

- Integrated Data Cleaning options to seamlessly handle missing values prior to model estimation.
- Support for multiple missing data treatment methods: Listwise Deletion, Mean, Median, Mode, Regression, and KNN (K-Nearest Neighbors) imputation.
- Smart data type handling with automatic fallback (e.g., automatically applying Mode imputation if Mean or Median methods are assigned to categorical or non-numeric variables).
- Built-in error handling and safe execution to prevent module crashes due to missing values or incompatible data types.

**Main outputs**

- Data Cleaning Summary (displays the utilized cleaning method, total number of rows deleted, and total number of values imputed).

**Interface overview**

- A new "Data Cleaning Options" menu allows users to select their preferred missing data treatment method before proceeding with the measurement and structural model definitions.

**Composite-SEM 1.3 Update**
**What is included in 1.3**

- Reflective latent constructs and formative composite constructs.
- Drag-and-drop structural roles for endogenous and exogenous constructs.
- Directional path blocks for specifying predictors of each endogenous construct.
- Automatic correlated model mode when no directional paths are defined.
- Estimation with PLS, GSCA, or MAXVAR.
- Optional bootstrapping with confidence intervals, standard errors, and p-values.
- Optional exact fit testing when bootstrapping is enabled.
- Optional composite outer loadings display.
- Optional disattenuation control for cSEM estimation.
- Optional out-of-sample prediction benchmark against a linear model.
- Multi-group analysis support through the grouping variable option.

**Main outputs**

- Model structure summary.
- Model information.
- Model fit indices.
- Exact fit test.
- Outer model of composites.
- Outer model of common factors.
- Construct reliability for common factors.
- Discriminant validity with HTMT and HTMT2.
- VIF values for Mode B weights.
- Construct correlations.
- Inner structural relationships with R-squared values.
- Indirect and total effects when mediation paths exist.
- Prediction benchmark metrics when enabled.

**Interface overview**

1. Define latent and composite constructs in the measurement model sections.
2. Optionally assign a multigroup variable.
3. Choose the estimation method and bootstrap settings.
4. Use Structural Roles to classify constructs as endogenous or exogenous.
5. Use Directional Paths to drag predictors into each endogenous construct block.

If the structural sections are left empty, CompositeSEM estimates a correlated model automatically.

-----------------------------------------------------------------------------------------------------------------
