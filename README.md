**Composite-SEM for jamovi**

Composite-SEM is a comprehensive module for jamovi designed to estimate, analyze, and study composites and latent variables simultensouly using structural eqquation modeling (SEM). It mainly relies on the cSEM R package while utlizing user-friendly graphical interface if jamovi.

**Core Features**

Advanced estimation techniques: Supports Partial Least Squares (PLS), Corrected PLS (PLSc) for latent variables, Generalized Structured Component Analysis (GSCA), and Maxvar.

Flexible measurement models: Easily define reflective (latent) and formative (composite) constructs.

Model flexibility: Support for testing both types of models: measurement (inner) model and structural (outer) model.

Comprehensive diagnostics: Produces model fit indices, confidence intervals via bootstrapping, and out-of-sample prediction measures.

Multigroup analysis (MGD): Perform difference tests across groups using differnt methods like permutation-based test .

PLS predcit (out-of-sample prediction): Compare PLS model performance against standard linear model (LM) benchmarks for predictive assessment.

-----------------------------------------------------------------------------------------------------------------

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
