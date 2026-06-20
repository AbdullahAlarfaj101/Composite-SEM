**Composite-SEM for jamovi**

Composite-SEM is a comprehensive module for jamovi designed to estimate, analyze, test, and study linear and multi-group structural equation models. Built upon the robust cSEM R package, this module brings professional-grade composite-based Structural Equation Modeling (SEM) to a user-friendly graphical interface.

**Core Features**

Advanced Estimation Techniques: Supports Partial Least Squares (PLS), Generalized Structured Component Analysis (GSCA), and Maxvar.

Flexible Measurement Models: Easily define Reflective (Latent) and Formative (Composite) constructs.

Model Flexibility: Support for testing both types of models: measurement (inner) model and structural (outer) model.

Comprehensive Diagnostics: Produces model fit indices, confidence intervals via bootstrapping, and out-of-sample prediction measures.

Multigroup Analysis (MGD): Perform difference tests across groups using differnt methods like permutation-based test .

Linear Benchmarking: Compare PLS model performance against standard linear model (LM) benchmarks for predictive assessment.



**Interface Overview**

1. Measurement (outer) Model Setup
The interface allows for intuitive "drag-and-drop" variable assignment to define your model's building blocks.

Latent Variables: Define reflective constructs where indicators are caused by the latent factor.

Composite Variables: Define formative constructs where indicators form the composite proxy.

Multigroup Analysis: Assign a grouping variable to compare path coefficients and weights across different segments of your data.


2. Model Estimation & Structural (inner) Model
Fine-tune your analysis through advanced estimation settings and text-based structural syntax.

Estimation Method: Choose between PLS, GSCA, or MAXVAR.

Bootstrapping: Enable resampling to calculate stable confidence intervals and p-values.

Structural (inner) Relations: Define paths using standard syntax:

Y ~ X for direct effects.

Predictive Assessment: Toggle the Linear Model Benchmark to assess the out-of-sample predictive power of your composite model.


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
