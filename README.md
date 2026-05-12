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
