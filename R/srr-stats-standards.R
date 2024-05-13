#' srr_stats
#'
#' All of the following standards initially have `@srrstatsTODO` tags.
#' These may be moved at any time to any other locations in your code.
#' Once addressed, please modify the tag from `@srrstatsTODO` to `@srrstats`,
#' or `@srrstatsNA`, ensuring that references to every one of the following
#' standards remain somewhere within your code.
#' (These comments may be deleted at any time.)
#'
#' @srrstatsVerbose TRUE
#'
#'
#' @srrstats {G1.2} *Life Cycle "Maturing".* New features are being added over
#' time.
#'
#' @srrstats {G1.3} *All statistical terminology should be clarified and
#'  unambiguously defined.* I have prepared a small paper that attempts to
#'  clarify all statistical terms, which I am happy to send to reviewers.
#'
#' @srrstats {G1.4} *`roxygen2` is used throughout the package to document
#' all functions.*
#'
#' @srrstats {G1.4a} *All internal (non-exported) functions should also be
#' documented in standard [`roxygen2`](https://roxygen2.r-lib.org/) format,
#' along with a final `@noRd` tag to suppress automatic generation of `.Rd`
#' files.* Done.
#'
#' @srrstats {G1.6} *Software should include code necessary to compare
#' performance claims with alternative implementations in other R packages.*
#' Is a link to this blog post sufficient?
#' ://s3alfisc.github.io/blog/post/1000x-faster-wild-cluster-bootstrap
#' -inference-in-r-with-fwildclusterboot/
#'
#' @srrstats {G2.0a} Provide explicit secondary documentation of any
#' expectations on lengths of inputs. I think this is sufficiently documented.
#'

#'
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure
#'  values are appropriately pre-processed regardless of class structures.* (?)
#'   different methods are checked via 'check_methods_equivalence.R'
#'
#' @srrstats {G2.7} *Software should accept as input as many of the above
#' standard tabular forms as possible, including extension to domain-specific
#' forms.* No tabular data is passed to boottest().
#'
#' @srrstats {G2.10} *Software should ensure that extraction or filtering of
#'  single columns from tabular inputs should not presume any particular
#'  default behaviour, and should ensure all column-extraction operations
#'  behave consistently regardless of the class of tabular data used as input.*
#'  NA, as no tabular data inputs.
#'
#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular
#'  objects which have columns which do not themselves have standard class
#'   attributes (typically, `vector`) are appropriately processed, and do
#'   not error without reason. This behaviour should be tested. Again, columns
#'    created by the [`units` package](https://github.com/r-quantities/units/)
#'    provide a good test case.* NA, as no tabular input data.
#'
#' @srrstats {G2.12} *Software should ensure that `data.frame`-like tabular
#'  objects which have list columns should ensure that those columns are
#'  appropriately pre-processed either through being removed, converted to
#'  equivalent vector columns where appropriate, or some other appropriate
#'  treatment such as an informative error. This behaviour should be tested.*
#'   Does not happen. NA, as no tabular input data.
#'
#'
#' @srrstats {G2.14} *see G2.13*
#'
#' @srrstats {G2.14a} *see G2.13*
#'
#' @srrstats {G2.14b} *see G2.13*
#'
#' @srrstats {G2.14c} *see G2.13*
#'
#' @srrstats {G3.0} *No floating point numbers (rational numbers) are
#'  compared with equality.* Yes, checked.
#'
#' @srrstats {G3.1} *Statistical software which relies on covariance
#' calculations should enable users to choose between different algorithms
#' for calculating covariances, and should not rely solely on covariances
#' from the `stats::cov` function.* The package deals with "clustered"
#' standard errors, but does not produce covariance matrices.
#'  Non-clustered tests based on the 'regular' wild bootstrap
#'  are also supported.
#'
#' @srrstats {G3.1a} *The ability to use arbitrarily specified covariance
#' methods should be documented (typically in examples or vignettes).*
#' See above.
#'
#' @srrstats {G4.0} *Statistical Software which enables outputs to be
#' written to local files should parse parameters specifying file names
#' to ensure appropriate file suffices are automatically generated where
#' not provided.* No output can be written to local files.
#'
#'
#' @srrstats {G5.2a} *Every message produced within R code by `stop()`,
#'  `warning()`, `message()`, or equivalent should be unique* Done.
#'
#'
#' @srrstats {G5.8} **Edge condition tests** *to test that these conditions
#' produce expected behaviour such as clear warnings or errors when confronted
#' with data with extreme properties including but not limited to:* Tests for
#'  non-randomness under full enumeration:
#'
#' @srrstats {RE1.1} *Regression Software should document how formula interfaces
#'  are converted to matrix representations of input data.* Not applicable. Formulas
#' only create scalars.
#'
#' @srrstats {RE1.4} *Regression Software should document any assumptions made
#' with regard to input data; for example distributional assumptions, or
#' assumptions that predictor data have mean values of zero. Implications of
#' violations of these assumptions should be both documented and tested.*
#' The bootstrap weight options are described in a separate vignette article.
#' In general, the wild bootstrap does not make any distributional assumptions
#' for estimation.
#'
#' @srrstats {RE3.1} *Enable such messages to be optionally suppressed, yet
#' should ensure that the resultant model object nevertheless includes
#'  sufficient data to identify lack of convergence.* In my experience,
#'   convergence is never a problem, but confidence interval inversion
#'   sometimes fails. I suspect that I can improve the error messageas :)
#'

#' @srrstats {RE4.4} *The specification of the model, generally as a
#' formula (via `formula()`)* Done as not applicable.
#'
#'
#' @srrstats {RE4.17} *Model objects returned by Regression Software should
#' implement or appropriately extend a default `print` method which provides
#' an on-screen summary of model (input) parameters and (output) coefficients.*
#' Done.
#'
#'
#' @srrstats {RE5.0} *Scaling relationships between sizes of input data
#'  (numbers of observations, with potential extension to numbers of
#'  variables/columns) and speed of algorithm.*
#'
#'
#' @srrstats {RE6.1} *Where the default `plot` method is **NOT** a generic
#'  `plot` method dispatched on the class of return objects (that is,
#'  through an S3-type `plot.<myclass>` function or equivalent), that
#'   method dispatch (or equivalent) should nevertheless exist in order
#'   to explicitly direct users to the appropriate function.*
#'
#' @srrstats {RE6.2} *The default `plot` method should produce a plot of
#'  the `fitted` values of the model, with optional visualisation of confidence
#'   intervals or equivalent.*
#'
#' @srrstats {RE6.3} *Where a model object is used to generate a forecast
#' (for example, through a `predict()` method), the default `plot` method
#' should provide clear visual distinction between modelled (interpolated)
#'  and forecast (extrapolated) values.*
#'
#' @srrstats {RE7.0} *Tests with noiseless, exact relationships between
#' predictor (independent) data.*
#'
#' @noRd
NULL

#' NA_standards
#'
#' Any non-applicable standards can have their tags changed from `@srrstatsTODO`
#' to `@srrstatsNA`, and placed together in this block, along with explanations
#' for why each of these standards have been deemed not applicable.
#' (These comments may also be deleted at any time.)
#' @noRd
#'
#' @srrstatsNA {G1.5} *Software should include all code necessary to reproduce
#'  results which form the basis of performance claims made in associated
#'   publications.* There are no associated publications.
#'
#' @srrstatsNA {G2.4b} *explicit conversion to continuous via `as.numeric()`*
#' Does not happen.
#'
#' @srrstatsNA {G2.4e} *explicit conversion from factor via `as...()` functions*
#'  Does not happen.
#'
#' @srrstatsNA {G2.5} *Where inputs are expected to be of `factor` type,
#' secondary documentation should explicitly state whether these should be
#'  `ordered` or not, and those inputs should provide appropriate error or
#'   other routines to ensure inputs follow these expectations.* No input
#'   assumed to be of type factor.
#'
#' @srrstatsNA {G2.9} *Software should issue diagnostic messages for type
#' conversion in which information is lost (such as conversion of variables
#'  from factor to character; standardisation of variable names; or removal
#'   of meta-data such as those associated with
#'   [`sf`-format](https://r-spatial.github.io/sf/) data) or added
#'   (such as insertion of variable or column names where none were provided).*
#'    Type conversion with information loss never happens.
#'
#' @srrstatsNA {G5.3} *For functions which are expected to return objects
#'  containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the
#'  absence of any such values in return objects should be explicitly tested.*
#'  Multiple tests for output correctness, but not dedicated tests for NA
#'   values.
#'
#' @srrstatsNA {G5.10} *Extended tests should included and run under a common
#' framework with other tests but be switched on by flags such as as a
#' `<MYPKG>_EXTENDED_TESTS=1` environment variable.* TO do. Currently,
#' I only run "test-julia-vs-r" locally as codecov fails
#' (these tests take too long). Here I would need some help to set this
#' up properly.
#'
#' @srrstatsNA {G5.11} *Where extended tests require large data sets or
#' other assets, these should be provided for downloading and fetched as
#' part of the testing workflow.* All tests rely on internally created
#'  data sets.
#'
#' @srrstatsNA {G5.11a} *When any downloads of additional data necessary
#'  for extended tests fail, the tests themselves should not fail, rather
#'   be skipped and implicitly succeed with an appropriate diagnostic
#'   message.* All tests rely on internally created data sets.
#'
#' @srrstatsNA {G5.12} *Any conditions necessary to run extended tests
#' such as platform requirements, memory, expected runtime, and artefacts
#'  produced that may need manual inspection, should be described in developer
#'   documentation such as a `CONTRIBUTING.md` or `tests/README.md` file.*
#'   There are no particular tests to run, but I should update the
#'   tests/readme.md for a potential review.
#'
#' @srrstatsNA {G5.4a} *For new methods, it can be difficult to separate
#' out correctness of the method from the correctness of the implementation,
#'  as there may not be reference for comparison. In this case, testing may
#'  be implemented against simple, trivial cases or against multiple
#'  implementations such as an initial R implementation compared with results
#'   from a C/C++ implementation.*  Code is tested against
#'   WildBootTests.jl / fixest::feols().
#'
#' @srrstatsNA {G5.4c} *Where applicable, stored values may be drawn from
#'  published paper outputs when applicable and where code from original
#'   implementations is not available* Code is tested against
#'   WildBootTests.jl / fixest::feols().
#'
#' @srrstatsNA {G5.8a} *Zero-length data*
#'
#' @srrstatsNA {G5.8b} *Data of unsupported types (e.g., character or complex
#'  numbers in for functions designed only for numeric data)*
#'
#' @srrstatsNA {G5.8d} *Data outside the scope of the algorithm (for example,
#'  data with more fields (columns) than observations (rows) for some
#'  regression algorithms)* Needs to be handled by the regression package.
#'
#' @srrstatsNA {G5.9} **Noise susceptibility tests** *Packages should test
#'  for expected stochastic behaviour, such as through the following
#'  conditions:* This might not make sense for a bootstrap package?
#'
#' @srrstatsNA {G5.9a} *Adding trivial noise (for example, at the scale of
#' `.Machine$double.eps`) to data does not meaningfully change results*
#'
#' @srrstatsNA {RE1.2} *Regression Software should document expected format
#' (types or classes) for inputting predictor variables, including descriptions
#'  of types or classes which are not accepted.* This is handled by the
#'   regression package, unless fixed effects are projected out - then
#'    boottest() might transform them into factors, if required.
#'
#' @srrstatsNA {RE1.3} *Regression Software which passes or otherwise
#' transforms aspects of input data onto output structures should ensure
#' that those output structures retain all relevant aspects of input data,
#' notably including row and column names, and potentially information from
#' other `attributes()`.*
#'
#' @srrstatsNA {RE1.3a} *Where otherwise relevant information is not
#' transferred, this should be explicitly documented.*
#'
#' @srrstatsNA {RE2.1} *Regression Software should implement explicit
#' parameters controlling the processing of missing values, ideally
#' distinguishing `NA` or `NaN` values from `Inf` values (for example,
#' through use of `na.omit()` and related functions from the `stats`
#' package).* All of this should not be handled by fwildclusterboot::boottest()
#'  but the initial regression function.
#'
#' @srrstatsNA {RE2.2} *Regression Software should provide different options
#'  for processing missing values in predictor and response data. For example,
#'  it should be possible to fit a model with no missing predictor data in
#'  order to generate values for all associated response points, even where
#'  submitted response values may be missing.* NA as boottest() is not a proper
#'   regression package.
#'
#' @srrstatsNA {RE2.3} *Where applicable, Regression Software should enable
#'  data to be centred (for example, through converting to zero-mean equivalent
#'  values; or to z-scores) or offset (for example, to zero-intercept equivalent
#'   values) via additional parameters, with the effects of any such parameters
#'    clearly documented and tested.* If the regression package centers data,
#'    boottest() will use the centered data.
#'
#' @srrstatsNA {RE2.4} *Regression Software should implement pre-processing
#'  routines to identify whether aspects of input data are perfectly collinear,
#'   notably including:*
#'
#' @srrstatsNA {RE2.4a} *Perfect collinearity among predictor variables* Needs
#' to be done by the regression package.
#'
#' @srrstatsNA {RE2.4b} *Perfect collinearity between independent and dependent
#'  variables*Needs to be done by the regression package.
#'
#' @srrstatsNA {G2.16} *All functions should also provide options to handle
#'  undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially
#'  ignoring or removing such values.* This is handled by the regression
#'  modeling function.
#'
#' @srrstatsNA {RE4.1} *Regression Software may enable an ability to generate
#'  a model object without actually fitting values. This may be useful for
#'  controlling batch processing of computationally intensive fitting
#'   algorithms.* Not relevant (?)
#'
#' @srrstatsNA {RE4.2} *Model coefficients (via `coeff()` / `coefficients()`)*
#' As boottest() is concerned with inference, a coef() method does not make
#' sense to me :)
#'
#' @srrstatsNA {RE4.6} *The variance-covariance matrix of the model parameters
#'  (via `vcov()`)* Inference based on p-values and t-stats, not vcov's.
#'
#' @srrstatsNA {RE4.7} *Where appropriate, convergence statistics* I think
#' supplying convergence stats for the test inversion procedure would be too
#'  much.
#'
#' @srrstatsNA {RE4.8} *Response variables, and associated "metadata" where
#' applicable.*
#'
#' @srrstatsNA {RE4.9} *Modelled values of response variables.*
#'
#' @srrstatsNA {RE4.10} *Model Residuals, including sufficient documentation
#' to enable interpretation of residuals, and to enable users to submit
#' residuals to their own tests.*
#'
#' @srrstatsNA {RE4.11} *Goodness-of-fit and other statistics associated such
#' as effect sizes with model coefficients.*
#'
#' @srrstatsNA {RE4.12} *Where appropriate, functions used to transform input
#'  data, and associated inverse transform functions.*
#'
#' @srrstatsNA {RE4.13} *Predictor variables, and associated "metadata" where
#' applicable.*
#'
#' @srrstatsNA {RE4.14} *Where possible, values should also be provided for
#'  extrapolation or forecast *errors*.*
#'
#' @srrstatsNA {RE4.15} *Sufficient documentation and/or testing should be
#'  provided to demonstrate that forecast errors, confidence intervals, or
#'   equivalent values increase with forecast horizons.*
#'
#' @srrstatsNA {RE4.16} *Regression Software which models distinct responses
#' for different categorical groups should include the ability to submit new
#' groups to `predict()` methods.*
#'
#' @srrstatsNA {RE7.0a} In particular, these tests should confirm ability to
#'  reject perfectly noiseless input data.
#'
#' @srrstatsNA {RE7.1} *Tests with noiseless, exact relationships between
#'  predictor (independent) and response (dependent) data.*
#'
#' @srrstatsNA {RE7.1a} *In particular, these tests should confirm that model
#' fitting is at least as fast or (preferably) faster than testing with
#'  equivalent noisy data (see RE2.4b).*
#'
#' @srrstatsNA {RE7.2} Demonstrate that output objects retain aspects of input
#'  data such as row or case names (see **RE1.3**).
#'
#' @srrstatsNA {RE7.3} Demonstrate and test expected behaviour when objects
#' returned from regression software are submitted to the accessor methods
#' of **RE4.2**--**RE4.7**.
#'
#' @srrstatsNA {RE7.4} Extending directly from **RE4.15**, where appropriate,
#' tests should demonstrate and confirm that forecast errors, confidence
#' intervals, or equivalent values increase with forecast horizons.

NULL
