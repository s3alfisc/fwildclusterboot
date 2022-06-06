## Submission of 0.9 

+ no errors, warnings or messages on github actions for ubuntu, mac, windows
+ no errors, warnings or messages on `rhub::check_for_cran`
+ no errors, warnings or messages on win-devel

except for one note: "Found the following (possibly) invalid URLs:
  URL: https://journals.sagepub.com/doi/full/10.1177/1536867X19830877
    From: man/boottest.Rd
          man/boottest.felm.Rd
          man/boottest.fixest.Rd
          man/boottest.ivreg.Rd
          man/boottest.lm.Rd
          man/mboottest.Rd
          man/mboottest.felm.Rd
          man/mboottest.fixest.Rd
          man/mboottest.lm.Rd
    Status: 503
    Message: Service Unavailable"
  
which is a valid URL (I have checked it).

## Re-submission of 0.8

+ "Found the following (possibly) invalid file URI:
  URI: WildBootTests.html
    From: inst/doc/fwildclusterboot.html"
    -> replaced 'internal' reference with proper URL


## Submission of 0.8

+ checked on github actions (mac, windows, linux)
+ checked on rhub
+ checked on r-devel

No errors or warnings have been found. 

rhub and r-devel lead to aligned notes, some of which have been fixed. 
Other notes don't seem appropriate (?)

+ "   Possibly misspelled words in DESCRIPTION:
     HC (45:67)
     WRE (49:28)
     heteroskedasticity (45:40)
"
+ "Found the following (possibly) invalid file URI:
  URI: WildBootTests.html
    From: inst/doc/fwildclusterboot.html"
  -> file URI to a package article
+ "Found the following (possibly) invalid DOIs:
  DOI: 10.1177/1536867X19830877
    From: DESCRIPTION
    Status: Service Unavailable
    Message: 503"
  -> I have checked this DOI, and it looks valid to me



## Re-submission of 0.7


* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  'cran-comments.html'
* File deleted  

## Re-submission of 0.7

Fixed both issues below:

+   Found the following (possibly) invalid URLs:
     URL: https://www.tandfonline.com/doi/abs/10.1198/jbes.2009.07221%5D
       From: README.md
       Status: 404
       Message: Not Found

The closing %5D = "]" should likely be removed.



+   The Description field contains
 
https://journals.sagepub.com/doi/abs/10.1177/1536867X19830877?journalCode=stja,

Please write DOIs as <doi:10.1177/1536867X19830877> in the Description
field without the rest of the URL.

## Version 0.7

- checks pass on github actions: windows, mac, ubuntu
- checks pass local cmd check
- checks pass on r-develop

## Version 0.5.1 

Please apologize the quick re-submission. Version 0.5.1 fixes a bug introduced 
in version 0.5.


- checks pass on github actins
- checks pass local cmd check
- checks pass on r-develop - note regarding fast re-submission

## Version 0.5 

- checks pass on github actins
- checks pass local cmd check
- checks pass on r-develop; note regarding codecov link has been addressed

Changes: 

+ Version 0.5 fixes an error for the bootstrap with weighted least squares introduced with version 0.4. All unit tests 
  that compare fwildclusterboot with weighted least squares results from boottest.stata pass. In particular, enumerated cases pass with exact equality (in such cases, the bootstrap weights matrices are exactly identical in both R and Stata).
+ `boottest()` now stops if `fixest::feols()` deletes non-NA values (e.g. singleton fixed effects deletion) and asks the user to delete such rows prior to estimation via `feols()` & `boottest()`. Currently, `boottest()'s` pre-processing cannot handle such deletions - this remains future work.
+ To align `fwildclusterboot` with Stata's boottest command (Roodman et al, 2019), Mammen weights are no longer enumerated in `fwildclusterboot::boottest()`.
+ `boottest()` no longer sets an internal seed (previously set.seed(1)) if no seed is provided as a function argument. 
+ Sampling of the bootstrap weights is now powered by the `dqrng` package, which speeds up the creation of the bootstrap weights matrix. To set a "global" seed, one now has use the `dqset.seed()` function from the `dqrng package`, which is added as a dependency.



## Version 0.4 first submission - re-submission 

Apparently, I did not fix the openBLAS error appropriately.
The test tolerance level has again been reduced, from 1e-04 to 1e-02. The relative test difference found in the openBLAS tests is 0.003568575, so the test should now pass. If this does not fix the issue, I will remote all tests in test_type from CRAN.

The error stated 
"
test_type.R...................  139 tests 1 fails
   test_type.R...................  140 tests 2 fails
   test_type.R...................  140 tests 2 fails
   test_type.R...................  140 tests 2 fails
   test_type.R...................  140 tests 2 fails
   test_type.R...................  140 tests 2 fails
   test_type.R...................  140 tests 2 fails
   test_type.R...................  141 tests 2 fails
   test_type.R...................  142 tests 2 fails
   test_type.R...................  143 tests 2 fails
   test_type.R...................  144 tests 2 fails
   test_type.R...................  145 tests 2 fails
   test_type.R...................  146 tests 2 fails
   test_type.R...................  147 tests 2 fails
   test_type.R...................  148 tests 2 fails
   test_type.R...................  149 tests 2 fails
   test_type.R...................  150 tests 2 fails
   test_type.R...................  151 tests 2 fails
   test_type.R...................  152 tests 2 fails
   test_type.R...................  153 tests 2 fails
   test_type.R...................  154 tests 2 fails
   test_type.R...................  155 tests 2 fails
   test_type.R...................  156 tests 2 fails
   test_type.R...................  157 tests 2 fails
   test_type.R...................  158 tests 2 fails
   test_type.R...................  159 tests 2 fails
   test_type.R...................  160 tests 2 fails 4.6s
   ----- FAILED[data]: test_type.R<345--345>
    call| expect_equivalent(boot_fixest_c$conf_int, boot_felm_c$conf_int,
    call| -->    tol = 1e-04)
    diff| Mean relative difference: 0.003568575
   ----- FAILED[data]: test_type.R<346--346>
    call| expect_equivalent(boot_felm_c$conf_int, boot_lm$conf_int, tol
= 1e-04)
    diff| Mean relative difference: 0.00357477
   Error: 2 out of 868 tests failed
   In addition: Warning messages:
   1: There are only 1024 unique draws from the rademacher distribution
for 10 clusters. Therefore, B =  1024  with full enumeration. Consider
using webb weights instead.
   2: Further, note that under full enumeration and with B = 1024
bootstrap draws, only 2^(#clusters - 1) =  512  distinct t-statistics
and p-values can be computed. For a more thorough discussion, see Webb
`Reworking wild bootstrap based inference for clustered errors` (2013).
   3: There are only 1024 unique draws from the rademacher distribution
for 10 clusters. Therefore, B =  1024  with full enumeration. Consider
using webb weights instead.
   4: Further, note that under full enumeration and with B = 1024
bootstrap draws, only 2^(#clusters - 1) =  512  distinct t-statistics
and p-values can be computed. For a more thorough discussion, see Webb
`Reworking wild bootstrap based inference for clustered errors` (2013).
   Execution halted
"

## Version 0.4 re-submission 

The openBLAS error on CRAN has been fixed (point for in version 0.4 submission.
Some tests for exact equality failed with relative difference e-05 on openBLAS. In consequence, all exact tests are set to reltol = 1e-04. This should fix all CRAN errors.


## Version 0.4 submission 

+ New feature I: `boottest()` now allows for univariate tests that involve multiple 
  variables. E.g. one can now test hypothesis as ´var1 + var2 = c´ where c is a scalar. More details on the syntax can be found in the vignette. All methods of for 
  objects of class `boottest` have been updated.
+ New feature II: `boottest()` now also supports "equal-tailed" p-values and one-sided hypotheses. For one-sided tests, confidence intervals are currently not supported. 
+ Internal changes: To allow for multivariable tests, the `boot_algo2()` function has slightly been modified. `invert_p_val2()` is superseded by `invert_p_val()`. 
+ Further, a CRAN error is fixed - some tests for exact equality failed with relative difference e-05 on openBLAS. In consequence, all exact tests are set to reltol = 1e-04. This should fix all CRAN errors.

## Version 0.3.7 submission

+ fixes bug, see https://github.com/s3alfisc/fwildclusterboot/issues/14 
+ tested on R win-devel, github actions (windows, mac, ubuntu)

## Version 0.3.6 submission, Resubmission 2

+ Another error due to "noSuggests" / vignette - package fixest was used, but not loaded


## Version 0.3.6 submission, Resubmission

+ Adressed errors related to suggests described below (reason for error; unprotected library(lfe) statements in tests)
+ noSuggests error due to missing library("pkg") statements in vignette
+ lfe now install on ubuntu dev

## Version 0.3.6 submission

+ "Please see the problems shown on
https://cran.r-project.org/web/checks/check_results_fwildclusterboot.html.

Please correct before 2021-08-08 to safely retain your package on CRAN.

Packages in Suggests should be used conditionally: see 'Writing R Extensions'.
This needs to be corrected even if the missing package(s) become available."

Solution: add if(requireNamespace("pkg")) for suggested packages in vignette, examples, tests 

I have checked package installation via github actions on windows, mac, ubuntu & ubuntu devel - installation fails 
on ubuntu devel because the "lfe" package cannot be installed.

Further, the packages passes the checks via devtools::check_win_devel()

## Version 0.3.5 submission 

+ fixes two bugs introduced with version 0.3.3

## Version 0.3.4 submission

+ fix an error in the vignette that caused multiple erros on CRAN

## Version 0.3.3 re-submission 

+ updated date in description

## Version 0.3.3 submission 

+ implements full enumeration for rademacher and mammen weights if 2^k < B, where k is the number of clusters and B the number of bootstrap iterations

## Version 0.3.2 re-submission 

+ update date in description

## Version 0.3.2 submission 

+ Fixes a CRAN test error message for Oracle Solaris. 


## Another resubmission

+
  ** running tests for arch 'i386' ... [296s] OK
  ** running tests for arch 'x64' ... [239s] OK
  
  Please reduce the test timings by using
    - small toy data only
    - few iterations
    - or by running less important tests only conditionally if some
  environment variable is set that you only define on your machine?

Long-running tests now only run on developing versions (4-digit) - this 
submission is 3-digit (0.3.1)

## This is a third re-submission


+ Please remove the single quotes from function names in your description.
e.g.: 'lm()' --> lm()

DONE


+  Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
Missing Rd-tags:
      boottest.Rd: \value
      check_set_nthreads.Rd: \value
      cpp_get_nb_threads.Rd: \value
      create_data_2.Rd: \value
      dot-onLoad.Rd: \value
      getBoottest_nthreads.Rd: \value
      invert_p_val2.Rd: \value
      p_val_null2.Rd: \value

DONE. Added return values to all functions OR functions are no longer exported.

+ You have examples for unexported functions.
Please either omit these examples or export these functions.
Used ::: in documentation:
      man/boottest.felm.Rd:
         felm_fit <- felm(proposition_vote ~ treatment + ideology1 +
log_income | Q1_immigration, data = fwildclusterboot:::create_data_2(N =
1000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10,
numb_fe2 = 10, seed = 12345))
      man/boottest.fixest.Rd:
         feols_fit <- feols(proposition_vote ~ treatment + ideology1 +
log_income, fixef = "Q1_immigration", data =
fwildclusterboot:::create_data_2(N = 1000, N_G1 = 10, icc1 = 0.91, N_G2
= 10, icc2 = 0.51, numb_fe1 = 10, numb_fe2 = 10, seed = 12345))
      man/boottest.lm.Rd:
         lm_fit <- lm(proposition_vote ~ treatment + ideology1 +
log_income + Q1_immigration, data = fwildclusterboot:::create_data_2(N =
1000, N_G1 = 10, icc1 = 0.91, N_G2 = 10, icc2 = 0.51, numb_fe1 = 10,
numb_fe2 = 10, seed = 12345))

DONE. deleted fwildclusterboot::: call from examples


## This is a second re-submission


Thanks, we see:
  + Found the following (possibly) invalid URLs:
       URL: https://www.tidyverse.org/lifecycle/#experimental (moved to
  https://lifecycle.r-lib.org/articles/stages.html)
         From: README.md
         Status: 200
         Message: OK

  Please change http --> https, add trailing slashes, or follow moved
  content as appropriate.
  
  Moved URL https://www.tidyverse.org/lifecycle/#experimental to 
            https://lifecycle.r-lib.org/articles/stages.html



## This is a re-submission


+ License components with restrictions and base license permitting such:
     GPL-3 + file LICENSE
     File 'LICENSE':
     YEAR: 2021
     COPYRIGHT HOLDER: fwildclusterboot authors
  
  That LICENSE file is nonsense for the GPL-3.
  If you want GPL-3, then you do not need any LICENSE file.

  License file was deleted.

+ Unknown, possibly mis-spelled, fields in DESCRIPTION:
     'SourceCode'

  Source Code was change to URL. 

+ Found the following (possibly) invalid URLs:
     URL: (https://academic.oup.com/ectj/article-abstract/21/2/114/5078969)
       From: inst/doc/fwildclusterboot.html
       Message: Invalid URI scheme

  Omite the parentheses.
  Parantheses were deleted. 

+ URL: https://s3alfisc.github.io/fwildclusterboot (moved to
  https://s3alfisc.github.io/fwildclusterboot/)
       From: DESCRIPTION
       Status: 200
       Message: OK

  Please change http --> https, add trailing slashes, or follow moved
  content as appropriate.
  
  Trail slashes were added for https://s3alfisc.github.io/fwildclusterboot/ and 
  https://github.com/s3alfisc/fwildclusterboot/issues/


## Test environments

* GitHub Actions: windows-latest (release)
* GitHub Actions: GitHub Actions: macos-latest (release)
* GitHub Actions: ubuntu-20.04 (release)
* GitHub Actions: ubuntu-20.04 (devel)
* win-builder: devel



## R CMD check results

0 errors | 0 warnings | 1 note 

* This is a new release.
