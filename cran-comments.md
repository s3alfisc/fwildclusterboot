## Version 0.3.6 submission

+ "Please see the problems shown on
https://cran.r-project.org/web/checks/check_results_fwildclusterboot.html.

Please correct before 2021-08-08 to safely retain your package on CRAN.

Packages in Suggests should be used conditionally: see 'Writing R Extensions'.
This needs to be corrected even if the missing package(s) become available."

Solution: add if(requireNamespace("pkg")) for suggested packages in vignette, examples, tests 

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
