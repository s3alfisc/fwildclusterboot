## Submission of 0.15

I have tested the package on: 
- rhub
- win devel
- github actions

and received the following notes: 

* checking for non-standard things in the check directory ... NOTE
Found the following files/directories:
  ''NULL''

* checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'
Found the following files/directories:

* Found the following (possibly) invalid URLs:
  URL: https://journals.sagepub.com/doi/pdf/10.1177/1536867X19830877
    From: inst/doc/Literature.html
    Status: 403
    Message: Forbidden
  URL: https://onlinelibrary.wiley.com/doi/abs/10.1002/jrsm.1554
    From: inst/doc/Literature.html
    Status: 403
    Message: Forbidden
  URL: https://www.tandfonline.com/doi/abs/10.1198/jbes.2009.07221
    From: inst/doc/Literature.html
          inst/doc/fwildclusterboot.html
          README.md
    Status: 403
    Message: Forbidden
    
I have checked all links - all are valid.


## Submission of 0.13

I have tested the package on: 
- rhub
- win devel
- github actions

and received the following notes: 

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'


## Resubmission of 0.12.1 

Change JuliaConnectoR link in readme to "https://CRAN.R-project.org/package=JuliaConnectoR".

## Resubmission of 0.12.1

I have added additional information on how to download WildBootTests.jl to the description file. 

I have also corrected the link to the JuliaConnectoR package, which now points to CRAN instead of the github repo.

Tests via rhub and github actions pass without notes. 


## Resubmission of 0.12.1

I have added comments regarding the SystemRequirements for Julia and WildBootTests.jl, including a link where to download 
Julia from. I also mention that WildBootTests.jl can be downloaded from the Julia package manager. 

Note that the package runs completely fine without either Julia or WildBootTests.jl - 
the API to the Julia package is completely optional. 

In consequence, I could also completely drop the SystemRequirements section from the description file and simply mention the minimal package versions in the vignette.

I have checked the package on rhub and github actions, without any additional notes / warnings. 

## fwildclusterboot 0.12.1

This is a hot-fix release which turns of some tests that fail on  ATLAS MKL OpenBLAS . 

No tests fail on github actions, rhub and win-devel.

Only one note found: 

"❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'".