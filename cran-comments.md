## Re-Submission of 0.12

The pre-submission checks complained about misspelled words in the description 
file and the json.meta file. I have deleted the json.meta file and put all flagged words into '' parentheses. Win-devel and rhub no longer flag these issues. 

## Submission of 0.12 

I checked the package following (almost all) steps in thinkR's [prepare-for-cran script](https://github.com/ThinkR-open/prepare-for-cran).

I received no errors or warnings, but the following notes: 

- the spelling errors are false positives - all words are spelled correctly
- I have checked that the URL is working

❯ checking CRAN incoming feasibility ... [36s] NOTE
  Maintainer: 'Alexander Fischer <alexander-fischer1801@t-online.de>'
  
  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    HC (46:67)
    MacKinnon (35:50, 38:70)
    Roodman (34:37)
    STATA (34:58)
    WCR (42:55)
    WCU (43:15)
    al (34:48, 35:63)
    et (34:45, 35:60)
    heteroskedasticity (46:40)
    multiway (40:23)
    subcluster (41:51)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2022-10-09 as requires archived package
      'Matrix.utils'.

❯ checking top-level files ... NOTE
  Non-standard file/directory found at top level:
    'codemeta.json'

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
Found the following (possibly) invalid URLs:
  URL: https://www.econ.queensu.ca/sites/econ.queensu.ca/files/wpaper/qed_wp_1485.pdf
    From: README.md
          NEWS.md
    Status: Error
    Message: libcurl error code 60:
      	SSL certificate problem: unable to get local issuer certificate
      	(Status without verification: OK)

* checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    libs   4.0Mb

* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 3 notes ✖
