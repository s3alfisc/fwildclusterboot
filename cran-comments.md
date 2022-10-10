## Test environment

- local test (windows), R 4.2.1
- [github actions](https://github.com/s3alfisc/fwildclusterboot/actions)
- [win-devel](https://win-builder.r-project.org/eCVI5mKh15EF/)
- [rhub]()


## Check Results

- rhub failed with prep errors for the following setups:  
  - Debian Linux, R-devel, GCC ASAN/UBSAN
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC

There were no errors or warnings. There were multiple notes:

- Fedora Linux (R-hub): 
  - Found the following (possibly) invalid URLs:
  URL: https://www.econ.queensu.ca/sites/econ.queensu.ca/files/wpaper/qed_wp_1485.pdf. This is a valid URL.
  - checking installed package size ... NOTE installed size is  5.1Mb sub-directories of 1Mb or more:
    libs   4.0Mb
  - Non-standard file/directory found at top level:
  ‘codemeta.json’
  - checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found.  I cannot change that Tidy is not on the path, or update Tidy on the external Fedora Linux server.
  - spelling errors = false positives

- Windows (R-hub): 
  - checking HTML version of manual ... NOTE
Found the following HTML validation problems:
setBoottest_engine.html:32:1: Warning: <div> isn't allowed in <h2> elements. Don't know what this means. 
  - checking for detritus in the temp directory ... NOTE
  'lastMiKTeXException'. 
  - spelling errors = false positives

- Windows-devel:
  - spelling errors -> false positives
  - Non-standard file/directory found at top level:
  'codemeta.json'
  - checking HTML version of manual ... [7s] NOTE
Encountered the following conversion/validation errors:
missing value where TRUE/FALSE needed

- Debian (devel):
  - Found the following (possibly) invalid URLs:
  URL: https://www.econ.queensu.ca/sites/econ.queensu.ca/files/wpaper/qed_wp_1485.pdf. Same as above, is valid. 
  - Non-standard file/directory found at top level:
  ‘codemeta.json’
  - checking HTML version of manual ... [1s/1s] NOTE Found the following HTML validation problems: setBoottest_engine.html:32:1     (setBoottest_engine.Rd:19): Warning: `<div> isn't allowed   in <h2>` elements. Again, I don't know what this means nor how to solve it. 
