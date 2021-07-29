if (requireNamespace("tinytest", quietly = TRUE)) {
  # run tests only if both lfe and fixest are installed
  if(requireNamespace("lfe") && requireNamespace("fixest")){
    library(lfe)
    library(fixest)
    tinytest::test_package("fwildclusterboot")
  }
}

