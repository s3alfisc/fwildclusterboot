fwildclusterboot_setup <- function(){
  
  #' install Julia, connect R and Julia, install and pre-compile WildBootTests.jl
  #' @importFrom JuliaConnectoR juliaEval
  #' @importFrom utils install.packages installed.packages
  #' @importFrom usethis edit_r_environ
  #' @importFrom JuliaCall install_julia
  #' @export
  
  
  install_julia <- readline(prompt= "Install Julia? If Yes, type 'Yes', else 'No': " )
  if(install_julia == "Yes"){
    install_julia <- TRUE
    if(!requireNamespace(JuliaCall)){
      install_JuliaCall <- readline("To install Julia from within R, the JuliaCall package needs to be installed. To install the JuliaCall package, type 'Yes', else 'No':")
      if(install_JuliaCall){
        install.packages(JuliaCall)
      } else {
        stop("Please download Julia from https://julialang.org/downloads/.")
      }
    }
    
  } else if(install_julia == "No"){
    install_julia <- FALSE
  }
  
  # if(!is.logical(install_julia)){
  #   cat("Please provide a logical: TRUE if you want to install Julia, and FALSE if not.")
  # }
  
  if(install_julia){
    JuliaCall::install_julia()
  }
  
  connect_r_julia <- readline("Have you already added your Julia path to your renviron file? If yes, type 'Yes', else type 'No': ")
  if(connect_r_julia == "No"){
    # check if usethis is installed
    if(!requireNamespace(usethis)){
      install_usethis <- readline("To set the path to Julia from within R, the 'usethis' package needs to be installed. Reply 'Yes' to install 'usethis':")
      if(install_usethis){
        install.packages(usethis)
      } else {
        stop("fwildclusterboot_setup() cannot proceed without the 'usethis' package.")
      }
    }
    message("Add JULIA_BINDIR= '.../Julia-1.X.X/bin' to your renviron file and save the changes.
    Restart your R session and rerun fwildclusterboot_setup(), but skip all previously completed steps.", appendLF = FALSE)
    suppressMessages(usethis::edit_r_environ())
  }
  
  if(connect_r_julia == "Yes"){
    
    install_wildboottests <- readline(prompt="Install WildBootTests.jl? If Yes, type 'Yes', else 'No': " )
    if(install_wildboottests == "Yes"){
      install_wildboottests <- TRUE
    } else if(install_wildboottests == "No"){
      install_wildboottests <- FALSE
    } else {
      install_wildboottests <- readline(prompt= paste0("You need to reply with 'Yes' or 'No', but you replied with ", install_wildboottests, ". Please reply with 'Yes' or 'No': "))
      if(install_wildboottests == "Yes"){
        install_wildboottests <- TRUE
      } else {
        message("WildBootTests.jl will not be installed.")
        install_wildboottests <- FALSE
      }
    }
    
    if(as.logical(install_wildboottests)){
      JuliaConnectoR::juliaEval("using Pkg")
      JuliaConnectoR::juliaEval('Pkg.add("WildBootTests")')
      cat("Pre-compile WildBootTests.jl. This might take a few seconds.", "\n")
      JuliaConnectoR::juliaEval("using WildBootTests")
    }
    
  }
  
  cat("Great! fwildclusterboot is ready for use!", "\n")
  
  
}