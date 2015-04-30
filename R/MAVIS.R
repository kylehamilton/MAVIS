#' Start MAVIS
#' @title Launch MAVIS interactive User Interface
#' @return Nothing
#' @description startmavis starts loads the web browser an interactive user interface built using R shiny.
#' @details The purpose of building the interactive user interface is to provide an easy for people who are learning how to do their first meta-analysis and/or are not comfortable with the R command line system. Includes example data for testing out a meta-analysis.
#' @keywords MAVIS
#' @examples
#' \dontrun{
#' library(shiny)
#' startmavis()
#' }
#' @export
.onAttach <-
  function (libname, pkgname) 
  {
    loadmsg <- "Loading MAVIS: Meta Analysis via Shiny  package (version 1.1). To start MAVIS please type: startmavis()."
    packageStartupMessage(loadmsg, domain = NULL, appendLF = TRUE)
  }

startmavis <- function() {

  shiny::runApp(appDir = system.file("shiny", package="MAVIS"))

}
