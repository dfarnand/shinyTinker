.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler(
    "shiny.triStateCheckbox",
    function(value, session, inputName) {
      # Simple passthrough
      return(value)
    },
    force = TRUE
  )
}
