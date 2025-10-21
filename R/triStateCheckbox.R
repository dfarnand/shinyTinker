#' Create a tri-state checkbox input control
#'
#' This function creates a tri-state checkbox input that lets users cycle
#' through three states: Include (1), Exclude (-1), and Neither/Neutral (0).
#' The checkbox visually indicates the state with different icons and colors.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control, or `NULL` for no label.
#' @param value Initial value: -1 (exclude), 0 (neutral), or 1 (include)
#'
#' @return A custom Shiny input control that can be added to a UI definition
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   triStateCheckboxInput("checkbox", "Select option", 0)
#' )
#' server <- function(input, output) {
#'   observeEvent(input$checkbox, {
#'     print(getTriStateLabel(input$checkbox))
#'   })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @importFrom shiny getDefaultReactiveDomain
triStateCheckboxInput <- function(inputId, label, value = 0) {
  # Validate input value
  if (!value %in% c(-1, 0, 1)) {
    stop("value must be -1 (exclude), 0 (neutral), or 1 (include)")
  }

  # Create the input element
  htmltools::div(
    htmltools::tagList(
      # Make sure dependencies are included
      triStateCheckboxDependencies(),

      # Create the actual input element
      htmltools::div(
        class = "tri-state-container",
        htmltools::span(
          id = inputId,
          class = "tri-state-checkbox",
          `data-state` = value
        ),
        htmltools::span(class = "tri-state-label", label)
      )
    )
  )
}

#' Update a tri-state checkbox input
#'
#' Change the value and/or label of a tri-state checkbox input on the client.
#'
#' @param session The Shiny session object (default is getDefaultReactiveDomain())
#' @param inputId The ID of the input object to update
#' @param label The new label (optional)
#' @param value The new value (optional): -1 (exclude), 0 (neutral), or 1 (include)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In server function
#' updateTriStateCheckboxInput(session, "checkbox", label = "New Label", value = 1)
#' }
#'
#' @importFrom shiny getDefaultReactiveDomain
updateTriStateCheckboxInput <- function(
  session = shiny::getDefaultReactiveDomain(),
  inputId,
  label = NULL,
  value = NULL
) {
  # Build message to send to the input
  message <- list()

  if (!is.null(label)) {
    message$label <- label
  }

  if (!is.null(value)) {
    if (!value %in% c(-1, 0, 1)) {
      stop("value must be -1 (exclude), 0 (neutral), or 1 (include)")
    }
    message$value <- value
  }

  # Send message to input
  session$sendInputMessage(inputId, message)

  invisible(NULL)
}

#' Helper function to convert numeric state to a readable label
#'
#' @param state The state value: -1, 0, or 1
#' @return A string description of the state: "Exclude", "Neutral", or "Include"
#' @export
#'
#' @examples
#' getTriStateLabel(-1) # "Exclude"
#' getTriStateLabel(0)  # "Neutral"
#' getTriStateLabel(1)  # "Include"
getTriStateLabel <- function(state) {
  switch(
    as.character(state),
    "-1" = "Exclude",
    "0" = "Neutral",
    "1" = "Include",
    "Unknown"
  )
}

#' Register dependencies for tri-state checkbox
#' @keywords internal
triStateCheckboxDependencies <- function() {
  # Create CSS and JS dependencies
  css_dep <- htmltools::htmlDependency(
    name = "tri-state-checkbox-css",
    version = "1.0.0",
    src = system.file("css", package = "shinyTinker"),
    stylesheet = "tri-state-checkbox.css"
  )

  js_dep <- htmltools::htmlDependency(
    name = "tri-state-checkbox-js",
    version = "1.0.0",
    src = system.file("js", package = "shinyTinker"),
    script = "tri-state-checkbox-binding.js"
  )

  # Return combined dependencies
  htmltools::tagList(css_dep, js_dep)
}
