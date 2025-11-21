#' Info Radio Buttons Input Control
#'
#' Create a set of radio buttons with info bubbles used to select an item from a list.
#'
#' @param inputId The `input` slot that will be used to access the value.
#' @param label Display label for the control.
#' @param choices List of values to show radio buttons for. If elements of the list
#'  are named then that name rather than the value is displayed to the user. If
#'  this argument is provided, then `choiceNames` and `choiceValues` must not be provided,
#'  and vice-versa. The values should be strings; other types (such as logicals and
#'  numbers) will be coerced to strings.
#' @param selected The values that should be initially selected,
#' (if not specified then defaults to the first value).
#' @param descriptions List of description strings to display in info bubbles for each choice.
#'  Must be the same length as choices. Use `NULL` or empty string for choices without descriptions.
#' @param status Add a class to the radio,
#' you can use Bootstrap status like 'info', 'primary', 'danger', 'warning' or 'success'.
#' @param shape Shape of the radio between `square`, `curve` and `round`.
#' @param outline Color also the border of the radio (`TRUE` or `FALSE`).
#' @param fill Fill the radio with color (`TRUE` or `FALSE`).
#' @param thick Make the content inside radio smaller (`TRUE` or `FALSE`).
#' @param animation Add an animation when radio is checked, a value between
#' `smooth`, `jelly`, `tada`, `rotate`, `pulse`.
#' @param icon Optional, display an icon on the radio, must be an icon created with `icon`.
#' @param plain Remove the border when radio is checked (`TRUE` or `FALSE`).
#' @param bigger Scale the radio a bit bigger (`TRUE` or `FALSE`).
#' @param inline If `TRUE`, render the choices inline (i.e. horizontally).
#' @param width The width of the input, e.g. `400px`, or `100%`.
#' @param choiceNames List of names to display to the user.
#' @param choiceValues List of values corresponding to `choiceNames`
#'
#' @return A character vector or `NULL` server-side.
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   infoRadioButtons(
#'     "radio", "Choose an option:",
#'     choices = c("Option 1", "Option 2"),
#'     descriptions = c("Description for option 1", "Description for option 2")
#'   )
#' )
#' server <- function(input, output) {
#'   observeEvent(input$radio, {
#'     print(input$radio)
#'   })
#' }
#' shinyApp(ui, server)
#' }
#'
infoRadioButtons <- function(
  inputId,
  label,
  choices = NULL,
  selected = NULL,
  descriptions = NULL,
  status = "primary",
  shape = c("round", "square", "curve"),
  outline = FALSE,
  fill = FALSE,
  thick = FALSE,
  animation = NULL,
  icon = NULL,
  plain = FALSE,
  bigger = FALSE,
  inline = FALSE,
  width = NULL,
  choiceNames = NULL,
  choiceValues = NULL
) {
  status <- match.arg(
    status,
    c("default", "primary", "success", "info", "danger", "warning")
  )
  shape <- match.arg(shape)
  if (is.null(choices) && is.null(choiceNames) && is.null(choiceValues)) {
    choices <- character(0)
  }
  args <- shinyWidgets:::normalizeChoicesArgs(
    choices,
    choiceNames,
    choiceValues
  )

  # Validate descriptions
  if (!is.null(descriptions)) {
    if (length(descriptions) != length(args$choiceValues)) {
      stop(
        "'descriptions' must be the same length as 'choices' (or 'choiceValues')"
      )
    }
  } else {
    descriptions <- rep("", length(args$choiceValues))
  }

  # Convert descriptions from markdown to HTML (use vapply for stable type)
  if (!is.null(descriptions)) {
    descriptions <- vapply(
      descriptions,
      FUN.VALUE = "",
      function(desc) {
        if (nzchar(as.character(desc))) {
          as.character(shiny::markdown(as.character(desc)))
        } else {
          ""
        }
      }
    )
  }

  selected <- shiny::restoreInput(id = inputId, default = selected)
  selected <- if (is.null(selected)) {
    args$choiceValues[[1]]
  } else {
    as.character(selected)
  }
  if (length(selected) > 1) {
    stop("The 'selected' argument must be of length 1")
  }
  options <- generateInfoPretty(
    inputId = inputId,
    selected = selected,
    inline = inline,
    type = "radio",
    choiceNames = args$choiceNames,
    choiceValues = args$choiceValues,
    descriptions = descriptions,
    status = status,
    shape = shape,
    outline = outline,
    fill = fill,
    thick = thick,
    animation = animation,
    icon = icon,
    plain = plain,
    bigger = bigger
  )
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) {
    divClass <- paste(divClass, "shiny-input-container-inline")
  }
  radioTag <- htmltools::tags$div(
    id = inputId,
    style = htmltools::css(width = htmltools::validateCssUnit(width)),
    class = divClass,
    shinyWidgets:::label_input(inputId, label),
    options
  )

  # Attach local dependencies: CSS and initializer JS
  css_dep <- htmltools::htmlDependency(
    name = "info-radio-buttons-css",
    version = "1.0.0",
    src = system.file("css", package = "shinyTinker"),
    stylesheet = "info-radio-buttons.css"
  )

  js_dep <- htmltools::htmlDependency(
    name = "info-radio-buttons-js",
    version = "1.0.0",
    src = system.file("js", package = "shinyTinker"),
    script = "info-radio-buttons.js"
  )

  radioTag <- htmltools::attachDependencies(radioTag, list(css_dep, js_dep))

  shinyWidgets:::attachShinyWidgetsDep(radioTag, "pretty")
}


#' Internal function to generate pretty radio options with info bubbles
generateInfoPretty <- function(
  inputId,
  selected,
  inline,
  type = "checkbox",
  choiceNames,
  choiceValues,
  descriptions = NULL,
  status = "primary",
  shape = "square",
  outline = FALSE,
  fill = FALSE,
  thick = FALSE,
  animation = NULL,
  icon = NULL,
  plain = FALSE,
  bigger = FALSE
) {
  icon <- shinyWidgets:::tag_add_class_icon(icon)

  if (is.null(descriptions)) {
    descriptions <- rep("", length(choiceValues))
  }

  options <- mapply(
    choiceValues,
    choiceNames,
    descriptions,
    FUN = function(value, name, desc) {
      inputTag <- htmltools::tags$input(
        type = type,
        name = inputId,
        value = value
      )
      if (identical(as.character(value), as.character(selected))) {
        inputTag$attribs$checked <- "checked"
      }

      # Create info bubble using Bootstrap popover if description exists
      infoBubble <- if (nchar(desc) > 0) {
        htmltools::tags$span(
          class = "info-bubble-container",
          htmltools::tags$span(
            class = "info-bubble",
            `data-toggle` = "popover",
            `data-content` = desc,
            # Provide title for all supported Bootstrap variants
            title = as.character(name),
            `data-original-title` = as.character(name),
            `data-bs-title` = as.character(name),
            icon("info")
          )
        )
      } else {
        NULL
      }

      # Build class strings once to avoid repeated class= args
      pretty_classes <- c("pretty")
      if (is.null(icon)) {
        pretty_classes <- c(pretty_classes, "p-default")
      }
      if (plain) {
        pretty_classes <- c(pretty_classes, "p-plain")
      }
      if (bigger) {
        pretty_classes <- c(pretty_classes, "p-bigger")
      }
      if (shape != "square") {
        pretty_classes <- c(pretty_classes, paste0("p-", shape))
      }
      if (fill) {
        pretty_classes <- c(pretty_classes, "p-fill")
      }
      if (thick) {
        pretty_classes <- c(pretty_classes, "p-thick")
      }
      if (!is.null(animation)) {
        pretty_classes <- c(pretty_classes, paste0("p-", animation))
      }
      if (!is.null(icon)) {
        pretty_classes <- c(pretty_classes, "p-icon")
      }

      state_classes <- c("state")
      if (status != "default") {
        state_classes <- c(
          state_classes,
          paste0("p-", status, if (outline) "-o")
        )
      }

      if (inline) {
        htmltools::tags$div(
          class = paste(pretty_classes, collapse = " "),
          inputTag,
          htmltools::tags$div(
            class = paste(state_classes, collapse = " "),
            if (!is.null(icon)) icon,
            htmltools::tags$label(htmltools::tags$span(name))
          ),
          infoBubble
        )
      } else {
        htmltools::tagList(
          htmltools::tags$div(
            style = "display: flex; align-items: center;",
            htmltools::tags$div(
              class = paste(pretty_classes, collapse = " "),
              inputTag,
              htmltools::tags$div(
                class = paste(state_classes, collapse = " "),
                if (!is.null(icon)) icon,
                htmltools::tags$label(htmltools::tags$span(name))
              )
            ),
            infoBubble
          ),
          htmltools::tags$div(style = "height:3px;")
        )
      }
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  htmltools::tags$div(
    if (!inline) htmltools::tags$div(style = "height:7px;"),
    class = "shiny-options-group",
    options
  )
}
