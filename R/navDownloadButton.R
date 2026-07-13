#' Create a navbar download button for bs4Dash (AdminLTE 3)
#'
#' Renders a single icon link in the AdminLTE navbar that triggers a Shiny
#' download handler directly — no dropdown required.
#'
#' The `<li class="nav-item dropdown">` wrapper satisfies bs4Dash's
#' `dashboardHeader()` assertion on `rightUi` children. The inner `<a>` carries
#' `shiny-download-link` so Shiny's `DownloadLinkOutputBinding` picks it up
#' without any custom JS.
#'
#' @param outputId The output ID matching a `downloadHandler()` in the server.
#' @param tooltip Label shown on hover. Pass `NULL` to suppress.
#' @param icon_name Font Awesome icon name passed to `shiny::icon()`.
#'
#' @return A `shiny.tag` suitable for `rightUi`/`leftUi` in
#'   `bs4Dash::dashboardHeader()`.
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(bs4Dash)
#' ui <- dashboardPage(
#'   header = dashboardHeader(
#'     title = "Demo",
#'     rightUi = navDownloadButton("download_data", tooltip = "Download CSV")
#'   ),
#'   sidebar = dashboardSidebar(),
#'   body = dashboardBody()
#' )
#' server <- function(input, output) {
#'   output$download_data <- downloadHandler(
#'     filename = "data.csv",
#'     content = function(file) write.csv(mtcars, file)
#'   )
#' }
#' shinyApp(ui, server)
#' }
navDownloadButton <- function(
  outputId,
  tooltip = "Download CSV",
  icon_name = "download"
) {
  htmltools::tags$li(
    class = "nav-item dropdown",
    htmltools::tags$a(
      id = outputId,
      class = "nav-link shiny-download-link disabled",
      href = "",
      target = "_blank",
      download = NA,
      `aria-disabled` = "true",
      tabindex = "-1",
      `data-toggle` = if (!is.null(tooltip)) "tooltip",
      `data-placement` = if (!is.null(tooltip)) "bottom",
      title = tooltip,
      shiny::icon(icon_name),
      navDownloadButtonDependencies()
    )
  )
}

#' Register dependencies for the navbar download button
#' @keywords internal
navDownloadButtonDependencies <- function() {
  htmltools::htmlDependency(
    name = "nav-download-button-css",
    version = "1.0.0",
    src = system.file("css", package = "shinyTinker"),
    stylesheet = "nav-download-button.css"
  )
}
