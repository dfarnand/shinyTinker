## Demo app demonstrates all custom widgets in the shinyTinker package.
## Run this with: shiny::runApp("inst/examples/demo.R")

message("Loading shinyTinker from source...")
devtools::load_all()

library(shiny)
library(shinyTinker)

ui <- fluidPage(
    titlePanel("shinyTinker Demo"),

    sidebarLayout(
        sidebarPanel(
            width = 4,

            h3("Tri-State Checkbox"),
            p("Click to cycle through: Include → Exclude → Neutral"),
            triStateCheckboxInput(
                "tristate",
                "Enable feature",
                value = 0
            ),

            hr(),

            h3("Info Radio Buttons"),
            p(
                "Radio buttons with info bubbles (hover over the",
                tags$i(class = "fa fa-info"),
                "icons)"
            ),
            infoRadioButtons(
                inputId = "radio1",
                label = "Select a data processing method:",
                choices = c(
                    "Mean Imputation" = "mean",
                    "Median Imputation" = "median",
                    "Forward Fill" = "ffill",
                    "Backward Fill" = "bfill"
                ),
                descriptions = c(
                    "Replace missing values with the **mean** of the column",
                    "Replace missing values with the **median** of the column",
                    "Propagate last valid observation forward to fill gaps",
                    "Use next valid observation to fill gaps (backward)"
                ),
                selected = "mean",
                status = "primary"
            ),

            hr(),

            infoRadioButtons(
                inputId = "radio2",
                label = "Choose visualization style:",
                choices = c(
                    "Very Long Option Name That Will Be Truncated in Sidebars" = "long",
                    "Another Extremely Long Name to Demonstrate Truncation Behavior" = "another",
                    "Short" = "short"
                ),
                descriptions = c(
                    "This demonstrates **text truncation** in narrow sidebars. The full label text appears as the title when you hover/click the info button.",
                    "The info button always remains visible, even when label text is truncated in narrow containers like bs4Dash sidebars.",
                    "A shorter option for comparison."
                ),
                selected = "short",
                status = "success",
                shape = "round"
            )
        ),

        mainPanel(
            h3("Output Values"),

            h4("Tri-State Checkbox Value:"),
            verbatimTextOutput("tristate_value"),
            textOutput("tristate_label"),

            hr(),

            h4("Info Radio Button Values:"),
            p(strong("Data processing method:")),
            verbatimTextOutput("radio1_value"),

            p(strong("Visualization style:")),
            verbatimTextOutput("radio2_value"),

            hr()
        )
    )
)

server <- function(input, output, session) {
    # Tri-state checkbox outputs
    output$tristate_value <- renderPrint({
        input$tristate
    })

    output$tristate_label <- renderText({
        paste("State:", getTriStateLabel(input$tristate))
    })

    # Info radio button outputs
    output$radio1_value <- renderPrint({
        input$radio1
    })

    output$radio2_value <- renderPrint({
        input$radio2
    })
}

shinyApp(ui, server)
