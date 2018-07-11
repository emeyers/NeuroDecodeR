library(shiny)

## Only run examples in interactive R sessions
if (interactive()) {
  
  # Example of UI with fluidPage
  ui <- fluidPage(
    
    # Application title
    titlePanel("Hello Shiny!"),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        sliderInput("obs",
                    "Number of observations:",
                    min = 0,
                    max = 1000,
                    value = 500)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )
  
  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
  }
  
  # Complete app with UI and server components
  shinyApp(ui, server)
  
  
  # UI demonstrating column layouts
  ui <- fluidPage(
    title = "Hello Shiny!",
    fluidRow(
      column(width = 4,
             "4"
      ),
      column(width = 3, offset = 10,
             "3 offset 2"
      )
    )
  )
  
  shinyApp(ui, server = function(input, output) { })
}