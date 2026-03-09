library(shiny)

ui <- fluidPage(
  titlePanel("Baseline sanity test"),
  sidebarLayout(
    sidebarPanel(
      actionButton("ping", "Ping"),
      textInput("txt", "Type something:", "hello")
    ),
    mainPanel(
      h4("Outputs below should update immediately:"),
      textOutput("clicked"),
      textOutput("echo")
    )
  )
)

server <- function(input, output, session) {
  output$clicked <- renderText({
    paste("Clicks:", input$ping)
  })
  output$echo <- renderText({
    paste("You typed:", input$txt)
  })
}

shinyApp(ui, server)