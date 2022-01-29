library(shinyWidgets)

if (interactive()) {

library(shiny)
library(shinyWidgets)

ui <- fluidPage(
  tags$h2("Download bttn"),
  downloadBttn(
    outputId = "downloadData",
    style = "bordered",
    color = "primary"
  )
)

server <- function(input, output, session) {

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(mtcars, con)
    }
  )

}

shinyApp(ui, server)

}