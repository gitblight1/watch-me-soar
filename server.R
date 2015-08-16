require(tm)

predict_next <- function(line, defaultAnswer) {
  wrds <- c(defaultAnswer, strsplit(line, ' ')[[1]])
  tail(wrds, n=1)
}

shinyServer(
  function(input, output, session) {
    defaultPred = "the" # the most common word in the sample
    # create the main result when the calculate button is clicked.
    output$result <- renderUI({
      input$predAction
      isolate(predict_next(input$txt, defaultPred))
    })
  }
)