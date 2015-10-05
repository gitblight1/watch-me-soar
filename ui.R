
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Predictive Text Model"),
    # Input section
    sidebarPanel(
      textInput('txt', 'Enter your text:'),
      actionButton('predAction', 'Predict!'),
      helpText('Enter a line of 0 or more words. This app will attempt to predict the next word.')
    ),
    # Output section
    mainPanel(
       h3('Results'),
       uiOutput('result') # output is dynamically rendered in server.R
    )
  )
)