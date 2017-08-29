require(shiny)
require(miniUI)

get_password <- function() {
  ui <- miniPage(
    gadgetTitleBar("Please enter your password"),
    miniContentPanel(
      passwordInput("password", "", width = "100%")
    )
  )
  
  server <- function(input, output) {
    observeEvent(input$done, {
      stopApp(input$password)
    })
    observeEvent(input$cancel, {
      stopApp(stop("No password.", call. = FALSE))
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer('Password', 450, 200))
}
get_password()

