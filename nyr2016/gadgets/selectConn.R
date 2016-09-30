require(shiny)
require(miniUI)

selectConn <- function() {
  ui <- miniPage(
    gadgetTitleBar("ODBC Connect"),
    miniContentPanel(
      selectInput('server', 'Server', c(
        'SQL Server',
        'Postgres'
      ), width='100%'),
      p(strong('Port')),
      verbatimTextOutput("selectedPort"),
      textInput('database', 'Database', '', width='100%'),
      textInput('uid', 'User ID', '', width='100%'),
      passwordInput("pwd", "Password", '', width='100%')
    )
  )

  server <- function(input, output) {

    output$selectedPort <- renderText({
      port()
    })

    port <- reactive({
      switch(
        input$server,
        `SQL Server` = 1433,
        `Postgres`   = 5432
      )
    })

    serverEndpoint <- reactive({
      switch(
        input$server,
        `SQL Server`  = 'mysshost',
        `Progress`    = 'mypsqlhost'
      )
    })

    driver <- reactive({
      switch(
        input$server,
        `SQL Server` = 'myssdriver.so',
        `Postgres`   = 'mypsqldriver.so'
      )
    })

    observeEvent(input$done, {

      connectionList <- list(
        Driver = driver(),
        Server = serverEndpoint(),
        Port = port(),
        Database = input$database,
        UID = input$uid,
        PWD = input$pwd
      )

      connectionString <- paste(names(connectionList), connectionList, sep = '=', collapse = ';')

      stopApp(connectionString)

    })

  }

  runGadget(ui, server, viewer = dialogViewer('ODBC Connection', 350, 450))

}

### Connect
selectConn()
