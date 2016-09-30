require(shiny)
require(miniUI)
require(ggplot2)

variableProfiler <- function()
{

  objects <- ls(pos = 1)
  if (length(objects) == 0)
    stop("No objects found. Please create a data.frame to continue", call. = FALSE)
  dataChoices <- objects[sapply(objects, function(x) is.data.frame(get(x)))]
  dataChoices <- c('iris','mtcars','cars')

  ui <- miniPage(
    gadgetTitleBar("Variable Profiler"),
    miniTabstripPanel(
      miniTabPanel("Inputs", icon = icon("sliders"),
                   miniContentPanel(
                     fillRow(
                       fillCol(
                         selectInput("data", "Data frame", choices = dataChoices),
                         selectInput("colSelect", "Columns", choices = NULL),
                         checkboxInput("logscale", "Log 10 transform", value = FALSE)
                       ),
                       fillCol(
                         plotOutput("profile1", width = "100%", height = "100%")
                       )
                     )
                   )
      ),
      miniTabPanel("Descriptive Statistics", icon = icon("bar-chart"),
                   miniContentPanel(
                     fillRow(
                       fillCol(
                         verbatimTextOutput("summary")
                       ),
                       fillCol(
                         plotOutput("profile2", width = "100%", height = "100%")
                       )
                     )
                   )
      ),
      miniTabPanel("Outliers", icon = icon("tasks"),
                   miniContentPanel(
                     fillRow(
                       fillCol(
                         checkboxInput("trim","Trim outliers", value = FALSE),
                         numericInput("min", "Minimum", value = NULL),
                         numericInput("max", "Maximum", value = NULL),
                         conditionalPanel(condition = "input.trim == true",
                                          textOutput("filter")
                         ),
                         actionButton("button", "Output Filter Code")
                       ),
                       fillCol(
                         plotOutput("profile3", width = "100%", height = "100%")
                       )
                     )
                   )
      )
    )
  )

  server <- function(input, output, session) {

    out <- reactiveValues()

    data <- reactive({
      validate(need(input$data != "", "No data frames found"))
      get(input$data)
    })

    vec <- reactive({
      x <- data()[[input$colSelect]]
      if(input$logscale){
        validate(need(class(x) %in% c('numeric', 'integer'), "Data must be numeric"))
        validate(need(all(x > 0), "Data must be greater than zero"))
        x <- log10(x)
      }
      x
    })

    observe({
      updateSelectInput(session, "colSelect", choices = names(data()))
    })

    observe({
      if(vclass() == 'quant'){
        minv <- min(vec())
        maxv <- max(vec())
        stepv <- signif((maxv - minv) / 10, 1)
        updateNumericInput(session, "min", value = signif(minv), step = stepv)
        updateNumericInput(session, "max", value = signif(maxv), step = stepv)
      }
      if(vclass() == 'cat'){
        updateNumericInput(session, "min", value = NULL)
        updateNumericInput(session, "max", value = NULL)
      }
    })

    vclass <- reactive({
      x <- class(vec())
      y <- NULL
      if(x %in% c('logical', 'character', 'factor')) y <- 'cat'
      if(x %in% c('integer', 'numeric')) y <- 'quant'
      validate(need(!is.null(y), "Class not supported"))
      y
    })

    output$summary <- renderPrint({
      x <- vec()
      y <- NULL
      if(vclass() == 'quant'){
        if(input$trim) x <- x[x>=input$min & x<=input$max]
        y <- list(N = length(x), Summary = summary(x), StdDev = sd(x))
        if(length(x) > 3 & length(x) < 5000) y$NormalityTest <- shapiro.test(x)
      }
      if(vclass() == 'cat'){
        y <- list(N = length(x), Summary = summary(x))
      }
      y
    })

    output$profile1 <- output$profile2 <- output$profile3 <- renderPlot({
      validate(need(!is.null(vec()), "No data found"))
      title1 <- paste(ifelse(input$logscale,'Log 10', ''), input$colSelect)
      if(vclass() == 'quant') {
        p1 <- qplot(vec(), geom = 'histogram', bins = 15, color = I('white'), fill = I('cornflowerblue'))
        if(input$trim) p1 <- p1 +
            geom_vline(xintercept = c(input$min, input$max), col = 'tomato')
      }
      if(vclass() == 'cat') p1 <- qplot(vec(), geom = 'bar', fill = factor(vec()))
      p1 + ggtitle(title1) + xlab('')
    })

    output$filter <- renderText({
      tail(out$filters, 1)
    })

    observe({
      input$button
      isolate({
        if(input$trim & vclass() == 'quant'){
          colSelect <- input$colSelect
          if(input$logscale) colSelect <- paste0("log10(", colSelect, ")")
          x <- paste0("filter(", input$data, ", ",
                      colSelect, " >= ", input$min, " & ",
                      colSelect, " <= ", input$max, ")")
          out$filters <- c(out$filters, x)
        }
      })
    })

    observeEvent(input$done, {
      x <- out$filters[!duplicated(out$filters)]
      stopApp(x)
    })
    
    observeEvent(input$cancel, {
      invisible(stopApp())
    })

  }

  runGadget(ui, server, viewer = dialogViewer('Variable Profiler', 900, 600))

}

### Profile variables for varias data sets

filters <- variableProfiler()
filters
