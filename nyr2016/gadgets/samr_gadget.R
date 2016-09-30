require(samr)
require(shiny)
require(miniUI)

runmodel <- function(data, w = "95%"){
  
  ui <- miniPage(
    gadgetTitleBar("Microarray Analysis"),
    miniTabstripPanel(
      miniTabPanel(
        "Inputs", 
        icon = icon("sliders"), 
        miniContentPanel(
          scrollable = F,
          fillCol(flex=c(9,1),
                  fillRow(
                    fillCol(
                      selectInput("resp.type", "Response Type",
                                  c("Quantitative","Two class unpaired",
                                    "Survival","Multiclass", "One class", "Two class paired",
                                    "Two class unpaired timecourse", "One class timecourse",
                                    "Two class paired timecourse", "Pattern discovery"),
                                  "Two class unpaired", width = w),
                      selectInput("assay.type", "Assay Type", c("array", "seq"), "array", width = w),
                      numericInput("array", "Array", NULL, width = w),
                      numericInput("s0.perc", "s0 Percentile", NULL, width = w)
                    ),
                    fillCol(
                      numericInput("nperms", "Number of Perms", 100, width = w),
                      selectInput("testStatistic", "Test statistic", c("standard","wilcoxon"), "standard", width = w),
                      selectInput("time.summary.type", "Time summary type", c("slope", "signed.area"), "slope", width = w),
                      selectInput("regression.method", "Regresson method", c("standard", "ranks"), "standard", width = w)
                    ),
                    fillCol(
                      numericInput("knn.neighbors", "KNN for imputation", 20, width = w),
                      numericInput("random.seed", "Random seed", NULL, width = w),
                      numericInput("nresamp", "N resamp", 20, width = w),
                      numericInput("nresamp.perm", "N resamp", NULL, width = w)
                    )
                  ),
                  fillRow(
                    checkboxInput("center.arrays", "Center arrays", FALSE, width = w),
                    checkboxInput("return.x", "Return matrix", FALSE, width = w)
                  )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    observeEvent(input$done, {
      modelout <- samr::samr(
        data,
        resp.type=input$resp.type,
        assay.type=input$assaytype,
        s0 = input$s0,
        s0.perc = if(is.na(input$s0.perc)) NULL else input$s0.perc,
        nperms=input$nperms,
        center.arrays=input$center.arrays,
        testStatistic=input$tesStatistic,
        time.summary.type = input$time.summary.type,
        regression.method=input$regression.method,
        return.x=input$return.x,
        knn.neighbors=input$knn.neighbors,
        random.seed = if(is.na(input$random.seed)) NULL else input$random.seed,
        nresamp=input$rsamp,
        nresamp.perm = input$nresamp.perm
      )
      stopApp(modelout)
    })
    observeEvent(input$cancel, {
      invisible(stopApp())
    })
  }
  
  runGadget(ui, server, viewer = dialogViewer('Significance Analysis of Microarrays', 600, 400))
  
}

