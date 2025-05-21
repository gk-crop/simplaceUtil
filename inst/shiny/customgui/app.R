library(shiny)
library(shinyFiles)
library(simplaceUtil)
library(DT)
library(simplace)

sd <- getShinyOption("simplacedirs")
solutionlist <- getShinyOption("solutions")
projectlist <- getShinyOption("projects")
paramlist <- getShinyOption("paramlist")
paramtransform <- getShinyOption("paramtransform")
plotlist <- getShinyOption("plotlist")
datalist <- getShinyOption("datalist")


tableOptions <- list(
  lengthMenu = list(c(10,25,100,200,-1),c("10","25","100","200","All")),
  pageLength=25
)


sp <- initSimplace(sd$instdir, sd$workdir, sd$outdir, javaparameters=sd$options)


openProject(sp, solutionlist, projectlist)


#mylist <- c("a","b","c")

ui <- fluidPage(
  titlePanel("Simplace R GUI"),
  sidebarLayout(
    sidebarPanel(
      textOutput("test"),
      uiOutput("inputs"),
      checkboxInput("queue","Keep results from previous simulations", value=FALSE),
      uiOutput("outselect"),
      width=3


    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs",
          uiOutput("variables"),
          uiOutput("plots")
        ),
        tabPanel("Data",
          uiOutput("outdata")
        ),
        tabPanel("Run Parameters",
          dataTableOutput("outruns"),
          downloadButton("dl_outruns","Save Run Parameters")
        )
      )
    )
  )
)






# Define server logic
server <- function(input, output) {



  v <- reactiveValues()
  v$simulated <- FALSE

  v$xvalues <- "CURRENT.DATE"
  v$yvalues <- "CURRENT.DATE"

  v$start <- NULL
  v$end <- NULL

  v$runs <- NULL

  elem <- getElementsFromSolutionFile(solutionlist)
  outids <- getMemoryOutputIds(elem$components)
  v$elem <- elem
  v$outids <- outids
  v$outid <- outids[1]



  param <- reactive({
    l <- list()
    for(i in names(paramlist)) {
      l[[i]] <- input[[i]]
    }
    l
  })

  alldata <- reactive({
    if(length(v$outids)>0) {
      par <- paramtransform(param())

      output$test <- renderText("running simulation")
      if(!input$queue) {
        resetSimulationQueue(sp)
      }


      simid = createSimulation(sp,par,queue = input$queue)
      runSimulations(sp)
      df <- list()
      for(outid in v$outids) {
        res <- getResult(sp, outid)
        df[[outid]] <- resultToDataframe(res)
      }

      v$simulated <- TRUE
      output$test <- renderText("simulation finished")
      if(!input$queue) {
        resetSimulationQueue(sp)
        if(!is.null(isolate(v$runs))) {
          isolate(v$runs$hasdata <-FALSE)
        }
      }
      if(length(par)>0) {
        run <- as.data.frame(par)
        run$simulationid <- simid
        run$hasdata <- TRUE
        v$runs <- rbind(isolate(v$runs),run)
      }


      print(paste("ran sim",unique(df[[1]]$simulationid)))
      df
    }

  }
  )

  data <- reactive({
    alldata()[[v$outid]]
  })

  # inputs
  il<-list()
  for(i in seq_along(paramlist))  {
    id <- names(paramlist)[i]
    par <- paramlist[[i]]
    if(length(par$options)>0) {
      ip <- selectInput(id, label=par$label, choices = par$options, multiple=FALSE)
    }
    else if(!is.null(par$min) && !is.null(par$max) && par$min < par$max) {
      ip <- sliderInput(id, label=par$label,
                        value=par$value,
                        min=par$min, max=par$max)
    }
    else {
      ip <- textInput(id, label=par$label, value=par$value)
    }

    il[[i]]<-ip
  }
  output$inputs <- renderUI({do.call(tagList,il)})


  output$outselect <-  renderUI(

    {
      ch_m <-v$outids
      names(ch_m)<-ch_m
      if(length(ch_m)>0) {
        selectInput("outid","Select outputs",choices = ch_m, selected=1,multiple=FALSE)
      }
    }
  )

  output$variables <- renderUI({
    ch <- names(data())
    dmin <- min(data()$CURRENT.DATE)
    dmax <- max(data()$CURRENT.DATE)
    if(is.null(v$start)) {v$start <- dmin}
    if(is.null(v$end)) {v$end <-dmax}

    tagList(
      fluidRow(
        column(3,selectInput("xvariable","x-Values",choices = ch,selected=v$xvalues)),
        column(3,selectInput("yvariable","y-Values",choices = ch,selected=v$yvalues,multiple=TRUE)),
        column(3,dateInput("start","Start",value=v$start,min=dmin-730,max=dmax)),
        column(3,dateInput("end","End", value=v$end,min=dmin,max=dmax+730))
      )
    )}
  )

  # plots
  tl <- list()
  for(i in seq_along(plotlist)) {
    n <- names(plotlist)[[i]]
    id = paste0("plot_",n)
    tl[[2*i-1]]<-h5(n)
    tl[[2*i]]<-plotOutput(id)
  }
  output$plots <- renderUI(do.call(tagList,tl))

  for(i in seq_along(plotlist)) {
    local({
      n <- names(plotlist)[[i]]
      id = paste0("plot_",n)
      output[[id]] <- renderPlot({plotlist[[n]](data(),input$xvariable, input$yvariable,
                                                NULL,
                                                input$start, input$end)})
    })
  }


  # tables
  tlt <- list()
  for(i in seq_along(datalist)) {
    n <- names(datalist)[[i]]
    id = paste0("data_",n)
    tlt[[3*i-2]]<-h5(n)
    tlt[[3*i-1]]<-dataTableOutput(id)
    tlt[[3*i]]<-downloadButton(paste0("dl_",id),"Save Data")

  }
  output$outdata <- renderUI(do.call(tagList,tlt))

  for(i in seq_along(datalist)) {
    local({
      n <- names(datalist)[[i]]
      id = paste0("data_",n)
      output[[id]] <- renderDataTable(datalist[[n]](data()),options=tableOptions)
      output[[paste0("dl_",id)]]<-downloadHandler(
        filename = function() {
          paste(input$outid,"_",id,"_", format(Sys.time(),format="%Y%m%d_%H%M%S"), ".csv", sep="")
        },
        content = function(file) {
          write.csv(data(), file)
        }
      )
    })
  }

  output[["dl_outruns"]]<-downloadHandler(
    filename = function() {
      paste("RunParameters","_", format(Sys.time(),format="%Y%m%d_%H%M%S"), ".csv", sep="")
    },
    content = function(file) {
      write.csv(v$runs, file)
    }
  )


  output$outruns <- renderDataTable(v$runs)

  observeEvent(input$xvariable,
               {v$xvalues<-input$xvariable;},
               ignoreInit=TRUE)
  observeEvent(input$yvariable,
               {v$yvalues<-input$yvariable;},
               ignoreInit=TRUE)
  observeEvent(input$start,
               {v$start<-input$start;},
               ignoreInit=TRUE)
  observeEvent(input$end,
               {v$end<-input$end;},
               ignoreInit=TRUE)
  observeEvent(input$outid,
               {v$outid<-input$outid;},
               ignoreInit=TRUE)
  observeEvent(input$queue,
               {if(!is.null(isolate(v$runs))) {
                 isolate(v$runs$hasdata <-FALSE)
               }})


}

# Run the application
shinyApp(ui = ui, server = server)
