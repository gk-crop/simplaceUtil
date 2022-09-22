library(shiny)
library(shinyFiles)
library(simplaceUtil)
library(DT)

source("helper.R")

tableOptions <- list(
  lengthMenu = list(c(10,25,100,200,-1),c("10","25","100","200","All")),
  pageLength=100
)


ui <- fluidPage(
  titlePanel("Simplace R GUI"),
  sidebarLayout(

    sidebarPanel(

      h4("Solution & Project"),
      shinyFilesButton('solution', label='Select solution',
                       title='Please select a solution', multiple=FALSE),
      shinyFilesButton('project', label='Select project',
                       title='Please select a project', multiple=FALSE),

      h4("Simplace"),
      fluidRow(
        column(5, shinyDirButton('instdir', label='Simplace Installation', title='Please select simplace installation dir', multiple=FALSE)),
        column(7, uiOutput("dirselect"))
      ),

      h4("Solution Graph & tables"),
      selectInput("graphlayout","Graph Layout",choices = c("nicely", "circle", "tree", "kk", "fr"),selected=1),
      uiOutput("componentselect"),
      fluidRow(
        column(4, selectInput("componenttype","Type",choices = c("all","var", "resource", "simcomponent", "output"),selected="all",multiple=TRUE)),
        column(3, selectInput("distance","Distance",choices = c(0:10),selected=1)),
        column(5, selectInput("linkage","Linked Values",
                  choices = c("All Timesteps"="allsteps", "Actual Timestep"="samestep", "Previous Timestep"="prevstep"),
                  selected=1))
      ),

      h4("Simulation Runs"),
      uiOutput("outselect"),
      textOutput("runstatus")

    ),


    mainPanel(
      tabsetPanel(
        tabPanel("Simplace",
          h4("Solution & Project"),
          textOutput("solutionlabel"),
          textOutput("projectlabel"),
          h4("Simplace Configuration"),
          textOutput("instdirlabel"),
          textOutput("workdirlabel"),
          textOutput("outputdirlabel"),
          uiOutput("initbutton"),
          h4("Run Simulations"),
          uiOutput("runbutton"),
          uiOutput("createsimbutton"),
        ),

        tabPanel("Solution Graph",
                 DiagrammeR::grVizOutput("diagram",width = "100%", height="75%")),

        tabPanel("Linking To",
                 DiagrammeR::grVizOutput("diagram_to",width = "100%", height="75%")),

        tabPanel("Linked From",
                 DiagrammeR::grVizOutput("diagram_from",width = "100%", height="75%")),

        tabPanel("Components",
                 dataTableOutput("components")),

        tabPanel("Links",
                 dataTableOutput("links")),

        tabPanel("User Variables",
                 dataTableOutput("variables")),

        tabPanel("Output",
                 dataTableOutput("output")),

        tabPanel("Plots",
                 uiOutput("plotcontrols"),
                 plotOutput("plot"),
                 uiOutput("layerplotcontrols"),
                 plotOutput("layerplot")
                 ),

        tabPanel("?",
                 h4("Analyze Solution"),
                 div("For analyzing a solution one does not need a simplace installation. The structure is derived from the solution xml file."),
                 h4("Run Simulations"),
                 div("For running a simulation, one has to specify the simplace installation dir. Please choose a directory that contains the folders simplace_core and simplace_modules."),
                 div("If one or more simplace installations are automatically detected, they show up in the directory choose dialog as volumes. Workdir and Outputdir are set to it's defaults withint the folder simplace_run. You can change them manually."),
                 div("Once the simplace installation dir has been set, you can start simplace in the Simplace-Panel and run the simulations (either by running a complete project, or just a standard simulation).")
                 )

      )
    )

  )

)

# Define server logic
server <- function(input, output) {

  vols <- my_volumes()

  v <- reactiveValues()


  v$sp <- NULL

  v$instdir <- NULL
  v$workdir <- NULL
  v$outputdir <- NULL

  v$solution <- NULL
  v$project <- NULL

  v$elem <- NULL

  v$simulated <- FALSE
  v$actsim <- NULL

  v$res <- NULL

  v$resultdf <- NULL

  v$outputfiles <- NULL


  # File and directory choose
  shinyFileChoose(input, 'solution', roots=vols,
                  defaultRoot = names(vols)[1],
                  filetypes=c('', 'xml'))
  shinyFileChoose(input, 'project', roots=vols,
                  defaultRoot = names(vols)[1],
                  filetypes=c('', 'xml'))
  shinyDirChoose(input, 'instdir', roots=vols,
                 defaultRoot = names(vols)[1],
                 defaultPath = "",
                 allowDirCreate=FALSE)
  shinyDirChoose(input, 'workdir', roots=vols,
                 defaultRoot = names(vols)[1],
                 defaultPath = "",
                 allowDirCreate=FALSE)
  shinyDirChoose(input, 'outputdir', roots=vols,
                 defaultRoot = names(vols)[1],
                 defaultPath = "",
                 allowDirCreate=FALSE)


  # Events
  observeEvent(input$solution,
              {
                v$simulated <- FALSE
                v$actsim <- NULL
                v$solution <- filename(input$solution,vols)
                if(!is.na(v$solution))
                {
                  v$elem<-getElementsFromSolutionFile(v$solution)

                  comps <- unique(v$elem$components[v$elem$components$type!="interface",]$id)
                  names(comps) <- comps
                  ch_c <- c("All"='all',comps)

                  if(length(ch_c)>0) {
                    output$componentselect <-  renderUI(selectInput("componentselect","Select components", choices = ch_c, selected="all",multiple=TRUE))
                  }

                }
                output$solutionlabel <- renderText(paste("Solution:",v$solution))

              },
              ignoreInit = TRUE)


  observeEvent(input$project,
               {
                 v$simulated <- FALSE
                 v$actsim <- NULL
                 v$project <- filename(input$project,vols)
                 output$projectlabel <- renderText(paste("Project:",v$project))

               })


  observeEvent(input$instdir,
    {
      d <- paste0(parseDirPath(vols,input$instdir),'/')
      drs <- dir(d)
      if('simplace_core' %in% drs & 'simplace_modules' %in% drs)
      {
        v$simulated <- FALSE
        v$actsim <- NULL

        v$instdir <- d
        v$workdir <- paste0(d,"simplace_run/simulation/")
        v$outputdir <- paste0(d,"simplace_run/output/")
        output$instdirlabel <- renderText(paste("Installation Dir:",d))
        output$workdirlabel <- renderText(paste("Work Dir:",v$workdir))
        output$outputdirlabel <- renderText(paste("Output Dir:",v$outputdir))
        output$dirselect <-  renderUI(
          tagList(
          shinyDirButton('workdir',
                     label='Workdir',
                     title='Please select simplace work dir', multiple=FALSE),
          shinyDirButton('outputdir',
                         label='Outputdir',
                         title='Please select simplace output dir', multiple=FALSE)
          )
        )
        output$initbutton <- renderUI(actionButton('init',"Start Simplace"))
      }
    },

    ignoreInit = TRUE
  )


  observeEvent(
    input$workdir,
    {
      v$workdir <- paste0(shinyFiles::parseDirPath(vols,input$workdir),'/')
      output$workdirlabel <- renderText(paste("Work Dir:",v$workdir))

    }
  )


  observeEvent(
    input$outputdir,
    {
      v$outputdir <- paste0(shinyFiles::parseDirPath(vols,input$outputdir),'/')
      output$outputdirlabel <- renderText(paste("Output Dir:",v$outputdir))

    }
  )


  observeEvent(
    input$init,
    {
      try(
        {
          v$simulated <- FALSE
          v$actsim <- NULL

          v$sp <- simplace::initSimplace(v$instdir, v$workdir, v$outputdir)
          output$runbutton <- renderUI(actionButton('run',"Run Project"))
          output$createsimbutton <- renderUI(actionButton('createsimulation',"Create and Run Simulation"))

        }
      )
    }
  )

  observeEvent(
    input$run,
    {
      try({
        if(!is.null(v$sp) && !is.null(v$solution)) {
          v$simulated <- FALSE
          v$actsim <- NULL
          v$outputfiles <- NULL
          output$runstatus <- renderText("running simulation in project mode")
          if(is.null(v$project))
          {
            simplace::openProject(v$sp,v$solution)
          }
          else
          {
            simplace::openProject(v$sp,v$solution,v$project)
          }

          simplace::runProject(v$sp)
          simplace::closeProject(v$sp)
          v$simulated <- TRUE
          v$outputfiles <- getFileOutputList(v)
          renderOutputSelect(input, output, v)
          output$runstatus <- renderText("ended simulation")
        }
      }
      )
    }
  )


  observeEvent(
    input$createsimulation,
    {
      try({
        if(!is.null(v$sp) && !is.null(v$solution)) {
          v$simulated <- FALSE
          simplace::openProject(v$sp,v$solution)
          sim <- simplace::createSimulation(v$sp)
          output$runstatus<-renderText(paste("running simulation",sim))
          simplace::runSimulations(v$sp)
          simplace::closeProject(v$sp)
          output$runstatus<-renderText(paste("ended simulation",sim))
          v$actsim <- sim
          v$simulated <- TRUE
          v$outputfiles <- getFileOutputList(v)
          renderOutputSelect(input, output, v)
        }
      }
      )

    }
  )


  observeEvent(input$outselect,getSimulationResult(input,output,v))
  observeEvent(v$simulated,{
    if(!v$simulated) {output$runstatus <- renderText("")}
    else {
      getSimulationResult(input,output,v)
    }
  })



  # outputs

  output$diagram <- DiagrammeR::renderGrViz(
    try({
      if(!is.null(v$elem))
      {
        dg <- componentsToGraph(v$elem$components, v$elem$links)
        dg <- getNeighborhood(dg,
                              input$componentselect, input$distance,
                              input$linkage, input$componenttype)
        DiagrammeR::render_graph(dg, layout=input$graphlayout)
      }
    }))


  output$diagram_from <- DiagrammeR::renderGrViz(
    try({
      if(!is.null(v$elem) &&
         !all(is.na(input$componentselect)) &&
         !all(input$componentselect=='all'))
      {
        dg <- componentsToGraph(v$elem$components, v$elem$links) |>
          getLinkingFromComponent(input$componentselect,
                                  input$distance, input$linkage,
                                  input$componenttype)
        DiagrammeR::render_graph(dg, layout=input$graphlayout)
      }
    }))


  output$diagram_to <- DiagrammeR::renderGrViz(
    try({
      if(!is.null(v$elem) &&
         !all(is.na(input$componentselect)) &&
         !all(input$componentselect=='all'))
      {
        dg <- componentsToGraph(v$elem$components, v$elem$links) |>
          getLinkingToComponent(input$componentselect,
                                input$distance, input$linkage,input$componenttype)

        DiagrammeR::render_graph(dg, layout=input$graphlayout)
      }
    }))


  output$components <- renderDataTable(
    {

      t <- v$elem$components
      if(!all(is.na(input$componenttype)) && !all(input$componenttype=='all'))
      {
        t <- t[t$type %in% input$componenttype,]
      }
      t

    }, options=tableOptions)


  output$links <- renderDataTable(
    {

      t <- v$elem$links

      if(!all(is.na(input$componentselect)) && !all(input$componentselect=='all'))
      {
        t <- t[!is.na(t$from) & (t$from %in% input$componentselect | t$to %in% input$componentselect),]
      }
      t
    }, options=tableOptions)


  output$variables <- renderDataTable(
    {
      v$elem$variables |> dplyr::left_join(
        v$elem$links |>
          dplyr::filter(from =="variables") |>
          dplyr::group_by(name) |>
          dplyr::summarise(used=paste(to,collapse=",")),
        by=c("id"="name")
      )
    }, options=tableOptions)


  output$output <- renderDataTable (
    {
      v$resultdf
    }, options=tableOptions)


  output$plot <- renderPlot({
    if(v$simulated)
    {
      plotScalarOutput(v$resultdf,
                       input$columnx, input$columny,
                       input$simulation,
                       input$daterange[1],
                       input$daterange[2])
    }
  })


  output$layerplot <- renderPlot({
    if(v$simulated)
    {
      plotLayeredOutput(v$resultdf,
                       input$layer,
                       input$simulation,
                       input$daterange[1],
                       input$daterange[2])
    }
  })


}

# Run the application
shinyApp(ui = ui, server = server)
