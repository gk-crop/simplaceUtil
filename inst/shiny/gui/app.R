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
      shinyDirButton('instdir', label='Simplace Installation', title='Please select simplace installation dir', multiple=FALSE),
      uiOutput("workdirselect"),
      uiOutput("outputdirselect"),

      h4("Solution Graph & tables"),
      selectInput("graphlayout","Graph Layout",choices = c("nicely", "circle", "tree", "kk", "fr"),selected=1),
      selectInput("componenttype","Type",choices = c("all","var", "resource", "simcomponent", "output"),selected=1),
      uiOutput("componentselect"),
      selectInput("distance","Distance from Component",choices = c(1:10,25,50),selected=1),
      selectInput("linkage","Linked Values",choices = c("All Timesteps"="allsteps", "Actual Timestep"="samestep", "Previous Timestep"="prevstep"),selected=1),

      h4("Simulation Runs"),
      uiOutput("memoryoutselect"),
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

        tabPanel("Component Table",
                 dataTableOutput("components")),

        tabPanel("Links Table",
                 dataTableOutput("links")),

        tabPanel("Memory Output",
                 dataTableOutput("memoutput"))

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

  v$cmp <- NULL

  v$simulated <- FALSE
  v$actsim <- NULL

  v$res <- NULL


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


  observeEvent(input$solution,
              {
                v$simulated <- FALSE
                v$actsim <- NULL
                v$solution <- filename(input$solution,vols)
                if(!is.na(v$solution))
                {
                  v$cmp<-getComponentsFromFile(v$solution)


                  comps <- unique(v$cmp$components$id)
                  names(comps) <- comps
                  ch_c <- c("All"='all',comps)

                  if(length(ch_c)>0) {
                    output$componentselect <-  renderUI(selectInput("componentselect","Select component", choices = ch_c, selected=1,multiple=FALSE))
                  }

                  memid <- getMemoryOutputIds(v$cmp$components)
                  names(memid)<-memid
                  ch_m <- memid
                  if(length(ch_m)>0) {
                    output$memoryoutselect <-  renderUI(selectInput("memoryoutselect","Select outputs", choices = ch_m, selected=1,multiple=FALSE))
                  }
                  else {
                    output$memoryoutselect <- renderText("")
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
      if('simplace_core' %in% drs & 'simplace_modules' %in%drs)
      {
        v$simulated <- FALSE
        v$actsim <- NULL

        v$instdir <- d
        v$workdir <- paste0(d,"simplace_run/simulation/")
        v$outputdir <- paste0(d,"simplace_run/output/")
        output$instdirlabel <- renderText(paste("Installation Dir:",d))
        output$workdirlabel <- renderText(paste("Work Dir:",v$workdir))
        output$outputdirlabel <- renderText(paste("Output Dir:",v$outputdir))
        output$workdirselect <-  renderUI(
          shinyDirButton('workdir',
                     label='Workdir',
                     title='Please select simplace work dir', multiple=FALSE)

        )
        output$outputdirselect <-  renderUI(
          shinyDirButton('outputdir',
                         label='Outputdir',
                         title='Please select simplace output dir', multiple=FALSE)
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

        }
      }
      )

    }
  )


  observeEvent(v$simulated,{
    if(!v$simulated) {output$runstatus <- renderText("")}
  })

  # outputs

  output$diagram <- DiagrammeR::renderGrViz({
      if(!is.null(v$cmp))
      {
        cid <- ""
        dg <- componentsToGraph(v$cmp$components, v$cmp$links)
        if(!is.na(input$componentselect) && !(input$componentselect=='all'))
        {
          cid <- input$componentselect
          dg <- getNeighborhood(dg,cid,input$distance, input$linkage)
        }
        if(!is.na(input$componenttype) && !(input$componenttype=='all'))
        {
          dg <- dg |>
            DiagrammeR::select_nodes(
              conditions = (.data$type==input$componenttype | .data$label==cid)) |>
            DiagrammeR::invert_selection() |>
            DiagrammeR::delete_nodes_ws()
        }

        DiagrammeR::render_graph(dg, layout=input$graphlayout)
      }
    })

  output$diagram_from <- DiagrammeR::renderGrViz({
    if(!is.null(v$cmp) && !is.na(input$componentselect) && !(input$componentselect=='all'))
    {
      cid <- input$componentselect
      dg <- componentsToGraph(v$cmp$components, v$cmp$links)
      dg <- getLinkingFromComponent(dg,cid,input$distance, input$linkage)

      if(!is.na(input$componenttype) && !(input$componenttype=='all'))
      {
        dg <- dg |>
          DiagrammeR::select_nodes(
            conditions = (.data$type==input$componenttype | .data$label==cid)) |>
          DiagrammeR::invert_selection() |>
          DiagrammeR::delete_nodes_ws()
      }

      DiagrammeR::render_graph(dg, layout=input$graphlayout)
    }
  })

  output$diagram_to <- DiagrammeR::renderGrViz({
    if(!is.null(v$cmp) && !is.na(input$componentselect) && !(input$componentselect=='all'))
    {
      cid <- input$componentselect
      dg <- componentsToGraph(v$cmp$components, v$cmp$links)
      dg <- getLinkingToComponent(dg,cid,input$distance, input$linkage)

      if(!is.na(input$componenttype) && !(input$componenttype=='all'))
      {
        dg <- dg |>
          DiagrammeR::select_nodes(
            conditions = (.data$type==input$componenttype | .data$label==cid)) |>
          DiagrammeR::invert_selection() |>
          DiagrammeR::delete_nodes_ws()
      }

      DiagrammeR::render_graph(dg, layout=input$graphlayout)
    }
  })


  output$components <- renderDataTable(
    {

      t <- v$cmp$components
      if(!is.na(input$componenttype) && !(input$componenttype=='all'))
      {
        t <- t[t$type==input$componenttype,]
      }
      t

    }, options=tableOptions)

  output$links <- renderDataTable(
    {

      t <- v$cmp$links
      if("componentselect" %in% input && !is.na(input$componentselect) && !(input$componentselect=='all'))
      {
        t <- t[!is.na(t$from) & (t$from==input$componentselect | t$to==input$componentselect),]
      }
      t
    }, options=tableOptions)


  output$memoutput <- renderDataTable (
    {

      if(v$simulated && !is.null(input$memoryoutselect))
      {
        if(!is.null(v$actsim))
        {
          res <- simplace::getResult(v$sp, input$memoryoutselect,v$actsim)
        }
        else {
          res <- simplace::getResult(v$sp, input$memoryoutselect)
        }
        df <- simplace::resultToDataframe(res)
        df
      }


    }, options=tableOptions)


}

# Run the application
shinyApp(ui = ui, server = server)
