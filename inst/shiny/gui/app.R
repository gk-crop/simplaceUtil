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

      h5("Solution & Project"),
      shinyFilesButton('solution', label='Select solution',
                       title='Please select a solution', multiple=FALSE),
      shinyFilesButton('project', label='Select project',
                       title='Please select a project', multiple=FALSE),

      h5("Simplace"),
      fluidRow(
        column(5, shinyDirButton('instdir', label='Simplace Installation', title='Please select simplace installation dir', multiple=FALSE)),
        column(7, uiOutput("dirselect"))
      ),

      h5("Solution Graph & tables"),
      selectInput("graphlayout","Graph Layout",choices = c("nicely", "circle", "tree", "kk", "fr"),selected=1),
      uiOutput("componentselect"),
      fluidRow(
        column(4, selectInput("componenttype","Type",choices = c("all","var", "resource", "simcomponent", "output"),selected=1)),
        column(3, selectInput("distance","Distance",choices = c(1:10),selected=1)),
        column(5, selectInput("linkage","Linked Values",
                  choices = c("All Timesteps"="allsteps", "Actual Timestep"="samestep", "Previous Timestep"="prevstep"),
                  selected=1))
      ),

      h5("Simulation Runs"),
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

        tabPanel("Components",
                 dataTableOutput("components")),

        tabPanel("Links",
                 dataTableOutput("links")),

        tabPanel("User Variables",
                 dataTableOutput("variables")),

        tabPanel("Mem Output",
                 dataTableOutput("memoutput")),

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

  v$cmp <- NULL

  v$simulated <- FALSE
  v$actsim <- NULL

  v$res <- NULL

  v$resultdf <- NULL

  
  

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
                  v$cmp<-getElementsFromSolutionFile(v$solution)

                  comps <- unique(v$cmp$components$id)
                  names(comps) <- comps
                  ch_c <- c("All"='all',comps)

                  if(length(ch_c)>0) {
                    output$componentselect <-  renderUI(selectInput("componentselect","Select component", choices = ch_c, selected=1,multiple=FALSE))
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
          renderMemoySelect(input, output, v)
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
          renderMemoySelect(input, output, v)
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

  
  observeEvent(input$memoryoutselect,getSimulationResult(input,output,v))
  observeEvent(v$simulated,{
    if(!v$simulated) {output$runstatus <- renderText("")}
    else {
      getSimulationResult(input,output,v)
    }
  })

  # outputs

  output$diagram <- DiagrammeR::renderGrViz(try({
      if(!is.null(v$cmp))
      {
        cid <- ""
        dg <- componentsToGraph(v$cmp$components, v$cmp$links) |>
          filterEdges(input$linkage)
        if(!is.na(input$componentselect) && !(input$componentselect=='all'))
        {
          cid <- input$componentselect
          dg <- getNeighborhood(dg,cid,input$distance)
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
    }))

  output$diagram_from <- DiagrammeR::renderGrViz(try({
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
  }))

  output$diagram_to <- DiagrammeR::renderGrViz(try({
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
  }))


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


  output$variables <- renderDataTable(
    {
      v$cmp$variables |> dplyr::left_join(
        v$cmp$links |> dplyr::filter(from =="variables") |> dplyr::group_by(name) |> dplyr::summarise(used=paste(to,collapse=",")),
        by=c("id"="name")
      )
    }, options=tableOptions)

  output$memoutput <- renderDataTable (
    {
      v$resultdf
    }, options=tableOptions)

  output$plot <- renderPlot({
    if(v$simulated)
    {
      data <- v$resultdf[v$resultdf$simulationid==input$simulation &
                           input$daterange[1]<=v$resultdf$CURRENT.DATE &
                           v$resultdf$CURRENT.DATE<=input$daterange[2],]
      if(nrow(data)>0)
      {
        if(length(input$columny)==1)
        {
          if(mode(data[,input$columnx])!="numeric")
          {
            data[,input$columnx] <- as.factor(data[,input$columnx])
          }
          if(mode(data[,input$columny])!="numeric")
          {
            data[,input$columny] <- as.factor(data[,input$columny])
          }        
          plot(data[,input$columnx],data[,input$columny],xlab=input$columnx, ylab=input$columny, pch=20)
        }
        else if(mode(data[,input$columnx])=="numeric")
        {
          nc <- sapply(input$columny,\(n)mode(data[,n])=='numeric')
          cols <- input$columny[nc]
          if(length(cols)>0)
          {
            matplot(data[,input$columnx],
                    data[,cols],type="l",lty=1,col = 1:length(cols),
                    ylab="Values",
                    xlab=input$columnx)
          }
        }
        
      }
    }
  })

  
  output$layerplot <- renderPlot({
    if(v$simulated)
    {
      data <- v$resultdf[v$resultdf$simulationid==input$simulation &
                           input$daterange[1]<=v$resultdf$CURRENT.DATE &
                           v$resultdf$CURRENT.DATE<=input$daterange[2],]
      if(nrow(data)>0)
      {
        lnames <- names(data)
        lcols <- substr(lnames,1,nchar(input$layer))
        lnames <- rev(lnames[lcols==input$layer])
        
        ldata <- data[,lnames]
        
        mat <- data.matrix(ldata,rownames.force = NA)
        
        filled.contour(
          x = data$CURRENT.DATE,
          y = -(ncol(ldata):1),
          z=mat,
          main = input$layer,
          xlab = "Date",
          ylab = "Layer")

      }
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
