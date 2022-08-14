library(shiny)
library(shinyFiles)
library(simplaceUtil)

source("helper.R")

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
      uiOutput("outputdirselect")
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
          uiOutput("runbutton")
                 
        
        ),
     
        tabPanel("Solution Graph",
                
                 
                 DiagrammeR::grVizOutput("diagram",width = "100%", height="75%")),
        tabPanel("Component Table",
                 
                 
                 tableOutput("components")),
        tabPanel("Links Table",
                 
                 
                 tableOutput("links"))
    
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
                v$solution <- filename(input$solution,vols)
                if(!is.na(v$solution))
                {
                  v$cmp<-getComponentsFromFile(v$solution)
                }
                output$solutionlabel <- renderText(paste("Solution:",v$solution))
                
              },
              ignoreInit = TRUE)
  observeEvent(input$project, 
               {
                 v$project <- filename(input$project,vols)
                 output$projectlabel <- renderText(paste("Project:",v$project))
                 
               })
  
  observeEvent(input$instdir,
    {
      d <- paste0(parseDirPath(vols,input$instdir),'/')
      drs <- dir(d)
      if('simplace_core' %in% drs & 'simplace_modules' %in%drs)
      {
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
  
  observeEvent(input$workdir,
    {
      
      v$workdir <- paste0(shinyFiles::parseDirPath(vols,input$workdir),'/')
      output$workdirlabel <- renderText(paste("Work Dir:",v$workdir))
      
    }     
  )
  
  observeEvent(input$outputdir,
    {
      v$outputdir <- paste0(shinyFiles::parseDirPath(vols,input$outputdir),'/')
      output$outputdirlabel <- renderText(paste("Output Dir:",v$outputdir))
      
    })
  
  observeEvent(input$init,
    {
      try(
        { print(paste(v$instdir, v$workdir, v$outputdir))
          v$sp <- simplace::initSimplace(v$instdir, v$workdir, v$outputdir)
          output$runbutton <- renderUI(actionButton('run',"Run Project"))
        }
      )
    })
  
  observeEvent(input$run,
               {
                 try({
                   if(!is.null(v$sp) && !is.null(v$solution)) {
                     print("running simulation")
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
                     print("ended simulation")
                   }
                 }
                 )
               })
  
  
  output$diagram <- DiagrammeR::renderGrViz({
      if(!is.null(v$cmp))
      {
        dg <- componentsToGraph(v$cmp$components, v$cmp$links)
        DiagrammeR::render_graph(dg)
      }
    })
  
  output$components <- renderTable(
    {
      
      v$cmp$components
    }
  )
  
  output$links <- renderTable(
    {
      
      v$cmp$links
    }
  )
  
    
 }
  
# Run the application 
shinyApp(ui = ui, server = server)