library(shiny)
library(shinyFiles)
library(simplaceUtil)

ui <- fluidPage(
  titlePanel("Simplace R GUI"),
  sidebarLayout(
    
    sidebarPanel(
      shinyFilesButton('files', label='Select solution', title='Please select a solution', multiple=FALSE),
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graph",
                
                 
                 DiagrammeR::grVizOutput("diagram2",width = "100%", height="90%"))
      )
    )
    
  )
  
)

# Define server logic 
server <- function(input, output) {
  shinyFileChoose(input, 'files', roots=getVolumes(), filetypes=c('', 'xml'))
  
  output$diagram2 <- 
      

      DiagrammeR::renderGrViz({
        fn <- parseFilePaths(getVolumes(),input$files)$datapath[1]
        if(!is.na(fn))
        {
          dg <- simplaceUtil::solutionToGraph(fn)
          DiagrammeR::render_graph(dg)
        }
      })
    
 }
  
# Run the application 
shinyApp(ui = ui, server = server)