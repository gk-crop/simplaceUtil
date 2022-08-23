my_volumes <- function()
{
  dirs <- simplace::findSimplaceInstallations()
  if(length(dirs)>0)
  {
    if(length(dirs)>1){
       names(dirs) <-paste0("simplace (",c("default",2:length(dirs)),")")
    }
    else{
      names(dirs) <-"simplace"
    }
    c(dirs, shinyFiles::getVolumes()())
  }
  else {
    shinyFiles::getVolumes()()
  }

}


filename <- function(fl,vol) shinyFiles::parseFilePaths(vol,fl)$datapath[1]

dirname <- function(fl,vol) shinyFiles::parseDirPath(vol,fl)$datapath[1]

renderMemoySelect <- function(input, output, v)
{
  memid <- getMemoryOutputIds(v$elem$components)
  names(memid)<-memid
  ch_m <- memid
  if(length(ch_m)>0) {
    output$memoryoutselect <-  renderUI(selectInput("memoryoutselect","Select outputs", choices = ch_m, selected=1,multiple=FALSE))
  }
  else {
    output$memoryoutselect <- renderText("")
  }

}


getSimulationResult <- function (input,output,v)
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
    v$resultdf <- resultToDataframeExpanded(simplace::resultToList(res, expand=TRUE))
    sims <- unique(v$resultdf$simulationid)
    cols <- names(v$resultdf)
    lcols <- unique(gsub("_[0-9]+$","",cols[grepl("_[0-9]+$",cols)]))
    mind <- min(v$resultdf$CURRENT.DATE)
    maxd <- max(v$resultdf$CURRENT.DATE)
    output$plotcontrols <- renderUI(
      tagList(fluidRow(
        column(7,selectInput("simulation", "SimulationId",sims)),
        column(5,dateRangeInput('daterange',
                                label = 'Date range to plot',
                                min = mind, max=maxd,
                                start = mind, end = min(mind+4*365+1,maxd)))),
        fluidRow(
          column(4,selectInput("columnx", "X-Column",cols, selected="CURRENT.DATE")),
          column(8,selectInput("columny", "Y-Column(s)",cols, selected="CURRENT.DATE",width = "100%", multiple=TRUE)),

        ))
    )
    output$layerplotcontrols <- renderUI(selectInput("layer","Value",lcols))
  }

}


