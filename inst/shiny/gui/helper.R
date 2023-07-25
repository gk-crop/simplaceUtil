#' get list of volumes for file dialogs
my_volumes <- function()
{
  dirs <- simplace::findSimplaceInstallations()
  if(!is.na(dirs) && length(dirs)>0)
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

#' extract filename from file dialog results
filename <- function(fl,vol) shinyFiles::parseFilePaths(vol,fl)$datapath[1]

#' extract filename from directory dialog results
dirname <- function(fl,vol) shinyFiles::parseDirPath(vol,fl)$datapath[1]

#' render list of available outputs
renderOutputSelect <- function(input, output, v)
{
  ch_m <- c(getMemoryOutputIds(v$elem$components),names(v$outputfiles))
  names(ch_m)<-ch_m
  if(length(ch_m)>0) {
    output$outselect <-  renderUI(selectInput("outselect","Select outputs",
                              choices = ch_m, selected=1,multiple=FALSE))
  }
  else {
    output$outselect <- renderText("")
  }

}

#' get list of available csv output files
getFileOutputList <- function(v)
{
  l <- character(0)
  if(v$simulated)
  {
    outp <- v$elem$component |>
      dplyr::filter(type=="output") |>
      dplyr::mutate(file=replaceVariablesWithValues(
        gsub("[^\\[]+\\[(.+)\\]","\\1",ref),
          v$elem$variables,
          c("_WORKDIR_"=v$workdir,"_OUTPUTDIR_"=v$outputdir)
        )
      ) |>
      dplyr::filter(file.exists(file))
    if(nrow(outp)>0){
      l <- outp$file
      names(l)<-outp$id
    }

  }
  l
}

#' fetch simulation result (from memory or from file)
getSimulationResult <- function (input,output,v)
{
  if(v$simulated && !is.null(input$outselect))
  {
    if(input$outselect %in% names(v$outputfiles))
    {
      l <- readLines(v$outputfiles[input$outselect], n=1)
      pos <- nchar("projectid")
      if(substr(l,1,pos)=="projectid")
      {
        del = substr(l,pos+1, pos+1)
        v$resultdf <- readr::read_delim(v$outputfiles[input$outselect],
                                        delim=del, show_col_types = FALSE) |>
          parseDate()
      }
    }
    else
    {
      if(!is.null(v$actsim))
      {
        res <- simplace::getResult(v$sp, input$outselect,v$actsim)
      }
      else {
        res <- simplace::getResult(v$sp, input$outselect)
      }
      # resl <- simplace::resultToList(res, expand = TRUE)
      # units <- simplace::getUnitsOfResult(res)
      # for(i in seq_along(resl))
      # {
      #   attr(resl[[i]], "unit") <- units[[i]]
      # }
      v$resultdf <- simplace::resultToDataframe(res, expand = TRUE)
    }


    sims <- unique(v$resultdf$simulationid)
    cols <- names(v$resultdf)
    lcols <- unique(gsub("_[0-9]+$","",cols[grepl("_[0-9]+$",cols)]))
    mind <- min(v$resultdf$CURRENT.DATE)
    maxd <- max(v$resultdf$CURRENT.DATE)
    output$plotcontrols <- renderUI(
      tagList(fluidRow(
        column(7,selectInput("simulation", "SimulationIds",sims, multiple = TRUE,
                    selected = sims[1])),
        column(5,dateRangeInput('daterange',
                                label = 'Date range to plot',
                                min = mind, max=maxd,
                                start = mind, end = min(mind+4*365+1,maxd)))),
        fluidRow(
          column(4,selectInput("columnx", "X-Column",cols,
                    selected="CURRENT.DATE")),
          column(8,selectInput("columny", "Y-Column(s)",cols,
                    selected="CURRENT.DATE",width = "100%", multiple=TRUE)),

        ))
    )
    output$layerplotcontrols <- renderUI(selectInput("layer","Value",lcols))
  }

}


