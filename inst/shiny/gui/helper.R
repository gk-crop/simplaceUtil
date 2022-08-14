my_volumes <- function()
{
  dirs <- simplace::findSimplaceInstallations()
  if(length(dirs)>0)
  {
    names(dirs) <- paste0("simplace_",1:length(dirs))
    c(dirs, shinyFiles::getVolumes()())
  }
  else {
    shinyFiles::getVolumes()()
  }
  
}


filename <- function(fl,vol) shinyFiles::parseFilePaths(vol,fl)$datapath[1]

dirname <- function(fl,vol) shinyFiles::parseDirPath(vol,fl)$datapath[1]