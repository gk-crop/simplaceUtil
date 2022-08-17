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
