

simComponentType <- function(s) ifelse(s=="net.simplace.sim.components.DefaultManagement","mgm",
                                       ifelse(s=="net.simplace.sim.components.FWSimpleSimComponent","simple","normal"))

mark_variables<-function(v){if(v[2]=="")c("variables",v[1])else v}

getVars <- function(s)
{
  m <- NULL
  if(stringr::str_detect(s,stringr::fixed("$")))
  {
    m <- stringr::str_match_all(s,pattern=stringr::regex('[$][{]([a-zA-Z0-9_]+)([.]*)([a-zA-Z0-9_]*)[}]'))[[1]][,c(2,4)]
  }
  else
  {
    m <- stringr::str_split(s,stringr::fixed('.'),n=2,simplify=TRUE)
  }
  df<-NULL
  if(is.matrix(m))
  {
    df<-as.data.frame(t(apply(m,1,mark_variables)),stringsAsFactors=FALSE)
  }
  else
  {
    v<-mark_variables(m)
    df<-data.frame(from=v[1],name=v[2],stringsAsFactors=FALSE)
  }
  colnames(df)<-c("from","name")
  df
}

#' Gets components for a solution
#'
#' @param x xml object (solution)
#' @export
getComponents <- function(x)
{
  df <- NULL

  df <- rbind(df,data.frame(id="CURRENT",type=("var"),subtype="var",ref="SYSTEM",javaclass=""))
  df <- rbind(df,data.frame(id="variables",type=("var"),subtype="var", ref="User",javaclass=""))

  res_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//resource"),'id')
  res_interf<- xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//resource"),'interface')
  res_file <- sapply(res_interf, function(oi) xml2::xml_text(xml2::xml_find_all(x,paste0("//solution//interfaces//interface[@id='",oi,"']//filename"))))
  if(length(res_id)>0)
  {
    df <- rbind(df,data.frame(id=res_id,type="resource",subtype="resource",ref=paste(res_interf, res_file),javaclass=""))
  }

  trf_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'id')
  trf_res<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'resource')
  trf_cls<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'class')
  if(length(trf_id)>0)
  {
    df <- rbind(df,data.frame(id=trf_id,type="resource",subtype="transform",ref=trf_res,javaclass=trf_cls))
  }

  cmp_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//simmodel//simcomponent"),'id')
  cmp_cls <-xml2::xml_attr(xml2::xml_find_all(x,"//solution//simmodel//simcomponent"),'class')
  cmp_clst<-simComponentType(cmp_cls)
  if(length(cmp_id)>0)
  {
    df <- rbind(df,data.frame(id=cmp_id,type="simcomponent",subtype=cmp_clst,ref=cmp_cls,javaclass=cmp_cls))
  }

  out_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//outputs//output"),'id')
  out_interf<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//outputs//output"),'interface')
  out_file <- sapply(out_interf, function(oi) xml2::xml_text(xml2::xml_find_all(x,paste0("//solution//interfaces//interface[@id='",oi,"']//filename"))))
  if(length(out_id)>0)
  {
    df <- rbind(df,data.frame(id=out_id,type="output",subtype="output",ref=paste(out_interf,out_file), javaclass=""))
  }

  df$nr <- 1:nrow(df)
  df$id <- as.character(df$id)
  df$type <- as.character(df$type)
  df$subtype <- as.character(df$subtype)
  df$ref <- as.character(df$ref)
  df
}

#' Get the dataframe of links between components
#'
#' @param x xml object (solution)
#' @param df dataframe of components
#' @export
getLinks <- function(x,df)
{

  vdf_all<-NULL
  for(i in df[df$type=="simcomponent","id"])
  {
    src = xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//simmodel//simcomponent[@id='",i,"']//input")),'source');
    actr = xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//simmodel//simcomponent[@id='",i,"']//action")),'rule');
    actr <- actr[stringr::str_detect(actr,stringr::fixed("$"))]
    mgmr = xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//simmodel//simcomponent[@id='",i,"']//action//mgm")),'rule');
    mgmr <- mgmr[stringr::str_detect(mgmr,stringr::fixed("$"))]

    vr = xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//simmodel//simcomponent[@id='",i,"']//process//var")),'rule');
    vr <- vr[stringr::str_detect(vr,stringr::fixed("$"))]


    inp <- c(src,actr, mgmr,vr)
    inp <- unique(inp[!is.na(inp)])
    if(length(inp)>0)
    {
      l<-lapply(inp,getVars)
      vdf <- (do.call(rbind, l))
      vdf$to = i
      vdf$rel="value"
      vdf_all<- rbind(vdf_all, vdf)
    }
  }
  trf_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'id')
  trans_resources <-  xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//transform")),'resource')
  vdf_all <-rbind(vdf_all,data.frame(from=trans_resources, to=trf_id, name="-",rel="value"))

  for(i in df[df$type=="resource","id"])
  {
    rls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//resource[@id='",i,"']")),'rule')
    trls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//transform[@id='",i,"']")),'rule')
    inp <- c(rls, trls)
    inp<-unique(inp[!is.na(inp)])
    if(length(inp)>0)
    {
      l<-lapply(inp,getVars)
      vdf <- (do.call(rbind, l))
      vdf$to = i
      vdf$rel="rule"
      vdf_all<- rbind(vdf_all, vdf)
    }
    ks <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//resource[@id='",i,"']//res")),'key')
    tks <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//transform[@id='",i,"']//res")),'key')
    inp <- c(ks, tks)
    inp<-unique(inp[!is.na(inp)])
    if(length(inp)>0)
    {
      l<-lapply(inp,getVars)
      vdf <- (do.call(rbind, l))
      vdf$to = i
      vdf$rel="key"
      vdf_all<- rbind(vdf_all, vdf)
    }

  }

  for(i in df[df$type=="output","id"])
  {
    rls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//outputs//output[@id='",i,"']")),'rule')
    rrls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//outputs//output[@id='",i,"']")),'resetrule')

    inp <- c(rls, rrls)
    inp<-unique(inp[!is.na(inp)])
    if(length(inp)>0)
    {
      l<-lapply(inp,getVars)
      vdf <- (do.call(rbind, l))
      vdf$to = i
      vdf$rel="rule"
      vdf_all<- rbind(vdf_all, vdf)
    }


    orls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//outputs//output[@id='",i,"']//out")),'rule')

    inp <- c(orls)
    inp<-unique(inp[!is.na(inp)])
    if(length(inp)>0)
    {
      l<-lapply(inp,getVars)
      vdf <- (do.call(rbind, l))
      vdf$to = i
      vdf$rel = "value"
      vdf_all<- rbind(vdf_all, vdf)
    }

  }
  vdf_all
}


isSolution <-function(file) {
  substr(file, nchar(file)-7, nchar(file)) == ".sol.xml"
}



getMetadataForFile <- function(file, rootdir)
{
  part <- gsub(paste0(rootdir,"/"),"",file)
  folders <- strsplit(part,"/")[[1]]
  folders <-folders[folders!="solution" & !isSolution(folders)]
  date <- file.info(file)[1,'mtime']
  list(date=date,folders=folders)
}

#' Get info for a solution
#'
#' @param file solution file
#' @param rootdir root directory
#' @export
getSolutionInfoAsDataframe <- function (file, rootdir)
{
  sol <- xml2::read_xml(file)
  comp <- getComponents(sol)
  md <- getMetadataForFile(file, rootdir)
  comp$file <- file
  comp$lastmodified <- md$date
  comp$user <- md$folders[1]
  comp$project <- md$folders[2]
  comp$subproject <- md$folders[3]
  comp$subsubproject <- md$folders[4]
  comp$subsubsubproject <- md$folders[5]
  comp
}
