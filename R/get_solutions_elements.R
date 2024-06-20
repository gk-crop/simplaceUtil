#' Determines the type of sim component by class attribute
#'
#' @param s class attribute
#' @return type of sim component
#' @keywords internal
simComponentType <- function(s) ifelse(s=="net.simplace.sim.components.DefaultManagement","mgm",
                                       ifelse(s=="net.simplace.sim.components.FWSimpleSimComponent","simple",ifelse(s=="net.simplace.sim.model.FWSimComponentGroup","grouped","normal")))


#' Add the prefix "variables" if there is none
#'
#' @param v vector with prefix and name
#' @return vector with prefix and name
#' @keywords internal
markVariables<-function(v){if(v[2]=="")c("variables",v[1])else v}



#' Get the variables from source, rule etc. attributes
#'
#' @param s attribute text (rule)
#' @param ruleonly check only for ${} variables
#' @keywords internal
getVars <- function(s, ruleonly=FALSE)
{
  m <- NULL
  if(stringr::str_detect(s,stringr::fixed("$")))
  {
    m <- stringr::str_match_all(s,pattern=stringr::regex('[$][{]([a-zA-Z0-9_]+)([.]*)([a-zA-Z0-9_]*)[}]'))[[1]][,c(2,4)]
  }
  else if(!ruleonly)
  {
    m <- stringr::str_split(s,stringr::fixed('.'),n=2,simplify=TRUE)
  }
  df<-NULL
  if(is.matrix(m))
  {
    df<-as.data.frame(t(apply(m,1,markVariables)),stringsAsFactors=FALSE)
  }
  else
  {
    v<-markVariables(m)
    df<-data.frame(from=v[1],name=v[2],stringsAsFactors=FALSE)
  }
  colnames(df)<-c("from","name")
  df
}

#' Gets user variables for a solution
#'
#' @param x xml object (solution)
#' @return data.frame with the user defined variable
#' @export
getUserVariables <- function(x)
{
  var <- xml2::xml_find_all(x,"//solution//variables//var")
  dyn <- xml2::xml_find_all(x,"//solution//variables//dyn")
  kind <- c(rep("var",length(var)), rep("dyn",length(dyn)))
  id <- c(xml2::xml_attr(var,'id'),xml2::xml_attr(dyn,'id'))
  value <- c(xml2::xml_text(var),
            xml2::xml_text(dyn))
  unit <-c(xml2::xml_attr(var,'unit'),
            xml2::xml_attr(dyn,'unit'))
  datatype <- c(xml2::xml_attr(var,'datatype'),
          xml2::xml_attr(dyn,'datatype'))
  data.frame(id,value, unit, datatype, kind)
}

#' Replaces variables with content
#'
#' @param text text to replace
#' @param variables variables dataframe
#' @param additional additional variables as named vector c("var1"="value1", ...), useful for directory placeholder
#' @export
replaceVariablesWithValues <- function(text, variables, additional=NULL)
{
  repl <- c(variables$value, additional)
  names(repl) <- paste0("${",c(variables$id,names(additional)),"}")
  stringr::str_replace_all(text,stringr::fixed(repl))
}


#' Gets components for a solution
#'
#' @param x xml object (solution)
#' @return data.frame with the solution components (resources, sim components, outputs)
#' @export
getComponents <- function(x)
{
  df <- NULL

  df <- rbind(df,data.frame(id="CURRENT",type=("var"),subtype="var",ref="SYSTEM",javaclass=""))
  df <- rbind(df,data.frame(id="variables",type=("var"),subtype="var", ref="User",javaclass=""))

  int_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//interfaces//interface"),'id')
  int_type<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//interfaces//interface"),'type')
  int_file <- sapply(int_id, function(oi) xml2::xml_text(xml2::xml_find_all(x,paste0("//solution//interfaces//interface[@id='",oi,"']//filename"))))
  int_file[lengths(int_file)==0] <- ""
  int_file <- unlist(int_file)

  if(length(int_id)>0)
  {
    df <- rbind(df,data.frame(id=int_id,type="interface",subtype=int_type,ref=int_file,javaclass=""))
  }

  res_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//resource"),'id')
  res_interf<- xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//resource"),'interface')
  res_file <- sapply(res_interf, function(oi) xml2::xml_text(xml2::xml_find_all(x,paste0("//solution//interfaces//interface[@id='",oi,"']//filename"))))
  if(length(res_id)>0)
  {
    df <- rbind(df,data.frame(id=res_id,type="resource",subtype="resource",ref=paste0(res_interf,"[", res_file,"]"),javaclass=""))
  }

  trf_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'id')
  trf_res<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'resource')
  trf_cls<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'class')
  if(length(trf_id)>0)
  {
    df <- rbind(df,data.frame(id=trf_id,type="resource",subtype="transform",ref=trf_res,javaclass=trf_cls))
  }

  als_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//alias"),'id')
  als_res<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//alias"),'resource')
  if(length(als_id)>0)
  {
    df <- rbind(df,data.frame(id=als_id,type="resource",subtype="alias",ref=als_res,javaclass=""))
  }


  cmp_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//simmodel//simcomponent"),'id')
  cmp_cls <-xml2::xml_attr(xml2::xml_find_all(x,"//solution//simmodel//simcomponent"),'class')
  cmp_clst<-simComponentType(cmp_cls)
  cmp_ref <- sapply(seq_along(cmp_id),\(i)ifelse(cmp_clst[i]=="grouped",
      xml2::xml_text(xml2::xml_find_all(x,paste0("//solution//simmodel//simcomponent[@id='",cmp_id[i],"']/group")))                                           ,cmp_cls[i]))
  if(length(cmp_id)>0)
  {
    df <- rbind(df,data.frame(id=cmp_id,type="simcomponent",subtype=cmp_clst,ref=cmp_ref,javaclass=cmp_cls))
  }

  out_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//outputs//output"),'id')
  out_interf<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//outputs//output"),'interface')
  out_file <- sapply(out_interf,
                     function(oi) {
                       typ <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//interfaces//interface[@id='",oi,"']")),"type")
                       is_mem = typ %in% c("MEMORY")
                       ifelse(is_mem,typ,xml2::xml_text(xml2::xml_find_all(x,paste0("//solution//interfaces//interface[@id='",oi,"']//filename"))))}
                    )
  if(length(out_id)>0)
  {
    df <- rbind(df,data.frame(id=out_id,type="output",subtype="output",ref=paste0(out_interf,"[",out_file,"]"), javaclass=""))
  }

  df$nr <- 1:nrow(df)
  df$id <- as.character(df$id)
  df$type <- as.character(df$type)
  df$subtype <- as.character(df$subtype)
  df$ref <- as.character(df$ref)
  rownames(df) <- NULL
  df
}

#' Get the dataframe of links between components
#'
#' @param x xml object (solution)
#' @param df data.frame of components
#' @return data.frame with the linked variables between components
#' @export
getLinks <- function(x,df)
{

  vdf_all<-NULL

  # sim components
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

  # interfaces
  dfi <- df[df$type=="interface" & grepl("[$][{}][^}]+[}]",df$ref),]
  for(j in seq_along(dfi))
  {
    i <- dfi[j,"id"]
    inp <- dfi[j,"ref"]
    inp <- unique(inp[!is.na(inp)])
    if(length(inp)>0)
    {
      l<-lapply(inp,getVars,ruleonly=TRUE)
      vdf <- (do.call(rbind, l))
      vdf$to = i
      vdf$rel="key"
      vdf_all<- rbind(vdf_all, vdf)
    }
  }
  # resources: interfaces, alias and transformers resource
  res_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//resource"),'id')
  res_resources <-  xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//resource")),'interface')
  if(length(res_resources)>0)
  {
    vdf_all <-rbind(vdf_all,data.frame(from=res_resources, to=res_id, name="-",rel="storage"))
  }

  trf_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//transform"),'id')
  trans_resources <-  xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//transform")),'resource')
  if(length(trans_resources)>0)
  {
    vdf_all <-rbind(vdf_all,data.frame(from=trans_resources, to=trf_id, name="-",rel="data"))
  }

  als_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//resources//alias"),'id')
  als_resources <-  xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//alias")),'resource')
  if(length(als_resources)>0)
  {
    vdf_all <-rbind(vdf_all,data.frame(from=als_resources, to=als_id, name="-",rel="data"))
  }

  # resources: rules and keys
  for(i in df[df$type=="resource","id"])
  {
    rls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//resource[@id='",i,"']")),'rule')
    trls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//transform[@id='",i,"']")),'rule')
    arls <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//alias[@id='",i,"']")),'rule')
    inp <- c(rls, trls,arls)
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
    aks <- xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//resources//alias[@id='",i,"']//res")),'key')
    inp <- c(ks, tks, aks)
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

  # outputs: interfaces
  out_id<-xml2::xml_attr(xml2::xml_find_all(x,"//solution//outputs//output"),'id')
  out_resources <-  xml2::xml_attr(xml2::xml_find_all(x,paste0("//solution//outputs//output")),'interface')
  if(length(out_resources)>0)
  {
    vdf_all <-rbind(vdf_all,data.frame(from=out_resources, to=out_id, name="-",rel="storage"))
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
  rownames(vdf_all)<-NULL
  vdf_all
}


#' Get component and links dataframe from solution file
#'
#' @param file solution
#' @return list with solution (xml2 object) and components, links and variables data.frame
#' @export
getElementsFromSolutionFile <- function(file)
{
  sol <- xml2::read_xml(file)
  comp <- getComponents(sol)
  links <- getLinks(sol,comp)
  vars <- getUserVariables(sol)
  list("solution"=sol,"components"=comp, "links"=links, "variables"=vars)
}

#' Get ids of memory outputs
#' @param comp components dataframe
#' @return character vector with the memory output ids
#' @export
getMemoryOutputIds <- function(comp) {
  comp[!is.na(comp$ref) & substr(comp$ref,nchar(comp$ref)-7,nchar(comp$ref))=="[MEMORY]", "id"]
}

#' Get filenames of file outputs
#' @param comp components dataframe
#' @param variables variables dataframe
#' @param additional additional variables as named vector c("var1"="value1", ...), useful for directory placeholder
#' @return character vector with the memory output ids
#' @export
getOutputFilenames <- function(comp,variables,additional=NULL) {
  refs <- comp[comp$type=="output" & !is.na(comp$ref) & substr(comp$ref,nchar(comp$ref)-7,nchar(comp$ref))!="[MEMORY]", "ref"]
  refs <- gsub("[^\\[]+\\[(.+)\\]","\\1",refs)
  replaceVariablesWithValues(refs,
      variables,
      additional=additional
  )
}


#' Determines the solution by it's file extension
#'
#' @param file filename
#' @keywords internal
isSolution <-function(file) {
  substr(file, nchar(file)-7, nchar(file)) == ".sol.xml"
}


#' Get metadata from file
#'
#' @param file filename (full path)
#' @param workdir working directory path
#' @keywords internal
getMetadataForFile <- function(file, workdir)
{
  part <- gsub(paste0(workdir,"/"),"",file)
  folders <- strsplit(part,"/")[[1]]
  folders <-folders[folders!="solution" & !isSolution(folders)]
  date <- file.info(file)[1,'mtime']
  list(date=date,folders=folders)
}



#' Get info for a solution
#'
#' @param file solution file
#' @param workdir working directory
#' @return data.frame with all solution components as well as meta data for solution file
#' @export
getSolutionInfoAsDataframe <- function (file, workdir)
{
  sol <- xml2::read_xml(file)
  comp <- getComponents(sol)
  md <- getMetadataForFile(file, workdir)
  comp$file <- file
  comp$lastmodified <- md$date
  comp$user <- md$folders[1]
  comp$project <- md$folders[2]
  comp$subproject <- md$folders[3]
  comp$subsubproject <- md$folders[4]
  comp$subsubsubproject <- md$folders[5]
  comp
}
