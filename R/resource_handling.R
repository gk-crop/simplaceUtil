#' Creates XML stubs for interface and resource section from CSV file structure
#'
#' @param filename name of the CSV file
#' @param id id for the resource
#' @param sep separator for CSV file
#' @param keyvals named vector of column indices or names that act as key column
#' @param arraycolumns vector of column indices or names that are arrays
#' @param frequence frequence attribute for the resource
#' @param rule rule attribute for the resource
#' @param data optional data.frame (otherwise data will be loaded from file)
#'
#' @return list of two strings ('interface' and 'resource')
#' @export
#'
#' @examples
#' stubs <- createResourceStubsFromCsv(
#'   filename = system.file("input","weather.csv", package="simplaceUtil"),
#'   id = "weather",
#'   sep =",",
#'   keyvals = c("CURRENT.DATE" = "Date")
#' )
#' cat(stubs$interface)
#' cat(stubs$resource)
#'
#' stubs <- createResourceStubsFromCsv(
#'   filename = system.file("input","soil.csv", package="simplaceUtil"),
#'   id = "soil",
#'   sep =";",
#'   keyvals = c("vSoilType" = "soiltype"),
#'   arraycolumns = 6:20
#' )
#' cat(stubs$resource)

#'
createResourceStubsFromCsv <- function(filename, id,
                                            sep=",",
                                            keyvals = NULL,
                                            arraycolumns = NULL,
                                            frequence = "DAILY",
                                            rule = NULL,
                                            data = NULL
                                            ) {
  if(!is.data.frame(data)) {
    data <- utils::read.delim(filename, sep=sep, header = TRUE, nrows = 100)
  }
  nm <- names(data)
  kn <- names(keyvals)
  if(is.numeric(keyvals)) {
    keyvals <- nm[keyvals]
    names(keyvals) <- kn
  }
  if(is.numeric(arraycolumns)) {
    arraycolumns <- nm[arraycolumns]
  }
  intf <- paste0('<interface id="',id,'_file" type="CSV">',"\n",
                 '  <poolsize>100</poolsize>',"\n",
                 '  <divider>',sep,'</divider',"\n",
                 '  <filename>',filename,'</filename>',"\n",
                 '</interface>',"\n")

  rl <- ""
  if(!is.null(rule) && frequence %in% c("BOOLEAN","COMPLEX")) {
    rl <- paste0(' rule="',rule,'"')
  }
  res <- paste0('<resource id="',id,'" interface="',id,'_file" frequence="',frequence,'"',rl,'>',"\n",'  <header>')
  for(i in 1:ncol(data)) {
    dt <- "DOUBLE"
    if(is.character(data[1,i])) {
      dt <- "CHAR"
    }
    else if ("Date" %in% class(data[1,i])){
      dt <- "DATE"
    }
    else if (is.logical(data[1,i])){
      dt <- "BOOLEAN"
    }
    else if (is.integer(data[1,i])){
      dt <- "INT"
    }
    if(nm[i] %in% arraycolumns) {
      dt <- paste0(dt,"ARRAY")
    }
    key <- ""
    if(nm[i] %in% keyvals) {
      key <- paste0(' key="',kn[keyvals==nm[i]],'"')
    }
    ri <- paste0("\n",'    <res id="',nm[i],'"',key,' datatype="',dt,'" unit=""/>')
    res <- paste0(res, ri)
  }
  res <- paste0(res,"\n",'  </header>',"\n",'</resource>')
  list(interface = intf, resource = res)
}


#' Creates XML stubs for interface and resource section from XML file structure
#'
#' @param filename name of the xml file
#' @param id id for the resource
#' @param keyvals named vector of column indices or names that act as key column
#' @param frequence frequence attribute for the resource
#' @param rule rule attribute for the resource
#' @param xmlnode optional xml_node (otherwise xml will be read from filename)
#'
#' @return list of two strings ('interface' and 'resource')
#' @export
#'
#' @examples
#' stubs <- createResourceStubsFromXml(
#'   filename = system.file("input","crop.xml",package="simplaceUtil"),
#'   id="soil",
#'   keyvals = c("vSoilType"=1)
#' )
#' cat(stubs$resource)
createResourceStubsFromXml <- function(filename, id,
                                            keyvals = NULL,
                                            frequence = "DAILY",
                                            rule = NULL,
                                            xmlnode = NULL) {

  if(!("xml_node" %in% class(xmlnode))) {
    xmlnode <- xml2::read_xml(filename)
  }

  kn <- names(keyvals)

  intf <- paste0('<interface id="',id,'_file" type="XML">',"\n",
                 '  <poolsize>100</poolsize>',"\n",
                 '  <filename>',filename,'</filename>',"\n",
                 '</interface>',"\n")

  rl <- ""
  if(!is.null(rule) && frequence %in% c("BOOLEAN","COMPLEX")) {
    rl <- paste0(' rule="',rule,'"')
  }
  res <- paste0('<resource id="',id,'" interface="',id,'_file" frequence="',frequence,'"',rl,'>',"\n",'  <header>')


  set <- xml2::xml_children(xmlnode)[[1]]
  i<- 0
  for(entry in xml2::xml_children(set)) {
    i <- i + 1
    id <- xml2::xml_attr(entry,"id")
    dt <- xml2::xml_attr(entry,"datatype")
    unit <- xml2::xml_attr(entry,"unit")

    if(is.na(dt)) {
      dt <- "CHAR"
      ch <- xml2::xml_children(entry)
      if(length(ch)>0) {
        v <- xml2::xml_text(ch[[1]])
      }
      else {
        v <- xml2::xml_text(entry)
      }
      if(v %in% c("true","TRUE","false","FALSE")) {
        dt <- "BOOLEAN"
      }
      else if (tryCatch({as.Date(v);TRUE}, error=\(x){FALSE})){
        dt <- "DATE"
      }
      else if (!is.na(suppressWarnings(as.numeric(v)))){
        dt <- "DOUBLE"
      }

      if(length(ch)>0) {
        dt <- paste0(dt,"ARRAY")
      }

    }


    key <- ""
    if(is.numeric(keyvals)) {
      kid <- i
    }
    else {
      kid <- id
    }
    if(kid %in% keyvals) {
      key <- paste0(' key="',kn[keyvals==kid],'"')
    }

    ri <- paste0("\n",'    <res id="',id,'"',key,' datatype="',dt,'" unit="',unit,'"/>')
    res <- paste0(res, ri)
  }
  res <- paste0(res,"\n",'  </header>',"\n",'</resource>')
  list(interface = intf, resource = res)
}


#' Read XML parameter file
#'
#' @param file filename of parameterfile
#'
#' @returns parameterdata (XML2 object)
#' @export
readXMLParameterFile <- function(file) {
  xml2::read_xml(file)
}

#' Write parameterdata to xml file
#'
#' @param paramdata parameterdata (xml2 object)
#' @param file filename of parameterfile
#'
#' @returns just for side effects, returs paramdata invisibly
#' @export
#'
writeXMLParameterFile <- function(paramdata, file) {
  xml2::write_xml(paramdata, file)
  invisible(paramdata)
}


#' Get IDs of all parametersets in the parameterfile
#'
#' @inheritParams writeXMLParameterFile
#' @param keyid id of the parameter that identifies the parameterset
#'
#' @returns character vector with all IDs
#' @export
#'
#' @examples
#'
#' params <- readXMLParameterFile(system.file("input", "crop.xml", package="simplaceUtil"))
#' getXMLParameterIDs(params, "CROPNAME")
#'
getXMLParameterIDs <- function(paramdata, keyid) {
  xpath <- paste0("/*/*/parameter[@id='",keyid[1],"']")
  xml2::xml_text(xml2::xml_find_all(paramdata, xpath))
}



#' Get a parameter value for a specific parameterset
#'
#' @inheritParams writeXMLParameterFile
#' @param key named vector `c(idname="value")` to identify the dataset, the name is the id of the parameter and the value has to match the parameter's value
#' @param id name of the parameter of which the value should be retrieved
#'
#' @returns the value of the parameter
#' @export
#'
#' @examples
#'
#' params <- readXMLParameterFile(system.file("input", "crop.xml", package="simplaceUtil"))
#' getXMLParameterIDs(params, "CROPNAME")
#' getXMLParameter(params, c(CROPNAME="soy bean"), "TSUM1")
#' getXMLParameter(params, c(CROPNAME="faba bean"), "TSUM1")
#'
getXMLParameter <- function(paramdata, key, id) {
  xpath <- paste0("/*/*[parameter[@id='",names(key)[1],"' and text()='",key[1],"']]")
  y<-xml2::xml_find_all(paramdata, xpath)
  if(length(y)==0) {
    warning(paste0("There is no parameterset identified by ",names(key[1]),"='",key[1],"'"))
    return(NA)
  }
  z <- xml2::xml_find_first(y,paste0("parameter[@id='",id,"']"))
  if(is.na(xml2::xml_type(z))) {
    warning(paste0("Parameter '",id,"' is not in the parameterset of ",names(key[1]),"='",key[1],"'"))
    return(NA)
  }
  vals <- xml2::xml_find_all(z,"value")
  if(length(vals)>0) {
    vals <- xml2::xml_text(vals, trim=TRUE)
  } else {
    vals <- xml2::xml_text(z, trim=TRUE)
  }
  vals
}


#' Set a parameter value for a specific parameterset
#'
#' @inheritParams getXMLParameter
#' @param id name of the parameter of which the value should be set
#' @param value new value to set (vector)
#'
#' @returns modified parameter set (xml2 object)
#' @export
#'
#' @examples
#'
#' params <- readXMLParameterFile(system.file("input", "crop.xml", package="simplaceUtil"))
#' getXMLParameter(params, c(CROPNAME="soy bean"), "TSUM1")
#' params_new <-setXMLParameter(params, c(CROPNAME="soy bean"), "TSUM1",400)
#' getXMLParameter(params_new, c(CROPNAME="soy bean"), "TSUM1")
#'
setXMLParameter <- function(paramdata, key, id, value="") {
  xpath <- paste0("/*/*[parameter[@id='",names(key)[1],"' and text()='",key[1],"']]")
  x <- xml_clone(paramdata)
  y<-xml2::xml_find_all(x, xpath)
  if(length(y)==0) {
    stop(paste0("There is no parameterset identified by ",names(key[1]),"='",key[1],"'"))
  }
  z <- xml2::xml_find_first(y,paste0("parameter[@id='",id,"']"))
  if(is.na(xml2::xml_type(z))) {
    stop(paste0("Parameter '",id,"' is not in the parameterset of ",names(key[1]),"='",key[1],"'"))
  }
  cz <- xml2::xml_children(z)
  sapply(cz, xml2::xml_remove)
  if(length(value)>1) {
    for(v in value) {
      xml2::xml_add_child(z, .value="value", v)
    }
  }
  else {
    xml2::xml_text(z) <- as.character(value)
  }
  x
}


#' Scales a parameter value by a value
#'
#' @inheritParams getXMLParameter
#' @param id name of the parameter which should be scaled
#' @param factor scaling factor
#'
#' @returns modified parameter set (xml2 object)
#' @export
#'
#' @examples
#'
#' params <- readXMLParameterFile(system.file("input", "crop.xml", package="simplaceUtil"))
#' getXMLParameter(params, c(CROPNAME="soy bean"), "RUETableRUE")
#' params_new <-scaleXMLParameter(params, c(CROPNAME="soy bean"), "RUETableRUE",0.9)
#' getXMLParameter(params_new, c(CROPNAME="soy bean"), "RUETableRUE")
#'
scaleXMLParameter <- function(paramdata, key, id, factor=1) {
  vals <- as.numeric(getXMLParameter(paramdata, key, id)) * factor
  setXMLParameter(paramdata, key, id, vals)
}


#' Adds a parameter to a specific parameterset
#'
#' When the parameter already exists in the parameter set,
#' it is removed and the new parameter is added at the end.
#'
#'
#' @inheritParams getXMLParameter
#' @param id name of the parameter which should added
#' @param value value to set (numeric or character vector)
#' @param unit unit of the parameter
#' @param description description of the parameter
#'
#' @returns modified parameter set (xml2 object)
#' @export
#'
#' @examples
#' params <- readXMLParameterFile(system.file("input", "crop.xml", package="simplaceUtil"))
#' getXMLParameter(params, c(CROPNAME="soy bean"), "ExtraTableYZZ")
#' params_new <- addXMLParameter(params, c(CROPNAME="soy bean"), "ExtraTableYZZ", c(1,2,4))
#' getXMLParameter(params_new, c(CROPNAME="soy bean"), "ExtraTableYZZ")


addXMLParameter <- function(paramdata, key, id, value="", unit="", description=NULL) {
  xpath <- paste0("/*/*[parameter[@id='",names(key)[1],"' and text()='",key[1],"']]")
  x <- xml_clone(paramdata)
  y<-xml2::xml_find_all(x, xpath)
  if(length(y)==0) {
    stop(paste0("There is no parameterset identified by ",names(key[1]),"='",key[1],"'"))
  }
  z <- xml2::xml_find_first(y,paste0("parameter[@id='",id,"']"))
  if(length(z)>0) {
    xml2::xml_remove(z)
  }
  xml2::xml_add_child(y, .value="parameter", id=id, unit=unit)
  z <- xml2::xml_find_first(y, paste0("parameter[@id='",id,"']"))
  if(!is.null(description)) {
    xml2::xml_attr(z, "description") <- description
  }
  if(length(value)>1) {
    for(v in value) {
      xml2::xml_add_child(z, .value="value", v)
    }
  }
  else {
    xml2::xml_text(z) <- as.character(value)
  }
  x
}

