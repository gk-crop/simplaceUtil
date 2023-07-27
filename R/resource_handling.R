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
#' stubs <- createCsvResourceStubs(
#'   filename = system.file("input","weather.csv", package="simplaceUtil"),
#'   id = "weather",
#'   sep =",",
#'   keyvals = c("CURRENT.DATE" = "Date")
#' )
#' cat(stubs$interface)
#' cat(stubs$resource)
#'
#' stubs <- createCsvResourceStubs(
#'   filename = system.file("input","soil.csv", package="simplaceUtil"),
#'   id = "soil",
#'   sep =";",
#'   keyvals = c("vSoilType" = "soiltype"),
#'   arraycolumns = 6:20
#' )
#' cat(stubs$resource)

#'
createCsvResourceStubs <- function(filename, id,
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
  res <- paste0('<resource id="',id,'" interface="',id,'_file" frequence="',frequence,'"',rl,'>')
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
    ri <- paste0("\n",'  <res id="',nm[i],'"',key,' datatype="',dt,'", unit=""/>')
    res <- paste0(res, ri)
  }
  res <- paste0(res,"\n",'</resource>')
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
#' stubs <- createXmlResourceStubs(
#'   filename = system.file("input","crop.xml",package="simplaceUtil"),
#'   id="soil",
#'   keyvals = c("vSoilType"=1)
#' )
#' cat(stubs$resource)
createXmlResourceStubs <- function(filename, id,
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
  res <- paste0('<resource id="',id,'" interface="',id,'_file" frequence="',frequence,'"',rl,'>')


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

    ri <- paste0("\n",'  <res id="',id,'"',key,' datatype="',dt,'", unit="',unit,'"/>')
    res <- paste0(res, ri)
  }
  res <- paste0(res,"\n",'</resource>')
  list(interface = intf, resource = res)
}
