#' Reorder elements 
#' 
#' Reorders elements of a vector by a given start and end order.
#' Elements that are not part of start or end order will stay in the middle.
#'
#' @param elements vector that should be reordered 
#' @param startorder element order at the head
#' @param endorder element order at the tail
#'
#' @returns reordered vector
#' @noRd

changeOrder <- function(elements, startorder, endorder) {
  start <- intersect(startorder, elements)
  end <- intersect(endorder,elements)
  additional <- setdiff(elements,c(start, end))
  c(start,additional,end)  
}

#' Reorder and format attributes of elements
#'
#' @param doc xml2 document (solution or project)
#' @param dropattributes vector of attribute names that should be dropped
#'
#' @returns xml2 document with reordered attributes
#' @noRd
reorderAndFormatAttributes <- function(doc, dropattributes=c())
{
  
  orders <- list(
  'res' = list(
    'tag' = 'res',
    'start' = c("id","datatype","mode","unit","description"),
    'end' = c("key")
  ),
  'var' = list(
    'tag' = 'var',
    'start' = c("id","datatype","mode","unit","description"),
    'end' = c("rule")
  ),
  'out' = list(
    'tag' = 'out',
    'start' = c("id","datatype","mode","format","unit","description"),
    'end' = c("rule")
  ),
  'input' = list(
    'tag' = 'input',
    'start' = c("id","datatype","source"),
    'end' = c()
  ),
  'resource' = list(
    'tag' = 'resource',
    'start' = c("id","interface","frequence"),
    'end' = c("rule")
  ),
  'transformer' = list(
    'tag' = 'transformer',
    'start' = c("id","resource","frequence"),
    'end' = c("rule","class")
  ),
  'simcomponent' = list(
    'tag' = 'simcomponent',
    'start' = c("id","description"),
    'end' = c("class")
  ),
  'output' = list(
    'tag' = 'output',
    'start' = c("id","interface","frequence"),
    'end' = c("resetrule", "rule")
  ),
  'action' = list(
    'tag' = 'action',
    'start' = c("default"),
    'end' = c("rule")
  )
  
  )
  
  for(o in orders) {
    elements <- xml2::xml_find_all(doc,paste0("//",o$tag))
    for(el in elements) {
      at <- xml2::xml_attrs(el)
      ord <- changeOrder(names(at),o$start,o$end)
      if(!is.na(at["rule"])) {
        at["rule"] <- gsub("[ ][ ]([ ]*)","\n \\1", at["rule"])
        at["rule"] <- gsub(" && "," and ", at["rule"], fixed=TRUE)
        at["rule"] <- gsub(" || "," or ", at["rule"], fixed=TRUE)
        at["rule"] <- gsub(" >= "," ge ", at["rule"], fixed=TRUE)
        at["rule"] <- gsub(" <= "," le ", at["rule"], fixed=TRUE)
        at["rule"] <- gsub(" > "," gt ", at["rule"], fixed=TRUE)
        at["rule"] <- gsub(" < "," lt ", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("}>=0","} ge 0", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("}>=1","} ge 1", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("}>=2","} ge 2", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("}>0","} gt 0", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("}>1","} gt 1", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("}>2","} gt 2", at["rule"], fixed=TRUE)
        at["rule"] <- gsub("} >=2","} ge 2", at["rule"], fixed=TRUE)
      }
      xml2::xml_attrs(el) <- NULL
      xml2::xml_attrs(el) <- at[ord]
      for(a in dropattributes) {
        xml2::xml_set_attr(el, a, NULL)
      }
    }
  }
  doc
}

#' Reformats solution or project files
#'
#' @param type "solution" or "project"
#' @inheritParams formatSolutionFile
#' @returns no return value, called for the side effect to write a file to disk
#' @noRd
formatXmlFile <- function(type, file, outfile= file, version="5.2", stripcomments = FALSE, dropattributes=c()) {
  
  dtd <- "SimSolution"
  if(type=="solution") {
    dtd <- "SimSolution"
  }
  else {
    dtd <- "SimProject"
  }
  
  doc <- xml2::read_xml(file)
  doc <- reorderAndFormatAttributes(doc, dropattributes)
  xml2::xml_remove(xml2::xml_find_all(doc, "//poolsize"))
  
  if(stripcomments) {
    xml2::xml_remove(xml2::xml_find_all(doc, "//comment()"))
  }
  
  simpools <- xml2::xml_find_all(doc, "//interfaces[@default='simpool']")
  xml2::xml_attr(simpools, "default") <- NULL
  
  if(type=="solution") {
    xml2::xml_attr(doc,"version") <- version
  }
  xml2::write_xml(doc, outfile,format=c())
  txt <- readr::read_file(outfile)
  txt <- gsub("&#10;","\n",txt)
  vs <- strsplit(version,".",fixed=TRUE)[[1]]
  txt <- gsub(paste0(dtd,"_[0-9]+_[0-9]+[.]dtd"),paste0(dtd, "_",vs[1],"_",vs[2],".dtd"),txt)
  readr::write_file(txt,outfile)
}

#' Formats / prettyprints a solution file
#' 
#' @param file filename of solution or project
#' @param outfile filename the reformatted document is written to
#' @param version version of the solution
#' @param stripcomments if TRUE, then strip xml comments
#' @param dropattributes vector of attribute names that will be dropped globally.
#'
#' @returns no return value, called for the side effect to write a file to disk

#' @export
#' 
#' @examples
#' \dontrun{
#' original <- system.file("solution","Yield.sol.xml",package="simplaceUtil")
#' formatted <- tempfile(fileext = ".sol.xml") 
#' formatSolutionFile(original, formatted, stripcomments = TRUE)
#' }


formatSolutionFile <- function(file, outfile=file, version="5.2", stripcomments = FALSE, dropattributes=c()) {
  formatXmlFile("solution", file, outfile, version, stripcomments, dropattributes)
}

#' Formats / prettyprints a project file
#' 
#' @inheritParams formatSolutionFile
#' 
#' @returns no return value, called for the side effect to write a file to disk
#' 
#' @export
#' 
formatProjectFile <- function(file, outfile=file, version="5.2", stripcomments = FALSE, dropattributes=c()) {
  formatXmlFile("project", file, outfile, version, stripcomments, dropattributes)
}

