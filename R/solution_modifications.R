#' Reads a solution from file
#'
#' @param file filename of the solution
#' @return parsed solution as xml_document
#'
#' @export
getSolutionFromFile <- function(file) {
  xml2::read_xml(file)
}

#' Writes solution to file
#'
#' @param sol solution object (xml_document)
#' @param file filename for the solution
#'
#' @export
writeSolutionToFile <- function(sol, file) {
  xml2::write_xml(sol, file)
}

#' Converts a solution to text
#'
#' @param sol solution object (xml_document)
#' @return text with xml code
#'
#' @export
getTextFromSolution <- function(sol) {
  as.character(sol)
}

#' Reads a solution from text
#'
#' @param text string with xml markup
#' @return parsed solution as xml_document
#'
#' @export
getSolutionFromText <- function(text) {
  xml2::read_xml(text)
}


#' Adds a new memory output to solution.
#'
#' Any output with the same id will be removed. The added output has no output
#' variables. One has to add new variables via `addOutputVariable`.
#'
#' @param sol solution object
#' @param outputid id of new output
#' @param frequence one of DAILY, YEARLY, BOOLEAN, COMPLEX
#' @param rule optional rule, when frequence is BOOLEAN or COMPLEX
#' @param resetrule optional resetrule, when frequence is BOOLEAN or COMPLEX
#' @return modified solution object
#'
#' @export
addMemoryOutput <- function(sol, outputid, frequence="DAILY", rule=NULL, resetrule=NULL) {
  x <- xml2::xml_new_root(sol)
  x <- removeOutput(x,outputid)
  intfs <- xml2::xml_find_first(x, '/solution/interfaces')
  xml2::xml_add_child(intfs, "interface", id=paste0(outputid,"_meminterface"),type="MEMORY")
  intf <- xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',outputid,'_meminterface"]'))
  xml2::xml_add_child(intf,"poolsize",20000)
  outs <- xml2::xml_find_first(x, '/solution/outputs')
  xml2::xml_add_child(outs, "output", id=outputid, interface=paste0(outputid,"_meminterface"),
                      frequence=frequence)
  out <- xml2::xml_find_first(x, paste0('/solution/outputs/output[@id="',outputid,'"]'))
  xml2::xml_add_child(out,"header")
  if(!is.null(rule)) {
    xml2::xml_attr(out,"rule")<-rule
  }
  if(!is.null(resetrule)) {
    xml2::xml_attr(out,"rule")<-resetrule
  }
  x
}


#' Removes output with given id
#'
#' Notice: the interface for the output will also be removed.
#'
#' @param sol solution object
#' @param outputid id of output to remove
#' @return modified solution object
#'
#' @export
removeOutput <- function(sol, outputid) {
  x <- xml2::xml_new_root(sol)
  out <- xml2::xml_find_first(x, paste0('/solution/outputs/output[@id="',outputid,'"]'))
  intfid <- xml2::xml_attr(out,"interface")
  intf <- xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',intfid,'"]'))
  xml2::xml_remove(out)
  xml2::xml_remove(intf)
  x
}

#' Removes all non-MEMORY outputs from solution.
#'
#' When running large amout of runs (e.g. calibration) it's recommended to avoid
#' to write outputs on disk.
#'
#' @param sol solution object
#' @return modified solution object
#'
#' @export
removeNonMemoryOutputs <- function(sol) {
  x <- xml2::xml_new_root(sol)
  intfs <- xml2::xml_attr(xml2::xml_find_all(x, '/solution/interfaces/interface[@type!="MEMORY"]'),"id")
  outs <- xml2::xml_attr(xml2::xml_find_all(x, '/solution/outputs/output'),"interface")
  to_remove <- intersect(intfs, outs)
  for(intf in to_remove) {
    xml2::xml_remove(xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',intf,'"]')))
    xml2::xml_remove(xml2::xml_find_first(x, paste0('/solution/outputs/output[@interface="',intf,'"]')))
  }
  x
}

#' Adds output variable to an existing output
#'
#' @param sol solution object
#' @param outputid id of output where the variable should be added
#' @param id name of the new variable
#' @param rule rule for the variable
#' @param datatype of the variable
#' @param mode one of FIRST, LAST, AVG, SUM (optional)
#' @param unit unit of the variable (optional)
#' @param description short description (optional)
#' @return modified solution object
#'
#' @export
addOutputVariable <- function(sol, outputid, id, rule, datatype, mode=NULL,
                              unit=NULL, description=NULL) {

  x <- xml2::xml_new_root(sol)
  x <- removeOutputVariable(x, outputid, id)
  cmp <- xml2::xml_find_first(x,paste0('/solution/outputs/output[@id="',outputid,'"]/header'))
  xml2::xml_add_child(cmp,'out', id=id, rule=rule, datatype=datatype)
  md <- xml2::xml_find_first(x,paste0('/solution/outputs/output[@id="',outputid,'"]/header/out[@id="',id,'"]'))
  if(!is.null(mode)) {
    xml2::xml_attr(md,"mode") <- mode
  }
  if(!is.null(unit)) {
    xml2::xml_attr(md,"unit") <- unit
  }
  if(!is.null(description)) {
    xml2::xml_attr(md,"description") <- description
  }
  x

}

#' Removes an output variable from a given output
#'
#' @param sol solution object
#' @param outputid id of output from where the variable should be removed
#' @param id name of the variable to remove
#' @return modified solution object
#'
#' @export
removeOutputVariable <- function(sol, outputid, id) {

  x <- xml2::xml_new_root(sol)
  outp <- xml2::xml_find_all(x,paste0('/solution/outputs/output[@id="',outputid,'"]/header/out[@id="',id,'"]'))
  xml2::xml_remove(outp)
  x
}

#' Adds an user variable to the `variables` section
#'
#' @param sol solution object
#' @param id id of the variable
#' @param value value of the variable
#' @param datatype of the variable
#' @param unit unit (optional)
#' @param description short description (optional)
#' @return modified solution object
#'
#' @export
addUserVariable <- function(sol, id, value, datatype, unit="", description="") {
  x <- xml2::xml_new_root(sol)
  vars <- xml2::xml_find_first(x,"/solution/variables")
  pos <- length(xml2::xml_find_all(vars,"/solution/variables/var"))
  xml2::xml_add_child(vars,'var', value, id=id, datatype=datatype, unit=unit, description=description, .where=pos)

  x
}




#' Adds an input to a sim component.
#'
#' Notice: One can only add inputs that are defined by the sim component. If source
#' is not given, then the parameter value is used.
#'
#' @param sol solution object
#' @param componentid id of sim component
#' @param id id of variable
#' @param source source of variable
#' @param value value of variable (is used only if source is not given or NULL)
#' @param datatype datatype (optional)
#' @param unit unit (optional)
#' @param description short description (optional)
#' @return modified solution object
#'
#' @export
addComponentInput <- function(sol, componentid, id, source=NULL, value=NULL, datatype=NULL, unit=NULL, description=NULL) {
  x <- xml2::xml_new_root(sol)
  x <- removeComponentInput(x, componentid, id)
  cmp <- xml2::xml_find_first(x,paste0('/solution/simmodel/simcomponent[@id="',componentid,'"]'))
  if(!is.null(source)) {
    xml2::xml_add_child(cmp,'input', id=id, source=source)

  } else {
    xml2::xml_add_child(cmp,'input', id=id, value)
  }
  md <- xml2::xml_find_first(x,paste0('/solution/simmodel/simcomponent[@id="',componentid,'"]/input[@id="',id,'"]'))
  if(!is.null(datatype)) {
    xml2::xml_attr(md,"datatype") <- datatype
  }
  if(!is.null(unit)) {
    xml2::xml_attr(md,"unit") <- unit
  }
  if(!is.null(description)) {
    xml2::xml_attr(md,"description") <- description
  }
  x
}

#' Removes an input for component
#'
#' @param sol solution object
#' @param componentid id of the sim component
#' @param id id of the input
#' @return modified solution object
#'
#' @export
removeComponentInput <- function(sol, componentid, id) {
  x <- xml2::xml_new_root(sol)
  inp <- xml2::xml_find_all(x,paste0('/solution/simmodel/simcomponent[@id="',componentid,'"]/input[@id="',id,'"]'))
  xml2::xml_remove(inp)
  x
}


#' Replaces a variable with another.
#'
#' Notice: there is no check, whether the ids exist. Variables are replaced
#' literarilly when they are the only content of an attribute or element content.
#' In other attributes and element contents ${oldid} will be replaced by ${newid}
#'
#' @param sol solution object
#' @param oldid id of variable to be replaced
#' @param newid id of the replacing variable
#' @return modified solution object
#'
#' @export
replaceVariable <- function(sol, oldid, newid) {
  # make a copy of the root node
  x <- xml2::xml_new_root(sol)

  # replace literal values in node text
  tl <- xml2::xml_find_all(x,paste0('/solution//*[.="',oldid,'"]'))
  xml2::xml_text(tl) <- gsub(oldid, newid, xml2::xml_text(tl), fixed=TRUE)

  # replace variable in formulas of note text
  tl <- xml2::xml_find_all(x,paste0('/solution//*//text()[contains(.,"${',oldid,'}")]'))
  xml2::xml_text(tl) <- gsub(paste0('${',oldid,'}'),paste0('${',newid,'}'),xml2::xml_text(tl),fixed = TRUE)

  attlist <- c("source", "rule", "key", "resetrule")
  for(att in attlist)
  {
    tl <- xml2::xml_find_all(x,paste0('/solution//*[@',att,'="',oldid,'"]'))
    xml2::xml_attr(tl, att) <- newid

    tl <- xml2::xml_find_all(x,paste0('/solution//*[contains(@',att,',"',oldid,'")]'))
    xml2::xml_attr(tl, att) <- gsub(oldid, newid, xml2::xml_attr(tl,att), fixed=TRUE)
  }

  x
}


#' Sets the value of an input for a given parent element
#'
#' @param sol solution object
#' @param parentid id of input parent
#' @param id id of the input
#' @param value new value
#' @return modified solution object
#'
#' @export
setInputValue <- function(sol, parentid, id, value) {
  x <- xml2::xml_new_root(sol)

  parent <- xml2::xml_find_first(x,paste0('/solution//*[@id="',parentid,'"]'))

  inp <- xml2::xml_find_first(parent,paste0('//input[@id="',id,'"]'))

  xml2::xml_attr(inp, "source") <- NULL
  xml2::xml_text(inp) <- as.character(value)
  x
}

#' Sets the value of an input in all elements of the given category
#'
#' It's main use is to change consistently inputs of all transformers,
#' e.g. `layerthickness`. It can also be used to set sim component inputs
#' to a fixed value.
#'
#' @param sol solution object
#' @param category tag of categories containing inputs, e.g. `transform` or `simcomponent`
#' @param id id of the input
#' @param value new value
#' @return modified solution object
#'
#' @export
setInputValueForCategory <- function(sol, category, id, value) {
  x <- xml2::xml_new_root(sol)

  parent <- xml2::xml_find_all(x,paste0('/solution//',category,''))

  inp <- xml2::xml_find_all(parent,paste0('//input[@id="',id,'"]'))

  xml2::xml_attr(inp, "source") <- NULL
  xml2::xml_text(inp) <- as.character(value)
  x
}
