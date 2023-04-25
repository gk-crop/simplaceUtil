xml_clone <- function(doc) {
  xml2::read_xml(as.character(doc))
}


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
#' @param cachesize optional cachesize
#' @return modified solution object
#'
#' @export
addMemoryOutput <- function(sol, outputid, frequence="DAILY", rule=NULL, resetrule=NULL, cachesize=10) {
  x <- xml2::read_xml(as.character(sol))
  x <- removeOutput(x,outputid)

  intfs <- xml2::xml_find_first(x, '/solution/interfaces')
  xml2::xml_add_child(intfs, "interface", id=paste0(outputid,"_meminterface"),type="MEMORY")
  intf <- xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',outputid,'_meminterface"]'))
  xml2::xml_add_child(intf,"poolsize",20000)
  outs <- xml2::xml_find_first(x, '/solution/outputs')
  xml2::xml_add_child(outs, "output", id=outputid, interface=paste0(outputid,"_meminterface"),
                      frequence=frequence, cachesize=10)
  out <- xml2::xml_find_first(x, paste0('/solution/outputs/output[@id="',outputid,'"]'))
  xml2::xml_add_child(out,"header")
  if(!is.null(rule)) {
    xml2::xml_attr(out,"rule")<-rule
  }
  if(!is.null(resetrule)) {
    xml2::xml_attr(out,"rule")<-resetrule
  }

  desc <- xml2::xml_find_first(x,"/solution/description")
  xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* added memory output ",outputid)

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
  x <- xml2::read_xml(as.character(sol))


  out <- xml2::xml_find_first(x, paste0('/solution/outputs/output[@id="',outputid,'"]'))
  if(length(out)>0)
  {
    intfid <- xml2::xml_attr(out,"interface")
    intf <- xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',intfid,'"]'))
    xml2::xml_remove(out)
    xml2::xml_remove(intf)

    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* removed output ",outputid,"")

  }
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
  x <- xml2::read_xml(as.character(sol))


  intfs <- xml2::xml_attr(xml2::xml_find_all(x, '/solution/interfaces/interface[@type!="MEMORY"]'),"id")
  outs <- xml2::xml_attr(xml2::xml_find_all(x, '/solution/outputs/output'),"interface")
  to_remove <- intersect(intfs, outs)
  if(length(to_remove)>0)
  {
    for(intf in to_remove) {
      xml2::xml_remove(xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',intf,'"]')))
      xml2::xml_remove(xml2::xml_find_first(x, paste0('/solution/outputs/output[@interface="',intf,'"]')))
    }

    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* removed non-memory outputs for interfaces",paste(to_remove,collapse=", "))
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

  x <- xml2::read_xml(as.character(sol))
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

  desc <- xml2::xml_find_first(x,"/solution/description")
  xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* added output variable",id," to output ",outputid,"")

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
  x <- xml2::read_xml(as.character(sol))


  outp <- xml2::xml_find_all(x,paste0('/solution/outputs/output[@id="',outputid,'"]/header/out[@id="',id,'"]'))
  if(length(outp)>0) {
    xml2::xml_remove(outp)

    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* removed output variable",id,"from output",outputid)
  }
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
addUserVariable <- function(sol, id, value, datatype, unit=NULL, description=NULL) {
  x <- xml2::read_xml(as.character(sol))

  vars <- xml2::xml_find_first(x,"/solution/variables")
  pos <- length(xml2::xml_find_all(vars,"/solution/variables/var"))
  if(length(value)>1) {
    value <- paste0("[",paste(value, collapse=", "),"]")
  }
  xml2::xml_add_child(vars,'var', value, id=id, datatype=datatype, .where=pos)
  var <- xml2::xml_find_first(x,paste0('/solution/variables/var[@id="',id,'"]'))
  if(!is.null(unit)) {
    xml2::xml_attr(var,"unit") <- unit
  }
  if(!is.null(description)) {
    xml2::xml_attr(var,"description") <- description
  }
  desc <- xml2::xml_find_first(x,"/solution/description")
  xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* added user variable",id,"with value",paste(value,collapse=","))

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
  x <- xml2::read_xml(as.character(sol))
  x <- removeComponentInput(x, componentid, id)


  cmp <- xml2::xml_find_first(x,paste0('/solution/simmodel/simcomponent[@id="',componentid,'"]'))

  if(length(cmp)>0)
  {
    xml2::xml_add_child(cmp,'input', id=id)
    md <- xml2::xml_find_first(x,paste0('/solution/simmodel/simcomponent[@id="',componentid,'"]/input[@id="',id,'"]'))
    if(!is.null(source)) {
      xml2::xml_attr(md,"source") <- source
    }
    else {
      if(length(value)>1) {
        for(v in value) xml2::xml_add_child(md, "value",v)
      }
      else {
        xml2::xml_text(md) <- as.character(value)
      }
    }
    if(!is.null(datatype)) {
      xml2::xml_attr(md,"datatype") <- datatype
    }
    if(!is.null(unit)) {
      xml2::xml_attr(md,"unit") <- unit
    }
    if(!is.null(description)) {
      xml2::xml_attr(md,"description") <- description
    }
    valtxt <- ""
    if(!is.null(source)) {
      valtxt <- paste("with source",source)
    }
    else if (!is.null(value)) {
      valtxt <- paste("with value",paste(value, collapse=","))
    }
    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* added input ",id,"to component",componentid,valtxt)

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
  x <- xml2::read_xml(as.character(sol))


  inp <- xml2::xml_find_all(x,paste0('/solution/simmodel/simcomponent[@id="',componentid,'"]/input[@id="',id,'"]'))

  if(length(inp)>0) {
    xml2::xml_remove(inp)

    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* removed input ",id,"from component",componentid)
  }

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
  x <- xml2::read_xml(as.character(sol))

  # save description, so that text is not affected by replacements
  olddesc <- xml2::xml_find_first(x,"/solution/description")
  desc <- xml2::xml_new_root(olddesc)
  xml2::xml_remove(olddesc)


  # replace literal values in node text
  tl <- xml2::xml_find_all(x,paste0('/solution//*[.="',oldid,'"]'))
  xml2::xml_text(tl) <- gsub(oldid, newid, xml2::xml_text(tl), fixed=TRUE)

  # replace variable in formulas of note text
  tl <- xml2::xml_find_all(x,paste0('/solution//*//text()[contains(.,"${',oldid,'}")]'))
  xml2::xml_text(tl) <- gsub(paste0('${',oldid,'}'),paste0('${',newid,'}'),xml2::xml_text(tl),fixed = TRUE)

  attlist <- c("source", "rule", "key", "resetrule")
  for(att in attlist) {
    tl <- xml2::xml_find_all(x,paste0('/solution//*[@',att,'="',oldid,'"]'))
    xml2::xml_attr(tl, att) <- newid

    tl <- xml2::xml_find_all(x,paste0('/solution//*[contains(@',att,',"',oldid,'")]'))
    xml2::xml_attr(tl, att) <- gsub(oldid, newid, xml2::xml_attr(tl,att), fixed=TRUE)
  }

  xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* replaced",oldid,"with",newid)
  xml2::xml_add_child(x,desc,.where=0)
  x
}


#' Sets the value of an input for a given parent element
#'
#' @param sol solution object
#' @param parentid id of input parent
#' @param id id of the input
#' @param value new value
#' @param datatype datatype (optional)
#' @return modified solution object
#'
#' @export
setInputValue <- function(sol, parentid, id, value, datatype=NULL) {
  x <- xml2::read_xml(as.character(sol))

  parent <- xml2::xml_find_first(x,paste0('/solution//*[@id="',parentid,'"]'))

  inp <- xml2::xml_find_first(parent,paste0('//input[@id="',id,'"]'))

  if (length(inp)>0) {
    xml2::xml_attr(inp, "source") <- NULL
    if(length(value)>1) {
      for(v in value) xml2::xml_add_child(inp, "value",v)
    }
    else {
      xml2::xml_text(inp) <- as.character(value)
    }

    if(!is.null(datatype)) {
      xml2::xml_attr(inp,"datatype") <- datatype
    }

    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* set input",id,"in",parentid,"to",paste(value,collapse=","))
  }

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
#' @param datatype datatype (optional)
#' @return modified solution object
#'
#' @export
setInputValueForCategory <- function(sol, category, id, value, datatype=NULL) {
  x <- xml2::read_xml(as.character(sol))

  parent <- xml2::xml_find_all(x,paste0('/solution//',category,''))

  inp <- xml2::xml_find_all(parent,paste0('//input[@id="',id,'"]'))

  if (length(inp)>0) {
    xml2::xml_attr(inp, "source") <- NULL
    if(length(value)>1) {
      for(v in value) xml2::xml_add_child(inp, "value",v)
    }
    else {
      xml2::xml_text(inp) <- as.character(value)
    }
    if(!is.null(datatype)) {
      xml2::xml_attr(inp,"datatype") <- datatype
    }
    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* set input",id,"in category",category,"to",paste(value,collapse=","))
  }

  x
}

#' Swaps the order of SimComponents in the solution
#'
#' Rearranges the order of SimComponents. All components that are mentioned in
#' the vector order will be rearranged according to their position in the order
#' vector. All other components remain on the same position.
#' E. g. an order of `c(5,6,1,3)` will put components on position 1,3,5,6 to
#' position 5, 6, 1, 3.
#'
#'
#' @param sol solution object
#' @param order a vector of component positions
#' @return modified solution object
#'
#' @export
swapComponents <-function(sol, order) {
  x <- xml2::read_xml(as.character(sol))
  sc <- xml2::xml_find_all(x,"/solution/simmodel/simcomponent")
  order <- order[order <= length(sc)]
  sorder <- sort(order)
  if(length(order)>0 && any(sorder !=order))
  {
    sc2 <- sc
    sc2[sorder] <- sc[order]
    xml2::xml_replace(sc,sc2)
    desc <- xml2::xml_find_first(x,"/solution/description")
    xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* changed order of components ",paste(sorder,collapse=","),"to",paste(order,collapse=","))

  }

  x
}


#' Adds elements for timing the execution time of sim components (and transformers)
#'
#' @param sol solution object
#' @param filename optional filename to write the timing information to csv file
#' @param componentlist optional list of component ids (if empty, all components will be timed)
#' @param interfaceid id for the interface (optional)
#' @param outputid  id for the output (optional)
#' @param simcomponentid id for the timing simcomponent (optional)
#'
#' @export
addTimingSimComponent <- function(sol, filename=NULL, componentlist = NULL,
                                  interfaceid = "automatic_timing_interface",
                                  outputid = "automatic_timing_output",
                                  simcomponentid = "AutomaticTiming") {
  x <- xml2::read_xml(as.character(sol))



  itype = ifelse(is.null(filename),"MEMORY","CSV")

  x <- removeOutput(x,outputid)

  intfs <- xml2::xml_find_first(x, '/solution/interfaces')
  xml2::xml_add_child(intfs, "interface", id=interfaceid,type=itype)
  intf <- xml2::xml_find_first(x, paste0('/solution/interfaces/interface[@id="',interfaceid,'"]'))
  xml2::xml_add_child(intf,"poolsize",20000)
  if(!is.null(filename))
  {
    xml2::xml_add_child(intf,"divider",",")
    xml2::xml_add_child(intf,"filename",filename)
  }


  sims <- xml2::xml_find_first(x,'/solution/simmodel')
  sc <- xml2::xml_add_child(sims,"simcomponent",
                      id=simcomponentid,
                      class="net.simplace.sim.components.FWAnalyticsSimComponent")
  pc <- xml2::xml_add_child(sc,'process')
  xml2::xml_add_child(pc,'var',id="iHeader", datatype="CHARARRAY")
  xml2::xml_add_child(pc,'var',id="iValues", datatype="INTARRAY")
  for(cmp in componentlist) {
    xml2::xml_add_child(pc,"var",id=cmp,datatype="CHAR")
  }

  outs <- xml2::xml_find_first(x, '/solution/outputs')
  xml2::xml_add_child(outs, "output", id=outputid, interface=paste0(interfaceid),
                      frequence="END", cachesize=10)
  out <- xml2::xml_find_first(x, paste0('/solution/outputs/output[@id="',outputid,'"]'))
  hd <- xml2::xml_add_child(out,"header")

  nr <- length(xml2::xml_find_all(x,"//resource"))+
    length(xml2::xml_find_all(x,"//transform"))+
    length(xml2::xml_find_all(x,"//simcomponent"))

  if(length(componentlist)>0) {
    nr <- length(componentlist)
    for(cmp in componentlist) {
      xml2::xml_add_child(hd, "out", id=cmp, rule=paste0(simcomponentid, ".",cmp),datatype="INT", mode="SUM")

    }

  }
  xml2::xml_add_child(hd, "out", id=paste0("iHeader|1-",nr), rule=paste0(simcomponentid, ".iHeader"),datatype="CHARARRAY")
  xml2::xml_add_child(hd, "out", id=paste0("iValues|1-",nr), rule=paste0(simcomponentid, ".iValues"), datatype="INTARRAY",mode="SUM")

  desc <- xml2::xml_find_first(x,"/solution/description")
  xml2::xml_text(desc) <- paste(xml2::xml_text(desc),"\n","* added timing elements",outputid)

  x
}
