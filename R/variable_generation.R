get_variable_prefix <- function(l) {
  switch(l[["contenttype"]],
         constant="c",
         output="",
         rate="r",
         state="s",
         input="i")
}


get_variable_contenttype <- function(l) {
  switch(l[["contenttype"]],
         constant="constant",
         output="out",
         out="out",
         rate="rate",
         state="state",
         input="input")
}

get_variable_datatype <- function(l) {
  switch(l[["datatype"]],
         "Double"="DOUBLE",
         "DOUBLE"="DOUBLE",
         "Double[]"="DOUBLEARRAY",
         "DOUBLEARRAY"="DOUBLEARRAY",
         "Integer"="INT",
         "INT"="INT",
         "Integer[]"="INTARRAY",
         "INTARRAY" = "INTARRAY",
         "Boolean"="BOOLEAN",
         "BOOLEAN" = "BOOLEAN",
         "Boolean[]"="BOOLEANARRAY",
         "BOOLEANARRAY" = "BOOLEANARRAY",
         "String"="CHAR",
         "CHAR"="CHAR",
         "String[]"="CHARARRAY",
         "CHARARRAY"="CHARARRAY",
         "")
}

get_variable_value <- function(l,pos,default=TRUE) {
  if(is.na(l[[pos]]) || l[[pos]]=='')
  {"null"}
  else
  {
    if(get_variable_datatype(l)=="DOUBLEARRAY")
    {
      n <- strsplit(l[[pos]],",",TRUE)[[1]]
      n <- stats::na.omit(as.numeric(n))
      if(length(n)>0)
      {
        if(default) {
          n <- paste(paste0(n,"d"),collapse=",")
          paste0("new Double[]{",n,"}")
        }
        else {
         paste0(n[1],"d")
        }
      }
      else {
        "null"
      }
    }
    else if(get_variable_datatype(l)=="INTARRAY")
    {
      n <- strsplit(l[[pos]],",",TRUE)[[1]]
      n <- stats::na.omit(as.integer(n))
      if(length(n)>0)
      {
        if(default) {
          n <- paste(n,collapse=",")
          paste0("new Integer[]{",n,"}")
        }
        else {
          n[1]
        }
      }
      else {
        "null"
      }

    }
    else if(get_variable_datatype(l)=="CHARARRAY")
    {
      n <- as.character(strsplit(l[[pos]],",",TRUE)[[1]])
      if(length(n)>0)
      {
        if(default) {
          n <- paste(paste0('"',n,'"'),collapse=",")
          paste0("new String[]{",n,"}")
        }
        else {
          paste0('"',n[1],'"')
        }
      }
      else {
        "null"
      }
    }
    else if(get_variable_datatype(l)=="DOUBLE")
    {
      paste0(l[[pos]],"d")
    }
    else if(l[["datatype"]]=="CHAR")
    {
      paste0('"',l[[pos]],'"')
    }
    else
    {
      l[[pos]]
    }
  }
}

value_to_xml <- function(l,pos) {
  if(is.na(l[[pos]]) || l[[pos]]=='')
  {""}
  else
  {
    if(l[["datatype"]]%in% c("Double[]","DOUBLEARRAY","Integer[]","INTARRAY","String[]","CHARRARAY"))
    {
      vals <- strsplit(l[[pos]],',')
      vals <- vals[[1]];
      Reduce(paste0,
             sapply(vals,
                    function(v) paste0('    <value>',v,'</value>\n  ')),'\n  ')
    }
    else
    {
      l[[pos]]
    }
  }
}

create_variable_java <- function(l)
{
  paste0("addVariable(FWSimVariable.createSimVariable(\"",
         get_variable_prefix(l),
         l[["id"]],"\", \"",
         l[["description"]],"\", DATA_TYPE.",
         get_variable_datatype(l), ", CONTENT_TYPE.",
         get_variable_contenttype(l),", \"",l[["unit"]],"\", ",
         get_variable_value(l,"min", FALSE),", ",
         get_variable_value(l,"max", FALSE),", ",
         get_variable_value(l,"default"), ", this));\n")
}

define_fields_java <- function(l) {
  paste0("private FWSimVariable<",l[["datatype"]],"> ",
         get_variable_prefix(l),l[["id"]],";\n")
}


init_variables_java <- function(l)
{

  if(l[["contenttype"]]=="output" ||
     l[["contenttype"]]=="state" ||
     l[["contenttype"]]=="rate")
  {
    paste0(get_variable_prefix(l),l[["id"]],".setDefaultValue();\n")
  }
  else
  {""}

}

parameter_xml <- function(l)
{
  if(l[["contenttype"]]=="constant")
  {
    paste0("  <parameter id=\"",l[["id"]],"\" datatype=\"",
           get_variable_datatype(l),"\" unit=\"",
           l[["unit"]],"\" description=\"",
           l[["description"]],"\">",
           value_to_xml(l,'default'),"</parameter>\n")
  }
  else
  {""}
}


resource_constants_xml <- function(l)
{
  if(l[["contenttype"]]=="constant")
  {
    paste0(" <res id=\"",l[["id"]],"\" datatype=\"",
           get_variable_datatype(l),
           "\" unit=\"",
           l[["unit"]],
           "\" description=\"",
           l[["description"]],"\" />\n")
  }
  else
  {""}
}

resources_inputs_xml <- function(l)
{
  if(l[["contenttype"]]=="input")
  {
    paste0(" <res id=\"",
           l[["id"]],"\" datatype=\"",
           get_variable_datatype(l),"\" unit=\"",
           l[["unit"]],"\" description=\"",
           l[["description"]],"\" />\n")
  }
  else
  {""}
}

inputs_xml <- function(l, component)
{
  parametername <- paste0(tolower(component),"_parameters")
  inputname <- paste0(tolower(component),"_inputs")

  if(l[["contenttype"]]=="constant")
  {
    paste0(" <input id=\"",get_variable_prefix(l),
           l[["id"]],"\" source=\"",
           parametername,
           ".",l["id"],"\" />\n")
  }
  else if(l[["contenttype"]]=="input")
  {
    paste0(" <input id=\"",
           get_variable_prefix(l),l[["id"]],"\" source=\"",
           inputname,
           ".",l[["id"]],"\" />\n")
  }

  else
  {""}
}

explicitinputs_xml <- function(l, component)
{
  if(l[["contenttype"]]=="constant")
  {
    paste0(" <input id=\"",
           get_variable_prefix(l),l[["id"]],"\">",
           l[["default"]],"</input>\n")
  }
  else if(l[["contenttype"]]=="input")
  {
    inputname <- paste0(tolower(component),"_inputs")
    paste0(" <input id=\"",
           get_variable_prefix(l),l[["id"]],"\" source=\"",
           inputname,".",l[["id"]],"\" />\n")
  }

  else
  {""}
}


outputs_xml <- function(l, component)
{
  if(l[["contenttype"]]=="rate" ||
     l[["contenttype"]]=="state" ||
     l[["contenttype"]]=="output")
  {
    paste0(" <out id=\"",l[["id"]],"\" datatype=\"",
           get_variable_datatype(l),"\" rule=\"",
           component,".",
           get_variable_prefix(l),l[["id"]],"\" />\n")
  }
  else
  {""}
}


#' Create java and xml code stubs from a csv file
#'
#' The function creates Java code stubs for defining the fields, adding
#' variables and initialising values.
#' It creates XML subs for parameter file, as well as resource definition for
#' parameter and input data, inputs for simcomponent and outputs.
#'
#' The dataframe / CSV file should have columns
#' * contenttype (one of: constant, input, state, rate, out)
#' * id
#' * description
#' * datatype (Simplace Datatype or corresponding Java Datatype)
#' * unit
#' * min
#' * max
#' * default
#'
#' @param variables  dataframe or path to csv file with variables
#' @param component name of SimComonent
#' @param outfolder optional, if given code is written the folder's files
#' @param ... parameters passed to read.table (e.g. sep, dec etc.)
#'
#' @return list of code snippets
#' @export
#'

createCodeStubsForSimVariables <- function(variables,
                      component="MyComponent" , outfolder=NULL, ...) {

  if(is.data.frame(variables)) {
    df <- variables
  }
  else if(is.character(variables) && file.exists(variables))
  {
    df <-utils::read.table(variables,header=TRUE,as.is=TRUE, ...)
  }
  else {
    return(NULL)
  }

  df[df$contenttype=="","contenttype"]<-"output"
  df <- df[order(df$contenttype),]

  vrs<-apply(df,1,create_variable_java)
  fls<-apply(df,1,define_fields_java)
  ini<-apply(df,1,init_variables_java)

  prms <- apply(df,1,parameter_xml)
  res <- apply(df,1,resource_constants_xml)
  res_inputs <- apply(df,1,resources_inputs_xml)
  inpt <- apply(df,1,inputs_xml, component=component)
  outp <- apply(df,1,outputs_xml, component=component)

  if(!is.null(outfolder))
  {
    if(!dir.exists(outfolder)) dir.create(outfolder)

    d <- paste0(outfolder,"/",component)

    cat(fls, file=paste0(d,"_fields.java"),sep="",fill=FALSE)
    cat(vrs,file=paste0(d,"_createvariables.java"),sep="",fill=FALSE)
    cat(ini,file=paste0(d,"_init.java"),sep="",fill=FALSE)
    cat(prms,file=paste0(d,"_parameters.xml"),sep="",fill=FALSE)
    cat(res, file=paste0(d,".resources.sol.xml"),sep="",fill=FALSE)
    cat(res_inputs, file=paste0(d,".resources_inputs.sol.xml"),sep="",fill=FALSE)
    cat(inpt, file=paste0(d,".inputs.sol.xml"),sep="",fill=FALSE)
    cat(outp,file=paste0(d,".outputs.sol.xml"),sep="",fill=FALSE)
  }


  invisible(list(JavaFields=fls,
                 JavaVariables=vrs,
                 JavaInit=ini,
                 ParametersXML = prms,
                 ResourceParametersXML = res,
                 ResourceInputsXML = res_inputs,
                 ComponentInputsXML=inpt,
                 OutputsXML=outp))

}
