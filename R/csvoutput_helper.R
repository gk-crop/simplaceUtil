#' Transform layered output data in long format
#'
#' @param data dataframe
#' @param sep character that separates layer number from variable name (default "_")
#' @return dataframe in long format
#' @export
transformLayeredData <- function(data, sep="_")
{
  if(sep %in% c(".","|","+","-","*","(",")","[","]","$","^",",",":","?")) {
    sep = paste0("\\",sep)
  }
  pattern <- paste0("(.+)",sep,"([0-9]+)$")
  if(sep!="" && any(grepl(pattern,names(data))))
  {
    tidyr::pivot_longer(
      data,
      dplyr::matches(pattern),
      names_to = c(".value","layer"),
      names_pattern =pattern,
      names_transform = list(layer=as.integer)
    )
  }
  else
  {
    data
  }
}

#' Converts date (class `character`) column from output file to column of class `Date`
#'
#' @param data data.frame
#' @param newName name of the transformed date column
#' @param format date format
#' @param oldName name of the column that holds the date to be transformed
#' @return data.frame with date column of class `Date`
#' @export
parseDate <- function(data, newName="CURRENT.DATE", format="%d.%m.%Y", oldName ="CURRENT.DATE")
{
  data[[newName]] <- as.Date(data[[oldName]],format=format)
  data
}

