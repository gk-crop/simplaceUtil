#' Transform layered output data in long format
#'
#' @param data dataframe
#' @export
transformLayeredData <- function(data)
{
  tidyr::pivot_longer(
    data,
    dplyr::matches("[a-zA-Z]+_[0-9]+"),
    names_to = c(".value","layer"),
    names_pattern ="([a-zA-Z]+)_([0-9]+)",
    names_transform = list(layer=as.integer)
  )
}

#' Converts date from output file to date object
#'
#' @param data dataframe
#' @param newName name of the transformed date column
#' @param format date format
#' @param oldName name of the column that holds the date to be transformed
#' @export
parseDate <- function(data, newName="Date", format="%d.%m.%Y", oldName ="CURRENT.DATE")
{
  data[[newName]] <- as.Date(data[[oldName]],format=format)
  data
}
