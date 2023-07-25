#' Transform layered output data in long format
#'
#' @param data dataframe
#' @export
transformLayeredData <- function(data)
{
  if(any(grepl("[a-zA-Z]+_[0-9]+",names(data))))
  {
    tidyr::pivot_longer(
      data,
      dplyr::matches("[a-zA-Z]+_[0-9]+"),
      names_to = c(".value","layer"),
      names_pattern ="([a-zA-Z_]+)_([0-9]+)$",
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

