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


transdf <- function(l, n)
{
  # u <- attr(l,"unit")
  if(is.list(l))
  {
    d <- as.data.frame(t(as.data.frame(l)))
    names(d) <- gsub("V",paste0(n,"_"),names(d))
    # for(i in seq_along(d)){attr(d[[i]],"unit")<-u}
  }
  else {
    d <- data.frame(l)
    names(d)<-n
    # attr(d[[1]],"unit")<-u
  }
  d
}

#' converts expanded resultlist to dataframe in wide format
#' @param reslist result list from simplace simulation
#' @return data.frame with layered values (wide format)
#' @export
resultToDataframeExpanded <- function(reslist) {
  df<-do.call(cbind,lapply(names(reslist),\(n)transdf(reslist[[n]], n)))
  rownames(df)<-NULL
  df
}
