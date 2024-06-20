#' Subsets data for plotting
#'
#' @param data data from memory output or file
#' @param simulationid get data only for simulationid
#' @param date_from simulation date from where values are taken
#' @param date_to simulateon date until values are taken
#' @param datecol column name for date  (default CURRENT.DATE)
#' @return data.frame that contains only values needed for plotting
#' @keywords internal
#' @noRd
subsetDataForPlot <-function(data, simulationid=NULL, date_from=NULL,
                             date_to=NULL, datecol="CURRENT.DATE")
{
  if(!is.null(simulationid) && length(simulationid)>0)
  {
    data <- data[data$simulationid %in% simulationid,]
  }
  if(!is.null(date_from) && length(date_from)==1)
  {
    data <- data[date_from <= data[[datecol]],]
  }
  if(!is.null(date_to) && length(date_to)==1)
  {
    data <- data[date_to >= data[[datecol]],]
  }
  data
}

#' Plots scalar output
#'
#' @param data data from memory output or file
#' @param column_x column name for x values
#' @param columns_y column name(s) for y values (vector of names)
#' @param simulationid plot only data for simulationids
#' @param date_from simulation date from where values are plotted
#' @param date_to simulateon date until values are plotted
#' @param datecol column name for date (default CURRENT.DATE)
#' @param nrow number of panel rows when plotting multiple simulation ids
#' @param ncol number of panels in a row when plotting multiple simulation ids
#' @export
#' @importFrom rlang .data
plotScalarOutput <- function (data, column_x, columns_y,
                              simulationid=NULL, date_from=NULL, date_to=NULL,
                              datecol="CURRENT.DATE",
                              nrow = NULL,
                              ncol = NULL)
{
  columns_y <- intersect(columns_y, names(data))
  cols <- unique(c("simulationid",datecol, column_x, columns_y))
  if(length(cols)>1)
  {
      data <- subset(data,TRUE, cols)
  }
  data <- subsetDataForPlot(data, simulationid, date_from, date_to, datecol)
  if(nrow(data)>0 && length(column_x)==1 && length(columns_y)>0)
  {
    if(length(columns_y)==1 && (mode(data[[column_x]])!="numeric" ||
                                mode(data[[columns_y]])!="numeric"))
    {
      if(mode(data[[column_x]])!="numeric")
      {
        data[[column_x]] <- as.factor(data[[column_x]])
      }
      if(mode(data[[columns_y]])!="numeric")
      {
        data[[columns_y]] <- as.factor(data[[columns_y]])
      }
      plot(data[[column_x]],data[[columns_y]],xlab=column_x, ylab=columns_y, pch=20)
    }
    else if(mode(data[[column_x]])=="numeric")
    {
      nc <- sapply(columns_y,\(n)mode(data[[n]])=='numeric')
      if(any(columns_y[nc]!=datecol))
      {
        cols <- setdiff(columns_y[nc],c(datecol,column_x))
        if(length(cols)>0)
        {
          dt <- tidyr::pivot_longer(data,cols,names_to="Variable",
                                    values_to="Value")
          ggplot2::ggplot(dt, ggplot2::aes(x=.data[[column_x]],
                                           y=.data$Value,colour=.data$Variable)) +
            ggplot2::geom_line() +
            ggplot2::facet_wrap(~.data$simulationid, nrow=nrow, ncol=ncol)
        }
      }
      else
      {
        ggplot2::ggplot(data, ggplot2::aes(x=.data[[column_x]],
                                         y=.data[[datecol]])) +
          ggplot2::geom_line() +
          ggplot2::facet_wrap(~.data$simulationid, nrow=nrow, ncol=ncol)
      }
    }
  }
}

#' Plots layered output
#'
#' @param data data from memory output or file
#' @param column column name used for the fill color
#' @param simulationid plot only data for simulationids
#' @param date_from simulation date from where values are plotted
#' @param date_to simulation date until values are plotted
#' @param date_to simulateon date until values are plotted
#' @param datecol column name for date (default CURRENT.DATE)
#' @param nrow number of panel rows when plotting multiple simulation ids
#' @param ncol number of panels in a row when plotting multiple simulation ids
#' @export
#' @importFrom rlang .data
plotLayeredOutput <- function(data, column, simulationid = NULL,
                              date_from=NULL, date_to=NULL,
                              datecol="CURRENT.DATE",
                              nrow=NULL,
                              ncol=NULL)
{

  data <- subsetDataForPlot(data, simulationid, date_from, date_to, datecol)
  if(nrow(data)>0 && !is.null(column) && !is.na(column))
  {
    dt <- transformLayeredData(data)
    if(nrow(dt)>0 && column %in% names(dt) && "layer" %in% names(dt))
    {
      ggplot2::ggplot(dt,
                    ggplot2::aes(x=.data[[datecol]],
                                 y=-.data$layer,
                                 fill=.data[[column]])) +
        ggplot2::ylab("Layer") +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_gradient(low="#ddeeff",high="#000055") +
        ggplot2::facet_wrap(~.data$simulationid, nrow=nrow, ncol=ncol)
    }

  }

}
