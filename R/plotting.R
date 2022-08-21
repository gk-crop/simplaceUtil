#' Subsets data for plotting
#' 
#' @param data data from memory output or file
#' @param simulationid get data only for simulationid
#' @param date_from simulation date from where values are taken
#' @param date_to simulateon date until values are taken
#' @param datecol column name for date (default CURRENT.DATA)
#' @keywords internal
subsetDataForPlot <-function(data, simulationid=NULL, date_from=NULL, 
                             date_to=NULL, datecol="CURRENT.DATE")
{
  if(!is.null(simulationid))
  {
    data <- data[data$simulationid==simulationid,]
  }
  if(!is.null(date_from))
  {
    data <- data[date_from <= data[[datecol]],]
  }
  if(!is.null(date_to))
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
#' @param simulationid plot only data for simulationid
#' @param date_from simulation date from where values are plotted
#' @param date_to simulateon date until values are plotted
#' @param datecol column name for date (default CURRENT.DATA)
#' @export 
#' @importFrom rlang .data
plotScalarOutput <- function (data, column_x, columns_y, 
                              simulationid=NULL, date_from=NULL, date_to=NULL,
                              datecol="CURRENT.DATE") 
{
  cols <- unique(c("simulationid",datecol, column_x, columns_y))
  data <- subset(data,TRUE, cols)
  data <- subsetDataForPlot(data, simulationid, date_from, date_to, datecol)

  if(nrow(data)>0)
  {
    if(length(columns_y)==1)
    {
      if(mode(data[,column_x])!="numeric")
      {
        data[,column_x] <- as.factor(data[,column_x])
      }
      if(mode(data[,columns_y])!="numeric")
      {
        data[,columns_y] <- as.factor(data[,columns_y])
      }        
      plot(data[,column_x],data[,columns_y],xlab=column_x, ylab=columns_y, pch=20)
    }
    else if(mode(data[,column_x])=="numeric")
    {
      nc <- sapply(columns_y,\(n)mode(data[,n])=='numeric')
      cols <- setdiff(columns_y[nc],c(datecol,column_x))
      if(length(cols)>0)
      {
        # opar <- par(no.readonly = TRUE)
        # par(mar = c(5, 5, 4, 10))
        # 
        # matplot(data[,column_x],
        #         data[,cols],type="l",lty=1,col = 1:length(cols),
        #         ylab="Values",
        #         xlab=column_x)
        # legend("topright", inset=c(-.15,0),legend=cols,col=1:length(cols),lty=1,xpd=TRUE)
        # on.exit(par(opar))
        dt <- tidyr::pivot_longer(data,cols,names_to="Variable", 
                                  values_to="Value")
        ggplot2::ggplot(dt, ggplot2::aes(x=.data[[column_x]],
                                         y=.data$Value,colour=.data$Variable)) +
          ggplot2::geom_line()
      }
    }
    
  }
}

#' Plots layered output
#' 
#' @param data data from memory output or file
#' @param column column name used for the fill color
#' @param simulationid plot only data for simulationid
#' @param date_from simulation date from where values are plotted
#' @param date_to simulation date until values are plotted
#' @param date_to simulateon date until values are plotted
#' @param datecol column name for date (default CURRENT.DATA)
#' @export 
#' @importFrom rlang .data
plotLayeredOutput <- function(data, column, simulationid = NULL,  
                              date_from=NULL, date_to=NULL, 
                              datecol="CURRENT.DATE") 
{
  
  data <- subsetDataForPlot(data, simulationid, date_from, date_to, datecol)
  if(nrow(data)>0 && !is.null(column) && !is.na(column))
  {
    # lnames <- names(data)
    # lcols <- substr(lnames,1,nchar(column))
    # lnames <- rev(lnames[lcols==column])
    # 
    # ldata <- data[,lnames]
    # 
    # mat <- data.matrix(ldata,rownames.force = NA)
    # 
    # filled.contour(
    #   x = data[[datecol]],
    #   y = -(ncol(ldata):1),
    #   z=mat                          
    #   main = column,
    #   xlab = "Date",
    #   ylab = "Layer")
    dt <- transformLayeredData(data)
    if(nrow(dt)>0 && column %in% names(dt))
    {
      ggplot2::ggplot(dt,
                    ggplot2::aes(x=.data[[datecol]],
                                 y=-.data$layer,
                                 fill=.data[[column]])) +
        ggplot2::ylab("Layer") +
        ggplot2::geom_raster() +
        ggplot2::scale_fill_gradient(low="#ddeeff",high="#000055")
    }
    
  }
  
}