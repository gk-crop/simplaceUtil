#' Determines the version subdirectory for the documentation
#'
#' @param version Use `current`, `trunk` or numeric version `#.#`
#' @return string with the subdirectory name
#' @keywords internal
#' @noRd
versionDir <- function(version="current") {
  vs <- ""
  suppressWarnings(vn <- as.numeric(version))
  if(version=="trunk") {
    vs <- "0.0/"
  }
  else if(!is.na(vn) && vn >= 5.0) {
    vs <- paste0(version,"/")
  }
  vs
}




#' Fetches the SimVariables table for a SimComponent from Simplace Website
#'
#' @param class class name of the SimComponent
#' @param version Use `current`, `trunk` or numeric version `#.#`
#' @return a data.frame with the SimVariables
#' @export
fetchSimVariablesFromWebsite <- function(class, version="current") {
  tryCatch({
    vs <- versionDir(version)
    url <- paste0("https://simplace.net/doc/",vs,"simplace_modules/",gsub(".","/",class,fixed=TRUE),".html")
    h <- xml2::read_html(url)
    vars <- xml2::xml_find_all(h,"//table[contains(@class,'simvariable')]/tbody/tr")

    if(length(vars)>0)
    {
      res <- do.call(rbind,
                     lapply(vars,
                            \(v) sapply(xml2::xml_find_all(v,".//td"), \(x) xml2::xml_text(x)))) |>
        as.data.frame()
      if(ncol(res)!=8){print(class)}
      names(res) <- c("contenttype", "id", "description", "datatype", "unit", "min", "max", "default")
      res$component <- class
      res$componentname <- sapply(strsplit(res$component,'\\.'), \(x) x[length(x)])

      res$min <- ifelse(!(res$datatype %in% c("CHAR","CHARARRAY")),
                            gsub("-","",trimws(res$min), fixed = TRUE),
                            trimws(res$min))
      res$max <- ifelse(!(res$datatype %in% c("CHAR","CHARARRAY")),
                            gsub("-","",trimws(res$max), fixed = TRUE),
                            trimws(res$max))
      res$default <- ifelse(!(res$datatype %in% c("CHAR","CHARARRAY")),
                            gsub("-","",trimws(res$default), fixed = TRUE),
                            trimws(res$default))

      res$default <- ifelse(res$datatype %in% c("INTARRAY","DOUBLEARRAY"),
                            gsub(" ",",",res$default, fixed = TRUE),
                            res$default)

      res
    }
    },
    error = function(e){message("Could not fetch data from website. There might be a problem with the internet connection or with the website itself.")})

}

#' Fetches the Description (as HTML source code) of a SimComponent from Simplace Website
#'
#' @param class class name of the SimComponent
#' @param version Use `current`, `trunk` or numeric version `#.#`
#' @return  string with the description text (including HTML tags)
#' @export
fetchDescriptionFromWebsite <- function(class, version="current") {
  tryCatch({
    vs <- versionDir(version)

    url <- paste0("https://simplace.net/doc/",vs,"simplace_modules/",gsub(".","/",class,fixed=TRUE),".html")
    h <- xml2::read_html(url)
    desc <- xml2::xml_find_all(h,"//section[@id='class-description']//div[contains(@class,'block')]") |> as.character()
    desc
    },
    error = function(e){message("Could not fetch data from website. There might be a problem with the internet connection or with the website itself.")})


}

#' Fetches the list of SimComponents from the Simplace Website
#'
#' @param version Use `current`, `trunk` or numeric version `#.#`
#' @return character vector with the class names of all SimComponents
#' @export
fetchSimComponentlistFromWebsite <- function(version = "current") {
  tryCatch({
    vs <- versionDir(version)
    h <- xml2::read_html(paste0("https://simplace.net/doc/",vs,"simplace_modules/"))
    xml2::xml_find_all(h, "//div[contains(@class,'summary-table')]//div[contains(@class,'col-first')]//a") |>
      xml2::xml_text()
    }  ,
    error = function(e){message("Could not fetch data from website. There might be a problem with the internet connection or with the website itself.")})

}
