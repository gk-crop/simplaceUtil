#' @importFrom shinyFiles getVolumes
#' @importFrom DT coerceValue
#' @importFrom simplace findFirstSimplaceInstallation
.mydummy <- function () {
  coerceValue(getVolumes()(),findFirstSimplaceInstallation())
}

.onLoad <- function(libname, pkgname) {
  utils::data("unitsymbols", package=pkgname, envir=parent.env(environment()))
}
