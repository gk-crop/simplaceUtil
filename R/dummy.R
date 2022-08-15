#' @importFrom shinyFiles getVolumes
#' @importFrom DT coerceValue
#' @importFrom simplace findFirstSimplaceInstallation
.mydummy <- function () {
  coerceValue(getVolumes()(),findFirstSimplaceInstallation())
}