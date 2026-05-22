#' @docType class
#'
#' @title GCP platform class
#'
#' @description This class is used to represent the GCP platform.
#' @name gcp-class
#'
#' @importClassesFrom AnVILBase Platform
#' @importFrom methods new
#' @importFrom AnVILBase cloud_platform
#'
#' @returns An object of class `gcp`.
#'
#' @examples
#' showClass("gcp")
#' @exportClass gcp
.gcp <- setClass("gcp", contains = "Platform")

#' @rdname gcp-class
#'
#' @export
gcp <- function() {
    .gcp()
}
