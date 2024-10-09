#' @name has_avworkspace-methods
#'
#' @title Helper to check AnVIL environment is set up to work with GCP
#'
#' @description `has_avworkspace()` checks that the AnVIL environment is set up
#'   to work with GCP.  If `strict = TRUE`, it also checks that the workspace
#'   name is set.
#'
#' @inheritParams AnVILBase::has_avworkspace
#'
#' @return `logical(1)` `TRUE` if the AnVIL environment is set up properly to
#'   interact with GCP, otherwise `FALSE`.
#'
#' @examples
#' has_avworkspace(platform = gcp())
#'
NULL

#' @describeIn has_avworkspace-methods Check if the AnVIL environment is set up
#'
#' @importFrom AnVILBase has_avworkspace
#'
#' @exportMethod has_avworkspace
setMethod("has_avworkspace", signature = c(platform = "gcp"), definition =
    function(strict = FALSE, ..., platform = cloud_platform()) {
        gcloud_exists() &&
            identical(AnVILBase::avplatform_namespace(), "AnVILGCP") &&
            (!strict || nzchar(avworkspace_name()))
    }
)
