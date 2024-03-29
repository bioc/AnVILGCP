.avnotebooks_runtime_path <-
    function(name)
{
    path.expand(file.path("~", name, "edit"))
}

.avnotebooks_workspace_path <-
    function(namespace, name)
{
    paste(avstorage(namespace, name), "notebooks", sep = "/")
}

#' @name avnotebooks-methods
#'
#' @title Notebook management
#'
#' @aliases avnotebooks avnotebooks_localize avnotebooks_delocalize
#'
#' @description `avnotebooks()` returns the names of the notebooks
#'     associated with the current workspace.
#'
#' @inheritParams avdata
#'
#' @param local = `logical(1)` notebooks located on the workspace
#'     (`local = FALSE`, default) or runtime / local instance (`local
#'     = TRUE`). When `local = TRUE`, the notebook path is
#'     `<avworkspace_name>/notebooks`.
#'
#' @return `avnotebooks()` returns a character vector of buckets /
#'     files located in the workspace 'Files/notebooks' bucket path,
#'     or on the local file system.
#'
#' @examples
#' library(AnVILBase)
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' ) {
#'     avnotebooks()
#'     avnotebooks_localize()  # dry run
#'     try(avnotebooks_delocalize())  # dry run, fails if no local resource
#' }
#' @include gcp-class.R
NULL

# avnotebooks -------------------------------------------------------------

#' @describeIn avnotebooks-methods List notebooks in the workspace
#'
#' @importFrom AnVILBase avnotebooks
#'
#' @exportMethod avnotebooks
setMethod("avnotebooks", signature = c(platform = "gcp"), definition =
    function(
        local = FALSE,
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarLogical(local),
            !local || (isScalarCharacter(namespace) && isScalarCharacter(name))
        )

        if (local) {
            dir(.avnotebooks_runtime_path(name))
        } else {
            basename(avlist(.avnotebooks_workspace_path(namespace, name)))
        }
    }
)

# avnotebooks_localize ----------------------------------------------------

#' @describeIn avnotebooks-methods Synchronizes the content of the workspace
#'   bucket to the local file system.
#'
#' @param destination missing or character(1) file path to the local
#'     file system directory for synchronization. The default location
#'     is `~/<avworkspace_name>/notebooks`. Out-of-date local files
#'     are replaced with the workspace version.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `avnotebooks_localize()` returns the exit status of
#'     `gsutil_rsync()`.
#'
#' @importFrom AnVILBase avnotebooks_localize
#'
#' @exportMethod avnotebooks_localize
setMethod("avnotebooks_localize",
    signature = c(platform = "gcp"),
    definition = function(
        destination,
        namespace = avworkspace_namespace(), name = avworkspace_name(),
        dry = TRUE,
        ...,
        platform = cloud_platform()
    ) {
    ## FIXME: localize to persistent disk independent of current location
    ## .avnotebooks_localize_runtime(source, name, runtime_name, dry)

        stopifnot(
            missing(destination) || isScalarCharacter(destination),
            isScalarCharacter(namespace),
            isScalarCharacter(name),
            isScalarLogical(dry)
        )

        source <- .avnotebooks_workspace_path(namespace, name)
        if (missing(destination)) {
            destination = .avnotebooks_runtime_path(name)
            if (!dry && !dir.exists(destination))
                dir.create(destination, recursive = TRUE)
        }
        localize(source, destination, dry = dry)
    }
)

# avnotebooks_delocalize --------------------------------------------------

#' @describeIn avnotebooks-methods Synchronizes the content of the notebook
#'   location of the local file system to the workspace bucket.
#'
#' @param source missing or character(1) file path to the local file
#'     system directory for synchronization. The default location is
#'     `~/<avworkspace_name>/notebooks`. Out-of-date local files are
#'     replaced with the workspace version.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @return `avnotebooks_delocalize()` returns the exit status of
#'     `gsutil_rsync()`.
#'
#' @importFrom AnVILBase avnotebooks_delocalize
#'
#' @exportMethod avnotebooks_delocalize
setMethod("avnotebooks_delocalize",
    signature = c(platform = "gcp"),
    definition = function(
        source,
        namespace = avworkspace_namespace(), name = avworkspace_name(),
        dry = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            missing(source) || isScalarCharacter(source),
            isScalarCharacter(namespace),
            isScalarCharacter(name),
            isScalarLogical(dry)
        )

        if (missing(source))
            source <- .avnotebooks_runtime_path(name)
        destination <- .avnotebooks_workspace_path(namespace, name)
        delocalize(source, destination, dry = dry)
    }
)
