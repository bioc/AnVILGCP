#' @name gcp-methods
#'
#' @title A number of methods compatible with the GCP platform class.
#'
#' @description `avcopy()`: copy contents of `source` to `destination`. At
#'   least one of `source` or `destination` must be Google cloud bucket;
#'   `source` can be a character vector with length greater than 1. Use
#'   `gsutil_help("cp")` for `gsutil` help.
#'
#'   `avlist()`: List contents of a google cloud bucket or, if `source` is
#'   missing, all Cloud Storage buckets under your default project ID
#'
#'   `avremove()`: remove contents of a Google Cloud Bucket.
#'
#'   `avbackup()`,`avrestore()`: synchronize a source and a destination. If the
#'   destination is on the local file system, it must be a directory or not yet
#'   exist (in which case a directory will be created).
#'
#'   `avstorage()` returns the workspace bucket, i.e., the google bucket
#'   associated with a workspace. Bucket content can be visualized under the
#'   'DATA' tab, 'Files' item.
#'
#' @param source `character(1)`, (`character()` for `avlist()`, `avcopy()`)
#'   paths to a google storage bucket, possibly with wild-cards for file-level
#'   pattern matching.
#'
#' @param destination `character(1)`, google cloud bucket or local file system
#'   destination path.
#'
#' @param recursive `logical(1)`; perform operation recursively from `source`?.
#'   Default: `FALSE`.
#'
#' @param parallel `logical(1)`, perform parallel multi-threaded /
#'   multi-processing (default is `TRUE`).
#'
#' @param platform `character(1)`; the cloud platform as given by
#'   `AnVILBase::cloud_platform()`. For GCP on Terra, this is 'gcp'.
#'
#' @param force `logical(1)`: continue silently despite errors when
#'     removing multiple objects. Default: `FALSE`.
#'
#' @param exclude `character(1)` a python regular expression of bucket
#'     paths to exclude from synchronization. E.g.,
#'     `'.*(\\.png|\\.txt)$"` excludes '.png' and .txt' files.
#'
#' @param dry `logical(1)`, when `TRUE` (default), return the
#'     consequences of the operation without actually performing the
#'     operation.
#'
#' @param delete `logical(1)`, when `TRUE`, remove files in
#'     `destination` that are not in `source`. Exercise caution when
#'     you use this option: it's possible to delete large amounts of
#'     data accidentally if, for example, you erroneously reverse
#'     source and destination.
#'
#' @param ... additional arguments passed as-is to the `gsutil` subcommand.
#'
#' @return `avcopy()`: exit status of `avcopy()`, invisibly.
#'   `avlist()`: `character()` listing of `source` content.
#'   `avremove()`: exit status of `gsutil rm`, invisibly.
#'   `avbackup()`: exit status of `gsutil rsync`, invisbly.
#'   `avrestore()`: exit status of `gsutil rsync`, invisbly.
#'   `avstorage()` returns a `character(1)` bucket identifier prefixed with
#'     `gs://`
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @examples
#' if (gcloud_exists()) {
#'    avcopy(src, tempdir())
#'    ## gsutil_*() commands work with spaces in the source or destination
#'    destination <- file.path(tempdir(), "foo bar")
#'    avcopy(src, destination)
#'    file.exists(destination)
#' }
#' if (gcloud_exists() && nzchar(avworkspace_name()))
#'     ## From within AnVIL...
#'     bucket <- avstorage()                        # discover bucket
#'
#' \dontrun{
#'     path <- file.path(bucket, "mtcars.tab")
#'     avlist(dirname(path))                    # no 'mtcars.tab'...
#'     write.table(mtcars, gsutil_pipe(path, "w")) # write to bucket
#'     gsutil_stat(path)                           # yep, there!
#'     read.table(gsutil_pipe(path, "r"))          # read from bucket
#' }
NULL

# avcopy ------------------------------------------------------------------

#' @describeIn gcp-methods copy contents of `source` to `destination` with
#'   `gsutil`
#'
#' @importFrom AnVILBase avcopy
#' @exportMethod avcopy
setMethod(
    f = "avcopy",
    signature = "gcp",
    definition =  function(
        source, destination,
        ...,
        recursive = FALSE, parallel = TRUE, platform = cloud_platform()
    ) {
        location <- c(source, destination)
        location_is_uri <- .gsutil_is_uri(location)
        stopifnot(
            isCharacter(source), isScalarCharacter(destination),
            any(location_is_uri),
            isScalarLogical(recursive), isScalarLogical(parallel)
        )

        args <- c(
            .gsutil_requesterpays_flag(location),
            if (parallel) "-m", ## Makes the operations faster
            "cp", ## cp command
            if (recursive) "-r",
            ...,
            .gsutil_sh_quote(source),
            .gsutil_sh_quote(destination)
        )
        result <- .gsutil_do(args)
        .gcloud_sdk_result(result)
    }
)

# avlist ------------------------------------------------------------------

#' @describeIn gcp-methods list contents of `source` with `gsutil`
#'
#' @importFrom AnVILBase avlist
#' @importFrom BiocBaseUtils isScalarLogical
#' @exportMethod avlist
setMethod(
    f = "avlist",
    signature = "gcp",
    definition = function(
        source = character(),
        recursive = FALSE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            .gsutil_is_uri(source),
            isScalarLogical(recursive)
        )

        args <- c(
            .gsutil_requesterpays_flag(source),
            "ls",
            if (recursive) "-r",
            ...,
            shQuote(source)
        )
        result <- .gsutil_do(args)
        result[nzchar(result) & !endsWith(result, ":")]
    }
)

# avremove ----------------------------------------------------------------

#' @describeIn gcp-methods remove contents of `source` with `gsutil`
#'
#' @importFrom AnVILBase avremove
#' @exportMethod avremove
setMethod(
    f = "avremove",
    signature = "gcp",
    definition = function(
        source,
        recursive = FALSE,
        force = FALSE,
        parallel = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            .gsutil_is_uri(source),
            isScalarLogical(force),
            isScalarLogical(recursive),
            isScalarLogical(parallel)
        )

        ## remove
        args <- c(
            .gsutil_requesterpays_flag(source),
            if (parallel) "-m",
            "rm",
            if (force) "-f",
            if (recursive) "-r",
            ...,
            shQuote(source)
        )
        result <- .gsutil_do(args)
        .gcloud_sdk_result(result)
    }
)

# avbackup ----------------------------------------------------------------

#' @describeIn gcp-methods backup contents of `source` with `gsutil`
#'
#' @details
#' `avbackup()': To make `"gs://mybucket/data"` match the contents
#' of the local directory `"data"` you could do:
#'
#' \code{avbackup("data", "gs://mybucket/data", delete = TRUE)}
#'
#' To make the local directory "data" the same as the contents of
#' gs://mybucket/data:
#'
#' \code{avrestore("gs://mybucket/data", "data", delete = TRUE)}
#'
#' If `destination` is a local path and does not exist, it will be
#' created.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @importFrom AnVILBase avbackup
#'
#' @exportMethod avbackup
setMethod(
    f = "avbackup",
    signature = "gcp",
    definition = function(
        source, destination,
        recursive = FALSE,
        exclude = NULL,
        dry = TRUE,
        delete = FALSE,
        parallel = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        if (!.gsutil_is_uri(destination))
            stop("'destination' must be a gs:// URI")

        gsutil_rsync(
            source = source, destination = destination,
            ...,
            exclude = exclude, dry = dry, delete = delete,
            recursive = recursive, parallel = parallel
        )
    }
)

# avrestore ---------------------------------------------------------------

#' @describeIn gcp-methods restore contents of `source` with `gsutil`
#'
#' @importFrom AnVILBase avrestore
#' @exportMethod avrestore
setMethod(
    f = "avrestore",
    signature = "gcp",
    definition = function(
        source, destination,
        recursive = FALSE,
        exclude = NULL,
        dry = TRUE,
        delete = FALSE,
        parallel = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        if (!.gsutil_is_uri(source))
            stop("'source' must be a gs:// URI")

        gsutil_rsync(
            source = source, destination = destination,
            ...,
            exclude = exclude, dry = dry, delete = delete,
            recursive = recursive, parallel = parallel
        )
    }
)

# avstorage ---------------------------------------------------------------

#' @describeIn gcp-methods get the storage bucket location
#'
#' @param namespace `character(1)` AnVIL workspace namespace as returned
#'     by, e.g., `avworkspace_namespace()`
#'
#' @param name `character(1)` AnVIL workspace name as returned by, e.g.,
#'     `avworkspace_name()`.
#'
#' @importFrom AnVILBase avstorage
#' @exportMethod avstorage
setMethod(
    f = "avstorage",
    signature = "gcp",
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            isScalarCharacter(namespace),
            isScalarCharacter(name)
        )

        if (.avbucket_cache$exists(namespace, name)) {
            bucket <- .avbucket_cache$get(namespace, name)
        } else {
            response <- Terra()$getWorkspace(
                namespace, URLencode(name), "workspace.bucketName"
            )
            avstop_for_status(response, "avbucket")
            bucket <- as.list(response)$workspace$bucketName
            .avbucket_cache$set(namespace, name, bucket)
        }

        paste0("gs://", bucket)
    }
)

# avworkspaces ------------------------------------------------------------

#' @describeIn gcp-methods list workspaces in the current project
#' @importFrom AnVILBase avworkspaces
#' @exportMethod avworkspaces
setMethod(
    f = "avworkspaces",
    signature = "gcp",
    definition = function(
        ...,
        platform = cloud_platform()
    ) {
        response <- Rawls()$listWorkspaces()
        avstop_for_status(response, "avworkspaces")

        AnVIL::flatten(response) |>
            .avworkspaces_clean()
    }
)
