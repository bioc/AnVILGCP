#' @rdname gsutil-deprecated
#'
#' @name gsutil-deprecated
#'
#' @title gsutil command line utility interface (DEPRECATED)
#'
#' @description These functions invoke the `gsutil` command line
#'     utility. See the "Details:" section if you have gsutil
#'     installed but the package cannot find it. These functions
#'     have been moved to the `GCPtools` package.
#'
#' @details The `gsutil` system command is required.  The search for
#'     `gsutil` starts with environment variable `GCLOUD_SDK_PATH`
#'     providing a path to a directory containing a `bin` directory
#'     containingin `gsutil`, `gcloud`, etc. The path variable is
#'     searched for first as an `option()` and then system
#'     variable. If no option or global variable is found,
#'     `Sys.which()` is tried. If that fails, `gsutil` is searched for
#'     on defined paths. On Windows, the search tries to find
#'     `Google\\Cloud SDK\\google-cloud-sdk\\bin\\gsutil.cmd` in the
#'     `LOCAL APP DATA`, `Program Files`, and `Program Files (x86)`
#'     directories.  On linux / macOS, the search continues with
#'     `~/google-cloud-sdk`.
#'
#' @examples
#' src <-
#'   "gs://genomics-public-data/1000-genomes/other/sample_info/sample_info.csv"
NULL

#' @rdname gsutil-deprecated
#'
#' @description `gsutil_requesterpays()`: does the google bucket
#'     require that the requester pay for access?
#'
#' @param source `character()` for `gsutil_requesterpays()` and
#'   `gsutil_exists()`: paths to a Google Storage Bucket, possibly with
#'   wild-cards for file-level pattern matching.
#'
#' @return `gsutil_requesterpays()`: named `logical()` vector TRUE
#'     when requester-pays is enabled.
#'
#' @importFrom BiocBaseUtils lifeCycle
#'
#' @examples
#' if (has_avworkspace(platform = gcp()))
#'     GCPtools::gsutil_requesterpays(src) # FALSE -- no cost download
#'
#' @export
gsutil_requesterpays <-
    function(source)
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_requesterpays(source)
}

#' @rdname gsutil-deprecated
#'
#' @description `gsutil_exists()`: check if the bucket or object
#'     exists.
#'
#' @return `gsutil_exists()`: logical(1) TRUE if bucket or object exists.
#'
#' @export
gsutil_exists <-
    function(source)
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_exists(source)
}

#' @rdname gsutil-deprecated
#'
#' @description `gsutil_stat()`: print, as a side effect, the status
#'     of a bucket, directory, or file.
#'
#' @return `gsutil_stat()`: `tibble()` summarizing status of each
#'     bucket member.
#'
#' @examples
#' if (has_avworkspace(platform = gcp())) {
#'     GCPtools::gsutil_exists(src)
#'     GCPtools::gsutil_stat(src)
#'     avlist(dirname(src))
#' }
#'
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @export
gsutil_stat <-
    function(source)
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_stat(source)
}

#' @name gsutil-deprecated
#'
#' @inheritParams gcp-methods
#'
#' @description `gsutil_rsync()`: synchronize a source and a
#'     destination. If the destination is on the local file system, it
#'     must be a directory or not yet exist (in which case a directory
#'     will be created).
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
#' @details
#'
#' `gsutil_rsync()': To make `"gs://mybucket/data"` match the contents
#' of the local directory `"data"` you could do:
#'
#' \code{gsutil_rsync("data", "gs://mybucket/data", delete = TRUE)}
#'
#' To make the local directory "data" the same as the contents of
#' gs://mybucket/data:
#'
#' \code{gsutil_rsync("gs://mybucket/data", "data", delete = TRUE)}
#'
#' If `destination` is a local path and does not exist, it will be
#' created.
#'
#' @return `gsutil_rsync()`: exit status of `gsutil_rsync()`, invisbly.
#'
#' @export
gsutil_rsync <-
    function(source, destination, ..., exclude = NULL, dry = TRUE,
        delete = FALSE, recursive = FALSE, parallel = TRUE)
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_rsync(
        source = source,
        destination = destination,
        ...,
        exclude = exclude,
        dry = dry,
        delete = delete,
        recursive = recursive,
        parallel = parallel
    )
}

#' @rdname gsutil-deprecated
#'
#' @description `gsutil_cat()`: concatenate bucket objects to standard output
#'
#' @param header `logical(1)` when `TRUE` annotate each
#'
#' @param range (optional) `integer(2)` vector used to form a range
#'     from-to of bytes to concatenate. `NA` values signify
#'     concatenation from the start (first position) or to the end
#'     (second position) of the file.
#'
#' @param ... additional arguments passed as-is to the `gsutil` subcommand.
#'
#' @return `gsutil_cat()` returns the content as a character vector.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @export
gsutil_cat <-
    function(source, ..., header = FALSE, range = integer())
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_cat(
        source = source,
        ...,
        header = header,
        range = range
    )
}

#' @rdname gsutil-deprecated
#'
#' @description `gsutil_help()`: print 'man' page for the `gsutil`
#'     command or subcommand. Note that only commandes documented on this
#'     R help page are supported.
#'
#' @param cmd `character()` (optional) command name, e.g.,
#'     `"ls"` for help.
#'
#' @return `gsutil_help()`: `character()` help text for subcommand `cmd`.
#'
#' @importFrom BiocBaseUtils isZeroOneCharacter
#'
#' @examples
#' if (has_avworkspace(platform = gcp()))
#'     GCPtools::gsutil_help("ls")
#'
#' @export
gsutil_help <-
    function(cmd = character(0))
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_help(
        cmd = cmd
    )
}

# higher-level implementations --------------------------------------------

#' @rdname gsutil-deprecated
#'
#' @description `gsutil_pipe()`: create a pipe to read from or write
#'     to a gooogle bucket object.
#'
#' @param open `character(1)` either `"r"` (read) or `"w"` (write)
#'     from the bucket.
#'
#' @return `gsutil_pipe()` an unopened R `pipe()`; the mode is
#'     \emph{not} specified, and the pipe must be used in the
#'     appropriate context (e.g., a pipe created with `open = "r"` for
#'     input as `read.csv()`)
#'
#' @examples
#' if (has_avworkspace(platform = gcp())) {
#'     df <- read.csv(gsutil_pipe(src), 5L)
#'     class(df)
#'     dim(df)
#'     head(df)
#' }
#'
#' @export
gsutil_pipe <-
    function(source, open = "r", ...)
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gsutil"
    )
    GCPtools::gsutil_pipe(
        source = source,
        open = open,
        ...
    )
}
