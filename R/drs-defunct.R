#' @name drs-defunct
#'
#' @title Defunct Martha V3 DRS API and related functions
#'
#' @aliases drs_stat drs_access_url
#'
#' @description `drs_stat()`, `drs_access_url()`, and `drs_cp()` are defunct
#'   functions that used the Martha V3 DRS API. They are no longer supported.
#'   Use the `drs_hub()` function instead.
#'
#' @section drs_stat():
#' `drs_stat()` sends requests in parallel to the DRS
#' server, using 8 forked processes (by default) to speed up queries. Use
#' `options(mc.cores = 16L)`, for instance, to set the number of processes to
#' use.
#'
#' `drs_stat()` uses the AnVIL 'pet' account associated with a
#' runtime. The pet account is discovered by default when evaluated on
#' an AnVIL runtime (e.g., in RStudio or a Jupyter notebook in the
#' AnVIL), or can be found in the return value of `avruntimes()`.
#'
#' Errors reported by the DRS service are communicated to the user,
#' but can be cryptic. The DRS service itself for `drs_stat` is called
#' 'martha'. Errors mentioning martha might commonly involve a
#' mal-formed DRS URI. Martha uses a service called 'bond' to
#' establish credentials with registered third party entities such as
#' Kids First. Errors mentioning bond might involve absence of
#' credentials, within Terra, to access the resource; check that, in
#' the Terra / AnVIL graphical user interface, the user profiles
#' 'External Entities' includes the organization to which the DRS uri
#' is being resolved.
#'
#' @param source `character()` DRS URLs (beginning with 'drs://') to resources
#'   managed by the 'martha' DRS resolution server (`drs_stat()`,
#'   `drs_access_url()`, `drs_cp()`)
#'
#' @param region `character(1)` Google cloud 'region' in which the DRS resource
#'   is located. Most data is located in \code{"US"} (the default); in principle
#'   \code{"auto"} allows for discovery of the region, but sometimes fails.
#'   Regions are enumerated at
#'   \url{https://cloud.Google.com/storage/docs/locations#available-locations}.
#'
#' @return `drs_stat()` returns a tbl with the following columns:
#'
#' - `fileName`: `character()` (resolver sometimes returns null).
#' - `size`: integer() (resolver sometimes returns null).
#' - `contentType`: `character()` (resolver sometimes returns null).
#' - `gsUri`: `character()` (resolver sometimes returns null).
#' - `timeCreated`: `character()` (the time created formatted using ISO
#'   8601; resolver sometimes returns null).
#' - `timeUpdated`: `character()` (the time updated formatted using ISO
#'   8601; resolver sometimes returns null).
#' - `bucket`: `character()` (resolver sometimes returns null).
#' - `name`: `character()` (resolver sometimes returns null).
#' - `googleServiceAccount`: list() (null unless the DOS url belongs to
#'   a Bond supported host).
#' - `hashes`: list() (contains the hashes type and their checksum
#'   value; if unknown. it returns null)
#'
#' @examples
#' drs <- c(
#'     vcf = "drs://dg.ANV0/6f633518-f2de-4460-aaa4-a27ee6138ab5",
#'     tbi = "drs://dg.ANV0/4fb9e77f-c92a-4deb-ac90-db007dc633aa"
#' )
#'
#' if (gcloud_exists() && startsWith(gcloud_account(), "pet-")) {
#'     ## from within AnVIL
#'     tbl <- drs_stat(uri)
#'     urls <- drs_access_url(uri)
#'     ## library(VariantAnnotation)
#'     ## vcffile <- VcfFile(urls[["vcf"]], urls[["tbi"]])
#'     ##
#'     ## header <- scanVcfHeader(vcffile)
#'     ## meta(header)[["contig"]]
#' }
#'
#' @importFrom rlang .data
#'
#' @importFrom parallel mclapply
#' @importFrom BiocBaseUtils lifeCycle
#'
#' @export
drs_stat <-
    function(source = character(), region = "US")
{
    lifeCycle(
        "drs_hub()", cycle = "defunct", package = "AnVILGCP", title = "drs"
    )
}

#' @rdname drs-defunct
#'
#' @description `drs_access_url()` returns a vector of 'signed' URLs
#'     that allow access to restricted resources via standard https
#'     protocols.
#'
#' @return `drs_access_url()` returns a vector of https URLs
#'     corresponding to the vector of DRS URIs provided as inputs to
#'     the function.
#'
#' @export
drs_access_url <-
    function(source = character(), region = "US")
{
    lifeCycle(
        newfun = "drs_hub()", cycle = "defunct",
        package = "AnVILGCP", title = "drs"
    )
}

#' @rdname drs-defunct
#'
#' @description `drs_cp()` copies 0 or more DRS URIs to a Google
#'     bucket or local folder
#'
#' @param destination `character(1)`, Google cloud bucket or local
#'     file system destination path.
#'
#' @param ... additional arguments, passed to `avcopy()` for file
#'     copying.
#'
#' @param overwrite logical(1) indicating that source `fileName`s
#'     present in `destination` should downloaded again.
#'
#' @return `drs_cp()` returns a tibble like `drs_stat()`, but with
#'     additional columns
#'
#' - simple: logical() value indicating whether resolution used a
#'   simple signed URL (`TRUE`) or auxilliary service account.
#' - destination: `character()` full path to retrieved object(s)
#'
#' @export
drs_cp <- function(source, destination, ..., overwrite = FALSE)
    UseMethod("drs_cp")

#' @export
drs_cp.drs_stat_tbl <-
    function(source, destination, ..., overwrite = FALSE)
{
    lifeCycle(
        newfun = "drs_hub()", cycle = "defunct",
        package = "AnVILGCP", title = "drs"
    )
}

#' @export
drs_cp.character <-
    function(source, destination, ..., region = "US", overwrite = FALSE)
{
    lifeCycle(
        newfun = "drs_hub()", cycle = "defunct",
        package = "AnVILGCP", title = "drs"
    )
}
