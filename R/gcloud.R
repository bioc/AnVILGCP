#' @rdname gcloud-deprecated
#'
#' @name gcloud-deprecated
#'
#' @title gcloud command line utility interface (DEPRECATED)
#'
#' @description These functions invoke the `gcloud` command line utility. See
#'   \link{gsutil} for details on how `gcloud` is located. **NOTE**. These
#'   functions have been moved to the `GCPtools` package.
NULL

#' @rdname gcloud-deprecated
#'
#' @aliases gcloud_access_token
#'
#' @title Obtain an access token for a service account
#'
#' @param service character(1) The name of the service, e.g. "terra" for which
#'   to obtain an access token for.
#'
#' @description `gcloud_access_token()` generates a token for the given service
#'   account. The token is cached for the duration of its validity. The token is
#'   refreshed when it expires. The token is obtained using the `gcloud` command
#'   line utility for the given `gcloud_account()`. The function is mainly used
#'   internally by API service functions, e.g., `AnVIL::Terra()`
#'
#' @return `gcloud_access_token()` returns a simple token string to be used with
#'   the given service.
#'
#' @importFrom BiocBaseUtils lifeCycle
#'
#' @examples
#' if (has_avworkspace(platform = gcp()) && interactive())
#'     GCPtools::gcloud_access_token("rawls") |> invisible()
#' @export
gcloud_access_token <- function(service) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_access_token(service = service)
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_exists()` tests whether the `gcloud()` command
#'     can be found on this system. After finding the binary location,
#'     it runs `gcloud version` to identify potentially misconfigured
#'     installations. See 'Details' section of `gsutil` for where the
#'     application is searched.
#'
#' @return `gcloud_exists()` returns `TRUE` when the `gcloud`
#'     application can be found, FALSE otherwise.
#'
#' @examples
#' GCPtools::gcloud_exists()
#'
#' @export
gcloud_exists <-
    function()
{
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_exists()
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_account()`: report the current gcloud account
#'     via `gcloud config get-value account`.
#'
#' @param account character(1) Google account (e.g., `user@gmail.com`)
#'     to use for authentication.
#'
#' @return `gcloud_account()` returns a `character(1)` vector
#'     containing the active gcloud account, typically a gmail email
#'     address.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#'
#' @examples
#' if (has_avworkspace(platform = gcp()))
#'     GCPtools::gcloud_account()
#'
#' @export
gcloud_account <- function(account = NULL) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_account(account = account)
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_project()`: report the current gcloud project
#'     via `gcloud config get-value project`.
#'
#' @param project character(1) billing project name.
#'
#' @return `gcloud_project()` returns a `character(1)` vector
#'     containing the active gcloud project.
#'
#' @export
gcloud_project <- function(project = NULL) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_project(project = project)
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_help()`: queries `gcloud` for help for a
#'     command or sub-comand via `gcloud help ...`.
#'
#' @param ... Additional arguments appended to `gcloud` commands.
#'
#' @return `gcloud_help()` returns an unquoted `character()` vector
#'     representing the text of the help manual page returned by
#'     `gcloud help ...`.
#'
#' @examples
#' if (has_avworkspace(platform = gcp()))
#'     GCPtools::gcloud_help()
#'
#' @export
gcloud_help <- function(...) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_help(...)
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_cmd()` allows arbitrary `gcloud` command
#'     execution via `gcloud ...`. Use pre-defined functions in
#'     preference to this.
#'
#' @param cmd `character(1)` representing a command used to evaluate
#'     `gcloud cmd ...`.
#'
#' @return `gcloud_cmd()` returns a `character()` vector representing
#'     the text of the output of `gcloud cmd ...`
#'
#' @export
gcloud_cmd <- function(cmd, ...) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_cmd(cmd, ...)
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_storage()` allows arbitrary `gcloud storage` command
#'   execution via `gcloud storage ...`. Typically used for bucket management
#'   commands such as `rm` and `cp`.
#'
#' @export
gcloud_storage <- function(cmd, ...) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_storage(cmd, ...)
}

#' @rdname gcloud-deprecated
#'
#' @description `gcloud_storage_buckets()` provides an interface to the
#'  `gcloud storage buckets` command. This command can be used to create a new
#'   bucket via `gcloud storage buckets create ...`.
#'
#' @param bucket_cmd `character(1)` representing a buckets command typically
#'   used to create a new bucket. It can also be used to
#'   `add-iam-policy-binding` or `remove-iam-policy-binding` to a bucket.
#'
#' @param bucket `character(1)` representing a unique bucket name to be created
#'   or modified.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @export
gcloud_storage_buckets <- function(bucket_cmd = "create", bucket, ...) {
    lifeCycle(
        newpackage = "GCPtools",
        package = "AnVILGCP",
        cycle = "deprecated",
        title = "gcloud"
    )
    GCPtools::gcloud_storage_buckets(
        bucket_cmd = bucket_cmd, bucket = bucket, ...
    )
}
