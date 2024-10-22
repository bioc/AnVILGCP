#' @name avworkspace-methods
#'
#' @title AnVIL Workspace GCP methods
#'
#' @description `avworkspace_namespace()` and `avworkspace_name()` are utiliity
#'   functions to retrieve workspace namespace and name from environment
#'   variables or interfaces usually available in AnVIL notebooks or RStudio
#'   sessions. `avworkspace()` provides a convenient way to specify workspace
#'   namespace and name in a single command. `avworkspace_clone()` clones
#'   (copies) an existing workspace, possibly into a new namespace (billing
#'   account).
#'
#' @details `avworkspace_namespace()` is the billing account. If the
#'     `namespace=` argument is not provided, try `gcloud_project()`,
#'     and if that fails try `Sys.getenv("WORKSPACE_NAMESPACE")`.
#'
#' `avworkspace_name()` is the name of the workspace as it appears in
#' \url{https://app.terra.bio/#workspaces}. If not provided,
#' `avworkspace_name()` tries to use `Sys.getenv("WORKSPACE_NAME")`.
#'
#' Namespace and name values are cached across sessions, so explicitly
#' providing `avworkspace_name*()` is required at most once per
#' session. Revert to system settings with arguments `NA`.
#'
#' @inheritParams gcp-methods
#' @inheritParams avdata
#'
#' @param warn logical(1) when `TRUE` (default), generate a warning
#'     when the workspace namespace or name cannot be determined.
#'
#' @param workspace when present, a `character(1)` providing the
#'     concatenated namespace and name, e.g.,
#'     `"bioconductor-rpci-anvil/Bioconductor-Package-AnVIL"`
#'
#' @return `avworkspace_namespace()`, and `avworkspace_name()` return
#'     `character(1)` identifiers. `avworkspace()` returns the
#'     character(1) concatenated namespace and name. The value
#'     returned by `avworkspace_name()` will be percent-encoded (e.g.,
#'     spaces `" "` replaced by `"%20"`).
#'
#' @include gcp-class.R
#'
#' @examples
#' if (has_avworkspace(platform = gcp())) {
#'     avworkspaces()
#'     avworkspace_namespace()
#'     avworkspace_name()
#'     avworkspace()
#' }
NULL

# avworkspaces ------------------------------------------------------------

#' @describeIn avworkspace-methods list workspaces in the current project as a
#'   tibble
#'
#' @importFrom AnVILBase avworkspaces
#' @exportMethod avworkspaces
setMethod(
    "avworkspaces",
    signature = c(platform = "gcp"),
    definition = function(
        ...,
        platform = cloud_platform()
    ) {
        checkInstalled("AnVIL")
        response <- AnVIL::Rawls()$listWorkspaces()
        avstop_for_status(response, "avworkspaces")

        AnVILBase::flatten(response) |>
            AnVILBase::avworkspaces_clean()
    }
)

# avworkspace_namespace ---------------------------------------------------

#' @describeIn avworkspace-methods Get or set the namespace of the current
#'   workspace
#'
#' @importFrom AnVILBase avworkspace_namespace
#' @exportMethod avworkspace_namespace
setMethod(
    "avworkspace_namespace",
    signature = c(platform = "gcp"),
    definition = function(
        namespace = NULL,
        warn = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        namespace <- .avworkspace(
            "avworkspace_namespace", "NAMESPACE", namespace, warn = FALSE
        )
        if (!nzchar(namespace)) {
            namespace <- tryCatch({
                gcloud_project()
            }, error = function(e) {
                NULL
            })
            namespace <- .avworkspace(
                "avworkspace_namespace", "NAMESPACE", namespace, warn = warn
            )
        }
        namespace
    }
)

# avworkspace_name --------------------------------------------------------

#' @describeIn avworkspace-methods Get or set the name of the current workspace
#'
#' @importFrom AnVILBase avworkspace_name
#' @importFrom utils URLencode
#'
#' @exportMethod avworkspace_name
setMethod(
    "avworkspace_name",
    signature = c(platform = "gcp"),
    definition = function(
        name = NULL,
        warn = TRUE,
        ...,
        platform = cloud_platform()
    ) {
        value <- .avworkspace("avworkspace_name", "NAME", name, warn = warn)
        URLencode(value)
    }
)

# avworkspace -------------------------------------------------------------

#' @describeIn avworkspace-methods Get the current workspace namespace and name
#'   combination
#'
#' @importFrom AnVILBase avworkspace
#' @exportMethod avworkspace
setMethod(
    "avworkspace",
    signature = c(platform = "gcp"),
    definition = function(workspace = NULL, ..., platform = cloud_platform())
    {
        stopifnot(
            `'workspace' must be NULL or of the form 'namespace/name'` =
                is.null(workspace) || .is_workspace(workspace)
        )
        if (!is.null(workspace)) {
            wkspc <- strsplit(workspace, "/")[[1]]
            avworkspace_namespace(wkspc[[1]])
            avworkspace_name(wkspc[[2]])
        }
        paste0(avworkspace_namespace(), "/", avworkspace_name())
    }
)

# avworkspace_clone -------------------------------------------------------

#' @describeIn avworkspace-methods Clone the current workspace
#'
#' @param to_namespace character(1) workspace (billing account) in
#'     which to make the clone.
#'
#' @param to_name character(1) name of the cloned workspace.
#'
#' @param storage_region character(1) region (NO multi-region, except the
#'   default) in which bucket attached to the workspace should be created.
#'
#' @param bucket_location character(1) DEPRECATED; use `storage_region` instead.
#'   Region (NO multi-region, except the default) in which bucket attached to
#'   the workspace should be created.
#'
#' @return `avworkspace_clone()` returns the namespace and name, in
#'     the format `namespace/name`, of the cloned workspace.
#'
#'
#' @importFrom AnVILBase avworkspace_clone avstop_for_status
#' @exportMethod avworkspace_clone
setMethod("avworkspace_clone",
    signature = c(platform = "gcp"),
    definition = function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        to_namespace = namespace,
        to_name,
        storage_region = "US",
        bucket_location = storage_region,
        ...,
        platform = cloud_platform()
    ) {

        stopifnot(
            isScalarCharacter(namespace),
            isScalarCharacter(name),
            isScalarCharacter(to_namespace),
            isScalarCharacter(to_name),
            isScalarCharacter(storage_region) ||
                isScalarCharacter(bucket_location),
            `source and destination 'namespace/name' must be different` =
                !identical(namespace, to_namespace) || !identical(name, to_name)
        )

        if (!missing(bucket_location)) {
            .Deprecated(
                new = "storage_region =",
                package = "AnVILGCP",
                msg = c(
                    "argument 'bucket_location' is deprecated; ",
                    "use 'storage_region' instead"
                ),
                old = "bucket_location ="
            )
            storage_region <- bucket_location
        }
        checkInstalled("AnVIL")
        response <- AnVIL::Terra()$cloneWorkspace(
            workspaceNamespace = namespace,
            workspaceName = URLencode(name),
            .__body__ = list(
                attributes = setNames(list(), character()),  # json '{}'
                bucketLocation = storage_region,
                copyFilesWithPrefix = "notebooks/",
                namespace = to_namespace,
                name = URLencode(to_name)
            )
        )
        avstop_for_status(response, "avworkspace_clone")

        paste(to_namespace, to_name, sep = "/")
    }
)
