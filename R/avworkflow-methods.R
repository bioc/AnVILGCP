.avworkflow_job <-
    function(x)
{
    succeeded <- 0L
    failed <- 0L
    if ("Succeeded" %in% names(x$workflowStatuses))
        succeeded <- x$workflowStatuses$Succeeded
    if ("Failed" %in% names(x$workflowStatuses))
        failed <- x$workflowStatuses$Failed

    list(
        submissionId = x[["submissionId"]],
        submitter = x[["submitter"]],
        submissionDate = x[["submissionDate"]],
        status = x[["status"]],
        succeeded = succeeded,
        failed = failed,
        submissionRoot = x[["submissionRoot"]]
    )
}

#' @name avworkflow-methods
#'
#' @title AnVIL workflow methods
#'
#'@description Methods for working with AnVIL workflow execution.
#'  `avworkflow_jobs()` returns a tibble summarizing submitted workflow jobs for
#'  a namespace and name.
#'
#' @return `avworkflow_jobs()` returns a `tibble`, sorted by
#'     `submissionDate`, with columns
#'
#' - submissionId character() job identifier from the workflow runner.
#' - submitter character() AnVIL user id of individual submitting the job.
#' - submissionDate POSIXct() date (in local time zone) of job submission.
#' - status character() job status, with values 'Accepted' 'Evaluating'
#'   'Submitting' 'Submitted' 'Aborting' 'Aborted' 'Done'
#' - succeeded integer() number of workflows succeeding.
#' - failed integer() number of workflows failing.
#'
#' @examples
#' if (
#'     gcloud_exists() && identical(avplatform_namespace(), "AnVILGCP") &&
#'     nzchar(avworkspace_name())
#' )
#'     ## from within AnVIL
#'     avworkflow_jobs()
#'
#' @importFrom dplyr bind_rows mutate desc
#' @importFrom AnVILBase avworkflow_jobs avstop_for_status
#'
#' @export
setMethod("avworkflow_jobs", signature = c(platform = "gcp"), definition =
    function(
        namespace = avworkspace_namespace(),
        name = avworkspace_name(),
        ...,
        platform = cloud_platform()
    ) {
        stopifnot(
            .is_scalar_character(namespace),
            .is_scalar_character(name)
        )

        response <- Terra()$listSubmissions(namespace, URLencode(name))
        avstop_for_status(response, "avworkflow_jobs")

        submissions <- content(response, encoding = "UTF-8")
        if (length(submissions)) {
            submissions <- lapply(submissions, .avworkflow_job)
        } else {
            submissions <- list(
                submissionId = character(),
                submitter = character(),
                submissionDate = character(),
                status = character(),
                succeeded = integer(),
                failed = integer(),
                submissionRoot = character()
            )
        }

        bind_rows(submissions) |>
            mutate(
                submissionDate = .POSIXct(as.numeric(
                    as.POSIXct(.data$submissionDate, "%FT%T", tz="UTC")
                )),
                namespace = namespace,
                name = name
            ) |>
            arrange(desc(.data$submissionDate))
    }
)
