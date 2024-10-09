.DRS_HUB <- "https://drshub.dsde-prod.broadinstitute.org"

.DRS_HUB_TEMPLATE <- list(
    drs = character(),
    fileName = character(),
    size = integer(),
    accessUrl = character(),
    timeUpdated = character(),
    timeCreated = character(),
    bucket = character(),
    name = character()
)

#' @importFrom httr POST add_headers
.drs_res_service <- function(drs_url, service_url, fields, token) {
    headers <- add_headers(
        Authorization = paste("Bearer", token),
        "content-type" = "application/json"
    )
    body <- list(fields = fields, url = jsonlite::unbox(drs_url))
    body_json <- jsonlite::toJSON(body)
    response <- POST(service_url, headers, body = body_json, encode="raw")
    avstop_for_status(response, "DRS resolution")

    ## add drs field to response
    lst <- c(as.list(response), list(drs = drs_url))

    ## unbox accessUrl; if accessUrl == NULL, then this is a no-op
    lst$accessUrl <- unlist(lst$accessUrl, use.names = FALSE)

    ## nest list elements so length == 1L
    is_list <-
        vapply(lst, is.list, logical(1))
    lst[is_list] <- lapply(lst[is_list], list)

    as_tibble(lst[lengths(lst) == 1L])
}

.DRS_REQ_FIELDS <- c(
    "bucket", "name", "size", "timeCreated",
    "timeUpdated", "fileName", "accessUrl"
)

#' @name drs
#'
#' @title DRS (Data Repository Service) URL management
#'
#' @description `drs_hub()` resolves zero or more DRS URLs to their Google
#'   bucket location using the DRS Hub API endpoint.
#'
#' @section drs_hub:
#' `drs_hub()` uses the DRS Hub API endpoint to resolve a single or multiple DRS
#' URLs to their Google bucket location. The DRS Hub API endpoint requires a
#' `gcloud_access_token()`. The DRS Hub API service is hosted at
#' <https://drshub.dsde-prod.broadinstitute.org>.
#'
#' @param source `character()` DRS URLs (beginning with 'drs://') to resources
#'   managed by the DRS Hub server (`drs_hub()`).
#'
#' @return `drs_hub()` returns a tbl with the following columns:
#'
#' - `drs`: `character()` DRS URIs
#' - `bucket`: `character()` Google cloud bucket
#' - `name`: `character()` object name in `bucket`
#' - `size`: `numeric()` object size in bytes
#' - `timeCreated`: `character()` object creation time
#' - `timeUpdated`: `character()` object update time
#' - `fileName`: `character()` local file name
#' - `accessUrl`: `character()` signed URL for object access
#'
#' @examples
#' if (gcloud_exists() && interactive()) {
#'     drs_urls <- c(
#'         "drs://drs.anv0:v2_b3b815c7-b012-37b8-9866-1cb44b597924",
#'         "drs://drs.anv0:v2_2823eac3-77ae-35e4-b674-13dfab629dc5",
#'         "drs://drs.anv0:v2_c6077800-4562-30e3-a0ff-aa03a7e0e24f"
#'     )
#'     drs_hub(drs_urls)
#' }
#' @export
drs_hub <- function(source = character()) {
    access_token <- gcloud_access_token("drs")
    service_url <- paste0(.DRS_HUB, "/api/v4/drs/resolve")

    Map(
        .drs_res_service,
        source,
        MoreArgs = list(
            service_url = service_url,
            fields = .DRS_REQ_FIELDS,
            token = access_token
        )
    ) |>
        do.call(rbind.data.frame, args = _) |>
        .tbl_with_template(.DRS_HUB_TEMPLATE)
}
