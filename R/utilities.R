#' @importFrom dplyr full_join select
.tbl_with_template <-
    function(tbl, tmpl)
{
    result <- as_tibble(tmpl)
    if (nrow(tbl)) {
        have <- intersect(names(tbl), names(tmpl))
        tbl <- select(tbl, have)
        result <-
            full_join(tbl, result, by = have) %>%
            select(names(tmpl))
    }
    result
}

.pretty_text <- function(..., indent = 0L, exdent = 0L) {
    text <- paste(..., collapse = " ")
    paste(strwrap(text, indent = indent, exdent = exdent), collapse = "\n")
}

.is_workspace <-
    function(x)
{
    isScalarCharacter(x) &&
        ## exactly 1 `/`
        identical(lengths(regmatches(x, gregexpr("/", x, fixed = TRUE))), 1L)
}

.isScalarCharacter_or_NULL <- function(x, na.ok = FALSE, zchar = FALSE)
    isScalarCharacter(x, na.ok, zchar) || is.null(x)

.avworkspaces_clean <- function(.data) {
    .data |>
        select(
            name = .data$workspace.name,
            lastModified = .data$workspace.lastModified,
            createdBy = .data$workspace.createdBy,
            namespace = .data$workspace.namespace,
            accessLevel = .data$accessLevel
        ) |>
        mutate(
            lastModified = as.Date(.data$lastModified)
        ) |>
        arrange(
            .data$name,
            desc(.data$lastModified)
        )
}

