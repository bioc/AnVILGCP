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

.is_local_directory <- function(x)
    isScalarCharacter(x) && dir.exists(x)

.isScalarCharacter_or_NULL <- function(x, na.ok = FALSE, zchar = FALSE)
    isScalarCharacter(x, na.ok, zchar) || is.null(x)

.avworkspace <- local({
    hash <- new.env(parent = emptyenv())
    function(fun, key, value, warn = TRUE) {
        sysvar <- toupper(paste0("WORKSPACE_", key))
        if (is.null(value)) {
            if (is.null(hash[[key]])) {
                ## initialize
                hash[[key]] <- Sys.getenv(sysvar)
                if (!nzchar(hash[[key]]) && warn && interactive())
                    warning("'", sysvar, "' undefined; use `", fun, "()` to set")
            }
        } else {
            hash[[key]] <- ifelse(is.na(value), Sys.getenv(sysvar), value)
        }
        hash[[key]]
    }
})
