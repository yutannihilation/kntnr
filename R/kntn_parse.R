#' Parse 'kintone' API Response
#'
#' Convert various kinds of fields to the correspondent R classes.
#'
#' @name kntn_parse
#'
#' @param record List object converted from a single kintone record.
#' @param records List objects converted from multiple kintone records.
#'
#' @examples
#' library(jsonlite)
#'
#' rcd_file <- system.file("extdata/record.json", package = "kntnr")
#' rcd <- fromJSON(rcd_file, simplifyVector = FALSE)$record
#' kntnr:::kntn_parse_record(rcd)
#'
#' rcds_file <- system.file("extdata/records.json", package = "kntnr")
#' rcds <- jsonlite::fromJSON(rcds_file, simplifyVector = FALSE)$records
#' kntnr:::kntn_parse_records(rcds)
#'
#' @seealso \url{https://developer.kintone.io/hc/en-us/articles/212494818/}
NULL

#' @rdname kntn_parse
#' @keywords internal
kntn_parse_records <- function(records) {
  tibble::as_tibble(
    purrr::map(purrr::transpose(records),
               kntn_parse_col)
  )
}

#' @rdname kntn_parse
#' @keywords internal
kntn_parse_record <- function(record) {
  kntn_parse_records(list(record))
}

NESTED_TYPES <- c("CHECK_BOX", "MULTI_SELECT", "CATEGORY",
                  "CREATOR", "MODIFIER",
                  "USER_SELECT", "ORGANIZATION_SELECT", "GROUP_SELECT", "STATUS_ASSIGNEE",
                  "FILE", "SUBTABLE")

#' @rdname kntn_parse
#' @keywords internal
kntn_parse_col <- function(x) {
  x_transposed <- purrr::transpose(x)

  type <- unique(purrr::flatten_chr(x_transposed$type))
  if(! type %in% NESTED_TYPES) {
    # NULL cannot be treated as character.
    x_transposed$value <- purrr::map_if(x_transposed$value, is.null, ~ NA_character_)
    x_transposed$value <- purrr::flatten_chr(x_transposed$value)
  }

  switch(type,
         `__ID__`       = as.integer(x_transposed$value),
         `__REVISION__` = as.integer(x_transposed$value),
         NUMBER         = as.numeric(x_transposed$value),
         DATE           = lubridate::ymd(x_transposed$value, quiet = TRUE),
         DATETIME       = kntn_parse_datetime(x_transposed$value),
         CREATED_TIME   = kntn_parse_datetime(x_transposed$value),
         UPDATED_TIME   = kntn_parse_datetime(x_transposed$value),
         TIME           = x_transposed$value, # TODO: R has no method to handle timestamp without date
         CHECK_BOX      = kntn_wrap_with_list(x_transposed$value),
         MULTI_SELECT   = kntn_wrap_with_list(x_transposed$value),
         CATEGORY       = kntn_wrap_with_list(x_transposed$value),
         CREATOR        = kntn_parse_single_user(x_transposed$value),
         MODIFIER       = kntn_parse_single_user(x_transposed$value),
         USER_SELECT    = kntn_parse_multi_user(x_transposed$value),
         ORGANIZATION_SELECT = kntn_parse_multi_user(x_transposed$value),
         GROUP_SELECT   = kntn_parse_multi_user(x_transposed$value),
         STATUS_ASSIGNEE = kntn_parse_multi_user(x_transposed$value),
         FILE           = kntn_parse_file(x_transposed$value),
         SUBTABLE       = kntn_parse_subtable(x_transposed$value),
         x_transposed$value)
}

kntn_parse_datetime <- function(x) {
  lubridate::parse_date_time2(x, "YmdHMS")
}

kntn_wrap_with_list <- function(x) {
  purrr::map(x, purrr::flatten_chr)
}

kntn_parse_single_user <- function(x) {
  # NULL$code is NULL
  purrr::map_chr(x, ~ .$code %||% NA_character_)
}

kntn_parse_multi_user <- function(x) {
  purrr::map(x, ~ purrr::map_chr(., "code"))
}

kntn_parse_file <- function(x) {
  purrr::map(x, dplyr::bind_rows)
}

kntn_parse_subtable <- function(x) {
  purrr::map(x, kntn_parse_subtable_one)
}

kntn_parse_subtable_one <- function(x) {
  if(length(x) == 0) return(tibble::tibble())

  x_trans <- purrr::transpose(x)
  kntn_parse_records(x_trans$value)
}
