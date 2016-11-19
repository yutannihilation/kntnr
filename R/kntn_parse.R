#' Parse 'kintone' API Response
#'
#' Convert various kinds of fields to the correspondent R classes.
#'
#' @name kntn_parse
#'
#' @param record List object converted from a single kintone record.
#' @param records List objects converted from multiple kintone records.
#' @param as Desired type of output: \code{data.frame} or \code{list}.
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
kntn_parse_record <- function(record, as = c("data.frame", "list")) {
  record <- record %>%
    purrr::map(kntn_parse_field)

  if (match.arg(as) == "data.frame") dplyr::as_data_frame(record) else record
}

#' @rdname kntn_parse
#' @keywords internal
kntn_parse_records <- function(records) {
  purrr::map_df(records, kntn_parse_record)
}

#' @rdname kntn_parse
#' @keywords internal
kntn_parse_field <- function(x) {
  switch(x$type,
         `__ID__`       = as.integer(x$value),
         `__REVISION__` = as.integer(x$value),
         NUMBER         = as.numeric(x$value),
         DATE           = lubridate::ymd(x$value, quiet = TRUE),
         DATETIME       = kntn_parse_datetime(x$value),
         CREATED_TIME   = kntn_parse_datetime(x$value),
         UPDATED_TIME   = kntn_parse_datetime(x$value),
         TIME           = as.character(x$value), # TODO: R has no method to handle timestamp without date
         CHECK_BOX      = kntn_wrap_with_list(x$value),
         MULTI_SELECT   = kntn_wrap_with_list(x$value),
         CATEGORY       = kntn_wrap_with_list(x$value),
         CREATOR        = kntn_parse_single_user(x$value),
         MODIFIER       = kntn_parse_single_user(x$value),
         USER_SELECT    = kntn_parse_multi_user(x$value),
         ORGANIZATION_SELECT = kntn_parse_multi_user(x$value),
         GROUP_SELECT   = kntn_parse_multi_user(x$value),
         STATUS_ASSIGNEE = kntn_parse_multi_user(x$value),
         FILE           = kntn_parse_file(x$value),
         SUBTABLE       = kntn_parse_subtable(x$value),
         as.character(x$value))
}

# check is.na() to surpress warnings about conversion
NA_Date <- as.Date(NA)
NA_POSIXct <- as.POSIXct(NA)

kntn_parse_datetime <- function(x) {
  if(is.na(x)) {
    NA_POSIXct
  } else {
    lubridate::parse_date_time2(x, "YmdHMS", exact = TRUE)
  }
}

kntn_wrap_with_list <- function(x) {
  if(all(is.na(x))) {
    list(character(0))
  } else {
    list(purrr::flatten_chr(x))
  }
}

kntn_parse_single_user <- function(x) {
  x$code %||% NA_character_
}

kntn_parse_multi_user <- function(x) {
  if(all(is.na(x))) {
    list(character(0))
  } else {
    list(purrr::map_chr(x, "code"))
  }
}

kntn_parse_file         <- function(x) {
  if(all(is.na(x))) {
    list(dplyr::data_frame())
  } else {
    list(dplyr::bind_rows(unclass(x)))
  }
}

kntn_parse_subtable     <- function(x) {
  if(all(is.na(x))) {
    list(dplyr::data_frame())
  } else {
    records <- purrr::map(x, "value")
    list(kntn_parse_records(records))
  }
}
