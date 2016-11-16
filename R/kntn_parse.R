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

kntn_set_S3Class <- function(x) {
  if(is.null(x$value) || length(x$value) == 0) {
    x$value <- NA
  }

  structure(x$value,
            class = c(x$type, class(x$value)))
}

#' @rdname kntn_parse
#' @keywords internal
kntn_parse_record <- function(record, as = c("data.frame", "list")) {
  record <- record %>%
    purrr::map(kntn_set_S3Class) %>%
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
kntn_parse_field <- function(x) UseMethod("kntn_parse_field")

#' @export
kntn_parse_field.default      <- function(x) as.character(x)

#' @export
kntn_parse_field.__ID__       <- function(x) as.integer(x)

#' @export
kntn_parse_field.__REVISION__ <- function(x) as.integer(x)

#' @export
kntn_parse_field.NUMBER       <- function(x) as.numeric(x)

# check is.na() to surpress warnings about conversion
NA_Date <- as.Date(NA)
NA_POSIXct <- as.POSIXct(NA)

#' @export
kntn_parse_field.DATE         <- function(x) lubridate::ymd(x, quiet = TRUE)

kntn_parse_datetime <- function(x) {
  if(is.na(x)) {
    NA_POSIXct
  } else {
    lubridate::parse_date_time2(x, "YmdHMS", exact = TRUE)
  }
}

#' @export
kntn_parse_field.DATETIME     <- kntn_parse_datetime

#' @export
kntn_parse_field.CREATED_TIME <- kntn_parse_datetime

#' @export
kntn_parse_field.UPDATED_TIME <- kntn_parse_datetime

# TODO: R has no method to handle timestamp without date
#' @export
kntn_parse_field.TIME         <- as.character

kntn_wrap_with_list <- function(x) {
  if(all(is.na(x))) {
    list(character(0))
  } else {
    list(purrr::flatten_chr(x))
  }
}

# wrap with list in order for dplyr to include in tbl_df
#' @export
kntn_parse_field.CHECK_BOX    <- kntn_wrap_with_list

#' @export
kntn_parse_field.MULTI_SELECT <- kntn_wrap_with_list

#' @export
kntn_parse_field.CATEGORY     <- kntn_wrap_with_list


kntn_parse_single_user <- function(x) x$code %||% NA_character_

#' @export
kntn_parse_field.CREATOR      <- kntn_parse_single_user

#' @export
kntn_parse_field.MODIFIER     <- kntn_parse_single_user

kntn_parse_multi_user <- function(x) {
  if(all(is.na(x))) {
    list(character(0))
  } else {
    list(purrr::map_chr(x, "code"))
  }
}

#' @export
kntn_parse_field.USER_SELECT  <- kntn_parse_multi_user

#' @export
kntn_parse_field.ORGANIZATION_SELECT <- kntn_parse_multi_user

#' @export
kntn_parse_field.GROUP_SELECT <- kntn_parse_multi_user

#' @export
kntn_parse_field.STATUS_ASSIGNEE <- kntn_parse_multi_user

#' @export
kntn_parse_field.FILE         <- function(x) {
  if(all(is.na(x))) {
    list(dplyr::data_frame())
  } else {
    list(dplyr::bind_rows(unclass(x)))
  }
}

#' @export
kntn_parse_field.SUBTABLE     <- function(x) {
  if(all(is.na(x))) {
    list(dplyr::data_frame())
  } else {
    records <- purrr::map(x, "value")
    list(kntn_parse_records(records))
  }
}
