#' 'kintone' Record API
#'
#' \code{kntn_record()} gets a single record from the specified kintone application.
#' \code{kntn_records()} retrieves multiple records at once. If the number of records is more than
#' \code{records_per_request} (the default is 100), \code{kntn_records()} automatically splits the
#' request into smaller subrequests.
#'
#' @name kntn_record
#'
#' @param app App ID.
#' @param id Record ID.
#' @param query Query (e.g. \code{"updated_time > \\"2012-02-03T09:00:00+0900\\" and updated_time < \\"2012-02-03T10:00:00+0900\\""}).
#' @param fields Names of fields.
#' @param verbose If \code{TRUE}, output logs verbosely.
#' @param as Desired type of output: \code{data.frame}, \code{list} or \code{text} (\code{data.frame} means \link[dplyr]{tbl_df}).
#' @param max_records Max number of records to get.
#' @param offset Offset of records.
#' @param records_per_request Number of records per request (max: 100).
#'
#' @details
#' A field will be converted to the correspondent object by the type:
#' \itemize{
#' \item RECORD_NUMBER: \code{character}
#' \item __ID__: \code{integer}
#' \item __REVISION__: \code{integer}
#' \item CREATOR: \code{character} (code)
#' \item CREATED_TIME: \code{POSIXct}
#' \item MODIFIER: \code{character} (code)
#' \item UPDATED_TIME: \code{POSIXct}
#' \item SINGLE_LINE_TEXT: \code{character}
#' \item NUMBER: \code{numeric}
#' \item CALC: \code{character}
#' \item MULTI_LINE_TEXT: \code{character}
#' \item RICH_TEXT: \code{character}
#' \item CHECK_BOX: nested \code{character}
#' \item RADIO_BUTTON: \code{character}
#' \item DROP_DOWN: \code{character}
#' \item MULTI_SELECT: nested \code{character}
#' \item FILE: nested \code{\link[dplyr]{tbl_df}}
#' \item LINK: \code{character}
#' \item DATE: \code{Date}
#' \item TIME: \code{character} (R has no correspondent class for this)
#' \item DATETIME: \code{POSIXct}
#' \item USER_SELECT: nested \code{character} (code)
#' \item ORGANIZATION_SELECT: nested \code{character} (code)
#' \item GROUP_SELECT: nested \code{character} (code)
#' \item CATEGORY: nested \code{character}
#' \item STATUS: \code{character}
#' \item STATUS_ASSIGNEE: \code{character}
#' \item SUBTABLE: nested \code{tbl}
#' }
#'
#' Some types will be converted to nested objects. You can unnest these fields by
#' \link{kntn_unnest}.
#'
#' @examples
#' \dontrun{
#' kntn_set_auth()
#'
#' app <- 10
#'
#' # get a single record
#' d <- kntn_record(app, id = 1)
#'
#' # get records up to 1000 (default)
#' d <- kntn_records(app)
#'
#' # get records up to 5000 records at the latency of 500 records/request.
#' d <- kntn_records(app, max_records = 5000, records_per_request = 500L)
#'
#' # get records as list
#' d <- kntn_records(app, as = "list")
#'
#' # get records matched with the specified query and fields.
#' # See https://developer.kintone.io/hc/en-us/articles/213149287/ for the query syntax
#' d <- kntn_records(app, fields = c("timestamp", "value"),
#'                   query = "updated_time > \"2016-10-03T09:00:00+0900\"")
#'
#' # Some types like SUBTABLE are converted as nested data.frame.
#' # You can unnest them by using kntn_unnest.
#' kntn_unnest(d)
#' }
#'
#' @seealso \url{https://developer.kintone.io/hc/en-us/articles/213149287/}
NULL

kntn_api <- function(verb, url, path, app, query, verbose = FALSE) {
  if(verbose) message("query is:", query)

  res <- httr::VERB(
    verb   = verb,
    url    = url,
    path   = path,
    config = kntn_get_authorization_header(),
    query = query
  )

  kntn_stop_for_status(res)

  res
}

#' @rdname kntn_record
#' @export
kntn_record <- function(app, id, as = c("data.frame", "list", "text"), verbose = FALSE) {
  if(!kntn_check_envvars_defined()) {
    stop("Please set necessary environmental variables by .Renviron or kntn_set_auth()")
  }

  url <- guess_url(Sys.getenv("KNTN_URL"))

  as <- match.arg(as)

  query_params <- list(`app` = app,
                       `id`  = id)

  res <- kntn_api(
    "GET",
    url = url,
    path = "/k/v1/record.json",
    query = query_params,
    verbose = verbose
  )

  record_text <- httr::content(res, as = "text")

  if (as == "text") {
    return(record_text)
  }

  record_json <- jsonlite::fromJSON(record_text, simplifyVector = FALSE)

  if (as == "list") {
    return(record_json)
  }

  kntn_parse_record(record_json$record)
}


kntn_records_once <- function(url, app, fields, query,
                              as = c("data.frame", "list", "text"), verbose = FALSE) {

  as <- match.arg(as)

  query_params <- list(`app` = app,
                       fields = paste0(fields, collapse = ","),
                       query = query) %>%
    purrr::discard(is.null)

  res <- kntn_api(
    "GET",
    url = url,
    path = "/k/v1/records.json",
    query = query_params,
    verbose = verbose
  )

  records_text <- httr::content(res, as = "text")

  if (as == "text") {
    return(records_text)
  }

  records_json <- jsonlite::fromJSON(records_text, simplifyVector = FALSE)

  if (as == "list") {
    return(records_json)
  }

  kntn_parse_records(records_json$records)
}

#' @rdname kntn_record
#' @export
kntn_records <- function(app, fields = NULL, query = "",
                         max_records = 1000L, offset = 0L, records_per_request = 100L,
                         as = c("data.frame", "list", "text"), verbose = FALSE) {
  if(!kntn_check_envvars_defined()) {
    stop("Please set necessary environmental variables by .Renviron or kntn_set_auth()")
  }

  url <- guess_url(Sys.getenv("KNTN_URL"))

  as <- match.arg(as)

  result <- list()

  # ignore if limit and offset is included in query
  if(!is.character(query) && is.na(query)) query <- ""
  query_base <- strip_query(query)

  # backdoor to boost request
  kntn_sleep_time <- Sys.getenv("KNTN_SLEEP_TIME")

  interval_time <- if(identical(kntn_sleep_time, "")) 1 else as.numeric(kntn_sleep_time)

  ranges <- calc_ranges(first_offset = offset, max_records = max_records, records_per_request = records_per_request)
  for (i in seq_len(nrow(ranges))) {
    limit  <- ranges$limits[i]
    offset <- ranges$offsets[i]
    query  <- paste(query_base, sprintf("limit %d offset %d", limit, offset))

    message(sprintf("Getting %d records from %d", limit, offset))
    result_tmp <- kntn_records_once(url = url, app = app, fields = fields,
                                     query = query, as = as, verbose = verbose)
    result[[i]] <- result_tmp

    # TODO: as="text"
    if(!is.character(result_tmp) && NROW(result_tmp) < limit) break

    Sys.sleep(interval_time)
  }

  if(as == "data.frame") dplyr::bind_rows(result) else result
}

kntn_stop_for_status <- function(res) {
  # Only HTTP status code 200 is success. Any other status codes are errors.
  code <- httr::status_code(res)
  if(identical(code, 200L)) {
    return(TRUE)
  }

  error_msg <- httr::content(res)$message %||% "(not available)"

  stop(sprintf("
  Status code: %d
  Error message: %s

If the error message is about authorization, please run kntn_set_auth(overwrite = TRUE)",
               code, error_msg))
}

strip_query <- function(x) {
  x <- stringr::str_replace(x, "limit\\s+\\d+(?=\\s+|$)", "")
  x <- stringr::str_replace(x, "offset\\s+\\d+(?=\\s+|$)", "")
  x
}
