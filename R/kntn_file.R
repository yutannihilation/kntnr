#' 'kintone' File API
#'
#' Get a file from kintone API and parse it with \link[httr]{content}. If you want to parse it by yourself,
#' specify \code{as = "raw"} or \code{as = "text"}.
#'
#' @name kntn_file
#'
#' @seealso \url{https://developer.kintone.io/hc/en-us/articles/212494468/}
#'
#' @param app App ID.
#' @param fileKey File key.
#' @param verbose If \code{TRUE}, output logs verbosely.
#' @param as \code{as} parameter passed to \link[httr]{content}.
#' @param type \code{type} parameter passed to \link[httr]{content}.
#' @param encoding \code{encoding} parameter passed to \link[httr]{content}.
#'
#' @examples
#' \dontrun{
#' kntn_set_auth()
#' app <- 10
#'
#' # get a single record with a file attachment field
#' d <- kntn_record(app, id = 1)
#'
#' f <- kntn_file(app, fileKey = x$Attachment[[1]]$fileKey[1])
#' }
#'
#' @export
kntn_file <- function(app, fileKey, verbose = FALSE, as = NULL, type = NULL, encoding = NULL) {
  query_params <- list(`app` = app,
                       `fileKey`  = fileKey)
  if(!purrr::is_scalar_character(fileKey)) {
    stop("fileKey must be a character scalar.")
  }

  if(!kntn_check_envvars_defined()) {
    stop("Please set necessary environmental variables by .Renviron or kntn_set_auth()")
  }

  url <- guess_url(Sys.getenv("KNTN_URL"))

  res <- kntn_api(
    "GET",
    url = url,
    path = "/k/v1/file.json",
    query = query_params,
    verbose = verbose
  )

  httr::content(res, as = as, type = type, encoding = encoding)
}
