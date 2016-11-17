#' Authorization Header for 'kintone'
#'
#' \code{kntn_get_authorization_header()} constructs a header for authorization using \link[httr]{add_headers}.
#' \code{kntn_set_auth()} interactivery asks for the type of authentication and the credential and store
#' them as environmental variables \code{KNTN_URL}, \code{KNTN_AUTH_TYPE} and \code{KNTN_AUTH}.
#' If you want to avoid interaction, please manually set these.
#' \code{kntn_unset_auth()} unsets these environmental variables.
#'
#' @name kntn_auth
#'
#' @param auth_type
#'   Type of Authentication. \code{password} uses username and password.
#'   \code{token} uses per-app token.
#' @param overwrite
#'   If \code{TRUE}, overwrite the existing environmental variables.
#'
#' @seealso \url{https://developer.kintone.io/hc/en-us/articles/212495188/#userauthentication}
#'
#' @examples
#' \dontrun{
#' # set KNTN_URL, KNTN_AUTH and KNTN_AUTH_TYPE interactively.
#' # By default, auth_type is password.
#' kntn_set_auth()
#'
#' Sys.getenv(c("KNTN_URL", "KNTN_AUTH", "KNTN_AUTH_TYPE"))
#'
#' # This will return `X-Cybozu-Authorization` header.
#' kntn_get_authorization_header()
#'
#' # Clear environmental variables before trying to use another set of authorization info.
#' kntn_unset_auth()
#' kntn_set_auth(auth_type = "token")
#'
#' # This will return `X-Cybozu-API-Token` header.
#' kntn_get_authorization_header()
#'
#' # To avoid interaction, set these environmental variables manually.
#' Sys.setenv("KNTN_URL" = "https://example.cybozu.com/")
#' Sys.setenv("KNTN_AUTH" = "abcdefg")
#' Sys.setenv("KNTN_AUTH_TYPE" = "token")
#' }
#' @export
kntn_get_authorization_header <- function() {
  if(!kntn_check_envvars_defined()) {
    stop("Please set necessary environmental variables by .Renviron or kntn_set_auth()")
  }

  auth <- Sys.getenv("KNTN_AUTH")
  auth_type <- Sys.getenv("KNTN_AUTH_TYPE")

  switch(auth_type,
         "password" = return(httr::add_headers(`X-Cybozu-Authorization` = auth)),
         "token"    = return(httr::add_headers(`X-Cybozu-API-Token` = auth)),
         stop(sprintf("Unknown KNTN_AUTH_TYPE: %s", auth_type)))
}

#' @rdname kntn_auth
#' @export
kntn_set_auth <- function(auth_type = c("password", "token"), overwrite = FALSE) {
  if (!interactive()) {
    stop("If you want to run kntnr in noninteractive environment,\n",
         "please manually set these environmental variables:\n",
         "  - KNTN_URL\n",
         "  - KNTN_AUTH\n",
         "  - KNTN_AUTH_TYPE\n")
  }

  if (kntn_check_envvars_defined(assume_all = FALSE) && !overwrite) {
    warning("At least one of KNTN_URL, KNTN_AUTH and KNTN_AUTH_TYPE is defined.\n",
            "To overrite these, please rerun with 'overwrite = TRUE'\n")
    return()
  }

  auth_type <- match.arg(auth_type)

  url <- ask_for_nonsecret("kintone URL (e.g. https://example.cybozu.com/): ")

  if(identical(auth_type, "password")) {
    username <- ask_for_secret("username for kintone: ")
    password <- ask_for_secret("password for kintone: ")
    auth <- base64enc::base64encode(charToRaw(paste(username, password, sep = ":")))
  } else {
    auth <- ask_for_secret("API token for kintone: ")
  }

  Sys.setenv("KNTN_URL" = url)
  Sys.setenv("KNTN_AUTH" = auth)
  Sys.setenv("KNTN_AUTH_TYPE" = auth_type)

  message("Environmental variables KNTN_URL, KNTN_AUTH and KNTN_AUTH_TYPE are set.")
  message("To unset these, please run kntn_unset_auth().")
}

kntn_check_envvars_defined <- function(assume_all = TRUE) {
  envvars_defined <- c("KNTN_URL", "KNTN_AUTH", "KNTN_AUTH_TYPE") %in% names(Sys.getenv())
  if(assume_all) {
    all(envvars_defined)
  } else {
    any(envvars_defined)
  }
}


#' @rdname kntn_auth
#' @export
kntn_unset_auth <- function() {
  Sys.unsetenv("KNTN_URL")
  Sys.unsetenv("KNTN_AUTH")
  Sys.unsetenv("KNTN_AUTH_TYPE")
}

ask_for_nonsecret <- function(prompt) {
  cat(prompt)
  readLines(n = 1L)
}

ask_for_secret <- function(prompt) {
  if(rstudioapi::isAvailable()) {
    rstudioapi::askForPassword(prompt)
  } else {
    # Fallback to ask_for_nonsecret
    ask_for_nonsecret(prompt)
  }
}
