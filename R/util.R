# util functions

guess_url <- function(url_str) {
  if(!purrr::is_scalar_character(url_str)) {
    stop("url_str must be character and the length is 1")
  }

  if(!stringr::str_detect(url_str, "^http")){
    url_str <- paste0("https://", url_str)
  }

  url <- httr::parse_url(url_str)

  url$scheme <- "https"
  url$path <- NULL

  httr::build_url(url)
}


calc_ranges <- function(first_offset = 0L,
                        max_records = 1000L,
                        records_per_request = 100L) {
  ranges <- list()

  ranges$offsets <- seq(from = first_offset,
                        to = first_offset + max_records,
                        by = records_per_request)

  ranges$limits <- rep(records_per_request, length(ranges$offsets) - 1)

  # treat a fraction
  fraction <- max_records %% records_per_request
  if(fraction == 0) {
    # remove the range of zero-length
    length(ranges$offsets) <- length(ranges$offsets) - 1
  } else {
    ranges$limits <- append(ranges$limits, fraction)
  }

  tibble::as_tibble(ranges)
}
