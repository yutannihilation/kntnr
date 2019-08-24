#' Unnest 'kintone' Records
#'
#' The data retrieved by \link{kntn_records} may contain nested data.frames.
#' \code{kntn_unnest()} unnests them by using \link[tidyr]{unnest}.
#' Note that this function is very experimental and may not work well for all data.
#'
#' @param records Data.frame retrieved by kntn_records()
#'
#' @export
kntn_unnest <- function(records) {
  # data.frame
  nested_df_cols <- purrr::map_lgl(records, is_nested_df)
  nested_df_colnames <- names(nested_df_cols)[nested_df_cols]

  try_unnest_recursively <- FALSE

  if (length(nested_df_colnames) > 0) {
    # SUBTABLE may contain nested fields
    try_unnest_recursively <- TRUE

    records <- dplyr::mutate_at(records, nested_df_colnames, fill_dummy_df)
  }

  # character
  nested_chr_cols <- purrr::map_lgl(records, is_nested_chr)
  nested_chr_colnames <- names(nested_chr_cols)[nested_chr_cols]

  if (length(nested_chr_colnames) > 0) {
    records <- dplyr::mutate_at(records, nested_chr_colnames, fill_dummy_chr)
  }

  # We have to unnest one by one, otherwise we will see the error:
  # "All nested columns must have the same number of elements."
  unnest_legacy <- get_unnest_legacy()
  for (col in c(nested_df_colnames, nested_chr_colnames)) {
    records <- unnest_legacy(records, !!rlang::sym(col), .drop = FALSE)
  }

  if (try_unnest_recursively) {
    records <- kntn_unnest(records)
  }

  records
}

# deal with the breaking change introduced in tidyr v1.0.0
# c.f. https://tidyr.tidyverse.org/dev/articles/in-packages.html
get_unnest_legacy <- function() {
  if (utils::packageVersion("tidyr") > "0.8.99") {
    # Using tidyr::unnest_legacy directly will cause CRAN check NOTE
    utils::getFromNamespace("unnest_legacy", "tidyr")
  } else {
    tidyr::unnest
  }
}


is_nested_df <- function(x) { is.list(x) && dplyr::is.tbl(x[[1]])}
is_nested_chr <- function(x) { is.list(x) && is.character(x[[1]])}

fill_dummy_chr <- function(x, nm) {
  purrr::map_if(x, ~ length(.) == 0, ~ NA_character_)
}

fill_dummy_df <- function(x, nm) {
  idx_empty <- purrr::map_int(x, nrow) == 0
  idx_nonempty <- !idx_empty

  if(any(idx_nonempty)) {
    dummy_df_example <- x[idx_nonempty][[1]]
    dummy_colnames <- colnames(dummy_df_example)
    dummy <- purrr::map(dummy_df_example,
                        ~ purrr::when(.,
                                    is_nested_chr(.) ~ list(character(0)),
                                    is_nested_df(.) ~ list(tibble::tibble()),
                                    ~ NA))
  } else {
    dummy_colnames <- nm
    dummy <- purrr::rerun(length(dummy_colnames), NA)
  }

  names(dummy) <- dummy_colnames
  dummy_df <- tibble::as_tibble(dummy)

  purrr::map_if(x, idx_empty, ~ dummy_df)
}
