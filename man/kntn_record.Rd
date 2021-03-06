% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kntn_record.R
\name{kntn_record}
\alias{kntn_record}
\alias{kntn_records}
\title{'kintone' Record API}
\usage{
kntn_record(app, id, as = c("data.frame", "list", "text"), verbose = FALSE)

kntn_records(
  app,
  fields = NULL,
  query = "",
  max_records = 1000L,
  offset = 0L,
  records_per_request = 100L,
  as = c("data.frame", "list", "text"),
  verbose = FALSE
)
}
\arguments{
\item{app}{App ID.}

\item{id}{Record ID.}

\item{as}{Desired type of output: \code{data.frame}, \code{list} or \code{text} (\code{data.frame} means \link[dplyr]{tbl_df}).}

\item{verbose}{If \code{TRUE}, output logs verbosely.}

\item{fields}{Names of fields.}

\item{query}{Query (e.g. \code{"updated_time > \\"2012-02-03T09:00:00+0900\\" and updated_time < \\"2012-02-03T10:00:00+0900\\""}).}

\item{max_records}{Max number of records to get.}

\item{offset}{Offset of records.}

\item{records_per_request}{Number of records per request (max: 100).}
}
\description{
\code{kntn_record()} gets a single record from the specified kintone application.
\code{kntn_records()} retrieves multiple records at once. If the number of records is more than
\code{records_per_request} (the default is 100), \code{kntn_records()} automatically splits the
request into smaller subrequests.
}
\details{
A field will be converted to the correspondent object by the type:
\itemize{
\item RECORD_NUMBER: \code{character}
\item __ID__: \code{integer}
\item __REVISION__: \code{integer}
\item CREATOR: \code{character} (code)
\item CREATED_TIME: \code{POSIXct}
\item MODIFIER: \code{character} (code)
\item UPDATED_TIME: \code{POSIXct}
\item SINGLE_LINE_TEXT: \code{character}
\item NUMBER: \code{numeric}
\item CALC: \code{character}
\item MULTI_LINE_TEXT: \code{character}
\item RICH_TEXT: \code{character}
\item CHECK_BOX: nested \code{character}
\item RADIO_BUTTON: \code{character}
\item DROP_DOWN: \code{character}
\item MULTI_SELECT: nested \code{character}
\item FILE: nested \code{\link[dplyr]{tbl_df}}
\item LINK: \code{character}
\item DATE: \code{Date}
\item TIME: \code{character} (R has no correspondent class for this)
\item DATETIME: \code{POSIXct}
\item USER_SELECT: nested \code{character} (code)
\item ORGANIZATION_SELECT: nested \code{character} (code)
\item GROUP_SELECT: nested \code{character} (code)
\item CATEGORY: nested \code{character}
\item STATUS: \code{character}
\item STATUS_ASSIGNEE: \code{character}
\item SUBTABLE: nested \code{tbl}
}

Some types will be converted to nested objects. You can unnest these fields by
\link{kntn_unnest}.
}
\examples{
\dontrun{
kntn_set_auth()

app <- 10

# get a single record
d <- kntn_record(app, id = 1)

# get records up to 1000 (default)
d <- kntn_records(app)

# get records up to 5000 records at the latency of 500 records/request.
d <- kntn_records(app, max_records = 5000, records_per_request = 500L)

# get records as list
d <- kntn_records(app, as = "list")

# get records matched with the specified query and fields.
# See https://developer.kintone.io/hc/en-us/articles/213149287/ for the query syntax
d <- kntn_records(app, fields = c("timestamp", "value"),
                  query = "updated_time > \"2016-10-03T09:00:00+0900\"")

# Some types like SUBTABLE are converted as nested data.frame.
# You can unnest them by using kntn_unnest.
kntn_unnest(d)
}

}
\seealso{
\url{https://developer.kintone.io/hc/en-us/articles/213149287/}
}
