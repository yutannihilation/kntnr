kntnr
=====
[![Travis-CI Build Status](https://travis-ci.org/yutannihilation/kntnr.svg?branch=master)](https://travis-ci.org/yutannihilation/kntnr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/yutannihilation/kntnr?branch=master&svg=true)](https://ci.appveyor.com/project/yutannihilation/kntnr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/kntnr)](https://cran.r-project.org/package=kntnr)

Retrieve data from [kintone](https://www.kintone.com/) via its API. kintone is an enterprise application platform.

## Installation

```r
devtools::install_github("yutannihilation/kntnr")
```

## Usage

### Authentication

To use kintone API, kntnr needs the following environmental variables.

* `KNTN_URL`: kintone URL
* `KNTN_AUTH_TYPE`: authentication type (password or token)
* `KNTN_AUTH`: API token or BASE64 encoded login name and password

`kntn_set_auth()` sets these envvars interactively.

```r
# password-based authentication
kntn_set_auth()

# token-based authentication
kntn_set_auth(auth_type = "token")
```

To set another information, run `kntn_set_auth()` with `overwrite = TRUE`.

```r
kntn_set_auth(overwrite = TRUE)
```

Or, unset the envvars by `kntn_unset_auth()`.

```r
kntn_unset_auth()
```

#### For non-interactive use

You can define envvars in `.Renviron` which will be loaded when R session starts up. For more information, see `?Startup`.

```r
KNTN_URL = https://example.kintone.com/
KNTN_AUTH_TYPE = token
KNTN_AUTH = 1234567890
```

### Record API

`kntn_record()` gets a single record from specified kintone application. `kntn_records()` retrieves multiple records at once. If the number of records is more than records_per_request (the default is 100), `kntn_records()` automatically split the request.

```r
app <- 10

# get a single record
d <- kntn_record(app, id = 1)

# get records up to 1000 (default)
d <- kntn_records(app)

# get records up to 5000 records at the latency of 500 records/request.
d <- kntn_records(app, max_records = 5000, records_per_request = 500L)
```

Some types like SUBTABLE are converted as nested data.frame.
You can unnest them by using `tidyr::unnest()`.

```r
tidyr::unnest(d, subtable)
```

### File API

Get a file from kintone API and parse it with `httr::content()`. If you want to parse it by yourself, specify `as = "raw"` or `as = "text"`. 

```r
d <- kntn_record(app, id = 1)

f <- kntn_file(app, fileKey = x$Attachment[[1]]$fileKey[1])
```
