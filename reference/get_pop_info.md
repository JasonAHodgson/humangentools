# Returns a table of information about populations in a given dataset.

`get_pop_info` returns a data frame containing information about
populations in the specified dataset.

## Usage

``` r
get_pop_info(
  samples = NULL,
  region = NULL,
  population = NULL,
  dataset = NULL,
  include = NULL,
  exclude = NULL
)
```

## Arguments

- samples:

  A gen_tibble object, or a string with sample id. Default is to return
  all populations in the package dataset.

- region:

  A character vector specifying regions to filter populations by.
  Default is NULL (no filtering).

- population:

  A character vector specifying populations to filter by. Default is
  NULL (no filtering).

- dataset:

  A string specifying the dataset to include. Default is all datasets in
  the package.

- include:

  A charcter vector specifying which columns to include in the output.
  Default is all columns.

- exclude:

  A character vector specifying which columns to exclude from the
  output. Default is no columns excluded.

## Value

A data frame with population information.
