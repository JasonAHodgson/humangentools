# Gets sample information for a list of sample IDs

`get_sample_information` takes a list of sample IDs and returns
population and region information for each sample

## Usage

``` r
get_sample_information(ID, na.fill = TRUE)
```

## Arguments

- ID:

  a list of sample IDs

- na.fill:

  Logical; if `TRUE` (the default), IDs not found in the sample
  information data are kept in the result with `NA` population/ region.
  If `FALSE`, they are dropped instead.

## Value

a data frame of sample id, population, and region
