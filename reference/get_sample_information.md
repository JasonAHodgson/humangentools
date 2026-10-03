# Gets sample information for a list of sample IDs

`get_sample_information` takes a list of sample IDs and returns the
canonical population code, human readable label, region and source
dataset for each.

## Usage

``` r
get_sample_information(ID, dataset = NULL, temporal = NULL, na.fill = TRUE)
```

## Arguments

- ID:

  a character vector of sample IDs.

- dataset:

  An optional character vector of source datasets to restrict to
  (matched against `source_dataset`). Default `NULL` returns every
  dataset a sample appears in.

- temporal:

  A character vector restricting samples by the age of their population:
  one or both of `"modern"` and `"ancient"`. Default `NULL` (no
  filtering).

- na.fill:

  Logical; if `TRUE` (the default), IDs not found in the sample
  information data are kept in the result with `NA` population/region.
  If `FALSE`, they are dropped instead.

## Value

A data frame of sample id, canonical population code, population label,
region and source dataset, ordered to follow `ID`.

## Details

`id` identifies the individual and is shared across datasets, so it
joins; `source_id` is the contributing dataset's own key, and
`data_type` records the AADR library type (.AG, .SG, .DG and so on),
`NA` elsewhere. One AADR individual sequenced by two methods yields two
rows with the same `id` and different `source_id`, so rows are unique on
`(source_id, pop)` rather than on `(id, pop)`.

A sample can legitimately appear more than once: the Simons Genome
Diversity Project resequenced HGDP cell lines, so `HGDP01414` is present
as both `BantuKenyaHGDP` and `BantuKenyaSGDP` – one individual, two
genotype datasets. The result therefore has *at least* one row per input
ID rather than exactly one. Pass `dataset` to restrict to a single
source and recover a one-row-per-ID result.
