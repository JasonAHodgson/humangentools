# Returns a table of information about populations in a given dataset.

`get_pop_info` returns a data frame of information about populations in
the package's bundled genetic datasets. Populations are keyed by `pop`,
a canonical code following the convention of `kgp::allmeta`: a bare
three letter code for 1000 Genomes populations (`YRI`), and
`<Name><DATASET>` elsewhere (`YorubaHGDP`, `YorubaSGDP`, `YRIHapMap`).
The same group sampled by two projects therefore gets two codes, which
keeps genotypes from different sequencing platforms separable.

## Usage

``` r
get_pop_info(
  samples = NULL,
  pop = NULL,
  population = NULL,
  region = NULL,
  dataset = NULL,
  temporal = NULL,
  location = c("origin", "sampling"),
  include = NULL,
  exclude = NULL
)
```

## Arguments

- samples:

  A gen_tibble object, or a character vector of sample ids. Default is
  to return all populations in the package dataset.

- pop:

  A character vector of canonical population codes to filter by. Default
  is `NULL` (no filtering).

- population:

  A character vector of human readable population labels to filter by. A
  population can carry more than one label (where source datasets named
  it differently), stored pipe-separated in `population_label`; a
  population is kept if any of its labels match. Default is `NULL` (no
  filtering).

- region:

  A character vector specifying regions to filter populations by.
  Default is NULL (no filtering).

- dataset:

  A character vector of source datasets to include (matched against
  `source_dataset`; see `dataset_information.Rtable` for the
  vocabulary). Default is all datasets in the package.

- temporal:

  A character vector restricting populations by age: one or both of
  `"modern"` (present-day) and `"ancient"`. Every population from HGDP,
  1000 Genomes, SGDP, HapMap and the individual-study datasets is
  `"modern"`; AADR contributes both. Default `NULL` (no filtering),
  which returns ancient populations alongside present-day ones – pass
  `temporal = "modern"` for analyses that assume a living population.

- location:

  One of `"origin"` (the default) or `"sampling"`, choosing which
  coordinate pair is returned in the `lat` and `lon` columns. The
  explicit `origin_*` and `sampling_*` columns are always returned as
  well.

- include:

  A character vector specifying which columns to include in the output.
  Default is all columns.

- exclude:

  A character vector specifying which columns to exclude from the
  output. Default is no columns excluded.

## Value

A data frame with population information.

## Details

Populations are also classified as present-day or ancient in the
`temporal` column, and can be filtered on it. This matters because the
AADR contributes several thousand ancient populations: an analysis that
assumes a living population, such as anything joined to ethnographic
data, wants `temporal = "modern"`.

Two sets of coordinates are stored for each population: `origin_lat`/
`origin_lon`, the group's ethnographic homeland, and `sampling_lat`/
`sampling_lon`, where the samples were actually collected. These differ
for diaspora cohorts – 1000 Genomes GIH was sampled in Houston but
originates in Gujarat – and for some populations an origin is not a
point at all, in which case the origin coordinates are `NA` and
`coord_note` says why. The `location` argument chooses which pair is
copied to the convenience columns `lat` and `lon`.
