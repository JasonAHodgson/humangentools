# Describes the genotype datasets bundled in humangentools

`get_dataset_info` returns one row per source genotype dataset, with a
description, the genotyping technology, and how many populations and
samples it contributes. The `dataset` column is the vocabulary used by
the `source_dataset` column of
[`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md)
and
[`get_sample_information()`](https://jasonahodgson.github.io/humangentools/reference/get_sample_information.md),
and by their `dataset` arguments.

## Usage

``` r
get_dataset_info(dataset = NULL)
```

## Arguments

- dataset:

  A character vector of dataset names to filter by. Default `NULL` (all
  datasets).

## Value

A data frame with columns `dataset`, `description`, `genotyping`,
`n_pops`, `n_samples` and `reference`.

## Details

Datasets are kept separate because their genotypes are not
interchangeable: they differ in platform, ascertainment and error
profile. The same individual appears under more than one dataset where
more than one project genotyped them, which is why
[`get_sample_information()`](https://jasonahodgson.github.io/humangentools/reference/get_sample_information.md)
can return several rows for one sample.
