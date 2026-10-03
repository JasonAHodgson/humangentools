# Package index

## Datasets

The bundled genotype datasets, and the vocabulary used by every
`source_dataset` column and `dataset` argument in the package.

- [`get_dataset_info()`](https://jasonahodgson.github.io/humangentools/reference/get_dataset_info.md)
  : Describes the genotype datasets bundled in humangentools

## Sample and population metadata

Look up populations and samples by canonical code, label, region, source
dataset or age. Populations are keyed by a code that carries the
genotype dataset, so a group genotyped by several projects stays
separable, and coordinates are recorded separately for a group’s origin
and its sampling location.

- [`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md)
  : Returns a table of information about populations in a given dataset.
- [`get_sample_information()`](https://jasonahodgson.github.io/humangentools/reference/get_sample_information.md)
  : Gets sample information for a list of sample IDs

## SNP panels

Browse and select SNP sets for the Axiom Human Origins array
ascertainment panels. Panel membership is not exclusive, which matters
for any comparison of diversity between populations.

- [`get_panel_information()`](https://jasonahodgson.github.io/humangentools/reference/get_panel_information.md)
  : Provides Axiom Human Origins panel ascertainment information
- [`get_axiom_snps()`](https://jasonahodgson.github.io/humangentools/reference/get_axiom_snps.md)
  : Get a list of SNP RS ids from desired Axiom Human Origins
  ascertainment Panels

## Cross-referencing with dplaceR

The hand-reviewed crosswalk linking populations to societies in
dplaceR’s D-PLACE data, for combined genetic and cross-cultural
analyses.

- [`get_dplace_link()`](https://jasonahodgson.github.io/humangentools/reference/get_dplace_link.md)
  : Get candidate links between humangentools populations and dplaceR
  societies
