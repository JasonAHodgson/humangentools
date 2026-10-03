# Changelog

## humangentools 0.0.0.9000

- Initial functions for sample/population metadata lookup
  ([`get_sample_information()`](https://jasonahodgson.github.io/humangentools/reference/get_sample_information.md),
  [`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md))
  and Axiom Human Origins SNP panel selection
  ([`get_panel_information()`](https://jasonahodgson.github.io/humangentools/reference/get_panel_information.md),
  [`get_axiom_snps()`](https://jasonahodgson.github.io/humangentools/reference/get_axiom_snps.md)).
- [`get_dplace_link()`](https://jasonahodgson.github.io/humangentools/reference/get_dplace_link.md)
  returns a candidate link table joining populations in the bundled
  genetic datasets (HGDP, 1000 Genomes, Pierron_2014) to societies in
  dplaceR’s D-PLACE data, so genetic and cross-cultural data can be
  combined via a common `soc_id`. Matching is a best-effort, fuzzy
  name/geography match rather than a guaranteed-correct crosswalk: every
  row carries a `confidence` tier (`"high"`/`"medium"`/`"low"`/
  `"none"`) and an unset `reviewed` flag, so `"medium"`/`"low"` rows
  should be checked before being relied on. See
  `data-raw/build_dplace_link.R` to regenerate the table.
