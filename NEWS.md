# humangentools (development version)

* `population_label` now holds exactly one label per population. Where source
  datasets named the same group differently, the other names moved to a new
  `population_alt` column; `get_pop_info(population = )` matches either. The
  `population` column of `get_sample_information()` is now looked up from `pop`
  rather than stored per sample, so a population can no longer split in two when
  samples are counted by `(pop, population)` -- six populations did, including
  `GBR` (`British` and `British|English`) and `MXL`.

# humangentools 0.0.0.9000

* Initial functions for sample/population metadata lookup
  (`get_sample_information()`, `get_pop_info()`) and Axiom Human Origins
  SNP panel selection (`get_panel_information()`, `get_axiom_snps()`).
* `get_dplace_link()` returns a candidate link table joining populations
  in the bundled genetic datasets (HGDP, 1000 Genomes, Pierron_2014) to
  societies in dplaceR's D-PLACE data, so genetic and cross-cultural data
  can be combined via a common `soc_id`. Matching is a best-effort,
  fuzzy name/geography match rather than a guaranteed-correct crosswalk:
  every row carries a `confidence` tier (`"high"`/`"medium"`/`"low"`/
  `"none"`) and an unset `reviewed` flag, so `"medium"`/`"low"` rows
  should be checked before being relied on. See
  `data-raw/build_dplace_link.R` to regenerate the table.
