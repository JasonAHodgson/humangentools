# Get candidate links between humangentools populations and dplaceR societies

`get_dplace_link` returns a table linking populations in humangentools'
bundled genetic datasets to societies in dplaceR's `dplace_societies`,
so genetic and cross-cultural (D-PLACE) data can be joined on a common
`soc_id`. Rows are keyed by `pop`, the canonical population code used
throughout the package (see
[`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md)).

## Usage

``` r
get_dplace_link(
  pop = NULL,
  population = NULL,
  dataset = NULL,
  confidence = NULL,
  min_marriage_vars = NULL,
  include_unmatched = FALSE
)
```

## Arguments

- pop:

  A character vector of canonical population codes to filter by. Default
  is `NULL` (no filtering).

- population:

  A character vector of human readable population labels to filter by
  (matched against `population_label`). Default is `NULL`.

- dataset:

  A character vector of source datasets to filter by (matched against
  `source_dataset`). Default is `NULL`.

- confidence:

  A character vector of confidence tiers to include (one or more of
  `"high"`, `"medium"`, `"low"`, `"none"`). Default is `NULL` (no
  filtering, i.e. all tiers included).

- min_marriage_vars:

  An optional integer; keep only rows where at least this many of the
  ten marriage/descent variables are coded for the linked society.
  Default `NULL` (no filtering). Rows with no linked society are dropped
  when this is set.

- include_unmatched:

  Logical; if `FALSE` (the default), rows with no matched society
  (`confidence == "none"`) are dropped. Set to `TRUE` to keep them, e.g.
  to see which populations have no D-PLACE candidate at all.

## Value

A data frame with columns `pop`, `population_label`, `source_dataset`,
`soc_id`, `society_name`, `society_region`, `match_method`,
`match_score`, `geo_distance_km`, `confidence`, `n_marriage_vars_coded`,
and `reviewed`.

## Details

The link table is a **best-effort, fuzzy candidate match**, not a
guaranteed-correct crosswalk: the two resources use different naming
conventions and ethnographic granularity, with no shared ID between
them. Matching can be many-to-many (one population can link to several
societies, and vice versa), and every population appears at least once
(with `soc_id = NA` if nothing matched), so the table is
review-complete. Each row carries a `confidence` tier (`"high"` for an
exact name match, `"medium"`/`"low"` for fuzzy matches of decreasing
reliability, `"none"` for populations with no candidate match at all)
and a `reviewed` flag that is `FALSE` for every row until a human
confirms it. Users should treat `"medium"`/`"low"` rows as candidates to
check, not settled fact, before relying on them – especially for any
analysis where a wrong link would matter. The low tier in particular
contains confident-looking nonsense (`ASW` to "American Samoans", `CDX`
to "Dani"), which is exactly what the tiers exist to flag.

Because a society link is a property of the population rather than of
the sequencing project, the same `soc_id` is attached to every canonical
code sharing a population label: `YorubaHGDP`, `YRI` and `YRIHapMap` all
link to the same society.

`n_marriage_vars_coded` counts how many of ten D-PLACE Ethnographic
Atlas marriage and descent variables (EA009, EA012, EA015, EA018, EA020,
EA023–EA026, EA043) have a genuine coded observation for the linked
society, missing-data sentinels excluded. It is a quick way to see
whether a link is analytically usable before committing to it.

The table is regenerated from `data-raw/build_dplace_link.R` (requires
dplaceR to be installed); see that script for the exact matching and
scoring rules.
