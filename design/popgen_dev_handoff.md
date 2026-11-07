# Work order for HGT_POPGEN_DEV — study-design toolkit, first tranche

**From:** HGT_DESIGN_ARCHITECT · **Source spec:** `design/design_proposals.md`
**Scope of this handoff:** the six proposals that need no data the package does
not already have, or whose data is drafted and awaiting a Curator commit.
P8–P12 are not in this handoff; they are blocked and specified in the source
document for when they are not.

Read §1 and §2 of `design_proposals.md` before starting. §1 is the data audit
every signature below is fitted to; §2 is why the marker functions carry
caveat columns. The numbers quoted in the test cases here were computed from
the tables at `main` and are reproducible from them.

---

## Ordering and dependencies

Build in this order. P1 and P7 are independent and can go first in either
order. P3 is a dependency of P6. P2 depends on a Curator commit (§4.1 of the
spec) and should be started only once `dataset_ascertainment.Rtable` is on
`main` — but it is the smallest of the six and unblocks P9/P11 later, so it is
worth chasing.

```
P1 get_sample_overlap        (independent)
P7 get_panel_overlap         (independent; also fixes get_axiom_snps)
P3 find_pops_near            (independent)
P4 summarise_coverage        (independent)
P6 get_coverage_gaps         -> needs P3
P2 get_dataset_ascertainment -> needs Curator table
```

---

## Conventions these must follow

Taken from the existing exported functions; not negotiable without a
maintainer decision.

- Filters are vectorised character arguments defaulting to `NULL`, meaning no
  filtering. A filter naming something absent **warns and returns zero rows**;
  it does not error. (`get_dataset_info()` is the model.)
- Column selection is the `include`/`exclude` pair, intersected with the actual
  column names, `include` applied first.
- `samples` accepts a `gen_tibble` or a character vector of sample ids, exactly
  as in `get_pop_info()`, including the `stop()` on anything else.
- Return value is a plain `data.frame` with `rownames(x) <- NULL`.
  `get_panel_overlap(format = "matrix")` is the one sanctioned exception, and
  it needs a `@return` paragraph saying why.
- Tables are read with `system.file("extdata", ..., package = "humangentools")`
  and `read.table()`, with the same `quote=` handling the existing readers use
  for each file — note `get_dplace_link()` reads with `quote = ""` for a
  documented reason and `get_pop_info()` with `quote = "\""`.
- `@importFrom utils read.table`. Adding a dependency to `Imports` is a
  maintainer decision; none of these six needs one. Haversine is four lines of
  base R, and `stats::dist` is not needed.

---

## P1 — `get_sample_overlap()`

```r
get_sample_overlap(samples = NULL, dataset = NULL, pop = NULL,
                   temporal = NULL, min_datasets = 2L,
                   include = NULL, exclude = NULL)
```

Returns one row per `(id, source_dataset)` for individuals in at least
`min_datasets` distinct source datasets. Columns: `id`, `source_dataset`,
`source_id`, `data_type`, `pop`, `population`, `temporal`, `n_datasets`,
`n_source_ids`, `datasets`. Grouped by `id`, ordered by descending
`n_datasets` then `id`.

`dataset` restricts the candidate pool **before** overlap is computed.
`n_source_ids` counts distinct `source_id` values for the individual across
the pool, which surfaces within-dataset library duplication (AADR `.AG`/`.SG`)
without letting it inflate `n_datasets`.

**Tests.**

```r
expect_equal(nrow(get_sample_overlap(dataset = c("HGDP","SGDP"))), 70L)
expect_equal(length(unique(get_sample_overlap(dataset = c("AADR","KGP"))$id)), 2514L)
expect_equal(length(unique(get_sample_overlap(dataset = c("AADR","HGDP"))$id)), 929L)
expect_equal(length(unique(get_sample_overlap()$id)), 3471L)
expect_true(all(get_sample_overlap()$n_datasets >= 2))
expect_true("HGDP01414" %in% get_sample_overlap()$id)
expect_equal(nrow(get_sample_overlap(dataset = "HGDP")), 0L)
expect_equal(names(get_sample_overlap(dataset = "HGDP")), names(get_sample_overlap()))
expect_warning(get_sample_overlap(samples = "NOT_A_SAMPLE"))
```

**Known incompleteness you must document.** Matching is on the exact `id`
string. 267 AADR individuals carry `S_`-prefixed ids from the SGDP naming
convention and **none** matches an SGDP id, while the AADR `Individual ID`
column — which `sample_information.Rtable` does not carry — resolves at least
95 of them (`S_Dusun-1.DG` → `Dusun22`). State in `@details` that the AADR↔SGDP
figure is a lower bound, and reference spec §4.3.3. When the Curator adds
`aadr_individual_id`, this function gains a `match_basis` column and the
counts in the tests above will change — that is expected, not a regression.

**Left to you.** Whether `datasets` is pipe-separated (matching
`population_alt`'s convention) or a list-column — I recommend pipe-separated
for consistency and because the return is a plain data frame. Whether
`min_datasets` validates as a single non-negative integer or coerces.

---

## P7 — `get_panel_overlap()`, and the `get_axiom_snps()` fix

```r
get_panel_overlap(panel = NULL, format = c("long","matrix"),
                  drop_missing_ids = TRUE)
```

Long format: one row per unordered panel pair including self-pairs, with
`panel_a`, `panel_b`, `n_a`, `n_b`, `n_shared`, `jaccard`, `prop_of_a`,
`prop_of_b`, `ascertainment_a`, `ascertainment_b` (the discovery individual
from `Axiom_Panel_info.Rtable`). Matrix format: symmetric integer `n_shared`.

**`n_a` is the count of distinct non-sentinel ids present in the panel table,
not `Axiom_Panel_info$SNPs`.** Those disagree by 8–12% and the reason is
unresolved (spec §1.4, §4.3.3). Say so in `@details`.

**The defect to fix in the same PR.** `get_axiom_snps()` returns `"---"` as an
rs id. Add `drop_missing_ids = TRUE` to `get_axiom_snps()` with the same
default and semantics, and a `NEWS.md` entry. Keep the existing `panel = "all"`
behaviour and the existing warning on unknown panel names.

**Tests.**

```r
expect_false("---" %in% get_axiom_snps("all"))
expect_equal(length(get_axiom_snps("all")), 478619L)
expect_true("---" %in% get_axiom_snps("all", drop_missing_ids = FALSE))
expect_equal(length(get_axiom_snps("all", drop_missing_ids = FALSE)), 478620L)

cross <- function(x) x$n_shared[x$panel_a != x$panel_b]

expect_equal(cross(get_panel_overlap(panel = c("panel4","panel5"))), 15450L)
expect_equal(cross(get_panel_overlap(panel = c("panel4","panel5"),
                                     drop_missing_ids = FALSE)), 15451L)
expect_equal(cross(get_panel_overlap(panel = c("panel4","panel13"))), 58623L)
expect_equal(get_panel_overlap(panel = "panel4")$n_a, 146606L)
expect_true(all(subset(get_panel_overlap(), panel_a == panel_b)$jaccard == 1))
m <- get_panel_overlap(format = "matrix")
expect_equal(dim(m), c(13L, 13L)); expect_true(isSymmetric(m))
```

The 15450/15451 pair is the whole argument for `drop_missing_ids`: the single
extra id is the `"---"` sentinel, present in both panels and currently counted
as a shared marker.

**Left to you.** Whether to cache the 12 MB panel table in a package-local
environment across calls. Reading it is 0.32 s and a pairwise intersection
0.04 s, so an uncached `get_panel_overlap()` at `panel = NULL` does 13 reads
unless you read once per call — read once per call and do not bother with a
cross-call cache unless profiling says otherwise. Also yours: whether
`prop_of_a`/`prop_of_b` are worth carrying given `jaccard`.

---

## P3 — `find_pops_near()`

```r
find_pops_near(lat, lon, radius_km = 500,
               location = c("origin","sampling"),
               dataset = NULL, temporal = NULL, region = NULL,
               max_n = NULL, include = NULL, exclude = NULL)
```

Haversine on the chosen pair, R = 6371.0088 km. Returns `get_pop_info()`'s
columns plus `distance_km` (1 dp) and `location_basis`, ascending by distance.

**The attribute is the point of the function.** Populations with `NA` on the
chosen pair go to `attr(result, "unplaced")` — a data frame of `pop`,
`population_label`, `source_dataset`, `n_samples`, `coord_note` — and the
function emits one `message()` naming the count. Do not fall back from a `NA`
origin to the sampling point; the eight diaspora populations have a `NA` origin
deliberately.

Validate `lat`/`lon` as length-1 finite numerics within ±90/±180 and `stop()`
otherwise. `radius_km = Inf` is legal. If you prefilter on a bounding box for
speed, widen correctly across the antimeridian.

**Tests.**

```r
near <- find_pops_near(-18.879, 47.508, radius_km = 1500, location = "origin")
expect_equal(nrow(near), 2L)
expect_setequal(near$pop, c("Tanzania_SwahiliAADR","Tanzania_Swahili-oNearEastAADR"))
up <- attr(near, "unplaced")
expect_equal(nrow(up), 132L)
expect_equal(sum(up$n_samples), 2222L)
expect_true(all(c("BetsileoRAK","MerinaRAK","VezoRAK") %in% up$pop))

all_o <- find_pops_near(0, 0, radius_km = Inf, location = "origin")
expect_equal(nrow(all_o), 3988L)
all_s <- find_pops_near(0, 0, radius_km = Inf, location = "sampling")
expect_equal(nrow(all_s), 3996L)
expect_equal(nrow(attr(all_s, "unplaced")), 124L)

h_s <- find_pops_near(29.76, -95.37, 100, location = "sampling")$pop
expect_true(all(c("GIH","GIHHapMap") %in% h_s))
h_o <- find_pops_near(29.76, -95.37, 100, location = "origin")$pop
expect_false("GIH" %in% h_o)
expect_true("GIHHapMap" %in% h_o)   # known data defect; see spec 4.2.2

expect_error(find_pops_near(91, 0))
expect_error(find_pops_near(c(1,2), 0))
```

The `GIHHapMap` assertion documents a defect rather than endorsing it. Add a
comment in the test naming spec §4.2.2, and when the Curator fixes the
coordinates this test should fail and be updated — that is the intent.

**Left to you.** Whether `find_pops_near()` takes a `pop` argument as a
convenience alternative to `lat`/`lon` ("near this population"). I think it
should, but it introduces a question about which coordinate pair of the
reference population to use, so decide it with the maintainer rather than in
passing.

---

## P4 — `summarise_coverage()`

```r
summarise_coverage(by = c("region","dataset","temporal"),
                   dataset = NULL, region = NULL, temporal = NULL,
                   location = c("origin","sampling"),
                   region_column = c("region","region_kgp"))
```

One row per observed combination of `by`. Columns: the grouping variables,
then `n_pops`, `n_samples`, `n_pops_placed`, `n_pops_unplaced`,
`n_samples_unplaced`, `n_pops_no_region`, `lat_min`, `lat_max`, `lon_min`,
`lon_max`, `region_column_used`, `location_basis`.

`NA` grouping values become the literal string `"(unassigned)"` and are never
dropped. Bounding box columns are `NA` when nothing in the group is placed.
Do not add a deduplicated sample count — see the spec for why.

**Tests.**

```r
expect_equal(sum(summarise_coverage(by = "dataset")$n_pops), 4120L)
r <- summarise_coverage(by = "region")
expect_true("(unassigned)" %in% r$region)
expect_equal(r$n_pops[r$region == "(unassigned)"], 3899L)
tt <- summarise_coverage(by = "temporal")
expect_equal(tt$n_pops_no_region[tt$temporal == "ancient"], 3643L)
expect_equal(sum(summarise_coverage(by = "dataset")$n_pops_unplaced), 132L)   # location = "origin"
expect_equal(sum(summarise_coverage(by = "dataset", location = "sampling")$n_pops_unplaced), 124L)
d <- summarise_coverage(by = "dataset")
expect_equal(d$n_pops[d$dataset == "AADR"],
             get_dataset_info("AADR")$n_pops)
expect_true(all(summarise_coverage(by = "region")$region_column_used == "region"))
```

**Left to you.** Whether `by = character(0)` returns a single all-holdings row
or errors. I lean towards returning the single row — it is a useful "what do I
have in total" call — but it is a taste question.

---

## P6 — `get_coverage_gaps()`

```r
get_coverage_gaps(target, radius_km = 500,
                  location = c("origin","sampling"),
                  dataset = NULL, temporal = NULL, min_samples = 1L)
```

`target` is a data frame with `label`, `lat`, `lon` and an optional `region`.
One output row per target row, with `label`, `lat`, `lon`, `n_pops_within`,
`n_samples_within`, `n_pops_modern_within`, `n_pops_ancient_within`,
`nearest_pop`, `nearest_distance_km`, `gap_class`, `unplaced_candidates`,
`notes`.

`gap_class` ∈ `covered`, `ancient_only`, `thin`, `unplaced_nearby`, `gap`,
never `NA`. `unplaced_candidates` is populated **only** by exact `region`
match against `target$region`; there is no name-based geocoding, deliberately
(spec P6). `notes` carries `"origin inconsistent across datasets"` and
`"AADR find locations"` where applicable.

Build it on `find_pops_near()` rather than reimplementing the distance
calculation, so the two can never disagree.

**Tests.** These values were computed from the tables at `main`.

```r
tgt <- data.frame(
  label  = c("Highland Madagascar","Swahili coast","Borneo","Vanuatu"),
  lat    = c(-18.879, -6.165,   1.5, -17.730),
  lon    = c( 47.508, 39.202, 114.0, 168.320),
  region = c("Madagascar", NA, NA, NA),
  stringsAsFactors = FALSE)

g <- get_coverage_gaps(tgt, radius_km = 800, temporal = "modern")
expect_equal(nrow(g), 4L)
expect_false(any(is.na(g$gap_class)))

mad <- g[g$label == "Highland Madagascar", ]
expect_equal(mad$gap_class, "unplaced_nearby")
expect_equal(mad$n_pops_within, 0L)
expect_equal(length(strsplit(mad$unplaced_candidates, "|", fixed = TRUE)[[1]]), 8L)
expect_equal(mad$nearest_pop, "Tanzania_SwahiliAADR")
expect_equal(round(mad$nearest_distance_km), 1388)

van <- g[g$label == "Vanuatu", ]
expect_equal(van$gap_class, "ancient_only")
expect_equal(van$n_pops_modern_within, 0L)

g3 <- get_coverage_gaps(tgt, radius_km = 800)          # temporal = NULL
sw <- g3[g3$label == "Swahili coast", ]
expect_equal(sw$n_pops_within, 35L)
expect_equal(sw$n_pops_modern_within, 8L)
expect_equal(sw$n_pops_ancient_within, 27L)
expect_match(sw$notes, "origin inconsistent across datasets")
expect_equal(g3$n_pops_ancient_within[g3$label == "Vanuatu"], 18L)
expect_equal(g3$n_pops_within[g3$label == "Borneo"], 3L)

g2 <- get_coverage_gaps(tgt[, c("label","lat","lon")], radius_km = 800, temporal = "modern")
expect_equal(g2$gap_class[g2$label == "Highland Madagascar"], "gap")
expect_equal(g2$unplaced_candidates[g2$label == "Highland Madagascar"], "")

expect_equal(nrow(get_coverage_gaps(tgt[0, ], radius_km = 800)), 0L)

# must agree with find_pops_near
expect_equal(g3$n_pops_within[g3$label == "Highland Madagascar"],
             nrow(find_pops_near(-18.879, 47.508, 800)))
```

**Left to you.** Whether `target` also accepts a bare `lat`/`lon` pair as a
one-row convenience, and whether `min_samples` applies per population or to the
sum within radius. I lean per-population — `thin` should mean "no single
population here is big enough", which is the design-relevant statement — but
make the choice explicit in `@details` either way.

---

## P2 — `get_dataset_ascertainment()` (start once the Curator table lands)

```r
get_dataset_ascertainment(dataset = NULL, include = NULL, exclude = NULL)
```

A thin reader over `inst/extdata/dataset_ascertainment.Rtable`, matching
`get_dataset_info()`'s filter-and-warn behaviour exactly. Columns as drafted in
spec §4.1.

**Tests.**

```r
a <- get_dataset_ascertainment()
expect_setequal(a$dataset, get_dataset_info()$dataset)
expect_equal(get_dataset_ascertainment("AADR")$genotype_representation, "mixed")
expect_equal(get_dataset_ascertainment("KGP")$genotype_representation, "diploid")
expect_true(all(a$confidence %in% c("documented","inferred","unknown")))
expect_true(all(is.na(a$discovery_panel[a$confidence == "unknown"])))
expect_false(any(is.na(a$ascertainment_note)))
expect_warning(get_dataset_ascertainment("nope"))
expect_equal(nrow(suppressWarnings(get_dataset_ascertainment("nope"))), 0L)
```

**Left to you.** Whether `get_dataset_info()` should gain a `merge(..., by =
"dataset")` convenience or whether users join the two themselves. I would keep
them separate; a merged default makes `get_dataset_info()`'s return shape
depend on whether a second table is present.

---

## Decisions I am deliberately not making

These are implementation judgements and I would rather you made them with the
code in front of you than have the spec pre-empt them:

1. **Pipe-separated strings versus list-columns** for `datasets`,
   `unplaced_candidates`, `caveats` and `reasons`. The package precedent is
   pipe-separated (`population_alt`) and the return-a-plain-data-frame
   convention points the same way, but it is your call per function — just make
   it the same choice in all of them.
2. **The `unplaced` attribute versus a second return mode.** I specified an
   attribute plus a `message()` because it keeps the return a plain data frame.
   If you would rather have `find_pops_near(..., what = c("near","unplaced"))`
   or a list return, argue it in the PR — what I care about is that the
   unplaced populations cannot be missed, not the mechanism.
3. **Caching of the 12 MB Axiom table** across calls.
4. **Argument naming**: `drop_missing_ids` versus `drop_na_rsid`;
   `region_column` versus `region_source`; `min_samples` semantics in P6.
5. **Whether `summarise_coverage(by = character(0))` is legal.**
6. **Vignette placement** — whether these go in the existing
   `vignettes/humangentools.Rmd` or a new `study-design.Rmd`. Given that the
   six together tell one story (what do I have, where are the gaps, can I merge
   it), I would write a new vignette and cross-link, but the maintainer should
   decide whether that fragments the documentation.

## What I am not leaving to you

1. **No function in this tranche reports a marker intersection across
   datasets.** `get_panel_overlap()` is within one array's ascertainment panels
   and must say so in `@details`; it is not cross-platform overlap and must not
   be documented as though it were.
2. **The unplaced and unassigned counts are part of the return value**, not a
   documentation note. A coverage function that silently omits 132 populations
   is wrong in a way that is hard for a user to detect.
3. **`get_axiom_snps()` must stop returning `"---"`.**
