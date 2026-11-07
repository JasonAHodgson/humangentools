# humangentools: study-design capability — design proposals

**Author:** HGT_DESIGN_ARCHITECT · **Status:** proposal, for maintainer review
**Scope:** specification only. No package code is proposed here for direct merge;
accepted proposals go to HGT_POPGEN_DEV, new tables to HGT_DATA_CURATOR, new
source datasets to HGT_DATASET_SCOUT.

---

## 0. How to read this document

Twelve functions are proposed, ordered by the sequence I think they should be
built in — a weighting of scientific value against what the package can support
today. Each carries a **Data status** block that says plainly whether it can be
written against the tables currently in `inst/extdata/`, and if not, exactly
what has to be added first.

Six are **ready**: they need no data the package does not already ship. One
(`get_dataset_ascertainment`) needs a nine-row table whose contents I have
drafted in §4.1, so it is ready in all but the commit. Two are blocked on data
that already sits in the repository under `data-raw/` and only needs a rebuild
step. Three are blocked on data the package does not have at all, and one of
those — per-dataset marker manifests — is the single blocker that holds back
most of the merge-feasibility capability.

Every number quoted in this document was computed from the tables at
`main` as of the audit (§1); none is recalled or estimated.

---

## 1. What the package actually holds today

This section is the evidence base. Proposals that contradict it are wrong.

### 1.1 Scale

`population_information.Rtable` holds **4,120 populations**;
`sample_information.Rtable` holds **28,920 rows** covering **24,579 distinct
individuals**. The nine source datasets:

| dataset | populations | sample rows | genotyping |
|---|---:|---:|---|
| AADR | 3,897 (3,643 ancient / 254 modern) | 23,089 | 1240K capture, shotgun, Human Origins array |
| KGP | 26 | 3,202 | WGS |
| SGDP | 88 | 339 | WGS |
| HGDP | 56 | 952 | SNP array / WGS |
| HapMap | 12 | 463 | SNP array |
| KGP_IGSR | 10 | 102 | varies by IGSR collection |
| Rakotoarivony_etal | 8 | 288 | SNP array |
| Perry_etal_unconfirmed | 2 | 230 | not recorded |
| unconfirmed | 21 | 255 | not recorded |

### 1.2 Geography is thinner than it looks

- **124 populations (1,654 samples) carry no coordinates at all** — neither
  origin nor sampling. They include **all eight Madagascar populations**
  (`Rakotoarivony_etal`, 288 samples), both `Perry_etal_unconfirmed`
  populations (230 samples), all 21 `unconfirmed` populations (255 samples),
  47 modern and 41 ancient AADR populations, two HGDP, two HapMap and one SGDP
  population.
- **3,988 populations have both coordinate pairs**, but the two pairs differ by
  more than 50 km for exactly **three** of them: `GIH` (14,037 km),
  `STU` (8,852 km) and `ITU` (7,981 km). For every other populated row the
  origin and sampling coordinates are identical to the stored precision.
- **Eight populations have a sampling point but a deliberately `NA` origin**
  (`ACB`, `ACBIGSR`, `ASW`, `ASWHapMap`, `CEU`, `CEUHapMap`, `MEX`/`MXL`,
  `MEXHapMap`), each with a `coord_note` explaining that the origin is a
  distribution rather than a point.

The origin/sampling distinction is therefore **correct and worth building on,
but currently load-bearing for only eleven populations**. A coverage function
must not imply more resolution than that, and must report populations it could
not place rather than silently omitting them.

- **`region` is `NA` for 3,899 of 4,120 populations.** **No ancient population
  has a region at all.** Region-based coverage summaries cover 221 modern
  populations and nothing else.
- `region` and `region_kgp` **disagree for 65 populations** where both are
  present. Any function that groups by "region" must name which column it used.

### 1.3 There is no ethnolinguistic data

No column in any shipped table records language, language family, Glottocode,
or any linguistic classification. The nearest thing is the D-PLACE crosswalk
(`population_dplace_link.Rtable`, 228 rows: 128 high, 41 medium, 1 low, 58
unmatched), which reaches *society* identifiers for 170 populations and would
reach language family only through dplaceR, which is a `Suggests`-level
external dependency and is not installed in the development environment.

**The ethnolinguistic half of the coverage mandate has no data behind it.**
That is a finding, not an obstacle to be worked around with fuzzy matching on
population labels. §4.2 specifies what would have to be added.

### 1.4 Marker-level data exists for exactly one platform

`Axiom_Human_Origins_Panels_10-2025.Rtable` is **698,766 rows of `(RS, panel)`**
across thirteen ascertainment panels — and that is the entire marker-level
holding of the package. It has **no chromosome, no position, no alleles, no
genome build**.

Two defects found in the audit:

1. **22,717 rows carry the literal string `---` in the `RS` column** — markers
   on the array that have no rs identifier. `get_axiom_snps()` returns
   `unique(d$RS)`, so **`"---"` is returned to the user as though it were an
   rs id**. Confirmed in R: `"---" %in% get_axiom_snps("all")` is `TRUE`, and
   `length(intersect(get_axiom_snps("panel4"), get_axiom_snps("panel5")))`
   returns 15,451 where the true count of shared rs ids is 15,450. The
   sentinel inflates every intersection by exactly one and will inflate any
   cross-platform intersection the same way.
2. **`Axiom_Panel_info.Rtable` SNP counts do not match the panel table.**
   Claimed versus actual rows: panel1 111,970 / 103,262; panel4 163,313 /
   149,706; panel5 124,115 / 113,626; panel13 151,435 / 139,559 — an 8–12%
   excess across all thirteen panels. Either the counts come from a different
   array release than the id list, or the id list is incomplete. Until this is
   resolved, no function should report a panel size from either source as
   authoritative.

Excluding the sentinel there are **478,619 distinct rs ids** across the
panels, which overlap substantially (panel4 ∩ panel13 = 58,623 — expected,
since the Denisova–San panel is ascertained against the same San genome).
Reading the 12 MB file takes 0.32 s and a panel intersection 0.04 s in R, so
caching is a convenience, not a requirement.

### 1.5 Individuals are shared across datasets, and the package already knows it

**3,471 of 24,579 individuals appear in more than one source dataset:**

| pair | shared individuals |
|---|---:|
| AADR ∩ KGP | 2,514 |
| AADR ∩ HGDP | 929 |
| AADR ∩ SGDP | 46 |
| HGDP ∩ SGDP | 35 |
| AADR ∩ HapMap | 11 |
| AADR ∩ Rakotoarivony_etal | 3 |
| AADR ∩ KGP_IGSR | 3 |

This is already derivable from `sample_information.Rtable` with no new data,
and it is the highest-value thing the package can tell a user today that it
currently does not.

**It is also an undercount, and the package can tell how much.** 267 AADR
individuals carry an `id` beginning `S_`, the Simons Genome Diversity Project's
own naming convention, and **not one of them matches an SGDP `id` exactly** —
against 46 AADR↔SGDP matches found by exact matching. The AADR `.anno` in
`data-raw/` carries an `Individual ID` column that `sample_information.Rtable`
does not: `S_Dusun-1.DG` has `Individual ID = Dusun22`, which *is* an SGDP id in
the package. Matching through that column resolves **at least 95** further
AADR↔SGDP links. The duplicate counts above are therefore lower bounds, and
`get_sample_overlap()` must say so until §4.3.3 is done.

A related integrity problem: four populations have samples carrying more than
one `region` value — `ASW` (Africa/America), `ASWHapMap`, `DusunSGDP`
(East_Asia/Oceania) and `IgorotSGDP` — which contradicts
`get_sample_information()`'s documented invariant that population-level
attributes are looked up from `pop` and cannot split one population in two.

### 1.6 Per-sample data quality is in the repository but not in the package

`data-raw/v66.p1_1240K.aadr.PUB.anno` is 23,089 rows × 49 columns and carries,
**complete for every row**: SNPs hit on autosomal targets scored separately on
the 1240K and Human Origins SNP sets, mean coverage on 1.15M autosomal targets,
library type, damage rate, X- and mtDNA-contamination estimates, an `ASSESSMENT`
verdict, free-text assessment warnings, molecular sex, family relations, ROH
totals, Y and mtDNA haplogroups, date mean and standard deviation in years BP,
full date string, locality and political entity.

None of this reaches the user. `get_sample_information()` returns eight columns
and `data_type` is the only quality-adjacent one.

What that costs: **15.5% of AADR individuals have fewer than 100,000 SNPs hit on
the 1240K autosomal targets**, and the `ASSESSMENT` column flags 962
`Questionable`, 373 `CRITICAL`, 83 `PROVISIONAL_CRITICAL` and 7
`MERGE_QUESTIONABLE` individuals against 18,549 `Pass`. A user who merges "the
AADR Madagascar-adjacent individuals" without that information is merging a set
whose per-individual missingness spans two orders of magnitude.

Dates: present for all 23,089 rows, median 1,938 BP, 5th–95th percentile
0–7,478 BP, with **3,970 individuals dated to 0 BP** (present-day individuals
carried in the AADR release). The package's `temporal` column collapses this to
`modern`/`ancient`.

---

## 2. The scientific frame: why an intersection count is not a merge

Everything in §3 that touches markers rests on this section. A function that
returns `|A ∩ B|` and stops is not merely incomplete — it is misleading, because
the number looks like an answer.

**2.1 Ascertainment is not a nuisance parameter; it is the design.** The Axiom
Human Origins array exists precisely because array SNPs discovered in a few
European genomes distort every allele-frequency-based statistic in populations
distant from the discovery panel. Its thirteen panels are ascertained as
heterozygous sites in single named individuals — French (`HGDP00521`), San
(`HGDP01029`), Yoruba (`HGDP00927`), and so on — so that the ascertainment is
*known* and can be conditioned on (Patterson et al. 2012, Lazaridis et al.
2014). Mixing panels, or mixing a panel with a differently ascertained array,
destroys that property. Ascertainment bias measurably shifts population
divergence estimates and the site-frequency spectrum (Albrechtsen et al. 2010;
Lachance & Tishkoff 2013), and the effect is asymmetric with respect to the
discovery population — which is exactly the axis most anthropological-genetic
questions run along. A function that intersects a Human Origins dataset with an
Omni-class array and reports "412,000 shared SNPs" has silently produced a
marker set with an ascertainment profile that is the *union* of two biases and
belongs to neither published design.

**2.2 Coordinates must be reconciled before counts mean anything.** The
packaged marker data is rs ids with no build. rs ids are not stable
coordinates: they merge, they are deprecated, they are reused across builds,
and a nontrivial fraction of array content has no rs id at all — in this
package's own Axiom table, 22,717 rows (§1.4). An intersection on rs ids is a
lower bound of unknown slack. The defensible unit is `(build, chrom, pos,
sorted allele pair)`, which requires per-dataset manifests the package does not
ship. Until it does, every marker count the package reports must be labelled as
an rs-id intersection with its known failure mode stated.

**2.3 Strand is a silent error, not a loud one.** A/T and C/G sites are
strand-ambiguous: merging two datasets whose strand conventions differ flips
those genotypes without any error, producing a dataset that loads fine and is
wrong. The standard mitigations — allele-frequency-based strand resolution, or
simply excluding ambiguous sites — are what tools such as Genotype Harmonizer
exist to do (Deelen et al. 2014). The package cannot perform the fix (it holds
no genotypes), but it can and should report the *exposure*: what fraction of a
candidate intersection is strand-ambiguous, so a user knows how much of the
merge is at risk. That too requires alleles, hence manifests.

**2.4 Ancient capture data is not low-coverage modern data.** Reference bias —
the preferential alignment of reads carrying the reference allele — is
systematic in ancient DNA, varies with read length and damage, and is not
removed by filtering on depth (Günther & Nettelblad 2019). AADR 1240K data is
additionally **pseudo-haploid**: one randomly drawn allele per site per
individual. Heterozygosity cannot be computed from it; f-statistics can, but
only with the representation accounted for. The assays themselves differ
(1240K versus Twist 1.4M versus shotgun; Rohland et al. 2022), and the AADR
release mixes all of them in one table under a `data_type` column the package
already carries but does not interpret. **Merging a diploid WGS dataset with a
pseudo-haploid capture dataset is a decision, not a format conversion**, and
any merge-feasibility function must surface it as a first-class field rather
than as a footnote.

**2.5 Missingness interacts multiplicatively with intersection.** The marker
intersection is an upper bound on usable sites. After per-individual
missingness filtering the realised set can be far smaller, and for ancient data
it is: 15.5% of AADR individuals hold fewer than 100,000 of the ~1.15M 1240K
targets (§1.6). An intersection of 1.1M markers between two 1240K datasets is
a true statement about the platforms and a false impression about the analysis.

**2.6 MAF filtering is not neutral across an ascertainment boundary.** A MAF
threshold applied after merging removes different sites in different
populations, and preferentially removes exactly the sites that carry the
population-specific signal in groups distant from the discovery panel. The
threshold interacts with ascertainment, so it has to be reported alongside the
intersection, not applied to it silently.

**2.7 The same individual in two projects is not two observations.** 3,471
individuals in this package's holdings are duplicated across datasets (§1.5).
Merging AADR and KGP naively yields 2,514 individuals counted twice — in a PCA,
in an ADMIXTURE run, in the sample size of an f4 standard error. The AADR
deliberately re-releases 1000 Genomes and HGDP individuals as comparative data;
that is a feature of the resource and a trap for the merger. Cryptic relatedness
across projects is the generalisation of the same problem and needs genotypes to
detect (Manichaikul et al. 2010), but *exact individual duplication is already
detectable from this package's metadata alone*.

**2.8 Batch structure is confounded with population by construction.** Each
source dataset was genotyped in its own laboratory on its own platform, so
"dataset" and "population" are nearly collinear in these holdings. A PC that
separates HGDP Yoruba from 1000 Genomes YRI is a batch axis, not a population
axis. Where the same population exists in two datasets — and this package's
canonical-code convention makes those pairs explicit — that pair is the control
that lets a user measure the batch effect instead of assuming it away. A design
tool should point the user at those pairs.

**Consequence for the API.** No proposed function returns a bare intersection
size. Every marker-level return carries, as columns: the coordinate basis used,
the genome build (or `NA` with a reason), the strand-ambiguous fraction, the
genotype representation on each side, the ascertainment scheme on each side,
and a `caveats` character column naming each of the above that applies. The
`caveats` column is not documentation; it is part of the return value, and it
is what distinguishes this from a call to `intersect()`.

---

## 3. Proposals

### P1 — `get_sample_overlap()`

**Priority 1 · Effort: low · Status: READY · Area: merge feasibility**

```r
get_sample_overlap(
  samples  = NULL,
  dataset  = NULL,
  pop      = NULL,
  temporal = NULL,
  min_datasets = 2L,
  include  = NULL,
  exclude  = NULL
)
```

**Problem.** 3,471 individuals in the holdings are present in more than one
source dataset. A user who merges AADR with 1000 Genomes to add ancient
comparanda duplicates 2,514 individuals and has no warning from the package.
Duplication inflates apparent sample size, distorts PCA density, biases
ADMIXTURE towards the duplicated group, and understates the standard error of
any f-statistic computed across the merge.

**Why here.** The package is the only place that holds the cross-dataset
individual key. `id` is explicitly documented as the joining identifier and
`source_id` as the per-dataset key; this function is the use that documentation
implies. No other resource in the tidypopgen ecosystem can answer it.

**Semantics.** Returns one row per `(id, source_dataset)` pair for individuals
appearing in at least `min_datasets` distinct source datasets, with columns
`id`, `source_dataset`, `source_id`, `data_type`, `pop`, `population`,
`n_datasets`, `datasets` (pipe-separated, sorted), `temporal`. Rows are grouped
by `id` and ordered by descending `n_datasets` then `id`.

- `samples` accepts a `gen_tibble` or a character vector of sample ids, matching
  `get_pop_info()`; `NULL` considers all holdings.
- `dataset` restricts the *candidate pool* before overlap is computed, so
  `dataset = c("HGDP","SGDP")` asks "which individuals are in both of these"
  and returns 35 individuals, not the 929 that HGDP shares with AADR.
- `min_datasets = 1L` is legal and degenerates to a listing; it must not error.
- An individual duplicated *within* one dataset by library type (AADR `.AG` and
  `.SG` rows for the same `id`) has `n_datasets == 1` and is **excluded** at the
  default. This is deliberate: within-dataset library duplication is a
  different problem with a different fix, and conflating the two makes the
  count uninterpretable. It is surfaced instead by `n_source_ids`, an extra
  column that is always returned.
- Returns a zero-row data frame with the full column set when nothing overlaps.

**Worked scenario.** A user assembling a Madagascar-focused dataset wants AADR
Swahili-coast ancients alongside 1000 Genomes African populations as a mainland
reference.

```r
ov <- get_sample_overlap(dataset = c("AADR", "KGP"))
nrow(ov) / 2                      # 2514 individuals double-counted
table(ov$pop[ov$source_dataset == "KGP"])
```

Every one of those 2,514 individuals is a present-day 1000 Genomes sample that
the AADR re-releases as comparative data. The user drops the AADR copies and
keeps the KGP genotypes, or vice versa — but makes the choice knowingly.

**Acceptance criteria.**
1. `nrow(get_sample_overlap(dataset = c("HGDP","SGDP"))) == 70` (35 individuals × 2 rows).
2. `all(get_sample_overlap()$n_datasets >= 2)`.
3. `length(unique(get_sample_overlap(dataset = c("AADR","KGP"))$id)) == 2514`.
4. `get_sample_overlap(dataset = "HGDP")` returns 0 rows and 11 columns, and does not error.
5. `"HGDP01414" %in% get_sample_overlap()$id` — the documented SGDP/HGDP case.
6. `get_sample_overlap(samples = "NOT_A_SAMPLE")` returns 0 rows with a warning, not an error.
7. Column set and types are identical between the populated and empty returns.

**Failure modes a naive user hits.** Reading `nrow()` as a count of individuals
rather than of rows. Assuming the absence of an overlap row means the
individuals are unrelated, when it only means they are not the *same*
individual — cryptic relatedness needs genotypes. Assuming `dataset = NULL`
and `dataset = c("AADR","KGP")` give the same answer restricted; they do not,
because the first counts datasets outside the pair.

And the one the function must warn about itself: **treating the result as a
complete duplicate list.** Matching is on the exact `id` string, and the AADR
re-releases SGDP individuals under names that do not match (§1.5) — 267 AADR
ids begin `S_` and none matches an SGDP id, while at least 95 are resolvable
through a column the package does not currently ship. The `@details` must state
that the AADR↔SGDP count in particular is a lower bound, and the documentation
should be updated when §4.3.3 lands.

**Data status.** **Ready.** `sample_information.Rtable` as shipped. No new
data. Its AADR↔SGDP completeness improves with §4.3.3.

---

### P2 — `get_dataset_ascertainment()`

**Priority 2 · Effort: low · Status: needs one 9-row table (drafted in §4.1) · Area: merge feasibility**

```r
get_dataset_ascertainment(
  dataset = NULL,
  include = NULL,
  exclude = NULL
)
```

**Problem.** `get_dataset_info()` returns a free-text `genotyping` column:
`"1240K capture, shotgun, and Human Origins array (see data_type)"`,
`"SNP array / WGS"`, `"WGS"`, and `NA` for two datasets. No program can act on
that. Every merge-feasibility statement in §2 — ascertainment scheme, genotype
representation, build, reference allele policy — needs these as fields.

**Why here.** It is metadata about bundled datasets, which is the package's
remit, and it is the lookup table that turns `get_marker_overlap()` (P8) from a
set operation into a scientific statement. It is also the cheapest proposal
here: nine rows, hand-curated, no pipeline.

**Semantics.** One row per dataset in `get_dataset_info()$dataset`, with:

| column | type | meaning |
|---|---|---|
| `dataset` | chr | joins `get_dataset_info()` |
| `assay_class` | chr | `wgs`, `array`, `capture`, `mixed` |
| `ascertainment` | chr | `none`, `panel_ascertained`, `common_variant_array`, `targeted_capture`, `unknown` |
| `discovery_panel` | chr | free text, `NA` for `none` |
| `genotype_representation` | chr | `diploid`, `pseudohaploid`, `mixed` |
| `nominal_build` | chr | `GRCh37`, `GRCh38`, `mixed`, `NA` |
| `ref_allele_policy` | chr | `reference`, `ancestral`, `mixed`, `unknown` |
| `marker_manifest` | chr | `none`, `rs_only`, `full` — what the package can supply |
| `ascertainment_note` | chr | one sentence a user should read before merging |
| `confidence` | chr | `documented` / `inferred` / `unknown` |

The `confidence` column is required, not optional: three of the nine datasets
(`Perry_etal_unconfirmed`, `unconfirmed`, and `Rakotoarivony_etal` pending the
preprint's methods) cannot be filled in with documented values, and a table
that quietly guesses for them is worse than one that says `unknown`.

**Worked scenario.**

```r
a <- get_dataset_ascertainment(dataset = c("AADR", "KGP"))
a$genotype_representation   # "mixed"  "diploid"
```

`mixed` is the honest value for AADR: the release contains pseudo-haploid 1240K
and Twist capture (`.AG`, `.TW`), diploid shotgun (`.DG`), and Human Origins
array (`.HO`) data side by side, distinguished per individual by `data_type`.
The dataset-level answer is therefore "it depends on the individual", which is
precisely why P5 (per-sample QC) is also needed and why this column must be
allowed to say `mixed` rather than being forced to a single value.

**Acceptance criteria.**
1. `nrow(get_dataset_ascertainment()) == nrow(get_dataset_info())` and the `dataset` vectors match as sets.
2. `get_dataset_ascertainment(dataset = "AADR")$genotype_representation == "mixed"`.
3. `get_dataset_ascertainment(dataset = "KGP")$genotype_representation == "diploid"`.
4. `all(get_dataset_ascertainment()$confidence %in% c("documented","inferred","unknown"))`.
5. Every row with `confidence == "unknown"` has `NA` in `discovery_panel` and a non-`NA` `ascertainment_note`.
6. `get_dataset_ascertainment(dataset = "nope")` warns and returns 0 rows, matching `get_dataset_info()`'s behaviour.

**Failure modes.** Treating `nominal_build` as a guarantee — it records what the
project published, not what the user's local files contain after whatever
liftover history they have. Treating `ascertainment = "none"` (WGS) as "no bias"
— WGS has no SNP-discovery ascertainment but still has reference bias, coverage
heterogeneity and callset filtering. Reading `marker_manifest = "rs_only"` for
AADR as meaning the package ships AADR markers; it does not (see §4.3).

**Data status.** **Needs one new shipped table**,
`inst/extdata/dataset_ascertainment.Rtable`, nine rows. Contents drafted in
§4.1 — the Data Curator's job is review and commit, not research.

---

### P3 — `find_pops_near()`

**Priority 3 · Effort: low · Status: READY · Area: coverage mapping**

```r
find_pops_near(
  lat, lon,
  radius_km = 500,
  location  = c("origin", "sampling"),
  dataset   = NULL,
  temporal  = NULL,
  region    = NULL,
  max_n     = NULL,
  include   = NULL,
  exclude   = NULL
)
```

**Problem.** "What has been genotyped near here?" is the first question of
study design and the package cannot answer it. The coordinates are shipped; the
query is not.

**Why here.** The origin/sampling distinction is a property of this package's
curation and does not exist in the upstream resources. A generic spatial join
written by a user would pick one coordinate column arbitrarily and would not
know that for `GIH` the two answers are 14,037 km apart.

**Semantics.** Great-circle (haversine) distance on the chosen coordinate pair,
WGS84 mean earth radius 6371.0088 km. Returns `get_pop_info()`'s columns plus
`distance_km` (numeric, rounded to 1 dp) and `location_basis` (the `location`
value used), ordered by ascending `distance_km`.

Edge cases that must be handled explicitly:

- **Populations with `NA` on the chosen coordinate pair are not silently
  dropped.** They are returned in an attribute, `attr(x, "unplaced")`, a data
  frame of `pop`, `population_label`, `source_dataset`, `n_samples`,
  `coord_note`, and the function emits one `message()` naming the count. This
  is the single most important semantic in the proposal — see the worked
  scenario.
- `location = "origin"` with a population whose origin is deliberately `NA`
  (the eight diaspora populations of §1.2) places that population in
  `unplaced`, not at its sampling site. Falling back would be a quiet lie.
- Antimeridian crossing must be correct; haversine handles it, a bounding-box
  prefilter does not. If a prefilter is used for speed it must widen in
  longitude across ±180.
- Polar inputs (`abs(lat) > 89`) are legal.
- `radius_km = Inf` returns every placed population sorted by distance.
- `max_n` truncates *after* sorting and does not affect `unplaced`.
- Invalid `lat`/`lon` (outside ±90 / ±180, non-finite, length ≠ 1) is an error,
  not a warning.

**Worked scenario — and why it is the argument for P4 and §4.2.** A researcher
working on Madagascar asks what the package holds within 1,500 km of
Antananarivo.

```r
near <- find_pops_near(-18.879, 47.508, radius_km = 1500, location = "origin")
nrow(near)   # 2
near$pop     # "Tanzania_SwahiliAADR"  "Tanzania_Swahili-oNearEastAADR"
attr(near, "unplaced")   # 8 Madagascar populations, 288 samples
```

**Two ancient Tanzanian populations, and not one of the eight Madagascar
populations the package actually ships**, because `Rakotoarivony_etal` has no
coordinates (§1.2). Without the `unplaced` attribute this function would tell a
Madagascar specialist that the package holds nothing in Madagascar, which is
false by 288 samples. With it, the function tells the truth and names the fix.

**Acceptance criteria.**
1. `find_pops_near(-18.879, 47.508, 1500, location = "origin")` returns exactly 2 rows, both `source_dataset == "AADR"` (`Tanzania_SwahiliAADR`, `Tanzania_Swahili-oNearEastAADR`, both 1,388 km).
2. For an unrestricted call with `location = "origin"`: `nrow(attr(., "unplaced")) == 132` and `sum(attr(., "unplaced")$n_samples) == 2222`. With `location = "sampling"`: `124` and `1654`. The two differ by the eight populations whose origin is deliberately `NA` (§1.2).
3. `"BetsileoRAK" %in% attr(., "unplaced")$pop` for the Antananarivo call, and all eight `*RAK` populations are present in `unplaced`.
4. `find_pops_near(29.76, -95.37, 100, location = "sampling")$pop` contains both `"GIH"` and `"GIHHapMap"`; the same call with `location = "origin"` contains `"GIHHapMap"` but **not** `"GIH"`. This asymmetry is a data defect, not a design intent — see Failure modes and §4.2.2.
5. `find_pops_near(0, 0, radius_km = Inf, location = "origin")` returns `3988` rows; with `location = "sampling"`, `3996`.
6. Distances are symmetric and a population at the query point has `distance_km == 0`.
7. `find_pops_near(91, 0)` errors.
8. A query 100 km west of the antimeridian returns populations 100 km east of it.

**Failure modes.** Reading an empty result as "nothing genotyped there" without
checking `unplaced`. Using `location = "sampling"` for an ancestry question
(1000 Genomes GIH is a Houston cohort of Gujarati origin; the sampling point
answers a question about recruitment, not about the population). Assuming AADR
ancient coordinates are population homelands — the `coord_note` says
"archaeological find location (median over individuals in the group)" for 3,644
populations, which is where the bodies were, not where the group lived.

And a failure mode the package creates rather than inherits: **the origin
column is not internally consistent across datasets for the same population.**
`GIH` has its origin in Gujarat and its sampling point in Houston, correctly;
`GIHHapMap`, the same group in HapMap, has Houston in *both* columns.
`STUIGSR` and `ITUIGSR` carry Birmingham, UK in both columns with
`coord_note = "sampled within the group's homeland"` — the pipeline has
asserted that Birmingham is the homeland of Sri Lankan Tamils and of Telugu
speakers, while the corresponding `STU` and `ITU` rows place those origins in
Sri Lanka and Telangana. Across all co-labelled populations, **13 population
labels have member populations whose origins are more than 50 km apart**, two
of them by more than 8,000 km. `find_pops_near()` will faithfully report both
answers. §4.2.2 is the fix.

**Data status.** **Ready.** No new data. Its usefulness for Madagascar, the
maintainer's own research area, is blocked on §4.2.1.

---

### P4 — `summarise_coverage()`

**Priority 4 · Effort: low–medium · Status: READY · Area: coverage mapping**

```r
summarise_coverage(
  by       = c("region", "dataset", "temporal"),
  dataset  = NULL,
  region   = NULL,
  temporal = NULL,
  location = c("origin", "sampling"),
  region_column = c("region", "region_kgp")
)
```

**Problem.** A user choosing between dataset combinations needs to see, in one
table, how many populations and individuals each combination contributes per
geographic stratum — and how much of each stratum is unmappable. There is no
such view.

**Why here.** It is a summary of this package's own holdings. It is also the
function that makes §1.2's gaps visible to every user rather than to whoever
reads this document.

**Semantics.** Returns one row per observed combination of the grouping
variables in `by` (any subset of `region`, `dataset`, `temporal`; order of `by`
sets column order), with:

`n_pops`, `n_samples`, `n_pops_placed`, `n_pops_unplaced`,
`n_samples_unplaced`, `n_pops_no_region`, `lat_min`, `lat_max`, `lon_min`,
`lon_max`, `region_column_used`, `location_basis`.

- Populations with `NA` in a grouping variable are **retained** under the
  literal group label `"(unassigned)"`, never dropped. With `by = "region"`
  this is a 3,899-population row, and it should be impossible to miss.
- `region_column` is explicit because `region` and `region_kgp` disagree for 65
  populations (§1.2); the chosen column is echoed in `region_column_used` so a
  saved table is self-describing.
- `n_samples` sums `population_information$n_samples`, which counts
  population-dataset rows. Where an individual is in two datasets it is counted
  twice — by design, since the grouping is by dataset. The documentation must
  say so and point at `get_sample_overlap()`; a `n_samples_deduplicated` column
  is **deliberately not** provided, because deduplication is only well defined
  once the user has chosen which copy to keep.
- Bounding boxes are `NA` when no population in the group is placed.

**Worked scenario.**

```r
summarise_coverage(by = c("region", "temporal"), region_column = "region")
```

returns, among others, a `(unassigned) / ancient` row with **3,643 populations
and 3,899 populations total unassigned across both temporal values** — the
immediate, visible statement that the AADR's geography is unclassified in this
package, and that any region-stratified design built on `region` covers only
the 221 modern populations that have one.

**Acceptance criteria.**
1. `sum(summarise_coverage(by = "dataset")$n_pops) == 4120`.
2. `summarise_coverage(by = "region")` contains a row with `region == "(unassigned)"` and `n_pops == 3899`.
3. `subset(summarise_coverage(by = "temporal"), temporal == "ancient")$n_pops_no_region == 3643`.
4. `sum(summarise_coverage(by = "dataset")$n_pops_unplaced) == 132` at the default `location = "origin"`, and `== 124` at `location = "sampling"`; `n_samples_unplaced` sums to 2,222 and 1,654 respectively.
5. `summarise_coverage(by = "dataset")` and `get_dataset_info()` agree on `n_pops` for every dataset.
6. `region_column = "region_kgp"` changes at least 65 populations' group assignment relative to `"region"`.
7. `by` in any order returns the same rows, reordered columns only.

**Failure modes.** Summing `n_samples` across datasets and reporting it as a
cohort size (double-counts 3,471 individuals). Dropping `"(unassigned)"` before
plotting, which silently removes 95% of the populations. Comparing a `region`
summary with a `region_kgp` summary without noticing the vocabularies differ
(`West_Eurasia` and `Central_Asia_and_Siberia` exist in one and not the other).

---

### P5 — `get_sample_qc()`

**Priority 5 · Effort: medium · Status: BLOCKED on a Data Curator rebuild; source already in repo · Area: merge feasibility**

```r
get_sample_qc(
  samples  = NULL,
  dataset  = NULL,
  pop      = NULL,
  temporal = NULL,
  min_snps_1240k = NULL,
  assessment     = NULL,
  include = NULL,
  exclude = NULL
)
```

**Problem.** Merge feasibility at the dataset level is an upper bound; the
realised marker set is set by per-individual missingness. For the AADR that
varies by two orders of magnitude, and 15.5% of individuals hold fewer than
100,000 of ~1.15M 1240K targets. The package exposes none of it, so a user
cannot tell a 1.1M-SNP individual from a 20,000-SNP individual in the same
population.

**Why here.** The source file is **already in this repository**
(`data-raw/v66.p1_1240K.aadr.PUB.anno`, 23,089 × 49, complete for the relevant
columns). This is not an acquisition problem; it is a pipeline step that has
not been written. It is the highest value-per-unit-effort item in this
document after P1.

**Semantics.** One row per `(id, source_id)` — matching
`get_sample_information()`'s grain, so it joins cleanly — with:

`id`, `source_id`, `source_dataset`, `data_type`, `pop`,
`n_snps_1240k`, `n_snps_ho`, `mean_coverage_autosomal`,
`genotype_representation` (`diploid` / `pseudohaploid`, derived per individual
from the AADR suffix, not from the dataset), `library_type`, `damage_rate`,
`contamination_x_lower`, `contamination_mt_upper`, `assessment`,
`assessment_warnings`, `molecular_sex`, `date_bp_mean`, `date_bp_sd`,
`qc_source` (`"AADR v66.p1 .anno"`).

- **Non-AADR datasets return rows with `NA` in every QC column**, not no rows.
  A user filtering `min_snps_1240k = 1e5` must not silently lose every 1000
  Genomes sample; `NA` propagation has to be handled with an explicit
  `na_action` documented as "non-AADR rows are retained unless `drop_na_qc =
  TRUE`". (I leave `na_action` versus `drop_na_qc` as the implementer's naming
  call — see the handoff.)
- `assessment` filters on the AADR verdict vocabulary
  (`Pass`, `PROVISIONAL_PASS`, `MERGE_PASS`, `Questionable`,
  `PROVISIONAL_QUESTIONABLE`, `MERGE_QUESTIONABLE`, `CRITICAL`,
  `PROVISIONAL_CRITICAL`). The vocabulary must be read from the shipped table at
  runtime, never hard-coded, because AADR releases change it.
- `genotype_representation` is per-individual and **must not** be inherited from
  `get_dataset_ascertainment()`'s dataset-level `mixed`.
- Numeric parsing: the `.anno` writes thousands separators and `..` sentinels;
  these become `NA`, and the rebuild script must assert that no column parses to
  all-`NA`.

**Worked scenario.** Before committing to an f4 test using AADR Swahili-coast
individuals:

```r
qc <- get_sample_qc(pop = "Tanzania_SwahiliAADR")
summary(qc$n_snps_1240k)
table(qc$assessment)
```

If the eight individuals span 20,000–900,000 SNPs hit, the merged intersection
with a 600K array is set by the worst individuals, not by the platforms — and
the user either drops individuals or accepts a marker set an order of magnitude
smaller than `get_marker_overlap()` would report.

**Acceptance criteria.**
1. `nrow(get_sample_qc(dataset = "AADR")) == 23089`.
2. `all(is.na(get_sample_qc(dataset = "KGP")$n_snps_1240k))` and `nrow() == 3202`.
3. `mean(get_sample_qc(dataset = "AADR")$n_snps_1240k < 1e5, na.rm = TRUE)` is within 0.001 of 0.155.
4. `sum(get_sample_qc(dataset = "AADR")$assessment == "CRITICAL", na.rm = TRUE) == 373`.
5. `get_sample_qc(dataset = "AADR")$genotype_representation` takes both `diploid` and `pseudohaploid` values.
6. `get_sample_qc(min_snps_1240k = 1e5)` retains all 3,202 KGP rows at the default `drop_na_qc = FALSE`.
7. The `(id, source_id)` key is unique and is a superset of `get_sample_information()`'s AADR keys.
8. `sum(!is.na(get_sample_qc(dataset = "AADR")$date_bp_mean)) == 23089`.

**Failure modes.** Filtering on `assessment == "Pass"` and losing the 2,908
`PROVISIONAL_PASS` individuals that the AADR considers usable. Comparing
`n_snps_1240k` across data types — a `.DG` shotgun genome and a `.AG` capture
individual are scored on the same targets but produced by different assays, so
the counts are not exchangeable as a quality ranking. Taking `date_bp_mean` as
a calendar date; it is years before 1950 CE, with `date_bp_sd` that is often
large and a `Full Date` string that is the authoritative form.

**Data status.** **Blocked on the Data Curator.** Everything needed is in
`data-raw/v66.p1_1240K.aadr.PUB.anno`. Required: a new
`data-raw/rebuild/build_sample_qc.py` and a shipped
`inst/extdata/sample_qc.Rtable`. Estimated shipped size ~2–3 MB for 23,089 rows
× ~17 columns, comparable to the existing `sample_information.Rtable`. See
§4.3.1 for the exact source columns.

---

### P6 — `get_coverage_gaps()`

**Priority 6 · Effort: medium · Status: READY · Area: coverage mapping**

```r
get_coverage_gaps(
  target,
  radius_km = 500,
  location  = c("origin", "sampling"),
  dataset   = NULL,
  temporal  = NULL,
  min_samples = 1L
)
```

where `target` is a data frame of `label`, `lat`, `lon` — the places the user's
question requires coverage of. (Accepting a bare `lat`/`lon` pair as a
one-row convenience is the implementer's call.)

**Problem.** `find_pops_near()` answers "what is here". Study design needs the
complement: "where does my question need data that the holdings do not have,
and *why* don't they have it". The three reasons are different problems with
different fixes, and a user needs them separated: nothing was ever sampled
there; something was sampled but the package cannot place it; something is
there but only as ancient individuals, or only below a usable sample size.

**Why here.** It is the function that turns the package from a lookup into a
design tool, and it is the one the maintainer's stated direction most directly
asks for. It also makes the package's own curation debt visible to users, which
is the honest way to ship a resource with 132 unplaced populations.

**Semantics.** One row per `target` row, with:

`label`, `lat`, `lon`, `n_pops_within`, `n_samples_within`,
`n_pops_modern_within`, `n_pops_ancient_within`, `nearest_pop`,
`nearest_distance_km`, `gap_class`, `unplaced_candidates`, `notes`.

`gap_class` is an ordered factor-like character taking one of:

- `covered` — at least one population within `radius_km` meeting `min_samples`
  and the `temporal` filter;
- `ancient_only` — populations within radius, but none `modern`;
- `thin` — populations within radius but all below `min_samples`;
- `unplaced_nearby` — no placed population within radius, but one or more
  populations exist whose geography the package cannot resolve and whose
  `region` or `population_label` plausibly refers to the target (see below);
- `gap` — nothing within radius and no unplaced candidate.

`unplaced_candidates` is a pipe-separated list of `pop` codes drawn from the
unplaced set. The matching rule must be **conservative and explicit**, not
fuzzy name matching: a population is a candidate only if its `region` value
equals a `region` the user supplied for that target row, via an optional
`target$region` column. If `target` has no `region` column,
`unplaced_candidates` is empty and `gap_class` never takes the
`unplaced_nearby` value. I am deliberately refusing to geocode population
labels here; a tool that guesses that `MikeaRAK` is in Madagascar from its name
would be inventing data, and the package has a `region` column for exactly
this purpose.

`notes` carries, where applicable: `"origin inconsistent across datasets"` when
two populations within radius share a `population_label` but have origins more
than 50 km apart (13 labels, §P3 failure modes); `"AADR find locations"` when
all populations within radius are ancient AADR, whose coordinates are
archaeological find sites.

**Worked scenario.**

The design brief for an Austronesian-origins question about Madagascar: the
island itself, the East African coast, the proposed Austronesian source region,
and an Oceanic Austronesian comparison.

```r
tgt <- data.frame(
  label  = c("Highland Madagascar", "Swahili coast", "Borneo", "Vanuatu"),
  lat    = c(-18.879, -6.165,   1.5, -17.730),
  lon    = c( 47.508, 39.202, 114.0, 168.320),
  region = c("Madagascar", NA, NA, NA),
  stringsAsFactors = FALSE
)
get_coverage_gaps(tgt, radius_km = 800, temporal = "modern")
```

Computed against the current tables, the four rows return four different
answers, and each is a different instruction to the user:

| target | within 800 km (all / modern / ancient) | `gap_class` | what it means |
|---|---|---|---|
| Highland Madagascar | 0 / 0 / 0 | `unplaced_nearby` | eight `*RAK` populations, 288 samples, exist and cannot be placed — fix the coordinates (§4.2.1) |
| Swahili coast | 35 / 8 / 27 | `covered` | the best-covered target; `notes` carries `origin inconsistent across datasets` because `LWK` and `LWKHapMap` are the same Luhya population with origins 287 km apart |
| Borneo | 3 / 2 / 1 | `covered`, but see below | the two modern populations are `DusunSGDP` and `DusunAADR` — the same group, and the AADR copy is a re-release of SGDP individuals that §1.5's id matching does not catch |
| Vanuatu | 18 / 0 / 18 | `ancient_only` | eighteen AADR ancient populations and no present-day population within 800 km |

The nearest placed population to Antananarivo is `Tanzania_SwahiliAADR`, 1,388
km away across the Mozambique Channel. For the question as posed, the package's
answer is: your island has no mapped data, your mainland comparanda are mostly
ancient, your proposed Austronesian source region is represented by one group
counted twice, and your Oceanic comparison is entirely prehistoric. That is a
real study-design finding, and no single existing function states it.

**Acceptance criteria.**
1. The Highland Madagascar row of the worked scenario has `gap_class == "unplaced_nearby"`, `n_pops_within == 0`, eight pipe-separated `unplaced_candidates`, and `nearest_pop == "Tanzania_SwahiliAADR"` with `nearest_distance_km` 1388 ± 1.
2. The same call with `target$region` removed gives that row `gap_class == "gap"` and empty `unplaced_candidates`.
3. The Vanuatu row has `gap_class == "ancient_only"`, `n_pops_ancient_within == 18`, `n_pops_modern_within == 0`.
4. The Swahili coast row has `n_pops_within == 35`, `n_pops_modern_within == 8`, and `notes` containing `"origin inconsistent across datasets"`.
5. `nrow(get_coverage_gaps(tgt)) == nrow(tgt)` for any `tgt`, including zero rows.
6. `gap_class` is never `NA`.
7. A target placed exactly on a population's coordinates has `nearest_distance_km == 0` and `gap_class == "covered"` at `min_samples = 1`.
8. `get_coverage_gaps()` and `find_pops_near()` agree: `n_pops_within` equals `nrow(find_pops_near(lat, lon, radius_km, ...))` for the same filters.

**Failure modes.** Treating `gap` as "this population was never studied" — it
means *this package does not hold it*, and the right next step is the Dataset
Scout, not a field season. Treating `unplaced_nearby` as coverage; the data
exists but cannot enter a spatial analysis until §4.2.1 is done. Setting
`radius_km` large enough that everything is `covered` and concluding there are
no gaps.

---

### P7 — `get_panel_overlap()`

**Priority 7 · Effort: low · Status: READY (and fixes a live defect) · Area: SNP overlap**

```r
get_panel_overlap(
  panel  = NULL,
  format = c("long", "matrix"),
  drop_missing_ids = TRUE
)
```

**Problem.** The Axiom Human Origins panels are the package's only marker-level
data and their relationships are not exposed. Two things a user needs and
cannot get: how much two ascertainment panels share, and which panels are
appropriate to a given comparison. The panels are not disjoint — panel4
(San) and panel13 (Denisova–San) share 58,623 rs ids, which is expected given
that both are ascertained against the same San genome and is exactly the kind
of non-independence that invalidates treating panel membership as a clean
stratification.

**Why here.** The data is bundled; nothing else holds it; and the function is
the natural generalisation of `get_axiom_snps()`.

**It also fixes a defect.** `get_axiom_snps()` currently returns the literal
string `"---"` among its rs ids (§1.4), so every intersection a user computes
today is inflated by exactly one and every id-based join will silently fail to
match on 22,717 markers' worth of array content. `drop_missing_ids = TRUE` is
the default here, and `get_axiom_snps()` should gain the same argument with the
same default. Treating that as a bug fix rather than a breaking change is a
maintainer call; I recommend fixing it and noting it in `NEWS.md`.

**Semantics.** `format = "long"` returns one row per unordered panel pair
(including self-pairs) with `panel_a`, `panel_b`, `n_a`, `n_b`, `n_shared`,
`jaccard`, `prop_of_a`, `prop_of_b`, plus `ascertainment_a`/`ascertainment_b`
carrying the discovery individual from `Axiom_Panel_info.Rtable`.
`format = "matrix"` returns a symmetric integer matrix of `n_shared` with
panel names as dimnames — the one documented exception to the plain-data-frame
return convention, justified because a symmetric matrix is the natural shape
and the user will immediately pass it to `heatmap()` or `as.dist()`.

- `n_a`, `n_b` and `n_shared` all exclude `"---"` at the default.
- `jaccard = n_shared / (n_a + n_b - n_shared)`; self-pairs have `jaccard == 1`.
- The function must **not** report `Axiom_Panel_info$SNPs` as the panel size.
  Those values exceed the id lists by 8–12% (§1.4) and the discrepancy is
  unexplained. Until §4.3.4 resolves it, `n_a` is the count of distinct non-
  sentinel ids actually present, and the documentation says so explicitly.

**Worked scenario.** A user plans an f4 test of the form
`f4(San, Mbuti; Malagasy, Bantu)` and wants an ascertainment that is not
polarised toward either African group.

```r
get_panel_overlap(panel = c("panel4", "panel5", "panel1"))
```

panel4 (San, 146,606 ids) and panel5 (Yoruba, 111,707) share 15,450 — about 6%
of their union. Restricting to either panel conditions the statistic on
heterozygosity in one of the two African genomes in the test, which is the
ascertainment asymmetry Patterson et al. (2012) designed the panel structure to
let you avoid. panel1 (French, 101,566) shares 13,974 with panel4 and 12,932
with panel5 — near-symmetric with respect to both, which is why the French
panel is the conventional choice for African comparisons even though it is
ascertained in a population irrelevant to the question.

**Acceptance criteria.**
1. `get_panel_overlap(panel = c("panel4","panel5"), format = "long")` has `n_shared == 15450` for the cross-pair.
2. The same call with `drop_missing_ids = FALSE` gives `15451`.
3. `get_panel_overlap(panel = c("panel4","panel13"))$n_shared` includes `58623`.
4. `subset(get_panel_overlap(), panel_a == panel_b)$jaccard` is all `1`.
5. `format = "matrix"` is symmetric, integer, with `13 × 13` dimensions at `panel = NULL`.
6. `n_a` for panel4 is `146606`, not the `163313` in `Axiom_Panel_info.Rtable`.
7. `!("---" %in% get_axiom_snps("all", drop_missing_ids = TRUE))`.
8. `length(get_axiom_snps("all", drop_missing_ids = TRUE)) == 478619`.

**Failure modes.** Reading panel overlap as cross-*platform* overlap — these
are thirteen ascertainment strata of one array, not thirteen datasets.
Assuming a large `n_shared` means two panels are interchangeable; panel4 and
panel13 share 58,623 ids precisely because they are *not* independent.
Interpreting `jaccard` as a biological quantity; it is a property of the array
design.

---

### P8 — `get_marker_overlap()`

**Priority 8 · Effort: high · Status: BLOCKED on per-dataset marker manifests · Area: SNP overlap**

```r
get_marker_overlap(
  dataset,
  basis    = c("position", "rsid"),
  build    = "GRCh37",
  panel    = NULL,
  strand_ambiguous = c("flag", "exclude", "include"),
  format   = c("long", "matrix")
)
```

**Problem.** This is the function the capability area is named after, and the
one that decides whether two datasets can be combined at all. Everything in §2
is its specification.

**Why here.** Marker manifests are dataset metadata, which is the package's
remit, and the user's alternative is to download several gigabytes of genotype
files to count rows in a `.bim`.

**Semantics.** One row per unordered dataset pair, with:

`dataset_a`, `dataset_b`, `n_a`, `n_b`, `n_shared`, `jaccard`,
`basis_used`, `build_used`, `n_build_lifted_a`, `n_build_lifted_b`,
`n_strand_ambiguous`, `prop_strand_ambiguous`, `n_allele_mismatch`,
`genotype_representation_a`, `genotype_representation_b`,
`ascertainment_a`, `ascertainment_b`, `caveats`.

`caveats` is a pipe-separated character column, and the function is specified
by which strings it must be able to emit:

| caveat string | emitted when |
|---|---|
| `rsid_basis` | `basis = "rsid"` was used, so the count is a lower bound of unknown slack |
| `no_rsid_markers_excluded` | either manifest has markers without an rs id, with the count |
| `build_lifted` | coordinates were lifted; names source and target builds |
| `build_unknown` | a manifest has no declared build; the count is not trustworthy |
| `strand_ambiguous_present` | A/T or C/G sites are in the intersection |
| `allele_mismatch` | the same position carries incompatible allele pairs across manifests |
| `ascertainment_mismatch` | the two datasets have different `ascertainment` values in P2 |
| `representation_mismatch` | diploid on one side, pseudo-haploid on the other |
| `missingness_not_accounted` | **always** emitted, pointing at `get_sample_qc()` |
| `maf_not_applied` | **always** emitted |
| `sample_overlap_present` | the pair shares individuals, with the count from P1 |

The last three are unconditional for a reason. The intersection is an upper
bound; the realised analysis set depends on per-individual missingness and on
whatever MAF threshold the user applies, and the package cannot know either.
A `caveats` column that is empty would be a claim the package is not entitled
to make.

Further semantics:

- `basis = "position"` is the default and requires manifests with
  `(chrom, pos, allele1, allele2, build)`. `basis = "rsid"` is the fallback and
  is permitted so the function is usable the day the first manifest lands, but
  it always emits `rsid_basis`.
- `build` names the target build. Lifting is **not** performed by this package:
  manifests are stored pre-harmonised to a single build by the Data Curator,
  and `build_used` reports which. If a requested build is not available, error
  with the available ones rather than silently returning the other build.
- Alleles are compared as a sorted pair after strand normalisation, so that
  `A/G` and `T/C` on opposite strands match. A/T and C/G pairs cannot be
  resolved this way and are handled by `strand_ambiguous`.
- `strand_ambiguous = "flag"` (default) counts them, keeps them in `n_shared`,
  and emits the caveat. `"exclude"` removes them from `n_shared` — and the
  documentation must say that this is the conservative choice and what it
  costs. `"include"` suppresses the caveat and exists only so a user can make
  that choice explicitly.
- `panel` optionally restricts one side to Axiom panels, for the
  ascertainment-controlled case of P11.
- Three or more datasets return all pairs; the function does **not** return an
  n-way intersection by default, because the n-way intersection of
  differently-ascertained platforms is rarely the set anyone should analyse.
  An explicit `format = "nway"` may be added later if there is demand.

**Worked scenario (what it would return once unblocked).** A user wants to
place the eight Madagascar populations (`Rakotoarivony_etal`, SNP array) in a
frame with AADR 1240K ancients from the Swahili coast and 1000 Genomes African
WGS.

```r
get_marker_overlap(dataset = c("Rakotoarivony_etal", "AADR", "KGP"))
```

The scientifically interesting cell is not the three-way count. It is that
`Rakotoarivony_etal` × `AADR` returns `ascertainment_mismatch` (a commercial
array against targeted capture) and `representation_mismatch` (diploid array
calls against pseudo-haploid capture), while `AADR` × `KGP` returns
`sample_overlap_present` with 2,514 individuals. Those three strings are the
answer to "can I merge these"; the integer in `n_shared` is a budget, not a
verdict.

**Acceptance criteria.** These are written against the manifests of §4.3.2 and
are not testable until they exist.
1. Self-pairs have `jaccard == 1` and `n_shared == n_a`.
2. `"missingness_not_accounted"`, `"maf_not_applied"` appear in `caveats` for every row, including self-pairs.
3. A pair with a known shared individual set emits `sample_overlap_present` and the count matches `get_sample_overlap()`.
4. `strand_ambiguous = "exclude"` gives `n_shared` strictly less than `"include"` whenever `n_strand_ambiguous > 0`, and equal otherwise.
5. `basis = "rsid"` always emits `rsid_basis`; `basis = "position"` never does.
6. Requesting an unavailable `build` errors and the message names the available builds.
7. For a dataset whose manifest has no build, `build_unknown` is emitted and `build_used` is `NA`.
8. `n_shared` is symmetric: the value for `(a,b)` equals that for `(b,a)`.
9. Against the Axiom panel data alone, `get_marker_overlap(basis = "rsid", panel = c("panel4","panel5"))` reproduces P7's `15450`.

**Failure modes.** Quoting `n_shared` in a methods section as the number of
SNPs analysed. Reading a large rs-id intersection as merge-readiness when
`build_unknown` is set. Using `strand_ambiguous = "include"` because it gives a
bigger number. Taking the intersection of an ascertained array and a WGS
callset and computing heterozygosity or any diversity statistic on it — the
result is a property of the array's discovery panel (Albrechtsen et al. 2010;
Lachance & Tishkoff 2013), not of the populations.

**Data status.** **Blocked.** The package ships rs ids for one array and
nothing for any other dataset. See §4.3.2 for exactly what the Data Curator
would have to add, and §5 for what the Dataset Scout would have to source.
This is the single blocker that holds back P8, P9, P11 and P12.

---

### P9 — `assess_merge()`

**Priority 9 · Effort: high · Status: PARTIAL — ships useful today, completes with P8 · Area: merge feasibility**

```r
assess_merge(
  dataset,
  pop      = NULL,
  samples  = NULL,
  temporal = NULL,
  basis    = c("position", "rsid"),
  build    = "GRCh37",
  min_snps_1240k = NULL
)
```

**Problem.** P1, P2, P5 and P8 each answer one question. The user's actual
question is "can I combine these, and what will it cost me", and they should
not have to assemble the answer from four calls and know which caveats
interact.

**Why here.** It is the top-level entry point for the merge half of the
project-design toolkit, and the function the package's stated direction is
about.

**Semantics.** Returns a plain data frame with one row per unordered dataset
pair and a `verdict` column taking one of:

- `compatible` — same ascertainment class, same genotype representation, same
  build, no unresolved caveat beyond the unconditional ones;
- `compatible_with_care` — mergeable, but at least one of
  `ascertainment_mismatch`, `representation_mismatch`, `strand_ambiguous_present`
  or `sample_overlap_present` applies, each named in `reasons`;
- `not_recommended` — `build_unknown`, `allele_mismatch` above a threshold, or
  an intersection below a usable floor;
- `insufficient_information` — a manifest or an ascertainment record is
  missing, so no verdict can be formed.

`reasons` is pipe-separated and every verdict carries at least one. Columns
otherwise are P8's, plus `n_individuals_a`, `n_individuals_b`,
`n_individuals_shared`, `n_individuals_after_dedup`, and — when `min_snps_1240k`
is set and P5 is available — `n_individuals_passing_qc`.

**`insufficient_information` must be the default verdict in the absence of
evidence, and the function must never substitute an optimistic one.** As of
today, with no manifests, every pair involving a dataset other than the Axiom
panels returns `insufficient_information` with
`reasons = "no_marker_manifest_a|no_marker_manifest_b"`. That is the honest
state of the package, it is still more useful than nothing, and it makes the
blocker visible at exactly the point where a user would otherwise assume the
merge is fine.

**Worked scenario — what it returns today, before P8 lands.**

```r
assess_merge(dataset = c("AADR", "KGP", "HGDP"))
```

| pair | verdict | reasons |
|---|---|---|
| AADR–KGP | `insufficient_information` | `no_marker_manifest_a\|no_marker_manifest_b\|sample_overlap_present:2514\|representation_mismatch\|ascertainment_mismatch` |
| AADR–HGDP | `insufficient_information` | `…\|sample_overlap_present:929\|representation_mismatch\|ascertainment_mismatch` |
| KGP–HGDP | `insufficient_information` | `…\|ascertainment_mismatch` |

The verdict column is uninformative; the `reasons` column is not. A user
planning to merge AADR and 1000 Genomes learns, from metadata the package
already ships, that 2,514 of their individuals would be duplicated and that
they are about to mix pseudo-haploid capture with diploid WGS. That alone
justifies shipping the partial function rather than waiting for manifests.

**Acceptance criteria.**
1. Every row has a non-empty `reasons`.
2. With no manifests present, every pair's `verdict == "insufficient_information"` and `reasons` contains `no_marker_manifest_`.
3. `subset(assess_merge(c("AADR","KGP")), TRUE)$n_individuals_shared == 2514`.
4. `n_individuals_after_dedup == n_individuals_a + n_individuals_b - n_individuals_shared`.
5. `assess_merge(dataset = "KGP")` (one dataset) returns 0 rows with the full column set and a message, not an error.
6. A pair where both sides are `genotype_representation = "diploid"` and ascertainment matches does not emit `representation_mismatch`.
7. `verdict` is never `NA` and never `compatible` while any `no_marker_manifest_` reason is present.

**Failure modes.** Reading `compatible` as "merge will work" — it is a
statement about metadata, not about the user's files, which may be on a
different build or have been filtered already. Reading
`insufficient_information` as "incompatible". Merging on the verdict and
skipping `reasons`.

**Data status.** **Partial.** Ships today against `sample_information.Rtable`
plus P2's table; the marker columns fill in when §4.3.2 lands.

---

### P10 — `get_pop_language()`

**Priority 10 · Effort: medium · Status: BLOCKED — no linguistic data exists in the package · Area: ethnolinguistic coverage**

```r
get_pop_language(
  samples   = NULL,
  pop       = NULL,
  population = NULL,
  family    = NULL,
  subgroup  = NULL,
  dataset   = NULL,
  temporal  = NULL,
  min_confidence = c("low", "medium", "high"),
  include = NULL,
  exclude = NULL
)
```

**Problem.** The second mandated coverage area has no data behind it (§1.3).
This matters disproportionately for the questions this package exists to serve:
an Austronesian–Bantu admixture question about Madagascar is an ethnolinguistic
question first and a geographic one second, and Malagasy's position in the
Barito subgroup of Malayo-Polynesian is the hypothesis, not an annotation.

**Why here.** The package already carries the hard part — a curated,
canonical-code population table with a review workflow and a precedent for
best-effort crosswalks carrying confidence tiers (`get_dplace_link()`). A
language crosswalk is the same shape of object with a different target
vocabulary, and the `get_dplace_link()` documentation is the template for how
to be honest about it.

**Semantics.** One row per `(pop, glottocode)` candidate link:

`pop`, `population_label`, `source_dataset`, `glottocode`, `language_name`,
`iso639_3`, `family`, `family_glottocode`, `subgroup`, `match_method`,
`match_score`, `confidence`, `reviewed`, `review_note`.

Following `get_dplace_link()` exactly:

- **Review-complete**: every population appears at least once, with
  `glottocode = NA` and `confidence = "none"` where nothing matched. A user must
  be able to count what is unlinked.
- `confidence` in `high` / `medium` / `low` / `none`; `reviewed` is `FALSE`
  until a human confirms.
- Many-to-many is permitted and expected: multilingual populations, and
  Glottolog dialects nesting under a language.
- `family` and `subgroup` filters match at any depth of the Glottolog
  classification, so `family = "Austronesian"` returns Malagasy varieties and
  `subgroup = "Barito"` narrows to the subgroup the Malagasy-origin literature
  argues about.
- Ancient populations: AADR group IDs are archaeological culture labels
  (`Tanzania_Swahili`), not language names. The honest default is
  `confidence = "none"` for ancient populations unless a specific, cited
  linguistic attribution exists. **The function must not infer a language for a
  prehistoric population from its geography.** This needs to be stated in the
  documentation as a design decision, because the inference is tempting and
  wrong, and it means roughly 3,643 of 4,120 populations will carry
  `confidence = "none"` in the first release.

**Worked scenario (once unblocked).**

```r
get_pop_language(family = "Austronesian", min_confidence = "medium")
```

should return the eight Madagascar populations alongside any island Southeast
Asian populations in the holdings, which is the comparison set for a Malagasy
origins question — and, by what it does *not* return, show how thin the island
Southeast Asian coverage is. Paired with `get_coverage_gaps()`, that is a
Dataset Scout work order.

**Acceptance criteria.** Not testable until the table exists.
1. `length(unique(get_pop_language()$pop)) == nrow(get_pop_info())` — review-complete.
2. Every row with `confidence == "none"` has `NA` glottocode, family and subgroup.
3. `all(get_pop_language()$reviewed == FALSE)` in the first release.
4. `get_pop_language(family = "Austronesian")` includes all eight `*RAK` populations once the Rakotoarivony populations are linked.
5. `get_pop_language(pop = "YRI")$family` is `"Atlantic-Congo"` with `confidence == "high"`.
6. No ancient AADR population has `confidence` above `none` without a non-`NA` `review_note` citing a source.
7. Glottocodes validate against the Glottolog pattern `^[a-z0-9]{4}[0-9]{4}$`.

**Failure modes.** Using language family as a proxy for ancestry — the whole
intellectual point of this package's maintainer's field is that they come
apart, and Madagascar is the canonical case: an Austronesian language on a
population with substantial Bantu ancestry (Pierron et al. 2017). Filtering a
coverage analysis on `family` and losing the 3,643 ancient populations that
carry `none`. Treating `medium` links as settled, which `get_dplace_link()`'s
documentation already warns about for its own tiers.

**Data status.** **Blocked — nothing exists.** See §4.2.3.

---

### P11 — `check_design()`

**Priority 11 · Effort: high · Status: BLOCKED on P2 and P8 · Area: merge feasibility**

```r
check_design(
  target,
  outgroup  = NULL,
  statistic = c("f4", "f3", "pca", "admixture", "diversity"),
  dataset   = NULL,
  panel     = NULL,
  basis     = c("position", "rsid")
)
```

where `target` is a character vector of `pop` codes in the order the statistic
requires.

**Problem.** The caveats of §2 bite differently depending on what the user
intends to compute. Heterozygosity from array data is a statement about the
array (Lachance & Tishkoff 2013). f4 is comparatively robust to ascertainment
*when the ascertainment is symmetric with respect to the four populations* and
badly biased when it is not (Patterson et al. 2012). Pseudo-haploid data
cannot give heterozygosity at all. A design-stage function can check these
before the compute, which is when it is cheap.

**Why here.** The package knows the ascertainment (P2), the panels (P7), the
platforms, the duplicate individuals (P1) and the per-sample completeness (P5).
No other tool in the tidypopgen stack holds all five.

**Semantics.** Returns one row per check with `check`, `status`
(`pass`/`warn`/`fail`/`not_applicable`), `detail`, and `affected` (pipe-separated
`pop` codes). Checks, by statistic:

| check | applies to | fails when |
|---|---|---|
| `shared_markers` | all | intersection across the target populations' datasets is empty or below a floor |
| `ascertainment_symmetry` | `f4`, `f3` | the discovery panel of the marker set corresponds to one of the target populations or its close relatives |
| `representation_homogeneity` | `diversity`, `pca` | mixes pseudo-haploid and diploid |
| `heterozygosity_validity` | `diversity` | any input is ascertained or pseudo-haploid |
| `duplicate_individuals` | all | the same `id` appears in two target populations |
| `batch_confound` | `pca`, `admixture` | target populations come from different datasets with no co-genotyped population to anchor them |
| `missingness` | all | any target population has individuals below a completeness floor (needs P5) |
| `outgroup_placement` | `f3` | outgroup shares a dataset or ascertainment with a target in a way that breaks the test |

`ascertainment_symmetry` is the check that justifies the function. Using
panel5 (Yoruba-ascertained) for `f4(Yoruba, Mbuti; Malagasy, X)` conditions on
heterozygosity in a genome from one of the test populations; the panel
structure exists so that you can avoid it, and the package knows the discovery
individual for all thirteen panels.

`batch_confound` is the check this package is uniquely able to make, because
its canonical-code convention makes co-genotyped populations explicit:
`YorubaHGDP` and `YRI` are the same group on two platforms, and their presence
in a merge is what lets a user measure the platform axis rather than assume it
away (§2.8). The check should name those anchor pairs in `detail` when they
exist and report their absence when they do not.

**Worked scenario (once unblocked).**

```r
check_design(target = c("MerinaRAK", "VezoRAK", "YRI", "DaiHGDP"),
             statistic = "f4", panel = "panel1")
```

Expected: `duplicate_individuals` passes (no shared ids among these four);
`batch_confound` warns — four populations from three datasets, with `DaiHGDP`
and `CDX` available as an anchor pair the user has not included;
`ascertainment_symmetry` passes for panel1 (French-ascertained, not closely
related to any of the four); `shared_markers` returns
`insufficient_information` until manifests exist.

**Acceptance criteria.** Not testable until P8 lands, except:
1. `duplicate_individuals` fails for `target = c("YorubaHGDP", "YorubaSGDP")` only if they share an `id`; for `c("BantuKenyaHGDP","BantuKenyaSGDP")` it must fail, since `HGDP01414` is in both.
2. `heterozygosity_validity` fails for any target drawn from AADR with `genotype_representation == "pseudohaploid"`.
3. `status` is never `NA`; `not_applicable` is used rather than omitting a check, so the returned shape is stable across statistics.
4. `check_design(target = "YRI", statistic = "f4")` errors with a message naming the required arity.
5. Every `warn` or `fail` row has a non-empty `affected`.

**Failure modes.** Treating all-`pass` as validation of the analysis; these are
design-stage metadata checks and cannot see the user's genotypes. Running `f3`
with an outgroup from the same capture assay as a target and assuming the
shared reference bias cancels. Ignoring `batch_confound` because the populations
"are obviously different anyway" — that is the assumption the check exists to
test.

---

### P12 — `suggest_merge_sets()`

**Priority 12 · Effort: high · Status: BLOCKED on P5, P8, P9 · Area: cross-cutting (the headline capability)**

```r
suggest_merge_sets(
  require_pops   = NULL,
  require_region = NULL,
  require_family = NULL,
  near           = NULL,
  radius_km      = 500,
  temporal       = NULL,
  min_markers    = 1e5,
  min_samples_per_pop = 5L,
  max_datasets   = 3L,
  basis          = c("position", "rsid"),
  n              = 10L
)
```

**Problem.** This is the maintainer's stated direction stated as a function:
*which combination of available datasets best answers my question*. Everything
above is a component of it.

**Why here.** It is the reason the other eleven exist.

**Semantics.** Enumerates subsets of the available datasets up to size
`max_datasets`, evaluates each against the requirements, and returns the top
`n` ranked, one row per candidate set:

`datasets` (pipe-separated), `n_datasets`, `n_pops_covered`,
`n_pops_required_missing`, `pops_missing`, `n_individuals`,
`n_individuals_after_dedup`, `n_markers_shared`, `marker_basis`,
`min_pairwise_verdict` (the worst `assess_merge()` verdict in the set),
`caveats`, `score`, `score_components`.

Ranking must be **transparent and overridable, never a black box**. `score` is
a documented weighted product of normalised components — populations covered,
deduplicated individuals, markers retained, and a penalty for each
`assess_merge()` caveat class — and `score_components` returns them
individually so a user can re-rank on whichever they care about. The
documentation must state the weights and say that they encode a judgement the
user is entitled to disagree with.

Two refusals I want on the record, both of which make the function less
impressive and more defensible:

1. **It must not return a single "best" answer.** It returns a ranked table
   with the components exposed. The trade-off between marker count and
   population coverage is a scientific judgement, and a tool that resolves it
   silently is making the user's argument for them.
2. **It must not rank a set above `insufficient_information` on optimism.** A
   candidate set containing a dataset with no manifest gets `n_markers_shared =
   NA` and is ranked below any set with a real count, with the reason stated —
   not imputed from a platform's nominal marker count.

**Worked scenario (once unblocked).** A Madagascar Austronesian-origins design:

```r
suggest_merge_sets(
  require_region = c("Madagascar", "East_Asia"),
  near           = data.frame(label = "Swahili coast", lat = -6.165, lon = 39.202),
  radius_km      = 800,
  temporal       = "modern",
  min_markers    = 2e5
)
```

The informative output is not the top row. It is that **every candidate set
containing `Rakotoarivony_etal` fails `require_region = "Madagascar"` on
geography** — because those eight populations have no coordinates (§1.2) — and
that no candidate set reaches `min_markers` because no manifest exists. The
function would correctly report that the package cannot currently design this
study, which is the true answer and the strongest possible argument for the
data work in §4.

**Acceptance criteria.** Not testable until P5, P8 and P9 land.
1. `nrow(suggest_merge_sets(n = 5)) <= 5`.
2. `score` is monotone in each component holding others fixed, and `score_components` multiplied by the documented weights reproduces `score`.
3. A set containing a manifest-less dataset has `n_markers_shared` `NA` and ranks below any set with a non-`NA` value.
4. `require_pops` naming a population absent from the holdings produces a warning and that population in `pops_missing` for every candidate set.
5. `max_datasets = 1` returns only singleton sets and never a `min_pairwise_verdict`.
6. `n_individuals_after_dedup` agrees with `get_sample_overlap()` for every returned set.
7. Candidate sets are unordered: `AADR|KGP` and `KGP|AADR` never both appear.

**Failure modes.** Treating the top row as the study design. Setting
`min_markers` from a platform's advertised content rather than from a realised
intersection. Taking `n_individuals_after_dedup` as a usable sample size
without applying `get_sample_qc()` — for AADR-containing sets the two differ by
roughly the 15.5% of individuals below 100,000 SNPs, and by more at any
stricter threshold.

---

## 4. Data work this depends on

Consolidated, so the Data Curator can read one section. Ordered by
value-per-unit-effort.

### 4.1 `dataset_ascertainment.Rtable` — nine rows, drafted here

Required by P2, P8, P9, P11, P12. This is a review-and-commit task, not a
research task. Draft contents (`confidence` is part of the data, not a
disclaimer):

| dataset | assay_class | ascertainment | discovery_panel | genotype_representation | nominal_build | ref_allele_policy | marker_manifest | confidence |
|---|---|---|---|---|---|---|---|---|
| KGP | wgs | none | NA | diploid | GRCh38 | reference | none | documented |
| SGDP | wgs | none | NA | diploid | GRCh37 | reference | none | documented |
| HGDP | mixed | common_variant_array | Illumina 650Y / HGDP-CEPH; WGS re-release available | diploid | mixed | reference | none | documented |
| HapMap | array | common_variant_array | HapMap phase 3 (Affymetrix 6.0 + Illumina 1M) | diploid | GRCh37 | reference | none | documented |
| AADR | mixed | targeted_capture | 1240K / Twist 1.4M targets; Human Origins subset | mixed | GRCh37 | reference | none | documented |
| KGP_IGSR | mixed | unknown | varies by IGSR data collection | mixed | mixed | unknown | none | inferred |
| Rakotoarivony_etal | array | common_variant_array | to confirm from the preprint methods | diploid | unknown | unknown | none | inferred |
| Perry_etal_unconfirmed | unknown | unknown | NA | unknown | unknown | unknown | none | unknown |
| unconfirmed | unknown | unknown | NA | unknown | unknown | unknown | none | unknown |

Two rows need checking before commit rather than accepting my draft: `HGDP`,
where `nominal_build` is `mixed` because the package's HGDP samples may come
from either the array release or the 2020 WGS re-release and the package does
not currently record which; and `Rakotoarivony_etal`, where the array platform
should be read out of the preprint rather than inferred from
`get_dataset_info()$genotyping == "SNP array"`. That read is a Dataset Scout
task (§5).

`marker_manifest` is `none` for all nine today. It becomes `full` per dataset
as §4.3.2 lands, and the Axiom panels remain a separate `rs_only` resource
rather than a dataset manifest.

### 4.2 Coordinates, regions and language

#### 4.2.1 Coordinates for the 132 unplaced populations — **highest priority**

P3, P4, P6 and P12 all degrade, and for Madagascar questions they fail
outright. The 132 populations unplaced on the origin basis (2,222 samples)
break down as: 8 `Rakotoarivony_etal` (288 samples), 2
`Perry_etal_unconfirmed` (230), 21 `unconfirmed` (255), 47 AADR modern (657),
41 AADR ancient (89), 2 HGDP (7), 2 HapMap (126), 1 SGDP (2), plus the 8
diaspora populations whose `NA` origin is deliberate and correct and must stay.

The eight Madagascar populations are the ones that matter most to this
package's maintainer and are the easiest to fix: Betsileo, Betsimisaraka,
Diego, Merina, Mikea, Sakalava_Tsimihety, Temoro and Vezo are ethnic groups
with well-defined regions of Madagascar, and the Rakotoarivony preprint should
give sampling sites directly. Until they have coordinates, a Madagascar
specialist's own data is invisible to every geographic function in this
package.

#### 4.2.2 Origin-coordinate consistency across datasets — **a correctness bug**

Thirteen population labels have member populations whose origin coordinates
differ by more than 50 km, two of them by more than 8,000 km:

- `GIHHapMap` carries Houston in both the origin and sampling columns, while
  `GIH` correctly carries Gujarat as origin and Houston as sampling.
- `STUIGSR` and `ITUIGSR` carry Birmingham, UK in both columns with
  `coord_note = "sampled within the group's homeland"`, while `STU` and `ITU`
  correctly place those origins in Sri Lanka and in Telangana/Andhra Pradesh
  and note that sampling was in the UK.

The IGSR rows appear to have had the sampling location copied into the origin
column with a `coord_note` asserting it is the homeland. The remaining ten
labels differ by 60–1,139 km and are more likely to be genuine differences in
how two source projects located the same group, but they should be reviewed
and, where they are genuine, left alone with a `coord_note` saying so. A
regression test asserting that co-labelled populations' origins agree within a
tolerance, with a documented exception list, would stop this recurring.

#### 4.2.3 Region for ancient populations, and an ethnolinguistic table

`region` is `NA` for all 3,643 ancient populations. Assigning a region to them
is mechanical — the AADR `.anno` already in the repository carries `Locality`
and `Political Entity` for all 23,089 individuals — and it makes every
region-stratified function useful on the ancient holdings rather than on 221
modern populations. Note that the AADR's political entities are modern
countries, so the mapping is to the package's existing `region` vocabulary via
country, and the `coord_note` convention should record that.

For P10 a new `population_language.Rtable` is needed with the columns listed in
that proposal. The source is Glottolog (glottolog.org, CC-BY), which supplies
glottocode, name, ISO 639-3, family and full classification path; D-PLACE
societies already linked by `get_dplace_link()` carry language identifiers that
can seed the match for the 170 linked populations. Build it with the same
machinery as `build_dplace_link.R`: fuzzy name match plus geographic distance,
confidence tiers, review-complete, `reviewed = FALSE`. I would expect roughly
250–400 of the 477 modern populations to reach `high` or `medium`, and
essentially all ancient populations to sit at `none`.

### 4.3 Marker data

#### 4.3.1 `sample_qc.Rtable` — from a file already in the repository

Required by P5, P9, P12. Source: `data-raw/v66.p1_1240K.aadr.PUB.anno`,
columns (1-indexed as in the file header): 1 `Genetic ID`, 11 date mean BP,
12 date sd, 15 `Group ID`, 22 `Data type`, 24 mean coverage on 1.15M autosomal
targets, 27 SNPs hit on 1240k snpset, 28 SNPs hit on HO snpset, 31 molecular
sex, 41 damage rate, 43 ANGSD contamination CI, 44 hapConX contamination CI,
45 library type, 48 `ASSESSMENT`, 49 assessment warnings. Genotype
representation is derived from the `Genetic ID` suffix (`.DG` → diploid;
`.AG`, `.TW`, `.BY`, `.AA`, `.EC`, `.WGC` → pseudo-haploid; `.SG` → diploid;
`.HO` → diploid array; `REF` → reference haploid).

Parsing notes the rebuild script must handle: thousands separators in the
count columns, `..` as a missing sentinel, contamination columns stored as
free-text intervals rather than numbers, and the fact that the header row
contains embedded tabs inside quoted descriptions. Assert that no output column
parses to all-`NA`.

A design decision for the maintainer: whether this ships as a separate
`sample_qc.Rtable` or as added columns on `sample_information.Rtable`. I
recommend separate, because the QC columns are AADR-only and would be `NA` for
the 5,831 non-AADR rows, and because an AADR release bump should be able to
replace one file without touching the sample table's keys.

#### 4.3.2 Per-dataset marker manifests — **the central blocker**

Required by P8, and through it by P9, P11 and P12. One file per dataset:

```
dataset  chrom  pos  allele1  allele2  rsid  build
```

sorted, with a single declared build per file, alleles on a declared strand,
and `rsid` permitted to be `NA`. Plus a small index table recording, per
dataset, the build, the strand convention, the source of the manifest, the
release it describes, and a checksum.

Sizes, so the maintainer can judge the packaging question: 1240K is ~1.15M
rows, Human Origins ~600K, Omni-class arrays ~1–2.4M, and a 1000 Genomes WGS
callset is ~80M sites — which is not shippable in an R package and should be
represented by its accessible-genome definition plus a pointer, not by a site
list. A realistic first cut ships manifests for the fixed-content platforms
(1240K, Human Origins, Twist 1.4M, the HGDP and HapMap array content) at
perhaps 40–80 MB compressed, which is already past what CRAN would accept and
argues for a data-package split (`humangentoolsdata`) or on-demand download
with a local cache. **That packaging decision should be made before manifests
are built, not after**, because it determines whether the manifest format is a
shipped file or a download target.

Harmonisation is the Data Curator's job and must happen once, at build time,
not per call: all manifests lifted to a single declared build, alleles
normalised to a single strand where resolvable, strand-ambiguous sites flagged
in the manifest itself rather than recomputed. P8's `build_used` column reports
the result.

#### 4.3.3 Carry the AADR `Individual ID` into `sample_information.Rtable`

Required by P1, P9 and P12 for a complete duplicate count. 267 AADR
individuals carry `S_`-prefixed ids that match no SGDP id, while the AADR
`.anno` column 3 (`Individual ID`) resolves at least 95 of them to SGDP ids
already in the package (`S_Dusun-1.DG` → `Dusun22`). Column 2 (`Persistent
Genetic ID`) is a stable AADR key worth carrying for the same reason.

Add both as columns on `sample_information.Rtable` — `aadr_individual_id`,
`aadr_persistent_id`, `NA` for non-AADR rows — rather than altering `id`, which
is the documented join key and should not change meaning. `get_sample_overlap()`
then matches on `id` or on `aadr_individual_id` and reports which rule fired
in a `match_basis` column. Expect the AADR↔SGDP count to roughly triple and
the other pair counts to move somewhat; the exact figures should be recomputed
after the rebuild rather than taken from this document.

While there: four populations (`ASW`, `ASWHapMap`, `DusunSGDP`, `IgorotSGDP`)
have samples carrying more than one `region` value, which contradicts the
documented invariant that region is a property of `pop`. Either resolve them to
one value or, if the split is meaningful, record why in `coord_note` and amend
`get_sample_information()`'s documentation. A regression test asserting
`all(tapply(region, pop, function(x) length(unique(na.omit(x)))) <= 1)` would
hold the line.

#### 4.3.4 Reconcile `Axiom_Panel_info.Rtable` with the panel id list

Required by P7 and §1.4. The claimed SNP counts exceed the id lists by 8–12% for all
thirteen panels, and 22,717 rows carry `---` instead of an rs id. Determine
whether the counts come from a different array release, whether the `---` rows
account for part of the gap, and whether an Affymetrix probeset id is available
for the unidentified markers — if it is, adding a `probeset_id` column makes
those 22,717 markers joinable instead of lost. Until then, document the
discrepancy in the table rather than leaving it to be rediscovered.

---

## 5. For the Dataset Scout

Falling out of the audit rather than proposed independently:

1. **The Rakotoarivony et al. Madagascar preprint**: array platform, marker
   content, sampling sites and per-population sample sizes. It is the package's
   only Madagascar holding, it is the maintainer's research area, and it is
   currently the least documented dataset in the table — no coordinates, no
   confirmed platform, `reference` reading only "bioRxiv preprint".
2. **Island Southeast Asian coverage.** A Malagasy-origins design needs Barito
   and wider western Malayo-Polynesian source populations. The holdings have 43
   East Asian and 9 Oceanian populations; whether any are in the relevant
   source region is exactly what P10 plus P6 would answer, and the answer is
   likely to be "not enough".
3. **`Perry_etal_unconfirmed` and the 21 `unconfirmed` populations** — 485
   samples with no recorded provenance. Either resolve the source or consider
   whether they should be flagged more loudly than a dataset name.
4. **AADR v67+** if released, since `sample_qc.Rtable` would be built against
   whichever release is current.

---

## 6. References

Methods claims in §2 rest on these. Retrieved via PubMed; DOIs verified.

- Albrechtsen A, Nielsen FC, Nielsen R (2010). Ascertainment biases in SNP chips affect measures of population divergence. *Mol Biol Evol* 27:2534–2547. doi:10.1093/molbev/msq148
- Deelen P et al. (2014). Genotype harmonizer: automatic strand alignment and format conversion for genotype data integration. *BMC Res Notes* 7:901. doi:10.1186/1756-0500-7-901
- Günther T, Nettelblad C (2019). The presence and impact of reference bias on population genomic studies of prehistoric human populations. *PLoS Genet* 15:e1008302. doi:10.1371/journal.pgen.1008302
- Lachance J, Tishkoff SA (2013). SNP ascertainment bias in population genetic analyses: why it is important, and how to correct it. *BioEssays* 35:780–786. doi:10.1002/bies.201300014
- Lazaridis I et al. (2014). Ancient human genomes suggest three ancestral populations for present-day Europeans. *Nature* 513:409–413. doi:10.1038/nature13673
- Mallick S et al. (2016). The Simons Genome Diversity Project: 300 genomes from 142 diverse populations. *Nature* 538:201–206. doi:10.1038/nature18964
- Mallick S et al. (2024). The Allen Ancient DNA Resource (AADR): a curated compendium of ancient human genomes. *Sci Data* 11:182. doi:10.1038/s41597-024-03031-7
- Manichaikul A et al. (2010). Robust relationship inference in genome-wide association studies. *Bioinformatics* 26:2867–2873. doi:10.1093/bioinformatics/btq559
- Patterson N et al. (2012). Ancient admixture in human history. *Genetics* 192:1065–1093. doi:10.1534/genetics.112.145037
- Pierron D et al. (2017). Genomic landscape of human diversity across Madagascar. *PNAS* 114:E6498–E6506. doi:10.1073/pnas.1704906114
- Rohland N et al. (2022). Three assays for in-solution enrichment of ancient human DNA at more than a million SNPs. *Genome Res* 32:2068–2078. doi:10.1101/gr.276728.122
- Byrska-Bishop M et al. (2022). High-coverage whole-genome sequencing of the expanded 1000 Genomes Project cohort. *Cell* 185:3426–3440. doi:10.1016/j.cell.2022.08.004

Glottolog (glottolog.org) and D-PLACE (d-place.org) are cited in §4.2.3 as data
sources rather than methods references.

---

## 7. Summary of status

| # | function | area | effort | status |
|---|---|---|---|---|
| P1 | `get_sample_overlap()` | merge | low | ready |
| P2 | `get_dataset_ascertainment()` | merge | low | needs §4.1 (drafted) |
| P3 | `find_pops_near()` | coverage | low | ready |
| P4 | `summarise_coverage()` | coverage | low–medium | ready |
| P5 | `get_sample_qc()` | merge | medium | needs §4.3.1 (source in repo) |
| P6 | `get_coverage_gaps()` | coverage | medium | ready |
| P7 | `get_panel_overlap()` | SNP overlap | low | ready |
| P8 | `get_marker_overlap()` | SNP overlap | high | blocked on §4.3.2 |
| P9 | `assess_merge()` | merge | high | partial; completes with P8 |
| P10 | `get_pop_language()` | ethnolinguistic | medium | blocked on §4.2.3 |
| P11 | `check_design()` | merge | high | blocked on P2, P8 |
| P12 | `suggest_merge_sets()` | cross-cutting | high | blocked on P5, P8, P9 |

Five are implementable today with no new data (P1, P3, P4, P6, P7); P2 needs a
table whose contents are drafted above. Those six are the work order in
`popgen_dev_handoff.md`.
