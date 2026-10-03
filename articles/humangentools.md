# Getting started with humangentools

``` r

library(humangentools)
```

humangentools bundles sample, population and SNP ascertainment panel
metadata for widely used human genomics reference datasets, so that
[tidypopgen](https://cran.r-project.org/package=tidypopgen) analyses can
be annotated and filtered without tracking that metadata down each time.
It is built to work alongside
[dplaceR](https://jasonahodgson.github.io/dplaceR/) for cross-cultural
context on the same populations.

## Populations are keyed by a canonical code

The central idea is the `pop` code. The same human group is often
genotyped by several projects, and those genotypes are **not
interchangeable**: they come from different arrays or sequencing
platforms, with different ascertainment and different error profiles. So
each (group, dataset) pair gets its own code, following the convention
of `kgp::allmeta` — a bare three-letter code for 1000 Genomes
populations, and `<Name><DATASET>` elsewhere.

``` r

get_pop_info(population = "Yoruba")[, c("pop", "source_dataset", "n_samples")]
#>          pop source_dataset n_samples
#> 1        YRI            KGP       178
#> 2  YRIHapMap         HapMap        32
#> 3 YorubaHGDP           HGDP        22
#> 4 YorubaAADR           AADR        24
```

Four codes, four genotype datasets, one ethnic group. Pooling them under
a single label would mix array and sequence data, and any resulting
difference in heterozygosity or call rate would look like biology.

The datasets available:

``` r

get_dataset_info()[, c("dataset", "n_pops", "n_samples", "genotyping")]
#>                  dataset n_pops n_samples
#> 1                   AADR   3897     23089
#> 2                   HGDP     56       952
#> 3                 HapMap     12       463
#> 4                    KGP     26      3202
#> 5               KGP_IGSR     10       102
#> 6 Perry_etal_unconfirmed      2       230
#> 7     Rakotoarivony_etal      8       288
#> 8                   SGDP     88       339
#> 9            unconfirmed     21       255
#>                                                        genotyping
#> 1 1240K capture, shotgun, and Human Origins array (see data_type)
#> 2                                                 SNP array / WGS
#> 3                                                       SNP array
#> 4                                                             WGS
#> 5                                  varies by IGSR data collection
#> 6                                                            <NA>
#> 7                                                       SNP array
#> 8                                                             WGS
#> 9                                                            <NA>
```

## Sample metadata

[`get_sample_information()`](https://jasonahodgson.github.io/humangentools/reference/get_sample_information.md)
looks up the canonical code, population label, region and source dataset
for one or more sample IDs:

``` r

get_sample_information("HGDP00001")
#>          id    source_id data_type        pop population       region
#> 1 HGDP00001    HGDP00001      <NA> BrahuiHGDP     Brahui Central_Asia
#> 2 HGDP00001 HGDP00001.DG        DG BrahuiAADR     Brahui         <NA>
#>   source_dataset temporal
#> 1           HGDP   modern
#> 2           AADR   modern
```

Two rows for one sample, because the AADR regenotyped this individual on
the 1240K panel. `id` identifies the person and joins across datasets;
`source_id` is the contributing dataset’s own key; `data_type` records
the AADR library type. Rows are unique on `(source_id, pop)`, not on
`(id, pop)`.

**If you are attaching metadata to a `gen_tibble`, pass `dataset`** —
otherwise you get more rows than samples and any column-wise assignment
silently misaligns:

``` r

get_sample_information("HGDP00001", dataset = "HGDP")
#>          id source_id data_type        pop population       region
#> 1 HGDP00001 HGDP00001      <NA> BrahuiHGDP     Brahui Central_Asia
#>   source_dataset temporal
#> 1           HGDP   modern
```

An ID that is not found is kept with `NA` fields and a warning;
`na.fill = FALSE` drops it instead:

``` r

get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill = FALSE)
#> Warning in get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill =
#> FALSE): The following IDs were not found in the sample information data:
#> not-a-real-id
#>          id    source_id data_type        pop population       region
#> 1 HGDP00001    HGDP00001      <NA> BrahuiHGDP     Brahui Central_Asia
#> 2 HGDP00001 HGDP00001.DG        DG BrahuiAADR     Brahui         <NA>
#>   source_dataset temporal
#> 1           HGDP   modern
#> 2           AADR   modern
```

## Population metadata

[`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md)
goes the other way: given a dataset, region, population or set of sample
IDs (including a `gen_tibble`), it returns population-level metadata.

``` r

get_pop_info(dataset = "HGDP", region = "Central_Asia",
             include = c("pop", "population_label", "temporal", "lat", "lon"))
#>           pop population_label temporal  lat  lon
#> 1 BalochiHGDP          Balochi   modern 30.5 66.5
#> 2  BrahuiHGDP           Brahui   modern 30.5 66.5
#> 3 BurushoHGDP          Burusho   modern 36.5 74.0
#> 4  KalashHGDP           Kalash   modern 36.0 71.5
#> 5 MakraniHGDP          Makrani   modern 26.0 64.0
#> 6  PathanHGDP           Pathan   modern 33.5 70.5
#> 7  SindhiHGDP           Sindhi   modern 25.5 69.0
```

### Ancient and present-day populations

The AADR contributes several thousand **ancient** populations alongside
present-day ones. Anything joined to ethnographic data wants present-day
populations only, so `temporal` is a filter:

``` r

table(get_pop_info()$temporal)
#> 
#> ancient  modern 
#>    3643     477
```

``` r

get_pop_info(temporal = "ancient", dataset = "AADR",
             include = c("pop", "population_desc", "lat", "lon"))[1:5, ]
#>                                      pop
#> 1      Afghanistan_DarraiKurCave_MBAAADR
#> 2            Albania_Barc_Medieval-oAADR
#> 3          Albania_Barc_PostMedievalAADR
#> 4 Albania_Barc_PostMedieval-oTurkishAADR
#> 5       Albania_Bardhoc_PostMedievalAADR
#>                                population_desc     lat     lon
#> 1  Afghanistan_DarraiKurCave_MBA (Afghanistan) 36.7833 70.0000
#> 2            Albania_Barc_Medieval-o (Albania) 40.6253 20.8011
#> 3          Albania_Barc_PostMedieval (Albania) 40.6253 20.8011
#> 4 Albania_Barc_PostMedieval-oTurkish (Albania) 40.6253 20.8011
#> 5       Albania_Bardhoc_PostMedieval (Albania) 42.1200 20.5181
```

### Two sets of coordinates

Where a group was sampled is not always where it comes from. Both are
stored, and `location` chooses which pair lands in the convenience
columns `lat` and `lon`:

``` r

get_pop_info(pop = "GIH", location = "origin")[, c("pop", "lat", "lon")]
#>   pop  lat  lon
#> 1 GIH 22.5 71.5
get_pop_info(pop = "GIH", location = "sampling")[, c("pop", "lat", "lon")]
#>   pop     lat      lon
#> 1 GIH 29.7589 -95.3677
```

GIH is Gujarati Indians sampled in Houston. For some populations an
origin is not a point at all — African Caribbean in Barbados originates
in a distribution over West and Central African source populations — and
those carry `NA` origin coordinates with the reason in `coord_note`:

``` r

get_pop_info(pop = c("ACB", "ASW"))[, c("pop", "origin_lat", "coord_note")]
#>   pop origin_lat
#> 1 ACB         NA
#> 2 ASW         NA
#>                                                                                                                    coord_note
#> 1           African Caribbean in Barbados; origin is a distribution over West/Central African source populations, not a point
#> 2 African ancestry in the southwestern US; origin is a distribution over West/Central African source populations, not a point
```

## SNP ascertainment panels

The Axiom Human Origins array was ascertained from thirteen distinct
panels, each discovered by sequencing one individual. Which panel a SNP
came from governs how its allele frequencies behave across populations,
so it matters for any comparison of diversity between groups.

``` r

get_panel_information()
#>      panel    population        sample   SNPs
#> 1   panel1        French     HGDP00521 111970
#> 2   panel2   Han_Chinese     HGDP00778  78253
#> 3   panel3       Papuan1     HGDP00542  48531
#> 4   panel4   San_Bushman     HGDP01029 163313
#> 5   panel5        Yoruba     HGDP00927 124115
#> 6   panel6 Mbuti_Pygmies     HGDP00456  12162
#> 7   panel7     Karitiana     HGDP00998   2635
#> 8   panel8     Sardinian     HGDP00665  12922
#> 9   panel9    Melanesian     HGDP00491  14988
#> 10 panel10     Cambodian     HGDP00711  16987
#> 11 panel11     Mongolian     HGDP01224  10757
#> 12 panel12       Papuan2     HGDP00551  12117
#> 13 panel13  Denisova-San Den-HGDP01029 151435
```

[`get_axiom_snps()`](https://jasonahodgson.github.io/humangentools/reference/get_axiom_snps.md)
returns the RS ids for one or more panels:

``` r

length(get_axiom_snps("panel4"))
#> [1] 146606
```

Panels are **not** mutually exclusive. Panel 4 is the San panel,
ascertained as heterozygous sites in HGDP01029, but only about 43% of
its SNPs are unique to it: the rest were also heterozygous in other
ascertainment individuals, which is a different and frequency-biased
discovery process. For a single ascertainment history, subtract every
other panel:

``` r

san <- setdiff(get_axiom_snps("panel4"), get_axiom_snps(paste0("panel", c(1:3, 5:13))))
c(panel4 = length(get_axiom_snps("panel4")), exclusive = length(san))
#>    panel4 exclusive 
#>    146606     62431
```

Note that panel 13 is subtracted along with the others. It is labelled
`Denisova-San` and its ascertainment sample is `Den-HGDP01029`, so
although the San genome is involved, the discovery criterion contrasts
it with Denisova — a second process rather than more of the same one.
Check
[`get_panel_information()`](https://jasonahodgson.github.io/humangentools/reference/get_panel_information.md)
before assuming two panels share an ascertainment history.

## Linking to dplaceR societies

[`get_dplace_link()`](https://jasonahodgson.github.io/humangentools/reference/get_dplace_link.md)
returns the crosswalk joining these populations to societies in
[dplaceR](https://jasonahodgson.github.io/dplaceR/)’s D-PLACE data, on a
common `soc_id`:

``` r

get_dplace_link(population = "Yoruba")[, c("pop", "soc_id", "society_name",
                                           "confidence", "n_marriage_vars_coded")]
#>          pop soc_id society_name confidence n_marriage_vars_coded
#> 1 YorubaHGDP    Af6   Oyo Yoruba     medium                    10
#> 2  YRIHapMap    Af6   Oyo Yoruba     medium                    10
#> 3        YRI    Af6   Oyo Yoruba     medium                    10
```

Every code carrying the label gets the same society, because a society
link is a property of the population and not of the sequencing project.

The table has been reviewed by hand: `reviewed` is `TRUE` on confirmed
rows, and rejected candidates have been removed rather than left to be
re-examined. A population with no society still appears, with
`soc_id = NA`, so the crosswalk records a checked dead end rather than
an absence — `include_unmatched = TRUE` shows those.

`n_marriage_vars_coded` counts how many of ten Ethnographic Atlas
marriage and descent variables (EA009, EA012, EA015, EA018, EA020,
EA023–EA026, EA043) have a genuine coded observation for the linked
society, missing-data sentinels excluded. A link can be ethnographically
correct and still analytically useless if the society has no coded
variables, so filter on it:

``` r

usable <- get_dplace_link(min_marriage_vars = 8)
c(rows = nrow(usable),
  populations = length(unique(usable$pop)),
  societies = length(unique(usable$soc_id)))
#>        rows populations   societies 
#>          67          65          54
```

That count, rather than any SNP total, is usually what limits a study
combining these resources.
