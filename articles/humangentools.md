# Getting started with humangentools

``` r

library(humangentools)
```

humangentools bundles sample, population, and SNP ascertainment panel
metadata for common human genomics reference datasets (currently HGDP
and 1000 Genomes), so that
[tidypopgen](https://cran.r-project.org/package=tidypopgen) analyses of
these datasets can be annotated and filtered without tracking down that
metadata yourself each time. It’s designed to work alongside
[dplaceR](https://jasonahodgson.github.io/dplaceR/) for cross-cultural
context on the same populations.

## Sample and population metadata

[`get_sample_information()`](https://jasonahodgson.github.io/humangentools/reference/get_sample_information.md)
looks up the population and region for one or more sample IDs:

``` r

get_sample_information(c("HGDP00001", "HGDP00003"))
#>          id population       region
#> 1 HGDP00001     Brahui Central_Asia
#> 2 HGDP00003     Brahui Central_Asia
```

By default, an ID that isn’t found is kept in the result with `NA`
population/region (and a warning); set `na.fill = FALSE` to drop it
instead:

``` r

get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill = FALSE)
#> Warning in get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill =
#> FALSE): The following IDs were not found in the sample information data:
#> not-a-real-id
#>          id population       region
#> 1 HGDP00001     Brahui Central_Asia
```

[`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md)
goes the other way: given a dataset, region, or population (or a set of
sample IDs, including a `tidypopgen` `gen_tibble`), it returns
population-level metadata – coordinates, source dataset, and citation:

``` r

get_pop_info(region = "Central_Asia")
#>          population       region      lat       lon dataset
#> 1            Brahui Central_Asia 30.50000  66.50000    HGDP
#> 2           Balochi Central_Asia 50.50000  66.50000    HGDP
#> 4           Makrani Central_Asia 26.00000  64.00000    HGDP
#> 5            Sindhi Central_Asia 25.50000  69.00000    HGDP
#> 6            Pathan Central_Asia 33.50000  70.50000    HGDP
#> 7            Kalash Central_Asia 36.00000  71.50000    HGDP
#> 8           Burusho Central_Asia 36.50000  74.00000    HGDP
#> 57         Gujarati Central_Asia 29.50000 -95.00000     KGP
#> 66          Brahmin Central_Asia       NA        NA    <NA>
#> 67             Mala Central_Asia       NA        NA    <NA>
#> 84          Punjabi Central_Asia       NA        NA     KGP
#> 89          Bengali Central_Asia       NA        NA     KGP
#> 91            Tamil Central_Asia       NA        NA     KGP
#> 92           Telugu Central_Asia       NA        NA     KGP
#> 105         Altaian Central_Asia 50.84000  85.65400    <NA>
#> 109         Chukchi Central_Asia 69.00000 169.00000    <NA>
#> 111  Eskimo_Chaplin Central_Asia 64.48000 172.86000    <NA>
#> 113         Itelman Central_Asia 57.00000 157.00000    <NA>
#> 114     Khonda_Dora Central_Asia 18.30000  82.90000    <NA>
#> 125           Aleut Central_Asia 55.18000 166.00000    <NA>
#> 137   Eskimo_Naukan Central_Asia 66.02000 169.71000    <NA>
#> 138 Eskimo_Sireniki Central_Asia 64.40000 173.90000    <NA>
#> 147           Irula Central_Asia 13.50000  80.00000    <NA>
#> 148            Kapu Central_Asia 17.70000  83.30000    <NA>
#> 151         Kusunda Central_Asia 28.07245  82.48778    <NA>
#> 152          Kyrgyz Central_Asia 42.90000  74.60000    <NA>
#> 155          Madiga Central_Asia 17.70000  83.30000    <NA>
#> 156           Mansi Central_Asia 63.80000  61.45000    <NA>
#> 160         Mongola Central_Asia 45.00000 111.00000    HGDP
#> 163           Relli Central_Asia 17.70000  83.30000    <NA>
#> 169         Tlingit Central_Asia 53.00000 158.65000    <NA>
#> 170         Tubalar Central_Asia 51.13333  87.00000    <NA>
#> 172           Ulchi Central_Asia 52.43000 140.42000    <NA>
#> 173          Yadava Central_Asia 17.70000  83.30000    <NA>
#> 177            Even Central_Asia 57.53000 135.88000    <NA>
#> 180 Kashmiri_Pandit Central_Asia 34.43333  75.75000    <NA>
#> 181          Kharia Central_Asia 22.50000  83.95000    <NA>
#> 182         Kurumba Central_Asia 10.78333  76.65000    <NA>
#> 183            Onge Central_Asia 11.66667  92.65000    <NA>
#> 190          Sherpa Central_Asia 27.80000  86.70000    <NA>
#> 191         Tibetan Central_Asia 28.90000  84.30000    <NA>
#>                                                                                                                reference
#> 1   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 2   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 4   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 5   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 6   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 7   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 8   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 57                                                                                                                  <NA>
#> 66                                                                                                                  <NA>
#> 67                                                                                                                  <NA>
#> 84                                                                                                                  <NA>
#> 89                                                                                                                  <NA>
#> 91                                                                                                                  <NA>
#> 92                                                                                                                  <NA>
#> 105                                                                                                                 <NA>
#> 109                                                                                                                 <NA>
#> 111                                                                                                                 <NA>
#> 113                                                                                                                 <NA>
#> 114                                                                                                                 <NA>
#> 125                                                                                                                 <NA>
#> 137                                                                                                                 <NA>
#> 138                                                                                                                 <NA>
#> 147                                                                                                                 <NA>
#> 148                                                                                                                 <NA>
#> 151                                                                                                                 <NA>
#> 152                                                                                                                 <NA>
#> 155                                                                                                                 <NA>
#> 156                                                                                                                 <NA>
#> 160 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 163                                                                                                                 <NA>
#> 169                                                                                                                 <NA>
#> 170                                                                                                                 <NA>
#> 172                                                                                                                 <NA>
#> 173                                                                                                                 <NA>
#> 177                                                                                                                 <NA>
#> 180                                                                                                                 <NA>
#> 181                                                                                                                 <NA>
#> 182                                                                                                                 <NA>
#> 183                                                                                                                 <NA>
#> 190                                                                                                                 <NA>
#> 191                                                                                                                 <NA>
```

``` r

get_pop_info(samples = c("HGDP00001", "HGDP00003"))
#>   population       region  lat  lon dataset
#> 1     Brahui Central_Asia 30.5 66.5    HGDP
#>                                                                                                              reference
#> 1 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
```

## SNP ascertainment panels

The Axiom Human Origins array was ascertained from several distinct
population panels, which matters for some population-genetic analyses.
[`get_panel_information()`](https://jasonahodgson.github.io/humangentools/reference/get_panel_information.md)
describes the panels available:

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
returns the RS ids for one or more panels, ready to use as a SNP filter
(e.g. before reading genotype data into a `tidypopgen` `gen_tibble`):

``` r

length(get_axiom_snps("panel1"))
#> [1] 101567
length(get_axiom_snps("all"))
#> [1] 478620
```
