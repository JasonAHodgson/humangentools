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
#>          id    source_id data_type        pop population       region
#> 1 HGDP00001    HGDP00001      <NA> BrahuiHGDP     Brahui Central_Asia
#> 2 HGDP00001 HGDP00001.DG        DG BrahuiAADR     Brahui         <NA>
#> 3 HGDP00003    HGDP00003      <NA> BrahuiHGDP     Brahui Central_Asia
#> 4 HGDP00003 HGDP00003.DG        DG BrahuiAADR     Brahui         <NA>
#>   source_dataset temporal
#> 1           HGDP   modern
#> 2           AADR   modern
#> 3           HGDP   modern
#> 4           AADR   modern
```

By default, an ID that isn’t found is kept in the result with `NA`
population/region (and a warning); set `na.fill = FALSE` to drop it
instead:

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

[`get_pop_info()`](https://jasonahodgson.github.io/humangentools/reference/get_pop_info.md)
goes the other way: given a dataset, region, or population (or a set of
sample IDs, including a `tidypopgen` `gen_tibble`), it returns
population-level metadata – coordinates, source dataset, and citation:

``` r

get_pop_info(region = "Central_Asia")
#>                   pop population_label                         population_desc
#> 1           AleutSGDP            Aleut                  Aleut in Russia (SGDP)
#> 2         AltaianSGDP          Altaian                Altaian in Russia (SGDP)
#> 3         BalochiHGDP          Balochi              Balochi in Pakistan (HGDP)
#> 4         BengaliSGDP          Bengali            Bengali in Bangladesh (SGDP)
#> 5         BrahminSGDP          Brahmin                 Brahmin in India (SGDP)
#> 6          BrahuiHGDP           Brahui               Brahui in Pakistan (HGDP)
#> 7         BurushoHGDP          Burusho              Burusho in Pakistan (HGDP)
#> 8         ChukchiSGDP          Chukchi                Chukchi in Russia (SGDP)
#> 9   EskimoChaplinSGDP   Eskimo_Chaplin         Eskimo Chaplin in Russia (SGDP)
#> 10   EskimoNaukanSGDP    Eskimo_Naukan          Eskimo Naukan in Russia (SGDP)
#> 11 EskimoSirenikiSGDP  Eskimo_Sireniki        Eskimo Sireniki in Russia (SGDP)
#> 12           EvenSGDP             Even                   Even in Russia (SGDP)
#> 13          GIHHapMap         Gujarati Gujarati Indian in Houston, TX (HapMap)
#> 14          IrulaSGDP            Irula                   Irula in India (SGDP)
#> 15        ItelmanSGDP          Itelman                Itelman in Russia (SGDP)
#> 16         KalashHGDP           Kalash               Kalash in Pakistan (HGDP)
#> 17           KapuSGDP             Kapu                    Kapu in India (SGDP)
#> 18 Kashmiri_PanditUNK  Kashmiri_Pandit                                    <NA>
#> 19          KhariaUNK           Kharia                                    <NA>
#> 20     KhondaDoraSGDP      Khonda_Dora             Khonda Dora in India (SGDP)
#> 21         KurumbaUNK          Kurumba                                    <NA>
#> 22        KusundaSGDP          Kusunda                 Kusunda in Nepal (SGDP)
#> 23         KyrgyzSGDP           Kyrgyz            Kyrgyz in Kyrgyzystan (SGDP)
#> 24         MadigaSGDP           Madiga                  Madiga in India (SGDP)
#> 25        MakraniHGDP          Makrani              Makrani in Pakistan (HGDP)
#> 26            MalaUNK             Mala                                    <NA>
#> 27          MansiSGDP            Mansi                  Mansi in Russia (SGDP)
#> 28        MongolaSGDP          Mongola                 Mongola in China (SGDP)
#> 29            OngeUNK             Onge                                    <NA>
#> 30         PathanHGDP           Pathan               Pathan in Pakistan (HGDP)
#> 31        PunjabiSGDP          Punjabi              Punjabi in Pakistan (SGDP)
#> 32          RelliSGDP            Relli                   Relli in India (SGDP)
#> 33          SherpaUNK           Sherpa                                    <NA>
#> 34         SindhiHGDP           Sindhi               Sindhi in Pakistan (HGDP)
#> 35         TibetanUNK          Tibetan                                    <NA>
#> 36        TlingitSGDP          Tlingit                Tlingit in Russia (SGDP)
#> 37        TubalarSGDP          Tubalar                Tubalar in Russia (SGDP)
#> 38          UlchiSGDP            Ulchi                  Ulchi in Russia (SGDP)
#> 39         YadavaSGDP           Yadava                  Yadava in India (SGDP)
#>    source_dataset temporal       region     lat      lon
#> 1            SGDP   modern Central_Asia 55.1800 166.0000
#> 2            SGDP   modern Central_Asia 51.9000  86.0000
#> 3            HGDP   modern Central_Asia 30.5000  66.5000
#> 4            SGDP   modern Central_Asia 23.7000  90.4000
#> 5            SGDP   modern Central_Asia 17.7000  83.3000
#> 6            HGDP   modern Central_Asia 30.5000  66.5000
#> 7            HGDP   modern Central_Asia 36.5000  74.0000
#> 8            SGDP   modern Central_Asia 69.0000 169.0000
#> 9            SGDP   modern Central_Asia 64.4800 172.8600
#> 10           SGDP   modern Central_Asia 66.0200 169.7100
#> 11           SGDP   modern Central_Asia 64.4000 173.9000
#> 12           SGDP   modern Central_Asia 57.5300 135.8800
#> 13         HapMap   modern Central_Asia 29.7589 -95.3677
#> 14           SGDP   modern Central_Asia 13.5000  80.0000
#> 15           SGDP   modern Central_Asia 57.0000 157.0000
#> 16           HGDP   modern Central_Asia 36.0000  71.5000
#> 17           SGDP   modern Central_Asia 17.7000  83.3000
#> 18    unconfirmed   modern Central_Asia      NA       NA
#> 19    unconfirmed   modern Central_Asia      NA       NA
#> 20           SGDP   modern Central_Asia 18.3000  82.9000
#> 21    unconfirmed   modern Central_Asia      NA       NA
#> 22           SGDP   modern Central_Asia 28.0725  83.3736
#> 23           SGDP   modern Central_Asia 42.9000  74.6000
#> 24           SGDP   modern Central_Asia 17.7000  83.3000
#> 25           HGDP   modern Central_Asia 26.0000  64.0000
#> 26    unconfirmed   modern Central_Asia      NA       NA
#> 27           SGDP   modern Central_Asia 63.7250  61.7750
#> 28           SGDP   modern Central_Asia 45.0000 111.0000
#> 29    unconfirmed   modern Central_Asia      NA       NA
#> 30           HGDP   modern Central_Asia 33.5000  70.5000
#> 31           SGDP   modern Central_Asia 31.5000  74.3000
#> 32           SGDP   modern Central_Asia 17.7000  83.3000
#> 33    unconfirmed   modern Central_Asia      NA       NA
#> 34           HGDP   modern Central_Asia 25.5000  69.0000
#> 35    unconfirmed   modern Central_Asia      NA       NA
#> 36           SGDP   modern Central_Asia 54.0900 162.3250
#> 37           SGDP   modern Central_Asia 51.1333  87.0000
#> 38           SGDP   modern Central_Asia 52.4000 140.4350
#> 39           SGDP   modern Central_Asia 17.7000  83.3000
#>                  region_kgp origin_lat origin_lon sampling_lat sampling_lon
#> 1  Central_Asia_and_Siberia    55.1800   166.0000      55.1800     166.0000
#> 2  Central_Asia_and_Siberia    51.9000    86.0000      51.9000      86.0000
#> 3        Central_South_Asia    30.5000    66.5000      30.5000      66.5000
#> 4                South_Asia    23.7000    90.4000      23.7000      90.4000
#> 5                South_Asia    17.7000    83.3000      17.7000      83.3000
#> 6        Central_South_Asia    30.5000    66.5000      30.5000      66.5000
#> 7        Central_South_Asia    36.5000    74.0000      36.5000      74.0000
#> 8  Central_Asia_and_Siberia    69.0000   169.0000      69.0000     169.0000
#> 9  Central_Asia_and_Siberia    64.4800   172.8600      64.4800     172.8600
#> 10 Central_Asia_and_Siberia    66.0200   169.7100      66.0200     169.7100
#> 11 Central_Asia_and_Siberia    64.4000   173.9000      64.4000     173.9000
#> 12 Central_Asia_and_Siberia    57.5300   135.8800      57.5300     135.8800
#> 13               South_Asia    29.7589   -95.3677      29.7589     -95.3677
#> 14               South_Asia    13.5000    80.0000      13.5000      80.0000
#> 15 Central_Asia_and_Siberia    57.0000   157.0000      57.0000     157.0000
#> 16       Central_South_Asia    36.0000    71.5000      36.0000      71.5000
#> 17               South_Asia    17.7000    83.3000      17.7000      83.3000
#> 18                     <NA>         NA         NA           NA           NA
#> 19                     <NA>         NA         NA           NA           NA
#> 20               South_Asia    18.3000    82.9000      18.3000      82.9000
#> 21                     <NA>         NA         NA           NA           NA
#> 22               South_Asia    28.0725    83.3736      28.0725      83.3736
#> 23 Central_Asia_and_Siberia    42.9000    74.6000      42.9000      74.6000
#> 24               South_Asia    17.7000    83.3000      17.7000      83.3000
#> 25       Central_South_Asia    26.0000    64.0000      26.0000      64.0000
#> 26                     <NA>         NA         NA           NA           NA
#> 27 Central_Asia_and_Siberia    63.7250    61.7750      63.7250      61.7750
#> 28 Central_Asia_and_Siberia    45.0000   111.0000      45.0000     111.0000
#> 29                     <NA>         NA         NA           NA           NA
#> 30       Central_South_Asia    33.5000    70.5000      33.5000      70.5000
#> 31               South_Asia    31.5000    74.3000      31.5000      74.3000
#> 32               South_Asia    17.7000    83.3000      17.7000      83.3000
#> 33                     <NA>         NA         NA           NA           NA
#> 34       Central_South_Asia    25.5000    69.0000      25.5000      69.0000
#> 35                     <NA>         NA         NA           NA           NA
#> 36 Central_Asia_and_Siberia    54.0900   162.3250      54.0900     162.3250
#> 37 Central_Asia_and_Siberia    51.1333    87.0000      51.1333      87.0000
#> 38 Central_Asia_and_Siberia    52.4000   140.4350      52.4000     140.4350
#> 39               South_Asia    17.7000    83.3000      17.7000      83.3000
#>                                                                                                                 coord_note
#> 1                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 2                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 3                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 4                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 5                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 6                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 7                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 8                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 9                                       population location from kgp::allmeta; collection in situ, exact site not recorded
#> 10                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 11                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 12                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 13 sampling location inherited from the same-named 1000 Genomes population (GIH); HapMap collection site assumed identical
#> 14                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 15                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 16                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 17                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 18                                                                                                                    <NA>
#> 19                                                                                                                    <NA>
#> 20                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 21                                                                                                                    <NA>
#> 22                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 23                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 24                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 25                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 26                                                                                                                    <NA>
#> 27                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 28                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 29                                                                                                                    <NA>
#> 30                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 31                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 32                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 33                                                                                                                    <NA>
#> 34                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 35                                                                                                                    <NA>
#> 36                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 37                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 38                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#> 39                                      population location from kgp::allmeta; collection in situ, exact site not recorded
#>    n_samples
#> 1          2
#> 2          1
#> 3         24
#> 4         13
#> 5         12
#> 6         25
#> 7         25
#> 8          1
#> 9          1
#> 10         2
#> 11         2
#> 12         3
#> 13        12
#> 14         2
#> 15         1
#> 16        23
#> 17         2
#> 18         1
#> 19         1
#> 20         1
#> 21         1
#> 22         2
#> 23         2
#> 24         2
#> 25        25
#> 26        12
#> 27         2
#> 28         2
#> 29         2
#> 30        24
#> 31        12
#> 32         2
#> 33         2
#> 34        24
#> 35         2
#> 36         2
#> 37         2
#> 38         2
#> 39         2
#>                                                                                                               reference
#> 1                                                                                                                  <NA>
#> 2                                                                                                                  <NA>
#> 3  Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 4                                                                                                                  <NA>
#> 5                                                                                                                  <NA>
#> 6  Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 7  Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 8                                                                                                                  <NA>
#> 9                                                                                                                  <NA>
#> 10                                                                                                                 <NA>
#> 11                                                                                                                 <NA>
#> 12                                                                                                                 <NA>
#> 13                                                                                                                 <NA>
#> 14                                                                                                                 <NA>
#> 15                                                                                                                 <NA>
#> 16 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 17                                                                                                                 <NA>
#> 18                                                                                                                 <NA>
#> 19                                                                                                                 <NA>
#> 20                                                                                                                 <NA>
#> 21                                                                                                                 <NA>
#> 22                                                                                                                 <NA>
#> 23                                                                                                                 <NA>
#> 24                                                                                                                 <NA>
#> 25 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 26                                                                                                                 <NA>
#> 27                                                                                                                 <NA>
#> 28 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 29                                                                                                                 <NA>
#> 30 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 31                                                                                                                 <NA>
#> 32                                                                                                                 <NA>
#> 33                                                                                                                 <NA>
#> 34 Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 35                                                                                                                 <NA>
#> 36                                                                                                                 <NA>
#> 37                                                                                                                 <NA>
#> 38                                                                                                                 <NA>
#> 39                                                                                                                 <NA>
```

``` r

get_pop_info(samples = c("HGDP00001", "HGDP00003"))
#>          pop population_label           population_desc source_dataset temporal
#> 1 BrahuiHGDP           Brahui Brahui in Pakistan (HGDP)           HGDP   modern
#> 2 BrahuiAADR           Brahui         Brahui (Pakistan)           AADR   modern
#>         region     lat  lon         region_kgp origin_lat origin_lon
#> 1 Central_Asia 30.5000 66.5 Central_South_Asia    30.5000       66.5
#> 2         <NA> 30.4987 66.5               <NA>    30.4987       66.5
#>   sampling_lat sampling_lon
#> 1      30.5000         66.5
#> 2      30.4987         66.5
#>                                                                           coord_note
#> 1 population location from kgp::allmeta; collection in situ, exact site not recorded
#> 2     population location from the AADR .anno (median over individuals in the group)
#>   n_samples
#> 1        25
#> 2        28
#>                                                                                                                                                reference
#> 1                                   Cann et al. 2002. A Human Genome Diversity Cell Line Panel. Science 296(5566) 261-262. 10.1126/science.296.5566.261b
#> 2 Mallick et al. 2024. The Allen Ancient DNA Resource (AADR): a curated compendium of ancient human genomes. Sci Data 11:182. 10.1038/s41597-024-03031-7
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

## Linking to dplaceR societies

[`get_dplace_link()`](https://jasonahodgson.github.io/humangentools/reference/get_dplace_link.md)
returns a candidate link table joining these populations to societies in
[dplaceR](https://jasonahodgson.github.io/dplaceR/)’s D-PLACE data, so
genetic and cross-cultural data can be combined on a common `soc_id`:

``` r

get_dplace_link(population = "Yoruba")
#>          pop population_label source_dataset soc_id society_name
#> 1 YorubaHGDP           Yoruba           HGDP    Af6   Oyo Yoruba
#> 2  YRIHapMap           Yoruba         HapMap    Af6   Oyo Yoruba
#> 3        YRI           Yoruba            KGP    Af6   Oyo Yoruba
#>         society_region match_method match_score geo_distance_km confidence
#> 1 West Tropical Africa        fuzzy         0.9             110     medium
#> 2 West Tropical Africa        fuzzy         0.9             110     medium
#> 3 West Tropical Africa        fuzzy         0.9             110     medium
#>   n_marriage_vars_coded reviewed review_note
#> 1                    10     TRUE        <NA>
#> 2                    10     TRUE        <NA>
#> 3                    10     TRUE        <NA>
```

The two resources use different naming conventions with no shared ID, so
this is a **best-effort, fuzzy candidate match**, not a
guaranteed-correct crosswalk – matching can be many-to-many, and a
population with no plausible match still appears once with `soc_id = NA`
(set `include_unmatched = TRUE` to see those). Each row’s `confidence`
column (`"high"`, `"medium"`, `"low"`, or `"none"`) reflects how much to
trust the match: `"high"` is an exact name match, while
`"medium"`/`"low"` rows are worth checking (e.g. against
`society_region` and `geo_distance_km`) before relying on them for an
analysis where a wrong link would matter:

``` r

get_dplace_link(confidence = "medium")
#>                  pop population_label         source_dataset        soc_id
#> 1           DaurHGDP             Daur                   HGDP           Eb4
#> 2            HanHGDP              Han                   HGDP CARNEIRO4_005
#> 3  HanHGDPunresolved              Han                   HGDP CARNEIRO4_005
#> 4           MayaHGDP             Maya                   HGDP  CCMCyuca1254
#> 5           MayaHGDP             Maya                   HGDP           Sa6
#> 6    NorthernHanHGDP              Han                   HGDP CARNEIRO4_005
#> 7         OroqenHGDP           Oroqen                   HGDP           B23
#> 8        RussianHGDP           Russia                   HGDP  CCMCruss1263
#> 9      SardinianHGDP         Sardinia                   HGDP  CCMCsard1257
#> 10        YorubaHGDP           Yoruba                   HGDP           Af6
#> 11         YRIHapMap           Yoruba                 HapMap           Af6
#> 12               CHS      China_South                    KGP CARNEIRO4_005
#> 13               FIN           Finish                    KGP  CCMCfinn1318
#> 14               YRI           Yoruba                    KGP           Af6
#> 15           CHSIGSR      China_South               KGP_IGSR CARNEIRO4_005
#> 16           FINIGSR           Finish               KGP_IGSR  CCMCfinn1318
#> 17          BatwaPER            Batwa Perry_etal_unconfirmed          Ah34
#> 18      ArmenianSGDP         Armenian                   SGDP  CCMCnucl1235
#> 19      ArmenianSGDP         Armenian                   SGDP          Ci10
#> 20      ArmenianSGDP         Armenian                   SGDP        SCCS56
#> 21         BiakaSGDP            Biaka                   SGDP          Ai23
#> 22         GreekSGDP            Greek                   SGDP  CCMCmode1248
#> 23         GreekSGDP            Greek                   SGDP           Ce7
#> 24       IranianSGDP          Iranian                   SGDP           Ea9
#> 25       ItelmanSGDP          Itelman                   SGDP          ec13
#> 26     JordanianSGDP        Jordanian                   SGDP           Cj6
#> 27   JuhoanNorthSGDP    Ju_hoan_North                   SGDP  CCMCjuho1239
#> 28    KhomaniSanSGDP      Khomani_San                   SGDP           B77
#> 29    KhondaDoraSGDP      Khonda_Dora                   SGDP          Eg12
#> 30 NorthOssetianSGDP   North_Ossetian                   SGDP           Ci6
#> 31       QuechuaSGDP          Quechua                   SGDP  CCMCayac1239
#> 32       QuechuaSGDP          Quechua                   SGDP  CCMCcusc1236
#> 33       QuechuaSGDP          Quechua                   SGDP  CCMCecua1248
#> 34       QuechuaSGDP          Quechua                   SGDP  CCMCsout2991
#> 35        SomaliSGDP           Somali                   SGDP          Ca10
#> 36        SomaliSGDP           Somali                   SGDP           Ca2
#> 37        SomaliSGDP           Somali                   SGDP        SCCS36
#> 38         UlchiSGDP            Ulchi                   SGDP          ec18
#> 39          MalayUNK            Malay            unconfirmed  CCMCjamb1236
#> 40          MalayUNK            Malay            unconfirmed           Ej8
#> 41        TibetanUNK          Tibetan            unconfirmed           Ee4
#>              society_name               society_region match_method match_score
#> 1                   Dagur                        China        fuzzy       0.800
#> 2     China (Han Dynasty)                        China        fuzzy       0.900
#> 3     China (Han Dynasty)                        China        fuzzy       0.900
#> 4            Yucatec Maya                       Mexico        fuzzy       0.900
#> 5            Yucatec Maya                       Mexico        fuzzy       0.900
#> 6     China (Han Dynasty)                        China        fuzzy       0.900
#> 7                 Orogens                        China        fuzzy       0.714
#> 8                 Russian               Eastern Europe        fuzzy       0.857
#> 9               Sardinian          Southwestern Europe        fuzzy       0.889
#> 10             Oyo Yoruba         West Tropical Africa        fuzzy       0.900
#> 11             Oyo Yoruba         West Tropical Africa        fuzzy       0.900
#> 12    China (Han Dynasty)                        China        fuzzy       0.900
#> 13                Finnish              Northern Europe        fuzzy       0.857
#> 14             Oyo Yoruba         West Tropical Africa        fuzzy       0.900
#> 15    China (Han Dynasty)                        China        fuzzy       0.900
#> 16                Finnish              Northern Europe        fuzzy       0.857
#> 17                   Bata         West Tropical Africa        fuzzy       0.800
#> 18       Eastern Armenian                     Caucasus        fuzzy       0.900
#> 19              Armenians                     Caucasus        fuzzy       0.900
#> 20              Armenians                     Caucasus        fuzzy       0.900
#> 21                  Bwaka West-Central Tropical Africa        fuzzy       0.800
#> 22           Modern Greek          Southeastern Europe        fuzzy       0.900
#> 23                 Greeks          Southeastern Europe        fuzzy       0.900
#> 24               Iranians                 Western Asia        fuzzy       0.900
#> 25                Itelmen             Russian Far East        fuzzy       0.857
#> 26             Jordanians                 Western Asia        fuzzy       0.900
#> 27       South-Eastern Ju              Southern Africa        fuzzy       0.900
#> 28         /'Auni-Khomani              Southern Africa        fuzzy       0.900
#> 29                  Khond          Indian Subcontinent        fuzzy       0.833
#> 30              Ossetians                     Caucasus        fuzzy       0.900
#> 31       Ayacucho Quechua        Western South America        fuzzy       0.900
#> 32          Cusco Quechua        Western South America        fuzzy       0.900
#> 33   Ecuadorian Quechua A        Western South America        fuzzy       0.900
#> 34 South Bolivian Quechua        Western South America        fuzzy       0.900
#> 35           Somali (Esa)    Northeast Tropical Africa        fuzzy       0.900
#> 36    Somali (Dolbahanta)    Northeast Tropical Africa        fuzzy       0.900
#> 37    Somali (Dolbahanta)    Northeast Tropical Africa        fuzzy       0.900
#> 38                   Ulch             Russian Far East        fuzzy       0.800
#> 39            Jambi Malay                      Malesia        fuzzy       0.900
#> 40                 Malays                      Malesia        fuzzy       0.900
#> 41       Central Tibetans                        China        fuzzy       0.900
#>    geo_distance_km confidence n_marriage_vars_coded reviewed review_note
#> 1               92     medium                    10     TRUE        <NA>
#> 2              518     medium                     0     TRUE        <NA>
#> 3              518     medium                     0     TRUE        <NA>
#> 4              216     medium                     0     TRUE        <NA>
#> 5              153     medium                    10     TRUE        <NA>
#> 6              518     medium                     0     TRUE        <NA>
#> 7              320     medium                     0     TRUE        <NA>
#> 8              598     medium                     0     TRUE        <NA>
#> 9               26     medium                     0     TRUE        <NA>
#> 10             110     medium                    10     TRUE        <NA>
#> 11             110     medium                    10     TRUE        <NA>
#> 12              NA     medium                     0     TRUE        <NA>
#> 13              NA     medium                     0     TRUE        <NA>
#> 14             110     medium                    10     TRUE        <NA>
#> 15              NA     medium                     0     TRUE        <NA>
#> 16              NA     medium                     0     TRUE        <NA>
#> 17              NA     medium                     9     TRUE        <NA>
#> 18             151     medium                     0     TRUE        <NA>
#> 19             151     medium                    10     TRUE        <NA>
#> 20             193     medium                     0     TRUE        <NA>
#> 21             248     medium                    10     TRUE        <NA>
#> 22              64     medium                     0     TRUE        <NA>
#> 23             127     medium                    10     TRUE        <NA>
#> 24              67     medium                    10     TRUE        <NA>
#> 25             373     medium                    10     TRUE        <NA>
#> 26              10     medium                    10     TRUE        <NA>
#> 27             110     medium                     0     TRUE        <NA>
#> 28             105     medium                     0     TRUE        <NA>
#> 29             101     medium                    10     TRUE        <NA>
#> 30              53     medium                    10     TRUE        <NA>
#> 31             254     medium                     0     TRUE        <NA>
#> 32              70     medium                     0     TRUE        <NA>
#> 33            1561     medium                     0     TRUE        <NA>
#> 34            1021     medium                     0     TRUE        <NA>
#> 35             762     medium                    10     TRUE        <NA>
#> 36             269     medium                    10     TRUE        <NA>
#> 37             395     medium                     0     TRUE        <NA>
#> 38             409     medium                    10     TRUE        <NA>
#> 39              NA     medium                     0     TRUE        <NA>
#> 40              NA     medium                    10     TRUE        <NA>
#> 41             660     medium                    10     TRUE        <NA>
```
