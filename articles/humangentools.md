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
#>          id        pop population       region source_dataset
#> 1 HGDP00001 BrahuiHGDP     Brahui Central_Asia           HGDP
#> 2 HGDP00003 BrahuiHGDP     Brahui Central_Asia           HGDP
```

By default, an ID that isn’t found is kept in the result with `NA`
population/region (and a warning); set `na.fill = FALSE` to drop it
instead:

``` r

get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill = FALSE)
#> Warning in get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill =
#> FALSE): The following IDs were not found in the sample information data:
#> not-a-real-id
#>          id        pop population       region source_dataset
#> 1 HGDP00001 BrahuiHGDP     Brahui Central_Asia           HGDP
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
#>    source_dataset       region     lat      lon               region_kgp
#> 1            SGDP Central_Asia 55.1800 166.0000 Central_Asia_and_Siberia
#> 2            SGDP Central_Asia 51.9000  86.0000 Central_Asia_and_Siberia
#> 3            HGDP Central_Asia 30.5000  66.5000       Central_South_Asia
#> 4            SGDP Central_Asia 23.7000  90.4000               South_Asia
#> 5            SGDP Central_Asia 17.7000  83.3000               South_Asia
#> 6            HGDP Central_Asia 30.5000  66.5000       Central_South_Asia
#> 7            HGDP Central_Asia 36.5000  74.0000       Central_South_Asia
#> 8            SGDP Central_Asia 69.0000 169.0000 Central_Asia_and_Siberia
#> 9            SGDP Central_Asia 64.4800 172.8600 Central_Asia_and_Siberia
#> 10           SGDP Central_Asia 66.0200 169.7100 Central_Asia_and_Siberia
#> 11           SGDP Central_Asia 64.4000 173.9000 Central_Asia_and_Siberia
#> 12           SGDP Central_Asia 57.5300 135.8800 Central_Asia_and_Siberia
#> 13         HapMap Central_Asia 29.7589 -95.3677               South_Asia
#> 14           SGDP Central_Asia 13.5000  80.0000               South_Asia
#> 15           SGDP Central_Asia 57.0000 157.0000 Central_Asia_and_Siberia
#> 16           HGDP Central_Asia 36.0000  71.5000       Central_South_Asia
#> 17           SGDP Central_Asia 17.7000  83.3000               South_Asia
#> 18    unconfirmed Central_Asia      NA       NA                     <NA>
#> 19    unconfirmed Central_Asia      NA       NA                     <NA>
#> 20           SGDP Central_Asia 18.3000  82.9000               South_Asia
#> 21    unconfirmed Central_Asia      NA       NA                     <NA>
#> 22           SGDP Central_Asia 28.0725  83.3736               South_Asia
#> 23           SGDP Central_Asia 42.9000  74.6000 Central_Asia_and_Siberia
#> 24           SGDP Central_Asia 17.7000  83.3000               South_Asia
#> 25           HGDP Central_Asia 26.0000  64.0000       Central_South_Asia
#> 26    unconfirmed Central_Asia      NA       NA                     <NA>
#> 27           SGDP Central_Asia 63.7250  61.7750 Central_Asia_and_Siberia
#> 28           SGDP Central_Asia 45.0000 111.0000 Central_Asia_and_Siberia
#> 29    unconfirmed Central_Asia      NA       NA                     <NA>
#> 30           HGDP Central_Asia 33.5000  70.5000       Central_South_Asia
#> 31           SGDP Central_Asia 31.5000  74.3000               South_Asia
#> 32           SGDP Central_Asia 17.7000  83.3000               South_Asia
#> 33    unconfirmed Central_Asia      NA       NA                     <NA>
#> 34           HGDP Central_Asia 25.5000  69.0000       Central_South_Asia
#> 35    unconfirmed Central_Asia      NA       NA                     <NA>
#> 36           SGDP Central_Asia 54.0900 162.3250 Central_Asia_and_Siberia
#> 37           SGDP Central_Asia 51.1333  87.0000 Central_Asia_and_Siberia
#> 38           SGDP Central_Asia 52.4000 140.4350 Central_Asia_and_Siberia
#> 39           SGDP Central_Asia 17.7000  83.3000               South_Asia
#>    origin_lat origin_lon sampling_lat sampling_lon
#> 1     55.1800   166.0000      55.1800     166.0000
#> 2     51.9000    86.0000      51.9000      86.0000
#> 3     30.5000    66.5000      30.5000      66.5000
#> 4     23.7000    90.4000      23.7000      90.4000
#> 5     17.7000    83.3000      17.7000      83.3000
#> 6     30.5000    66.5000      30.5000      66.5000
#> 7     36.5000    74.0000      36.5000      74.0000
#> 8     69.0000   169.0000      69.0000     169.0000
#> 9     64.4800   172.8600      64.4800     172.8600
#> 10    66.0200   169.7100      66.0200     169.7100
#> 11    64.4000   173.9000      64.4000     173.9000
#> 12    57.5300   135.8800      57.5300     135.8800
#> 13    29.7589   -95.3677      29.7589     -95.3677
#> 14    13.5000    80.0000      13.5000      80.0000
#> 15    57.0000   157.0000      57.0000     157.0000
#> 16    36.0000    71.5000      36.0000      71.5000
#> 17    17.7000    83.3000      17.7000      83.3000
#> 18         NA         NA           NA           NA
#> 19         NA         NA           NA           NA
#> 20    18.3000    82.9000      18.3000      82.9000
#> 21         NA         NA           NA           NA
#> 22    28.0725    83.3736      28.0725      83.3736
#> 23    42.9000    74.6000      42.9000      74.6000
#> 24    17.7000    83.3000      17.7000      83.3000
#> 25    26.0000    64.0000      26.0000      64.0000
#> 26         NA         NA           NA           NA
#> 27    63.7250    61.7750      63.7250      61.7750
#> 28    45.0000   111.0000      45.0000     111.0000
#> 29         NA         NA           NA           NA
#> 30    33.5000    70.5000      33.5000      70.5000
#> 31    31.5000    74.3000      31.5000      74.3000
#> 32    17.7000    83.3000      17.7000      83.3000
#> 33         NA         NA           NA           NA
#> 34    25.5000    69.0000      25.5000      69.0000
#> 35         NA         NA           NA           NA
#> 36    54.0900   162.3250      54.0900     162.3250
#> 37    51.1333    87.0000      51.1333      87.0000
#> 38    52.4000   140.4350      52.4000     140.4350
#> 39    17.7000    83.3000      17.7000      83.3000
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
#>          pop population_label           population_desc source_dataset
#> 1 BrahuiHGDP           Brahui Brahui in Pakistan (HGDP)           HGDP
#>         region  lat  lon         region_kgp origin_lat origin_lon sampling_lat
#> 1 Central_Asia 30.5 66.5 Central_South_Asia       30.5       66.5         30.5
#>   sampling_lon
#> 1         66.5
#>                                                                           coord_note
#> 1 population location from kgp::allmeta; collection in situ, exact site not recorded
#>   n_samples
#> 1        25
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
#>   n_marriage_vars_coded reviewed
#> 1                    10    FALSE
#> 2                    10    FALSE
#> 3                    10    FALSE
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
#>                     pop population_label         source_dataset        soc_id
#> 1        BantuKenyaHGDP      Bantu_North                   HGDP  CCMCbant1295
#> 2  BantuSouthAfricaHGDP      Bantu_South                   HGDP  CCMCbant1295
#> 3             BiakaHGDP      Biaka_Pygmy                   HGDP           B63
#> 4             BiakaHGDP      Biaka_Pygmy                   HGDP          Ai23
#> 5              DaurHGDP             Daur                   HGDP           Eb4
#> 6               HanHGDP              Han                   HGDP CARNEIRO4_005
#> 7     HanHGDPunresolved              Han                   HGDP CARNEIRO4_005
#> 8              MayaHGDP             Maya                   HGDP  CCMCyuca1254
#> 9              MayaHGDP             Maya                   HGDP           Sa6
#> 10      NorthernHanHGDP              Han                   HGDP CARNEIRO4_005
#> 11           OroqenHGDP           Oroqen                   HGDP           B23
#> 12          RussianHGDP           Russia                   HGDP  CCMCruss1264
#> 13          RussianHGDP           Russia                   HGDP  CCMCruss1263
#> 14        SardinianHGDP         Sardinia                   HGDP  CCMCsard1257
#> 15           YorubaHGDP           Yoruba                   HGDP           Af6
#> 16         JPTCHBHapMap      China_Japan                 HapMap CARNEIRO4_005
#> 17            MEXHapMap Mexican_American                 HapMap  CCMCamer1254
#> 18            MEXHapMap Mexican_American                 HapMap           Ii1
#> 19            YRIHapMap           Yoruba                 HapMap           Af6
#> 20                  CHS      China_South                    KGP CARNEIRO4_005
#> 21                  FIN           Finish                    KGP  CCMCfinn1318
#> 22                  MXL Mexican_American                    KGP  CCMCamer1254
#> 23                  MXL Mexican_American                    KGP           Ii1
#> 24                  YRI           Yoruba                    KGP           Af6
#> 25              CHSIGSR      China_South               KGP_IGSR CARNEIRO4_005
#> 26              FINIGSR           Finish               KGP_IGSR  CCMCfinn1318
#> 27             BatwaPER            Batwa Perry_etal_unconfirmed          Ah34
#> 28             DiegoRAK            Diego     Rakotoarivony_etal          Ad30
#> 29         ArmenianSGDP         Armenian                   SGDP  CCMCnucl1235
#> 30         ArmenianSGDP         Armenian                   SGDP          Ci10
#> 31         ArmenianSGDP         Armenian                   SGDP        SCCS56
#> 32            BiakaSGDP            Biaka                   SGDP           B63
#> 33            BiakaSGDP            Biaka                   SGDP          Ai23
#> 34            DusunSGDP            Dusun                   SGDP           Ib5
#> 35             EvenSGDP             Even                   SGDP          ec16
#> 36            GreekSGDP            Greek                   SGDP  CCMCmode1248
#> 37            GreekSGDP            Greek                   SGDP           Ce7
#> 38          IranianSGDP          Iranian                   SGDP           Ea9
#> 39          IranianSGDP          Iranian                   SGDP           Ef3
#> 40         IraqiJewSGDP        Iraqi_Jew                   SGDP           Ca4
#> 41          ItelmanSGDP          Itelman                   SGDP          ec13
#> 42        JordanianSGDP        Jordanian                   SGDP           Cj6
#> 43      JuhoanNorthSGDP    Ju_hoan_North                   SGDP  CCMCjuho1239
#> 44       KhomaniSanSGDP      Khomani_San                   SGDP           B77
#> 45       KhondaDoraSGDP      Khonda_Dora                   SGDP          Eg12
#> 46           MadigaSGDP           Madiga                   SGDP           Eg3
#> 47           MadigaSGDP           Madiga                   SGDP        SCCS60
#> 48    NorthOssetianSGDP   North_Ossetian                   SGDP           Ci6
#> 49          QuechuaSGDP          Quechua                   SGDP  CCMCayac1239
#> 50          QuechuaSGDP          Quechua                   SGDP  CCMCcusc1236
#> 51          QuechuaSGDP          Quechua                   SGDP  CCMCecua1248
#> 52          QuechuaSGDP          Quechua                   SGDP  CCMCsout2991
#> 53           SomaliSGDP           Somali                   SGDP          Ca10
#> 54           SomaliSGDP           Somali                   SGDP           Ca2
#> 55           SomaliSGDP           Somali                   SGDP        SCCS36
#> 56            UlchiSGDP            Ulchi                   SGDP          ec18
#> 57      YemeniteJewSGDP     Yemenite_Jew                   SGDP           Cj9
#> 58             BantuUNK            Bantu            unconfirmed  CCMCbant1295
#> 59             KongoUNK            Kongo            unconfirmed  CCMCmong1338
#> 60             KongoUNK            Kongo            unconfirmed          Ad44
#> 61             KongoUNK            Kongo            unconfirmed          Ae24
#> 62             KongoUNK            Kongo            unconfirmed          Ai35
#> 63             KongoUNK            Kongo            unconfirmed           Ca1
#> 64             KongoUNK            Kongo            unconfirmed          Ac25
#> 65             KongoUNK            Kongo            unconfirmed        SCCS35
#> 66             MalayUNK            Malay            unconfirmed  CCMCjamb1236
#> 67             MalayUNK            Malay            unconfirmed           Ej8
#> 68           TibetanUNK          Tibetan            unconfirmed           Ee4
#>              society_name               society_region match_method match_score
#> 1     Bantu A-B10-B20-B30 West-Central Tropical Africa        fuzzy       0.900
#> 2     Bantu A-B10-B20-B30 West-Central Tropical Africa        fuzzy       0.900
#> 3                    Baka West-Central Tropical Africa        fuzzy       0.800
#> 4                   Bwaka West-Central Tropical Africa        fuzzy       0.800
#> 5                   Dagur                        China        fuzzy       0.800
#> 6     China (Han Dynasty)                        China        fuzzy       0.900
#> 7     China (Han Dynasty)                        China        fuzzy       0.900
#> 8            Yucatec Maya                       Mexico        fuzzy       0.900
#> 9            Yucatec Maya                       Mexico        fuzzy       0.900
#> 10    China (Han Dynasty)                        China        fuzzy       0.900
#> 11                Orogens                        China        fuzzy       0.714
#> 12          Russia Buriat                      Siberia        fuzzy       0.900
#> 13                Russian               Eastern Europe        fuzzy       0.857
#> 14              Sardinian          Southwestern Europe        fuzzy       0.889
#> 15             Oyo Yoruba         West Tropical Africa        fuzzy       0.900
#> 16    China (Han Dynasty)                        China        fuzzy       0.900
#> 17 Latin American Spanish          Southwestern Europe        fuzzy       0.900
#> 18       American Samoans         Southwestern Pacific        fuzzy       0.900
#> 19             Oyo Yoruba         West Tropical Africa        fuzzy       0.900
#> 20    China (Han Dynasty)                        China        fuzzy       0.900
#> 21                Finnish              Northern Europe        fuzzy       0.857
#> 22 Latin American Spanish          Southwestern Europe        fuzzy       0.900
#> 23       American Samoans         Southwestern Pacific        fuzzy       0.900
#> 24             Oyo Yoruba         West Tropical Africa        fuzzy       0.900
#> 25    China (Han Dynasty)                        China        fuzzy       0.900
#> 26                Finnish              Northern Europe        fuzzy       0.857
#> 27                   Bata         West Tropical Africa        fuzzy       0.800
#> 28                   Digo         East Tropical Africa        fuzzy       0.800
#> 29       Eastern Armenian                     Caucasus        fuzzy       0.900
#> 30              Armenians                     Caucasus        fuzzy       0.900
#> 31              Armenians                     Caucasus        fuzzy       0.900
#> 32                   Baka West-Central Tropical Africa        fuzzy       0.800
#> 33                  Bwaka West-Central Tropical Africa        fuzzy       0.800
#> 34          Kadazan-Dusun                      Malesia        fuzzy       0.900
#> 35                  Evenk                      Siberia        fuzzy       0.800
#> 36           Modern Greek          Southeastern Europe        fuzzy       0.900
#> 37                 Greeks          Southeastern Europe        fuzzy       0.900
#> 38               Iranians                 Western Asia        fuzzy       0.900
#> 39           Indo-Iranian          Indian Subcontinent        fuzzy       0.900
#> 40                  Iraqw         East Tropical Africa        fuzzy       0.800
#> 41                Itelmen             Russian Far East        fuzzy       0.857
#> 42             Jordanians                 Western Asia        fuzzy       0.900
#> 43       South-Eastern Ju              Southern Africa        fuzzy       0.900
#> 44         /'Auni-Khomani              Southern Africa        fuzzy       0.900
#> 45                  Khond          Indian Subcontinent        fuzzy       0.833
#> 46                  Madia          Indian Subcontinent        fuzzy       0.833
#> 47                  Madia          Indian Subcontinent        fuzzy       0.833
#> 48              Ossetians                     Caucasus        fuzzy       0.900
#> 49       Ayacucho Quechua        Western South America        fuzzy       0.900
#> 50          Cusco Quechua        Western South America        fuzzy       0.900
#> 51   Ecuadorian Quechua A        Western South America        fuzzy       0.900
#> 52 South Bolivian Quechua        Western South America        fuzzy       0.900
#> 53           Somali (Esa)    Northeast Tropical Africa        fuzzy       0.900
#> 54    Somali (Dolbahanta)    Northeast Tropical Africa        fuzzy       0.900
#> 55    Somali (Dolbahanta)    Northeast Tropical Africa        fuzzy       0.900
#> 56                   Ulch             Russian Far East        fuzzy       0.800
#> 57                 Yemeni            Arabian Peninsula        fuzzy       0.750
#> 58    Bantu A-B10-B20-B30 West-Central Tropical Africa        fuzzy       0.900
#> 59                  Mongo West-Central Tropical Africa        fuzzy       0.800
#> 60                  Konjo         East Tropical Africa        fuzzy       0.800
#> 61                  Mongo West-Central Tropical Africa        fuzzy       0.800
#> 62                  Bongo    Northeast Tropical Africa        fuzzy       0.800
#> 63                  Konso    Northeast Tropical Africa        fuzzy       0.800
#> 64                  Songo West-Central Tropical Africa        fuzzy       0.800
#> 65                  Konso    Northeast Tropical Africa        fuzzy       0.800
#> 66            Jambi Malay                      Malesia        fuzzy       0.900
#> 67                 Malays                      Malesia        fuzzy       0.900
#> 68       Central Tibetans                        China        fuzzy       0.900
#>    geo_distance_km confidence n_marriage_vars_coded reviewed
#> 1              782     medium                     0    FALSE
#> 2             3988     medium                     0    FALSE
#> 3              259     medium                     0    FALSE
#> 4              248     medium                    10    FALSE
#> 5               92     medium                    10    FALSE
#> 6              518     medium                     0    FALSE
#> 7              518     medium                     0    FALSE
#> 8              216     medium                     0    FALSE
#> 9              153     medium                    10    FALSE
#> 10             518     medium                     0    FALSE
#> 11             320     medium                     0    FALSE
#> 12            4047     medium                     0    FALSE
#> 13             598     medium                     0    FALSE
#> 14              26     medium                     0    FALSE
#> 15             110     medium                    10    FALSE
#> 16              NA     medium                     0    FALSE
#> 17              NA     medium                     0    FALSE
#> 18              NA     medium                    10    FALSE
#> 19             110     medium                    10    FALSE
#> 20              NA     medium                     0    FALSE
#> 21              NA     medium                     0    FALSE
#> 22              NA     medium                     0    FALSE
#> 23              NA     medium                    10    FALSE
#> 24             110     medium                    10    FALSE
#> 25              NA     medium                     0    FALSE
#> 26              NA     medium                     0    FALSE
#> 27              NA     medium                     9    FALSE
#> 28              NA     medium                    10    FALSE
#> 29             151     medium                     0    FALSE
#> 30             151     medium                    10    FALSE
#> 31             193     medium                     0    FALSE
#> 32             259     medium                     0    FALSE
#> 33             248     medium                    10    FALSE
#> 34             293     medium                    10    FALSE
#> 35            1493     medium                    10    FALSE
#> 36              64     medium                     0    FALSE
#> 37             127     medium                    10    FALSE
#> 38              67     medium                    10    FALSE
#> 39            2280     medium                    10    FALSE
#> 40            4264     medium                     5    FALSE
#> 41             373     medium                    10    FALSE
#> 42              10     medium                    10    FALSE
#> 43             110     medium                     0    FALSE
#> 44             105     medium                     0    FALSE
#> 45             101     medium                    10    FALSE
#> 46             283     medium                    10    FALSE
#> 47             330     medium                     0    FALSE
#> 48              53     medium                    10    FALSE
#> 49             254     medium                     0    FALSE
#> 50              70     medium                     0    FALSE
#> 51            1561     medium                     0    FALSE
#> 52            1021     medium                     0    FALSE
#> 53             762     medium                    10    FALSE
#> 54             269     medium                    10    FALSE
#> 55             395     medium                     0    FALSE
#> 56             409     medium                    10    FALSE
#> 57              97     medium                     5    FALSE
#> 58              NA     medium                     0    FALSE
#> 59            1059     medium                     0    FALSE
#> 60            2060     medium                     6    FALSE
#> 61            1146     medium                     6    FALSE
#> 62            1970     medium                    10    FALSE
#> 63            2821     medium                    10    FALSE
#> 64            1082     medium                    10    FALSE
#> 65            2867     medium                     0    FALSE
#> 66              NA     medium                     0    FALSE
#> 67              NA     medium                    10    FALSE
#> 68             660     medium                    10    FALSE
```
