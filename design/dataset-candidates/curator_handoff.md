# Work order for HGT_DATA_CURATOR — dataset candidates with accessible metadata

**From** HGT_DATASET_SCOUT · 2026-11-07
**Companion files** `dataset_candidates.csv`, `dataset_candidates.md`

This order covers the seven candidates whose metadata I fetched and inspected during the survey. Every file path, column list and count below was read from the live source, not from a paper. Where I did not verify something I say so explicitly rather than guessing — treat those as questions for you, not instructions.

Do not take the proposed `pop` codes as settled. They are suggestions in the house style (`<Name><DATASET>` per `kgp::allmeta`); the maintainer's convention outranks mine.

---

## WO-1 — AADR v66.p1 Human Origins release (highest value, lowest effort)

**Why this first.** The package bundles the AADR 1240K release as 23,089 samples. The Human Origins release of the *same* AADR version carries 27,594 individuals. 3,325 of those are absent from the 1240K release, every one of them with `Data type == HumanOrigins`. 299 of the 3,325 fall in the priority regions — including the Swahili-coast Bajun communities of the Lamu archipelago, which is the closest thing to Indian Ocean rim coverage the AADR has.

### Exact file to ingest

```
https://dataverse.harvard.edu/api/access/datafile/13994528
  -> v66.p1_HO.aadr.PUB.anno   (15,465,906 bytes, 27,595 lines incl. header)
```

Companion files in the same Dataverse record (`doi:10.7910/DVN/FFIDCW`, dataset version 14.0, released 2026-06-08):

| datafile id | filename | bytes | rows (excl. header) |
|---|---|---|---|
| 13994515 | `v66.p1_1240K.aadr.PUB.anno` | 13,443,726 | 23,089 |
| 13994518 | `v66.p1_2M.aadr.PUB.anno` | 13,450,350 | 23,104 |
| 13994528 | `v66.p1_HO.aadr.PUB.anno` | 15,465,906 | 27,594 |

Network note: `dataverse.harvard.edu` 303-redirects file downloads to `dvn-cloud-iqss.s3.amazonaws.com`, so a sandboxed fetch needs both hosts allowlisted. Use `curl -L`.

### Columns

Tab-separated, 49 columns in all three annotation files, header row present. The header strings are long prose descriptions, not short names — strip and rename on read. Positions (1-based) and what they carry:

| # | content | maps to |
|---|---|---|
| 1 | Genetic ID (with `.DG`/`.SG`/`.AG`/`.HO`/`REF` suffixes encoding data type) | sample id |
| 2 | Persistent Genetic ID | sample id (stable) |
| 3 | Individual ID | **join key between releases** |
| 4–5 | Skeletal code, skeletal element | — |
| 6–9 | First publication, publication abbreviation, data DOI, repository link | `reference` |
| 10–13 | Dating method, date mean BP, date SD BP, full date string | `temporal`, date fields |
| 14 | Age at death, morphological sex | — |
| 15 | **Group ID** | `population_label` / `pop` stem |
| 16 | **Locality** | `coord_note` support |
| 17 | **Political Entity** | `region` assignment |
| 18 | **Latitude** | `sampling_lat` |
| 19 | **Longitude** | `sampling_lon` |
| 20–22 | Pulldown strategy, suffices, **Data type** | panel / `data_type` |
| 23–30 | Library count, coverage and SNP-hit statistics per SNP set | QC |
| 31 | Molecular Sex | sample attribute |
| 32 | Family relations | relatedness filter |
| 33–34 | ROH sums >20cM | QC |
| 35–37 | Y haplogroup (terminal-mutation, ISOGG, manual) | sample attribute |
| 38–40 | mtDNA coverage, haplogroup, consensus match | sample attribute |
| 41–47 | Damage rate, sex ratio, contamination estimates, library type, libraries, endogenous fraction | QC |
| 48–49 | ASSESSMENT, ASSESSMENT WARNINGS | **QC filter — do not ingest FAIL rows silently** |

### Schema mapping

- `pop` — Group ID verbatim, same as the existing AADR block. Do not append an `HO` suffix: these are AADR populations, and the suffix would fork `pop` values that should merge.
- `population_label` — Group ID with the AADR qualifier suffixes (`-o`, `_WGA`, `_oRelative`, `_lc`) stripped; the full string goes to `population_alt`.
- `source_dataset` — `AADR`, matching the bundled block.
- `temporal` — derive from columns 11–13: `modern` where the date is 0 BP / present-day, `ancient` otherwise. The existing AADR block already carries this split, so reuse that rule rather than inventing one.
- `sampling_lat` / `sampling_lon` — columns 18/19 directly.
- `origin_lat` / `origin_lon` — leave `NA`. AADR latitude/longitude is the archaeological site or collection point, never an ethnographic homeland. Set `coord_note` to say so.
- `reference` — column 6 (first publication) is the right one for provenance; column 7 is an abbreviation.

### Ambiguities you will have to resolve

1. **Which release is canonical.** `dataset_information.Rtable` describes AADR as "v66.0, 1240K release". The Dataverse record is labelled v66.p1 and now ships 1240K, 2M, HO and two compatibility sets. If the package's AADR block is to stay single-valued, you need a `data_type` or release column; if not, the same individual will appear under two `pop` entries. I would carry the release in a column and key populations on Group ID alone — but this is a maintainer decision.
2. **The 15-individual 2M delta.** The 2M annotation has 23,104 rows against 1240K's 23,089. I did not check whether those 15 are also in the HO set. Cheap to check once you have all three files loaded.
3. **Group ID suffix grammar.** AADR suffixes (`-o` outlier, `-oEurope` outlier toward Europe, `_WGA` whole-genome amplified, `_Henn-oRelative`) mix analytical qualifiers with sample-prep flags. `Kenya_Medieval_Swahili`, `Kenya_Medieval_Swahili-o` and `Kenya_Medieval_Swahili-oEurope` are one population or three depending on the rule you pick. Whatever you choose, the choice should be recorded in `assignment_basis`.
4. **QC rows.** Columns 48–49 flag contaminated and failing individuals. The bundled block's treatment of these is not documented in the shipped tables; match it, and state the rule.

### Priority-region delta (verified)

299 individuals across 54 Group IDs in Madagascar-adjacent, African and ISEA political entities. The largest:

```
Yemen        Yemeni_Highlands 29 · Yemeni_Northwest 28 · Yemeni_Desert 16
             Yemeni_Desert2-2 15 · Yemeni 6 · Jew_Yemenite 6
Kenya        Faza_Bajun 23 · Ndau_Bajun 19 · Jomvu_Mjomvu 13 · Somali 12
             Masai 10 · Tchundwa_Bajun 8 · Siyu_Bajun 6 · Luo 6 · Kikuyu 4
             Wasini_AlBaury 3 · Wasini_BaAlawy 3
South Africa Khomani 11
Philippines  Philippines_Kankanaey 8 · Tagalog 5 · Visayan 4 · Ilocano 2
Ethiopia     Ethiopia_BetaIsrael 7 · Afar 5
Tanzania     Hadza 4 · Datog 3 · Hadza_Henn-oRelative 2
India        Jew_Cochin 5 · Sherpa 4
Sudan        Shaigi_WGA 3
```

The Bajun and Jomvu/Wasini entries are Swahili-coast Muslim communities of coastal Kenya. They are the only Indian Ocean rim present-day populations in the AADR, and they are not in the package today.

---

## WO-2 — Larena et al. 2021, Philippine archipelago (115 groups, open table with coordinates)

**Exact file to ingest**

```
https://www.ebi.ac.uk/europepmc/webservices/rest/PMC8020671/supplementaryFiles
  -> pnas.2026132118.sd01.xlsx   (14,597 bytes, single sheet 'Sheet1', 116 rows × 6 columns)
```

The zip is 91.6 MB because it carries all ten SI datasets plus figures; `sd01.xlsx` is the one you want. `sd02.xlsx` holds per-individual ADMIXTURE output with sample IDs and population labels (sheets `k2`…`k15`) — useful if you need per-sample rows rather than per-population.

**Columns** (row 1 is the header; verified by parsing):

| column | header as written | content |
|---|---|---|
| 0 | `Group` | ethnic group name, e.g. `Agta Bulusan`, `PhBas_Itbayaten` |
| 1 | `Geographic Area` | Philippine region, e.g. `Bicol`, `Cagayan Valley`, `Calabarzon` |
| 2 | `Language Group` | e.g. `Bikol`, `Northeastern Luzon`, `Central Luzon` |
| 3 | `Negrito vs Non-Negrito` | values `Negrito` (25 groups) / `Non` (90 groups) |
| 4 | `GPS Cooridnates` *(sic — typo in the source)* | latitude, decimal degrees |
| 5 | *(blank header)* | longitude, decimal degrees |

Note the header typo and the unlabelled sixth column; do not key on header strings.

**Schema mapping**

- `pop` — `<Group>LAR`, with spaces removed: `AgtaBulusanLAR`, `AgtaCasiguranLAR`. 115 codes.
- `population_label` — `Group` verbatim.
- `population_alt` — `NA` at ingest; the paper also uses prefixed forms (`PhBas_Itbayaten`) that appear in `sd02`, so if you ingest per-sample rows those belong here pipe-separated.
- `population_desc` — compose from `Language Group` and the Negrito flag, e.g. `Bikol-speaking Negrito group, Bicol region`.
- `sampling_lat` / `sampling_lon` — columns 4 and 5.
- `origin_lat` / `origin_lon` — `NA`. These are collection-site coordinates; nothing in the SI claims they are ethnographic homelands.
- `region` — `Island_SE_Asia` or whatever the package settles on; the current `region` vocabulary has no value that fits the Philippines except `East_Asia`, which is wrong. **This needs a decision before ingest** (see WO-7).
- `temporal` — `modern`.
- `source_dataset` — `Larena_etal`.
- `reference` — Larena M et al. 2021 PNAS 118(13):e2026132118, doi 10.1073/pnas.2026132118.

**Ambiguities**

1. **No sample counts in `sd01`.** The table gives 115 groups but no n per group; the paper reports 1,028 individuals and the EGA dataset lists 1,094 samples. Per-group counts must be tallied from `sd02.xlsx` (individual-level) or the EGA dataset record.
2. **`Negrito` vs `Non`.** The value in column 3 is literally `Non`, not `Non-Negrito`. Decide whether to normalise.
3. **Group-name collisions.** `Kankanaey` appears both here and in the AADR HO delta (`Philippines_Kankanaey`, n=8), and `IgorotSGDP` is already bundled — Kankanaey is an Igorot group. Three sources, possibly overlapping, certainly not identical samples.
4. **Coordinate precision.** Values are given to two decimals (~1 km). Fine for mapping, not for anything finer.

---

## WO-3 — Hudjashov et al. 2017, GSE80534 (Indonesia; open metadata *and* open genotypes)

**Exact files to ingest**

```
https://ftp.ncbi.nlm.nih.gov/geo/series/GSE80nnn/GSE80534/soft/GSE80534_family.soft.gz   (metadata)
https://ftp.ncbi.nlm.nih.gov/geo/series/GSE80nnn/GSE80534/suppl/GSE80534_INDONESIA_AUG2016.bed.gz
                                                        .../GSE80534_INDONESIA_AUG2016.bim.gz
                                                        .../GSE80534_INDONESIA_AUG2016.fam.gz
```

**Columns.** Per-sample metadata lives in the SOFT record as `!Sample_characteristics_ch1` tag/value pairs. Verified keys, present on all 506 samples: `tissue`, `population`, `island`, `country`, `region`, `gender`. `!Sample_title` carries a readable id (`Alor02`). Platform is `GPL18441` = *[Axiom_GW_Hu_SNP] Affymetrix Axiom Genome-Wide CEU 1 Array Plate*.

**Verified per-population counts** (30 populations, 506 samples):

```
Sumatra 30 · Nias 28 · Bena 28 · Bama 28 · Anakalang 28 · Pantar 27 · Lembata 27
Wunga 26 · Mentawai 26 · Alor 23 · Java 21 · Baining 21 · Sulawesi 20 · Bali 19
Kamanasa 17 · Umanen Lawalu 16 · Taiwanese aborigines 15 · Vanuatu 14
Papuan-Highland 13 · Rampasasa 11 · Papuan-Coastal 11 · Western Samoa 10
Vietnamese 10 · Tonga 9 · Malay 8 · Tahiti 6 · Filipino 6 · Nasioi 4 · Rindi 3
Umaklaran 1
```

**Schema mapping**

- `pop` — `<Population>HUD`: `AlorHUD`, `MentawaiHUD`, `RampasasaHUD`.
- `population_label` — `population` verbatim.
- `population_desc` — compose from `island` + `country`.
- `sampling_lat` / `sampling_lon` — **not published**. The series gives island and country only. Either leave `NA` with a `coord_note`, or geocode island centroids and say in `coord_note` that the coordinate is an island centroid, not a collection site. Do not silently geocode.
- `temporal` — `modern`. `source_dataset` — `Hudjashov_etal`.
- `panel_information` — this is the Affymetrix **Axiom GW Hu SNP** array, *not* the Axiom Human Origins array the package already describes. It needs its own `panel_information` row, and the ascertainment note should say CEU-discovery rather than the Human Origins per-individual discovery panels.

**Ambiguities**

1. `Papuan-Highland` (13) and `Papuan-Coastal` (11) overlap conceptually with the bundled `PapuanHighlandsHGDP` (6) and `PapuanSepikHGDP` (8) — different samples, similar labels. `population_alt` must keep them distinct.
2. `Taiwanese aborigines` (15) is a cover term, not a population. It also overlaps GSE152200 (WO-4) which resolves Taiwanese groups by name.
3. `Malay` (8) here vs `MalayUNK` (25) bundled vs `Malay` (25) in GSE152200 — three entries, two of which match on count. See WO-5.
4. Genome build is not stated in the SOFT header; read it off the `.bim`.

---

## WO-4 — GSE152200, Taiwan Aborigines with ISEA and Madagascar comparatives

**Exact file to ingest**

```
https://ftp.ncbi.nlm.nih.gov/geo/series/GSE152nnn/GSE152200/soft/GSE152200_family.soft.gz
```

**Columns.** `!Sample_characteristics_ch1` keys, with coverage: `population` (298/298), `region` (298/298), `gender` (265), `tissue` (74), `sample type` (24), `cell type` (4). Three platforms in one series — `GPL8888` Illumina Human660W-Quad v1.0, `GPL13135` HumanOmniExpress BeadChip, `GPL22678` HumanOmniExpress-24 v1.1 — so the per-sample platform must be read from each GSM, not from the series.

**Verified per-population counts** (17 populations, summing exactly to 298):

```
Bajo 32 (Indonesia)       Malay 25 (Malaysia)        Vezo 24 (Madagascar)
Temoro/Antemoro 24 (Mad.) Lao 24 (Asia)              Murut 23 (Brunei)
Kankanaey/Igorot 22 (Ph.) Dusun 22 (Brunei)          Mikea 21 (Madagascar)
Burmese 20 (Myanmar)      Vietnamese 18 (Vietnam)    Lebbo 18 (Indonesia)
Luzon Filipino 12 (Ph.)   Vizayan Filipino 4 (Ph.)   Santhal 4 (Asia)
Nyishi 4 (Asia)           Naga 1 (Asia)
```

**Schema mapping.** `pop` as `<Population>TWN` *for the groups that are genuinely new* — Bajo, Dusun, Lebbo, Lao, Burmese, Santhal, Nyishi, Naga, Luzon Filipino, Vizayan Filipino. The rest need reconciliation first.

**The reconciliation, which is the real work here**

This series overlaps the package in two distinct ways, and I verified both by comparing counts:

| GSE152200 | n | bundled | n | reading |
|---|---|---|---|---|
| Mikea | 21 | `MikeaRAK` | 21 | exact match — Pierron et al. 2014 samples re-deposited |
| Vezo | 24 | `VezoRAK` | 24 | exact match |
| Temoro (Antemoro) | 24 | `TemoroRAK` | 24 | exact match |
| Malay | 25 | `MalayUNK` | 25 | exact match — likely resolves an `unconfirmed` block |
| Murut | 23 | `MurutUNK` | 17 | close, not equal |
| Vietnamese | 18 | `VietnameseUNK` | 22 | close, not equal |
| Kankanaey (Igorot) | 22 | `IgorotSGDP` | 35 | different source, same ethnonym |

The three Malagasy groups match the bundled `Rakotoarivony_etal` block exactly and are **duplicates, not new samples** — they are the Pierron et al. 2014 individuals (GEO `GSE53445`, 69 samples = 21 + 24 + 24) appearing in a second series. Ingesting them as new rows would double-count Madagascar.

`MalayUNK` matching at exactly 25 is strong but not conclusive evidence that the package's unconfirmed Malay block came from this series. `MurutUNK` (17 vs 23) and `VietnameseUNK` (18 vs 22) are near misses that could be QC filtering in either direction. **Please compare sample IDs before concluding anything** — the GSM titles are available in the SOFT record and the bundled `sample_information.Rtable` has `source_id`. If the IDs match, this series resolves four `unconfirmed` blocks and supplies a citable `source_dataset`; if they don't, the similarity is coincidence and should be recorded as such in `assignment_basis`.

**Other ambiguities**

1. No DOI resolved for this series in my pass. It is associated with the Mackay Memorial Hospital / Trejaut–Lin Taiwan group, but I did not confirm a specific publication — `reference` will need one before ingest.
2. `region` values are coarse and inconsistent (`Asia` for Lao, Santhal, Nyishi and Naga; country names elsewhere).
3. `Temoro (Antemoro)` and `Kankanaey (Igorot)` carry parenthetical synonyms — the natural source for `population_alt`.
4. No coordinates published.

---

## WO-5 — Hodgson et al. 2014, Dryad (provenance fix for `YemeniUNK`)

**Exact deposit**

```
https://datadryad.org/api/v2/datasets/doi%3A10.5061%2Fdryad.d9s74
  README_for_yemen.txt   787 bytes
  yemen.zip          6,052,691 bytes   (binary PED: .bed/.bim/.fam)
```

The Dryad file description states the deposit is "the complete unfiltered new genotype data from 64 Yemeni samples in a binary PED (BED) file". The package has `YemeniUNK`, n=64, `source_dataset = unconfirmed`. That is almost certainly this deposit.

**What I ask you to do**

This is not an ingest of new populations. It is a provenance fix: if the 64 IDs in `yemen.fam` match the 64 `YemeniUNK` rows in `sample_information.Rtable`, then `source_dataset` becomes `Hodgson_etal`, `reference` becomes Hodgson JA, Mulligan CJ, Al-Meeri A, Raaum RL 2014 *PLoS Genet* 10(6):e1004393 (doi 10.1371/journal.pgen.1004393), `pop` can move from `YemeniUNK` to a proper code, and `assignment_basis` records the match. One unconfirmed block of 64 samples becomes a cited, openly archived source.

**Ambiguities**

1. Genotyping platform is in the paper, not in the Dryad metadata — read the `.bim` SNP count and confirm against the PLoS Genet methods.
2. The Dryad *file-download* endpoint returned `{"error":"Unauthorized, must have current bearer token"}` for the README when called through the API. The dataset landing page serves files without a token; use the landing-page download route, not `/api/v2/files/{id}/download`.
3. The paper's analysis panel is larger than 64; only the 64 new Yemeni samples were deposited. Do not infer the rest of the panel from the citation.

---

## WO-6 — the two lower-priority open sources, in brief

**GSE73996 (Siberia + worldwide, Axiom).** `https://ftp.ncbi.nlm.nih.gov/geo/series/GSE73nnn/GSE73996/soft/GSE73996_family.soft.gz`. 651 samples, 40 populations, characteristic keys `tissue` / `region` / `population` / `gender` on all 651. Platform `GPL18441`, the same Axiom GW Hu SNP array as WO-3 — one `panel_information` row serves both. Open genotypes at `.../suppl/GSE73996_SIBERIA.bed.gz`, and the Axiom annotation CSV ships alongside. Expect sample-level overlap with SGDP Siberian populations; reconcile before merge.

**Bergstrom et al. 2020, HGDP WGS.** `https://www.ebi.ac.uk/ena/portal/api/filereport?accession=PRJEB6463&result=read_run&fields=run_accession,sample_accession,sample_title,country,collection_date&format=tsv` (plus `PRJEB14173`). Verified: `sample_title` is the HGDP identifier (`HGDP01196`, `HGDP00779`, `HGDP01033`), so this joins straight onto the bundled HGDP block on sample id. This adds **no new individuals** — it adds a WGS `data_type` and an open ENA accession to populations already present. Worth doing because the package's design-tool ambition needs ascertainment to be explicit: HGDP-on-array and HGDP-as-WGS have different ascertainment properties and a researcher choosing datasets to merge needs to see that.

---

## WO-7 — one schema question that blocks several of these

The `region` vocabulary in `population_information.Rtable` is currently `Africa`, `America`, `Central_Asia`, `East_Asia`, `Europe`, `Madagascar`, `Mid_East`, `North_Africa`, `Oceania`, `South_Asia`. Three of the candidates above (Larena Philippines, Hudjashov Indonesia, GSE152200) are Island Southeast Asian, and that region has no correct value — they would be forced into `East_Asia` or `Oceania`, both of which misdescribe them.

`Madagascar` already exists as its own region value, which suggests the vocabulary is allowed to grow where a region matters analytically. Island Southeast Asia matters analytically for exactly the same reason Madagascar does: it is the Austronesian source region. I would add `Island_SE_Asia`, but this is a maintainer decision, and ingesting ~160 new populations under a wrong region value would be expensive to undo.

**Please settle this before WO-2, WO-3 and WO-4.**

---

## Open items I could not close

1. **Brucato et al. 2018, Comoros** (AJHG, PMID 29304377, doi 10.1016/j.ajhg.2017.12.011) — the earliest Austronesian gene flow into the Swahili corridor, squarely in the top-priority region. No accession recoverable: PMC holds the abstract only, Europe PMC returned HTTP 500 for the full text, and there is no BioProject or GEO link. Needs the publisher PDF or an author enquiry.
2. **Pierron et al. 2017 (MAGE)** — EGA study `EGAS00001002549` is open and confirms 737 samples, but the Europe PMC supplementary package for `PMC5559028` is empty (verified), so the 257-village table must come from pnas.org directly. The grid design also needs a maintainer decision on how non-ethnonymic sampling units are keyed.
3. **Kusuma et al. 2016** (`EGAS00001001841`, Banjar/Ngaju) — study record open, no per-sample counts published at EGA; the MBE supplement is needed. High value for Madagascar work despite the small effort gap.
4. **Schlebusch et al. 2012, Scheinfeldt et al. 2019, Jinam et al. 2017, Patin et al. 2017, HUGO Pan-Asian 2009** — on-target scientifically, no machine-recoverable accession from open full text in this pass. Worth a manual revisit.
