# humangentools — dataset candidate survey

**Prepared by** HGT_DATASET_SCOUT · survey run 2026-11-07
**Scope** Public archives and literature search for human genotype and SNP-array datasets not already bundled in humangentools, prioritised by coverage gaps (Madagascar and the Indian Ocean rim first, then continental Africa and Island Southeast Asia) and by whether their sample- and population-level metadata is openly accessible.

## What the package currently holds, and where it is thin

Read from `inst/extdata/` at the HEAD of `main`:

| source_dataset | populations | samples |
|---|---|---|
| AADR | 3,897 | 23,089 |
| SGDP | 88 | 339 |
| HGDP | 56 | 952 |
| KGP | 26 | 3,202 |
| unconfirmed | 21 | 255 |
| HapMap | 12 | 463 |
| KGP_IGSR | 10 | 102 |
| Rakotoarivony_etal | 8 | 288 |
| Perry_etal_unconfirmed | 2 | 230 |

Three gaps dominate, and two of them are verified facts rather than impressions.

**Madagascar.** The entire Malagasy holding is the 288-sample, 8-population `Rakotoarivony_etal` block (Betsileo, Betsimisaraka, Diego, Merina, Mikea, Sakalava_Tsimihety, Temoro, Vezo). I downloaded the AADR v66.p1 annotation files and confirmed that **AADR contains zero individuals from Madagascar or the Comoros** in either the 1240K or the Human Origins release. There is no second Malagasy source in the package.

**Island Southeast Asia.** One Philippine population (`IgorotSGDP`, n=35), one Bornean block of unknown provenance (`BorneoUNK`, n=39), and scattered `unconfirmed` entries (MalayUNK 25, MurutUNK 17, VietnameseUNK 22). For a package built to support Austronesian-dispersal questions this is the weakest link in the chain between Taiwan and Madagascar.

**Provenance.** 23 populations and 485 samples sit under `source_dataset` values of `unconfirmed` or `Perry_etal_unconfirmed`. Two candidates below (ranks 5 and 7) are strong leads for resolving several of these.

## Ranking rationale

Candidates are ranked on four criteria, in this order of weight:

1. **Does it close a priority coverage gap?** Madagascar and the Indian Ocean rim outrank continental Africa, which outranks Island Southeast Asia, which outranks everything else.
2. **Can the metadata be ingested now?** The package ships metadata, not genotypes, so a controlled-access study with an open sample table is as useful as a fully open one. A study whose population table exists only behind a Data Access Committee is not.
3. **How much does it add per unit of curation work?** A 115-group open table with coordinates (rank 3) is worth more than a three-sample deposit.
4. **Does it interlock with structures the package already has?** Datasets on the Affymetrix Human Origins panel (ranks 1 and 8) key straight into `Axiom_Human_Origins_Panels` and `get_axiom_snps()`; an African-ascertained array (rank 15) gives `get_panel_information()` a contrast it currently lacks.

Rank 1 wins outright on criteria 2 and 4, and it is the only candidate that adds priority-region individuals with no access application at all.

## Addable now versus blocked

**Addable now — fully open metadata, no application needed (7):**

- **1. AADR v66.p1 Human Origins release.** Open `.anno`, 49 columns, 3,325 individuals absent from the bundled 1240K release.
- **4. Hudjashov et al. 2017 (GSE80534).** Open sample characteristics *and* open PLINK genotypes.
- **5. GSE152200 (Taiwan / ISEA / Madagascar).** Open sample characteristics; the single best lead for resolving `unconfirmed` blocks.
- **7. Hodgson et al. 2014 (Dryad).** Open deposit; converts `YemeniUNK` into a cited source.
- **12. GSE73996 (Siberia, Axiom).** Open sample characteristics and genotypes.
- **13. Bergstrom et al. 2020 (ENA).** Open run/sample table; a `data_type` refresh of the existing HGDP block.
- **3. Larena et al. 2021 — metadata only.** SI Dataset S1 is open and complete (115 groups, language group, Negrito status, coordinates). The genotypes are controlled, but nothing in the package needs them.

**Addable now at study/dataset level, with per-sample detail requiring an application (5):** ranks 8 (Oliveira), 9 (AGVP), 10 (Haber), 14 (Uganda Genome Resource), 15 (H3Africa panel). For each of these the EGA study and dataset records are open and already give population names, platforms and sample counts; per-sample tables sit behind an EGA Data Access Committee.

**Blocked or partial (3):**

- **2. Pierron et al. 2017 (MAGE).** The highest-priority dataset in the survey by region, and the most awkward. The EGA study record is open and confirms 737 samples, but the village-level table with coordinates is in the PNAS SI Appendix, and the Europe PMC supplementary package for PMC5559028 returned empty — the curator must fetch the SI from pnas.org directly. The 257-village grid design also does not map onto a named-population schema without a decision from the maintainer.
- **6. Kusuma et al. 2016.** Study record open, per-sample counts not published at EGA; needs the MBE supplement.
- **11. Fortes-Lima et al. 2024.** Metadata is scattered across nine EGA datasets, two EGA studies and a dbGaP study; one consolidation pass is needed before it is tractable.

## Considered and set aside

- **Brielle et al. 2023, medieval Swahili coast (Nature, PMID 36991187; ENA PRJEB58698).** Excluded as a separate candidate because it is *already in the bundled data*: the AADR 1240K release the package ships contains `Kenya_Medieval_Swahili` (48), `Kenya_Swahili` (22) and `Tanzania_Swahili` (8), verified by reading the annotation file.
- **Pierron et al. 2014 (PNAS, GEO GSE53445, 69 samples).** Already bundled — the Mikea (21), Vezo (24) and Temoro (24) counts in `population_information.Rtable` match the GEO series exactly.
- **Brucato et al. 2018, Comoros (AJHG, PMID 29304377).** A top-priority-region dataset — the earliest Austronesian gene flow into the Swahili corridor — but **no accession could be recovered**. PubMed Central holds only the abstract, Europe PMC returned HTTP 500 for the full text, and the paper has no BioProject or GEO link. This is an open item, not a negative finding: the accession likely exists and needs either the publisher PDF or a note to the authors.
- **Brucato et al. 2019 (EGAS00001003425).** Verified: Illumina Human Omni5 (4,284,426 SNPs), but only **three** samples (two Yemeni, one Somali). Too small to rank, though the three carry the Malagasy mtDNA motif and may be worth a footnote in a Madagascar-focused analysis.
- **Schlebusch et al. 2012 (Khoe-San), Scheinfeldt et al. 2019 (East African hunter-gatherers), Jinam et al. 2017 (Negritos), Patin et al. 2017 (Bantu dispersals), HUGO Pan-Asian SNP Consortium 2009.** All are scientifically on-target; none yielded a machine-recoverable accession from open full text in this pass (no PMC full text, or full text present with no accession pattern). They should be revisited by hand.
- **GSE21478 (Behar et al. 2010, Jewish populations, 76 GEO samples, Illumina 650Y/610-Quad/660W).** Fully open and well annotated, but outside the priority regions.

## Full dossiers

### 1. AADR v66.p1 Human Origins release (refresh/extension of bundled AADR)

**Citation.** Mallick S et al. 2024. The Allen Ancient DNA Resource (AADR): a curated compendium of ancient human genomes. Sci Data 11:182.

**DOI.** 10.1038/s41597-024-03031-7

**Accession.** Harvard Dataverse doi:10.7910/DVN/FFIDCW (dataset version 14.0, released 2026-06-08); files v66.p1_HO.aadr.PUB.anno, v66.p1_2M.aadr.PUB.anno, v66.p1_1240K.aadr.PUB.anno

**Genotyping platform.** Affymetrix Human Origins array (HO release); 1240K in-solution capture; 2M enhanced capture

**Ascertainment scheme.** HO: 13 panels ascertained on heterozygous sites in single individuals of known population (panel scheme already in Axiom_Panel_info.Rtable). 1240K/2M: targeted capture of a fixed SNP set.

**Sample count.** HO anno 27,594 individuals (VERIFIED line count); 1240K anno 23,089 (VERIFIED, == bundled AADR n_samples); 2M anno 23,104. 3,325 individuals in HO but NOT in 1240K, all Data type == HumanOrigins (VERIFIED).

**Population count.** 3,325 HO-only individuals span 54 Group IDs within the priority regions alone

**Geographic / ethnolinguistic coverage.** Worldwide. HO-only delta in priority regions = 299 individuals: Kenya Swahili-coast Bajun communities (Faza_Bajun 23, Ndau_Bajun 19, Tchundwa_Bajun 8, Siyu_Bajun 6, Jomvu_Mjomvu 13, Wasini_AlBaury 3, Wasini_BaAlawy 3), Yemen (Yemeni_Highlands 29, Yemeni_Northwest 28, Yemeni_Desert 16, Yemeni_Desert2-2 15, Yemeni 6, Jew_Yemenite 6), Philippines (Kankanaey 8, Tagalog 5, Visayan 4, Ilocano 2), Ethiopia (BetaIsrael 7, Afar 5), Tanzania (Hadza 4+2, Datog 3), Kenya (Somali 12, Masai 10, Luo 6, Kikuyu 4), South Africa (Khomani 11), India (Jew_Cochin 5, Sherpa 4), Sudan (Shaigi_WGA 3).

**Genome build.** hg19 / GRCh37

**Access tier and consent.** OPEN - no registration. Dataverse serves .anno/.ind/.snp/.geno directly (download redirects to dvn-cloud-iqss.s3.amazonaws.com).

**Published metadata.** FULLY OPEN. 49-column .anno per release (VERIFIED). Columns include Genetic ID, Individual ID, Group ID, Locality, Political Entity, Latitude, Longitude, Date mean in BP, Full Date, Data type, Molecular Sex, Family relations, Y/mtDNA haplogroups, ASSESSMENT.

**Metadata URL / accession.** https://dataverse.harvard.edu/api/access/datafile/13994528 (HO anno, 15,465,906 bytes); /13994515 (1240K); /13994518 (2M)

**Keying into the package `pop` convention.** Yes. Group ID is already the key the package's AADR block uses; new rows follow the same convention. temporal from the date columns; Latitude/Longitude -> sampling_lat/sampling_lon.

**Overlap with bundled data.** Heavy by design: 23,089 of 27,594 HO individuals are the bundled AADR block. Ingest ONLY the 3,325-row delta. VERIFIED: AADR contains ZERO Madagascar or Comoros individuals in either release.

**Addable now.** YES

**Blocker.** none


### 2. Pierron et al. 2017 - MAGE grid-based survey of Madagascar

**Citation.** Pierron D et al. 2017. Genomic landscape of human diversity across Madagascar. PNAS 114(32):E6498-E6506. PMID 28716916.

**DOI.** 10.1073/pnas.1704906114

**Accession.** EGAS00001002549

**Genotyping platform.** EGA technologies field records only 'Genotype' (VERIFIED); paper reports genome-wide SNP genotyping plus mtDNA/Y. Platform must be confirmed from the SI.

**Ascertainment scheme.** Commercial array ascertainment (panel to be confirmed).

**Sample count.** 737 samples in the EGA dataset (VERIFIED via metadata.ega-archive.org); paper describes a 2,704-individual grid survey with genome-wide data on a subset.

**Population count.** Grid design: 257 villages, not named ethnic populations

**Geographic / ethnolinguistic coverage.** Madagascar, island-wide grid - THE top coverage gap. Current package holds 288 Malagasy samples in 8 named groups only.

**Genome build.** GRCh37 (to confirm)

**Access tier and consent.** SPLIT. Study record OPEN (title, description, sample count, policy). Genotypes CONTROLLED: EGA Data Access Committee under policy EGAP00001000986; apply via the EGA DAC request form naming the dataset.

**Published metadata.** Study-level metadata open at the EGA REST endpoint. Per-village table with coordinates is in the PNAS SI Appendix - NOT retrievable from Europe PMC (supplementary package for PMC5559028 returned empty, VERIFIED); curator must pull it from pnas.org.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001002549 ; SI at https://www.pnas.org/doi/10.1073/pnas.1704906114

**Keying into the package `pop` convention.** Partly. Villages are not populations; needs a grid-cell or regional aggregation keyed as <Name>MAGE, or village codes carried in population_alt.

**Overlap with bundled data.** Likely overlaps the existing Rakotoarivony_etal/Pierron Malagasy block - sample identifiers must be compared before merge.

**Addable now.** PARTIAL

**Blocker.** Population table only in PNAS SI; EGA sample list behind DAC.


### 3. Larena et al. 2021 - Philippine archipelago, 115 ethnic groups

**Citation.** Larena M et al. 2021. Multiple migrations to the Philippines during the last 50,000 years. PNAS 118(13):e2026132118. PMID 33753512.

**DOI.** 10.1073/pnas.2026132118

**Accession.** EGAS00001005083 (modern genotypes); ENA PRJEB43078 (ancient DNA); companion Ayta study EGAS00001005407 / EGAS00001005408

**Genotyping platform.** EGA technologies field empty for EGAS00001005083 (VERIFIED); paper reports ~2.3M genotyped SNPs. Companion datasets list Illumina / Illumina NovaSeq 6000.

**Ascertainment scheme.** Commercial Illumina array; confirm chip from SI.

**Sample count.** 1,094 samples in the EGA dataset (VERIFIED); paper analyses 1,028 individuals.

**Population count.** 115 groups (VERIFIED from SI Dataset S1: 25 Negrito, 90 non-Negrito)

**Geographic / ethnolinguistic coverage.** Philippines, archipelago-wide; the densest ethnolinguistic sampling of Island Southeast Asia available. Package currently has ONE Philippine population (IgorotSGDP, n=35).

**Genome build.** GRCh37 (to confirm)

**Access tier and consent.** SPLIT. Population metadata FULLY OPEN; genotypes CONTROLLED (EGA DAC, policy EGAP00001001987, released on request). Ancient data at ENA PRJEB43078 is open.

**Published metadata.** FULLY OPEN and VERIFIED: SI Dataset S1 is a 115-row x 6-column table - Group, Geographic Area, Language Group, 'Negrito vs Non-Negrito', GPS latitude, GPS longitude. Downloaded and parsed from the Europe PMC supplementary package.

**Metadata URL / accession.** https://www.ebi.ac.uk/europepmc/webservices/rest/PMC8020671/supplementaryFiles -> pnas.2026132118.sd01.xlsx

**Keying into the package `pop` convention.** Yes, cleanly. Group -> population_label, <Group>LAR -> pop; GPS columns -> sampling_lat/sampling_lon; Language Group and Negrito status -> population_desc.

**Overlap with bundled data.** No sample-level overlap expected with bundled data. Kankanaey appears in both this study and AADR HO - check before double-counting.

**Addable now.** YES (metadata)

**Blocker.** none for metadata; genotypes need an EGA DAC application


### 4. Hudjashov et al. 2017 - Indonesian archipelago / Island Southeast Asia

**Citation.** Hudjashov G et al. 2017. Complex Patterns of Admixture across the Indonesian Archipelago. Mol Biol Evol 34(10):2439-2452. PMID 28957506.

**DOI.** 10.1093/molbev/msx196

**Accession.** GEO GSE80534

**Genotyping platform.** GPL18441 = [Axiom_GW_Hu_SNP] Affymetrix Axiom Genome-Wide CEU 1 Array Plate (VERIFIED from the series SOFT record)

**Ascertainment scheme.** Affymetrix Axiom GW Hu SNP - CEU-discovery ascertainment; a different panel from the Human Origins Axiom panels the package already describes, so panel_information would need a new entry.

**Sample count.** 506 samples (VERIFIED count of ^SAMPLE records)

**Population count.** 30 populations (VERIFIED per-population counts, e.g. Sumatra 30, Nias 28, Bena 28, Bama 28, Anakalang 28, Pantar 27, Lembata 27, Wunga 26, Mentawai 26, Alor 23, Java 21, Baining 21, Sulawesi 20, Bali 19, Kamanasa 17, Umanen Lawalu 16, Taiwanese aborigines 15, Vanuatu 14, Papuan-Highland 13, Rampasasa 11, Papuan-Coastal 11, Western Samoa 10, Vietnamese 10, Tonga 9, Malay 8, Tahiti 6, Filipino 6, Nasioi 4, Rindi 3, Umaklaran 1)

**Geographic / ethnolinguistic coverage.** Indonesia from Sumatra to Papua, plus Vanuatu, Samoa, Tonga, Tahiti, Taiwan. Directly addresses the Island Southeast Asia gap and the Austronesian source question for Madagascar.

**Genome build.** Not stated in the SOFT header; read from GSE80534_INDONESIA_AUG2016.bim.

**Access tier and consent.** FULLY OPEN - no registration. Genotypes themselves are open (PLINK bed/bim/fam supplementary files).

**Published metadata.** FULLY OPEN and VERIFIED. Sample characteristic keys: tissue, population, island, country, region, gender. One row per GSM with sample title (e.g. Alor02).

**Metadata URL / accession.** https://ftp.ncbi.nlm.nih.gov/geo/series/GSE80nnn/GSE80534/soft/GSE80534_family.soft.gz ; genotypes at .../suppl/GSE80534_INDONESIA_AUG2016.{bed,bim,fam}.gz

**Keying into the package `pop` convention.** Yes. population -> population_label, <Population>HUD -> pop; island + country -> sampling locality (coordinates must be assigned by the curator, not published here).

**Overlap with bundled data.** Possible overlap with SGDP/HGDP Papuan and Oceanian samples and with the package's BorneoUNK/MalayUNK blocks - compare sample IDs.

**Addable now.** YES

**Blocker.** none


### 5. GSE152200 - Taiwan Aborigines with ISEA and Madagascar comparatives

**Citation.** GEO series GSE152200, 'Aborigines of Taiwan and their genetic relationship to East Asian and Pacific populations' (Mackay Memorial Hospital / Trejaut-Lin group).

**DOI.** not resolved to a single publication in this pass

**Accession.** GEO GSE152200

**Genotyping platform.** Three platforms (VERIFIED): GPL8888 Illumina Human660W-Quad v1.0, GPL13135 HumanOmniExpress BeadChip, GPL22678 HumanOmniExpress-24 v1.1

**Ascertainment scheme.** Illumina commercial arrays (660W / OmniExpress) - mixed-panel series, so ascertainment differs between sample blocks.

**Sample count.** 298 samples (VERIFIED, and the per-population counts sum exactly to 298)

**Population count.** 17 populations (VERIFIED): Bajo 32 (Indonesia), Malay 25 (Malaysia), Vezo 24 (Madagascar), Temoro/Antemoro 24 (Madagascar), Lao 24, Murut 23 (Brunei), Kankanaey/Igorot 22 (Philippines), Dusun 22 (Brunei), Mikea 21 (Madagascar), Burmese 20, Vietnamese 18, Lebbo 18 (Indonesia), Luzon Filipino 12, Vizayan Filipino 4, Santhal 4, Nyishi 4, Naga 1

**Geographic / ethnolinguistic coverage.** Taiwan, Borneo, Philippines, mainland SE Asia, plus the three Malagasy groups of Pierron et al. 2014 - the Austronesian corridor from Taiwan to Madagascar in one open series.

**Genome build.** Not stated in the SOFT header.

**Access tier and consent.** FULLY OPEN - no registration.

**Published metadata.** FULLY OPEN and VERIFIED. Characteristic keys: population (298/298), region (298/298), gender (265), tissue (74), sample type (24), cell type (4).

**Metadata URL / accession.** https://ftp.ncbi.nlm.nih.gov/geo/series/GSE152nnn/GSE152200/soft/GSE152200_family.soft.gz

**Keying into the package `pop` convention.** Yes, but demands care - see the duplicate column.

**Overlap with bundled data.** SUBSTANTIAL AND VERIFIED. Mikea 21 / Vezo 24 / Temoro 24 match MikeaRAK (21), VezoRAK (24) and TemoroRAK (24) exactly - these are the Pierron et al. 2014 samples re-deposited. Malay 25 matches MalayUNK (25) exactly. Murut 23 vs MurutUNK 17 and Vietnamese 18 vs VietnameseUNK 22 are close but not equal. This series is therefore the strongest lead for resolving the package's 'unconfirmed' source_dataset blocks.

**Addable now.** YES

**Blocker.** none, but sample-ID reconciliation against the existing RAK and UNK blocks is mandatory before merge


### 6. Kusuma et al. 2016 - Banjar and Ngaju of Southeast Borneo (Malagasy Asian source)

**Citation.** Kusuma P et al. 2016. Malagasy Genetic Ancestry Comes from an Historical Malay Trading Post in Southeast Borneo. Mol Biol Evol 33(9):2396-2400. PMID 27381999.

**DOI.** 10.1093/molbev/msw117

**Accession.** EGAS00001001841

**Genotyping platform.** Illumina array (EGA technologies field truncated in the study listing; confirm from the dataset record)

**Ascertainment scheme.** Commercial Illumina array.

**Sample count.** To be read from the EGA dataset record (study has 1 dataset); paper reports ~200 Banjar and Ngaju individuals.

**Population count.** 2 named groups (Banjar, Ngaju) plus comparatives

**Geographic / ethnolinguistic coverage.** Southeast Borneo (South Kalimantan). The single most important non-Malagasy population for Madagascar research design - the proposed Asian parent population of the Malagasy.

**Genome build.** GRCh37 (to confirm)

**Access tier and consent.** SPLIT. Study record OPEN (title and abstract VERIFIED, PubMed link 27381999 present). Genotypes CONTROLLED via EGA DAC.

**Published metadata.** Study-level only at EGA; per-sample table is in the MBE supplementary material.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001001841

**Keying into the package `pop` convention.** Yes - BanjarKUS, NgajuKUS.

**Overlap with bundled data.** Possible overlap with the package's BorneoUNK (39 samples) - must be checked.

**Addable now.** PARTIAL

**Blocker.** per-sample counts need the MBE supplement or the EGA dataset record


### 7. Hodgson et al. 2014 - Yemeni panel on Dryad (resolves the package's YemeniUNK block)

**Citation.** Hodgson JA, Mulligan CJ, Al-Meeri A, Raaum RL. 2014. Early back-to-Africa migration into the Horn of Africa. PLoS Genet 10(6):e1004393. PMID 24921250.

**DOI.** 10.1371/journal.pgen.1004393

**Accession.** Dryad doi:10.5061/dryad.d9s74

**Genotyping platform.** Illumina (chip stated in the paper; the Dryad deposit is unfiltered binary PED)

**Ascertainment scheme.** Commercial Illumina array.

**Sample count.** 64 Yemeni samples (VERIFIED from the Dryad file description)

**Population count.** 1 (Yemeni); the paper's analysis panel is larger

**Geographic / ethnolinguistic coverage.** Yemen - southern Arabian / Indian Ocean rim, a documented Austronesian-contact region.

**Genome build.** GRCh37 (to confirm from the .bim)

**Access tier and consent.** FULLY OPEN. Dryad deposit, 2 files: README_for_yemen.txt (787 bytes) and yemen.zip (6,052,691 bytes, binary PED). VERIFIED via the Dryad API. Note: the file-download endpoint returned 'Unauthorized, must have current bearer token' for the README; the dataset landing page serves files without a token.

**Published metadata.** File-level metadata open and verified; sample-level annotation is inside the zip and in the PLoS Genet supplement.

**Metadata URL / accession.** https://datadryad.org/api/v2/datasets/doi%3A10.5061%2Fdryad.d9s74

**Keying into the package `pop` convention.** Yes - YemeniHOD or similar.

**Overlap with bundled data.** ALMOST CERTAINLY the package's existing YemeniUNK block (n=64, source_dataset 'unconfirmed'). Ingesting this converts an unconfirmed block into a cited, openly archived source rather than adding new samples.

**Addable now.** YES

**Blocker.** none


### 8. Oliveira et al. 2023 - Angolan Namib Desert, pre-Bantu ancestry

**Citation.** Oliveira S et al. 2023. Genome-wide variation in the Angolan Namib Desert reveals unique pre-Bantu ancestry. Sci Adv 9(38):eadh3822. PMID 37738339.

**DOI.** 10.1126/sciadv.adh3822

**Accession.** EGAS00001007011

**Genotyping platform.** Affymetrix Axiom Genome-Wide Human Origins array (VERIFIED from the EGA dataset technologies field)

**Ascertainment scheme.** Human Origins panel ascertainment - directly compatible with the package's Axiom_Human_Origins_Panels and get_axiom_snps().

**Sample count.** 209 samples in the EGA dataset (VERIFIED); the paper's data-availability statement says 208 Angolan individuals.

**Population count.** Multiple Namib Desert communities (Kwepe, Kwisi, Twa, Himba, Kuvale and others)

**Geographic / ethnolinguistic coverage.** Southwest Angola - a southern African ancestry stratum not represented by HGDP, SGDP or 1000 Genomes.

**Genome build.** GRCh37

**Access tier and consent.** SPLIT. Study record OPEN. Genotypes CONTROLLED: EGA DAC under policy EGAP00001003136 (VERIFIED).

**Published metadata.** Study-level open at EGA; per-sample population assignments in the Sci Adv supplementary materials (Sci Adv is fully open access).

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001007011

**Keying into the package `pop` convention.** Yes - <Name>OLI.

**Overlap with bundled data.** None expected with bundled data.

**Addable now.** YES (metadata)

**Blocker.** genotypes need an EGA DAC application


### 9. African Genome Variation Project (AGVP)

**Citation.** Gurdasani D et al. 2015. The African Genome Variation Project shapes medical genetics in Africa. Nature 517(7534):327-332. PMID 25470054.

**DOI.** 10.1038/nature13997

**Accession.** EGAS00001000959 (14 datasets, e.g. EGAD00010001045 'APCDR AGV Igbo', 99 individuals)

**Genotyping platform.** Illumina HumanOmni2.5-4v1_B and HumanOmni2-5_8v1_A (VERIFIED from the EGA dataset technologies fields)

**Ascertainment scheme.** Illumina Omni 2.5M commercial panel.

**Sample count.** 1,481 individuals (study description, VERIFIED text); EGA num_samples fields are 0 for these datasets so per-dataset counts must come from the dataset titles/descriptions.

**Population count.** 18 ethnolinguistic groups

**Geographic / ethnolinguistic coverage.** Sub-Saharan Africa: West, East and Southern Africa including Ethiopia, Uganda, Zambia, Nigeria, Ghana, South Africa.

**Genome build.** GRCh37

**Access tier and consent.** SPLIT. Study and all 14 dataset records OPEN (titles name the ethnic group and sample size). Genotypes CONTROLLED: EGA DAC, policy EGAP00001000325 and siblings; the APCDR also operates its own access route.

**Published metadata.** Dataset-level metadata is open and per-group - e.g. 'APCDR AGV Project: Array data from 99 Igbo'. That alone yields population labels and sample counts.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001000959/datasets

**Keying into the package `pop` convention.** Yes - <Name>AGVP. Several groups (Igbo, Yoruba, Zulu, Baganda) already exist under other source_dataset values; population_alt will need the AGVP spelling.

**Overlap with bundled data.** Population-name overlap with 1000 Genomes YRI/LWK and HGDP Bantu groups; no sample-level overlap expected.

**Addable now.** YES (metadata)

**Blocker.** genotypes need an EGA DAC / APCDR application


### 10. Haber et al. 2016 - Chad and Yemen genotyping

**Citation.** Haber M et al. 2016. Chad Genetic Diversity Reveals an African History Marked by Multiple Holocene Eurasian Migrations. Am J Hum Genet 99(6):1316-1324. PMID 27889059.

**DOI.** 10.1016/j.ajhg.2016.10.012

**Accession.** EGAS00001001231 (6 datasets; EGAD00001001440, EGAD00001002742 cited in the paper)

**Genotyping platform.** Illumina HumanOmni2.5-8 v1.1 B / v1.2 A (VERIFIED from the EGA dataset technologies fields)

**Ascertainment scheme.** Illumina Omni 2.5M commercial panel.

**Sample count.** 385 samples across the study's datasets (VERIFIED sum of EGA num_samples)

**Population count.** Multiple Chadian ethnolinguistic groups plus Yemeni

**Geographic / ethnolinguistic coverage.** Chad (Sahel/Central Africa, thin in the package) and Yemen (Indian Ocean rim).

**Genome build.** GRCh37

**Access tier and consent.** SPLIT. Study record OPEN but flagged as a Wellcome Sanger pre-publication release with a possible moratorium (VERIFIED in the description). Genotypes CONTROLLED: policies EGAP00001000210 and EGAP00001000390.

**Published metadata.** Study and dataset records open; per-sample population labels are in the AJHG supplement.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001001231

**Keying into the package `pop` convention.** Yes - <Name>HAB.

**Overlap with bundled data.** Yemeni samples may overlap the package's YemeniUNK block and Hodgson et al. 2014.

**Addable now.** YES (metadata)

**Blocker.** check the pre-publication moratorium note before citing dataset contents


### 11. Fortes-Lima et al. 2024 - genetic legacy of the Bantu expansion

**Citation.** Fortes-Lima CA et al. 2024. The genetic legacy of the expansion of Bantu-speaking peoples in Africa. Nature 625(7995):540-547. PMID 38030719.

**DOI.** 10.1038/s41586-023-06770-6

**Accession.** EGAS00001007519 'AfricanNeo aDNA Study' (VERIFIED) plus modern-data datasets EGAD00001004220, 4316, 4334, 4393, 4448, 4505, 4533, 4557, 5076; EGAS50000000006; dbGaP phs001396.v1.p1

**Genotyping platform.** Illumina NovaSeq 6000 for the aDNA component (VERIFIED); modern component is array + sequence across the listed EGADs

**Ascertainment scheme.** Mixed - sequencing for aDNA, commercial arrays for the modern panels.

**Sample count.** 12 samples in EGAS00001007519 (VERIFIED); the paper assembles the largest Bantu-speaker genomic dataset to date across the other accessions.

**Population count.** Many; newly sampled regions across Central, East and Southern Africa

**Geographic / ethnolinguistic coverage.** Continental Africa along the Bantu expansion routes - the package's largest African gap after Madagascar.

**Genome build.** GRCh37/GRCh38 mixed across components

**Access tier and consent.** SPLIT and FRAGMENTED. Study records OPEN; data CONTROLLED across EGA (policy EGAP00001003469) and dbGaP (phs001396 - NIH dbGaP Authorized Access, eRA Commons login and DAR required).

**Published metadata.** Study-level open at EGA; dbGaP study page phs001396 publishes open study metadata and variable summaries; Nature supplementary tables list populations.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001007519 ; https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/study.cgi?study_id=phs001396

**Keying into the package `pop` convention.** Yes but laborious - the dataset is spread over nine EGADs and a dbGaP study.

**Overlap with bundled data.** Overlaps published Bantu-speaker panels (Patin 2017, AGVP) - deduplication needed.

**Addable now.** PARTIAL

**Blocker.** metadata is split across EGA, dbGaP and the Nature supplement; needs one consolidation pass


### 12. GSE73996 - Siberian indigenous populations plus worldwide comparatives (Axiom)

**Citation.** GEO series GSE73996, 'The genetic basis of adaptation to climatic stress in Siberian indigenous populations' (Cardona/Kivisild-associated dataset).

**DOI.** not resolved to a single publication in this pass

**Accession.** GEO GSE73996

**Genotyping platform.** GPL18441 = [Axiom_GW_Hu_SNP] Affymetrix Axiom Genome-Wide CEU 1 Array Plate (VERIFIED)

**Ascertainment scheme.** Affymetrix Axiom GW Hu SNP, CEU discovery - same panel as GSE80534, so one new panel_information row covers both.

**Sample count.** 651 samples (VERIFIED)

**Population count.** 40 populations (VERIFIED distinct values): Altai-Kizhi, Avar, British, Buryat, Dinka, Dolgan, Egyptian, Even, Evenk, Forest Nentsi, German, Han South, Indian, Indonesia-Java, Iranian, Italian, Kazakh, Ket, Khant, Komi, Korean, Koryak, Manchu, Mari, Mongol, Mordva, Nganasan, Papua New Guinea, Selkup, Sri Lankan, Tajik, Teleut, Tibetan, Tundra Nentsi, Turk, Turkmen, Uygur, Vietnamese, Yakut, Yukagir.

**Geographic / ethnolinguistic coverage.** Siberia and North Asia in depth, with Indian Ocean rim comparatives (Sri Lankan, Indian, Indonesia-Java).

**Genome build.** Not stated in the SOFT header; read from GSE73996_SIBERIA.bed/bim.

**Access tier and consent.** FULLY OPEN - genotypes (SIBERIA.bed.gz) and the Axiom annotation CSV are both served without registration.

**Published metadata.** FULLY OPEN and VERIFIED. Characteristic keys present on all 651 samples: tissue, region, population, gender.

**Metadata URL / accession.** https://ftp.ncbi.nlm.nih.gov/geo/series/GSE73nnn/GSE73996/soft/GSE73996_family.soft.gz

**Keying into the package `pop` convention.** Yes - <Population>SIB. Note Siberian group names overlap SGDP spellings; population_alt must capture both.

**Overlap with bundled data.** Likely sample-level overlap with SGDP Siberian populations - reconcile IDs.

**Addable now.** YES

**Blocker.** none


### 13. Bergstrom et al. 2020 - HGDP high-coverage genomes (refresh of the bundled HGDP block)

**Citation.** Bergstrom A et al. 2020. Insights into human genetic variation and population history from 929 diverse genomes. Science 367(6484):eaay5012. PMID 32193295.

**DOI.** 10.1126/science.aay5012

**Accession.** ENA PRJEB6463 and PRJEB14173

**Genotyping platform.** Illumina whole-genome sequencing, ~35x

**Ascertainment scheme.** None - WGS, so no array ascertainment. This matters for the package's design-tool ambition: HGDP WGS and HGDP array data have different ascertainment properties.

**Sample count.** 929 genomes; the ENA read-run table resolves sample titles to HGDP identifiers (VERIFIED, e.g. HGDP01196, HGDP00779, HGDP01033).

**Population count.** 54 HGDP populations

**Geographic / ethnolinguistic coverage.** Worldwide; same panel as the bundled HGDP block.

**Genome build.** GRCh38 (and GRCh37 releases)

**Access tier and consent.** FULLY OPEN at ENA - no registration.

**Published metadata.** FULLY OPEN and VERIFIED. ENA filereport API returns run_accession, sample_accession, sample_title (= HGDP ID), country, collection_date for every run.

**Metadata URL / accession.** https://www.ebi.ac.uk/ena/portal/api/filereport?accession=PRJEB6463&result=read_run&fields=run_accession,sample_accession,sample_title,country&format=tsv

**Keying into the package `pop` convention.** Yes - identical to the existing HGDP pop codes; this is a data_type refresh, not new populations.

**Overlap with bundled data.** COMPLETE overlap by design: the same cell-line panel the package already bundles as HGDP (952 samples). Value is in recording a WGS data_type and the ENA accessions, not in new individuals.

**Addable now.** YES

**Blocker.** none


### 14. Uganda Genome Resource (APCDR)

**Citation.** Gurdasani D et al. 2019. Uganda Genome Resource Enables Insights into Population History and Genomic Discovery in Africa. Cell 179(4):984-1002. PMID 31675503.

**DOI.** 10.1016/j.cell.2019.10.004

**Accession.** EGAS00001001558 (plus EGAS00001000545, EGAD00001001639, EGAD00001005346, EGAD00010000965)

**Genotyping platform.** Array genotyping plus low-coverage WGS (EGA technologies field empty for the study record, VERIFIED)

**Ascertainment scheme.** Commercial array plus sequence.

**Sample count.** 4,778 samples (VERIFIED from the EGA study's dataset num_samples)

**Population count.** Multiple Ugandan ethnolinguistic groups (Baganda, Banyarwanda, Barundi, Banyankole and others) in the GPC cohort

**Geographic / ethnolinguistic coverage.** Southwest Uganda - the largest single-country East African panel available, and a key Bantu-speaker reference for Madagascar's African ancestry.

**Genome build.** GRCh37

**Access tier and consent.** SPLIT. Study record OPEN. Data CONTROLLED: EGA DAC under policy EGAP00001001473, administered through the APCDR.

**Published metadata.** Study-level open; ethnolinguistic breakdown is in the Cell supplementary tables.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001001558

**Keying into the package `pop` convention.** Yes - <Name>UGR. This is a cohort rather than an ethnographic sample, so origin coordinates will often be unavailable and coord_note should say so.

**Overlap with bundled data.** No sample-level overlap expected; population-name overlap with AGVP Baganda.

**Addable now.** YES (metadata)

**Blocker.** genotypes need an EGA DAC / APCDR application


### 15. H3Africa genotyping array design study

**Citation.** H3Africa Consortium chip design study; see also Choudhury A et al. 2020. High-depth African genomes inform human migration and health. Nature 586(7831):741-748. PMID 33116287.

**DOI.** 10.1038/s41586-020-2859-7

**Accession.** EGAS00001002976

**Genotyping platform.** Illumina HiSeq 2500 sequencing underlying the H3Africa array design (VERIFIED from the EGA dataset technologies field)

**Ascertainment scheme.** THE point of this candidate: the H3Africa array is ascertained on African sequence data rather than CEU/HapMap discovery panels. Adding it to panel_information gives get_panel_information() an African-ascertained comparator alongside the Axiom Human Origins panels.

**Sample count.** 696 samples across 9 datasets (VERIFIED)

**Population count.** Pan-African design panel

**Geographic / ethnolinguistic coverage.** Continental Africa.

**Genome build.** GRCh37

**Access tier and consent.** SPLIT. Study and dataset records OPEN. Sequence data CONTROLLED: EGA DAC, policy EGAP00001001790. The H3Africa array manifest itself is published openly by the consortium.

**Published metadata.** Study-level open at EGA; the array manifest and SNP content are distributed openly by H3Africa/Illumina.

**Metadata URL / accession.** https://metadata.ega-archive.org/studies/EGAS00001002976

**Keying into the package `pop` convention.** Not a population dataset - this is a panel_information candidate, not a population_information one.

**Overlap with bundled data.** None.

**Addable now.** YES (panel metadata)

**Blocker.** none for the panel description

