# Build a candidate link table between humangentools populations (HGDP,
# 1000 Genomes, Pierron_2014) and dplaceR societies (dplace_societies).
#
# This is inherently a fuzzy, best-effort matching problem: the two
# resources use different population/society naming conventions, at
# different levels of ethnographic granularity, with no shared ID. This
# script produces a *candidate* link table -- exact + fuzzy name matches,
# with a geographic sanity-check distance -- for manual review, not a
# guaranteed-correct crosswalk. Every population appears at least once
# (with soc_id = NA if nothing matched), so the output is review-complete.
#
# Re-run this script (Rscript data-raw/build_dplace_link.R) to regenerate
# inst/extdata/population_dplace_link.Rtable from scratch. If you've
# hand-corrected the .Rtable directly, re-running this will overwrite
# those edits -- see the review workflow in the package README/vignette.

pop <- read.table(
  "inst/extdata/population_information.Rtable",
  header = TRUE, sep = "\t", stringsAsFactors = FALSE, quote = "\""
)

# dplace_societies ships with dplaceR, not humangentools -- this script
# (unlike the package itself) depends on dplaceR being installed locally
# to build the link table.
if (!requireNamespace("dplaceR", quietly = TRUE)) {
  stop("Install dplaceR to regenerate the link table: remotes::install_github('jasonahodgson/dplaceR')")
}
soc <- dplaceR::dplace_societies
soc <- soc[soc$type == "society", , drop = FALSE]

# ---- normalization -----------------------------------------------------

normalize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[_.\\-]+", " ", x)      # underscores/dots/hyphens -> space
  x <- gsub("[^a-z ]", "", x)        # drop anything else non-alphabetic
  x <- gsub("\\s+", " ", trimws(x))
  x
}

# build candidate name variants for a population label: the whole label,
# its underscore-separated parts ("Piapoco_and_Curripaco" ->
# "Piapoco"/"Curripaco", "Bantu_South" -> "Bantu"/"South"), and, for any
# of those ending in the Chinese ethnonym suffix "-zu" ("Miaozu", "Yizu"),
# the same name with that suffix stripped ("Miao", "Yi")
build_candidates <- function(x) {
  parts <- strsplit(x, "_and_|_", fixed = FALSE)[[1]]
  cand <- unique(c(x, parts))
  has_zu <- grepl("zu$", cand, ignore.case = TRUE) & nchar(cand) > 3
  unique(c(cand, sub("zu$", "", cand[has_zu], ignore.case = TRUE)))
}

pop_norm <- normalize_name(pop$population)
soc_norm <- normalize_name(soc$name)

# ---- geographic distance (haversine, km) --------------------------------

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  to_rad <- function(d) d * pi / 180
  dlat <- to_rad(lat2 - lat1)
  dlon <- to_rad(lon2 - lon1)
  a <- sin(dlat / 2)^2 + cos(to_rad(lat1)) * cos(to_rad(lat2)) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# ---- matching ------------------------------------------------------------

# generic/directional qualifier words that are common ethnonym modifiers
# ("Bantu_South", "Ju_hoan_North") but are also common, unrelated words in
# many society names ("South Tlingit", "Western Apache (San Carlos)").
# Containment of ONE of these words alone is not meaningful evidence of a
# match -- it's exactly the kind of coincidence that produced spurious,
# geographically-impossible matches (e.g. "San" <-> "San Juan", "Khomani_San"
# <-> "Western Apache (San Carlos)", "Bantu_South" <-> "South Tlingit").
# Real matches on these words are still caught by edit-distance similarity
# or by other, non-generic candidate parts (e.g. "Bantu" itself).
GENERIC_WORDS <- c("north", "south", "east", "west", "san", "de", "la", "le",
                    "upper", "lower", "new", "old", "the")
is_generic <- function(w) tolower(w) %in% GENERIC_WORDS

# similarity in [0, 1], 1 = identical: the better of (a) normalized
# edit-distance similarity, and (b) a whole-word containment score, so a
# short name that's a clean whole word inside a longer, qualified society
# name (e.g. "yoruba" inside "oyo yoruba", "somali" inside "somali esa")
# isn't penalized just for the extra qualifier word the way raw edit
# distance would penalize it. Word equality also tolerates a trailing "s"
# either side (simple singular/plural handling: "tibetan" vs "tibetans").
# Containment triggered ONLY by a generic/directional word (see above) is
# excluded -- it falls back to plain edit-distance similarity instead,
# which correctly reflects how dissimilar the full names actually are.
depluralize <- function(w) sub("s$", "", w)
name_similarity <- function(a, b) {
  d <- adist(a, b)[1, 1]
  edit_sim <- 1 - d / max(nchar(a), nchar(b), 1)

  a_words <- strsplit(a, " ")[[1]]
  b_words <- strsplit(b, " ")[[1]]
  a_words_s <- depluralize(a_words)
  b_words_s <- depluralize(b_words)
  a_dep <- depluralize(a)
  b_dep <- depluralize(b)
  contains <- ((a %in% b_words) && !is_generic(a)) ||
    ((b %in% a_words) && !is_generic(b)) ||
    ((a_dep %in% b_words_s) && !is_generic(a_dep)) ||
    ((b_dep %in% a_words_s) && !is_generic(b_dep))
  # a flat score rather than length-ratio: containment of a whole word is
  # strong evidence regardless of how many extra qualifier words the
  # society name carries (e.g. "Somali (Dolbahanta)")
  contain_sim <- if (contains) 0.9 else 0

  max(edit_sim, contain_sim)
}

FUZZY_THRESHOLD <- 0.65 # similarity >= this counts as a fuzzy candidate

rows <- list()

for (i in seq_len(nrow(pop))) {
  candidates_i <- build_candidates(pop$population[i])
  candidates_norm <- normalize_name(candidates_i)

  # exact match on any candidate name part
  exact_idx <- which(soc_norm %in% candidates_norm)

  if (length(exact_idx) > 0) {
    for (j in exact_idx) {
      rows[[length(rows) + 1]] <- data.frame(
        population = pop$population[i], dataset = pop$dataset[i],
        soc_id = soc$soc_id[j], society_name = soc$name[j],
        society_region = soc$region[j],
        match_method = "exact", match_score = 1,
        stringsAsFactors = FALSE
      )
    }
    next
  }

  # fuzzy: best similarity of any candidate name part against each society
  sims <- vapply(soc_norm, function(sn) {
    max(vapply(candidates_norm, name_similarity, numeric(1), b = sn))
  }, numeric(1))

  fuzzy_idx <- which(sims >= FUZZY_THRESHOLD)
  if (length(fuzzy_idx) > 0) {
    # keep only the very best-scoring matches to avoid flooding the table
    best <- max(sims[fuzzy_idx])
    fuzzy_idx <- fuzzy_idx[sims[fuzzy_idx] >= best - 0.05]
    for (j in fuzzy_idx) {
      rows[[length(rows) + 1]] <- data.frame(
        population = pop$population[i], dataset = pop$dataset[i],
        soc_id = soc$soc_id[j], society_name = soc$name[j],
        society_region = soc$region[j],
        match_method = "fuzzy", match_score = round(sims[j], 3),
        stringsAsFactors = FALSE
      )
    }
    next
  }

  # unmatched -- one placeholder row so every population is represented
  rows[[length(rows) + 1]] <- data.frame(
    population = pop$population[i], dataset = pop$dataset[i],
    soc_id = NA_character_, society_name = NA_character_,
    society_region = NA_character_,
    match_method = "unmatched", match_score = NA_real_,
    stringsAsFactors = FALSE
  )
}

link <- do.call(rbind, rows)

# geographic sanity-check distance for actual matches (not computable for
# unmatched rows)
pop_idx <- match(link$population, pop$population)
soc_idx <- match(link$soc_id, soc$soc_id)
link$geo_distance_km <- ifelse(
  is.na(link$soc_id), NA_real_,
  round(haversine_km(pop$lat[pop_idx], pop$lon[pop_idx],
                      soc$latitude[soc_idx], soc$longitude[soc_idx]), 0)
)

# a quick triage tier so review doesn't require cross-referencing
# match_score and geo_distance_km by eye for every row: a so-so name match
# with strong geographic backing (score >= 0.65 but <= 500km away) is
# promoted, while a name coincidence on the other side of the world (e.g.
# Karitiana/Tahitians, score 0.667 but 9309km apart) is not.
#
# Belt-and-suspenders safety net: regardless of how high match_score is,
# a match on opposite sides of the globe (>5000km) is never promoted to
# "medium" on name alone -- this guards against any other coincidental
# name overlap the fuzzy matcher might latch onto that isn't caught by
# the GENERIC_WORDS exclusion above.
GEO_IMPLAUSIBLE_KM <- 5000
link$confidence <- ifelse(
  link$match_method == "unmatched", "none",
  ifelse(
    link$match_method == "exact", "high",
    ifelse(
      !is.na(link$geo_distance_km) & link$geo_distance_km > GEO_IMPLAUSIBLE_KM,
      "low",
      ifelse(
        link$match_score >= 0.8 | (!is.na(link$geo_distance_km) & link$geo_distance_km <= 500),
        "medium", "low"
      )
    )
  )
)

link$reviewed <- FALSE

link <- link[order(
  link$population,
  factor(link$confidence, levels = c("high", "medium", "low", "none")),
  -ifelse(is.na(link$match_score), -1, link$match_score)
), ]
rownames(link) <- NULL

cat("Populations:", length(unique(link$population)), "\n")
cat("  exact matches:", sum(link$match_method == "exact"), "rows\n")
cat("  fuzzy matches:", sum(link$match_method == "fuzzy"), "rows\n")
cat("  unmatched:", sum(link$match_method == "unmatched"), "populations\n")
cat("Confidence tiers (rows):\n")
print(table(link$confidence))

write.table(
  link, "inst/extdata/population_dplace_link.Rtable",
  sep = "\t", row.names = FALSE, quote = FALSE, na = "NA"
)
