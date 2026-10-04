test_that("get_dplace_link returns matched rows by default", {
  out <- get_dplace_link()
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(c(
    "pop", "population_label", "source_dataset", "soc_id", "society_name",
    "society_region", "match_method", "match_score", "geo_distance_km",
    "confidence", "n_marriage_vars_coded", "reviewed"
  ) %in% names(out)))
  expect_false("none" %in% out$confidence)
})

test_that("get_dplace_link filters by pop, dataset and confidence", {
  out <- get_dplace_link(pop = "YRI")
  expect_true(all(out$pop == "YRI"))

  out <- get_dplace_link(dataset = "HGDP")
  expect_true(all(out$source_dataset == "HGDP"))

  out <- get_dplace_link(confidence = "high")
  expect_true(all(out$confidence == "high"))
  # High confidence means a reviewer accepted the link, not that the names
  # matched character for character: a hand-made match (Mozabite -> Cc4, where
  # Mozabite and Mzab are synonyms) is as good as an exact one.
  expect_true(all(out$match_method %in% c("exact", "manual")))
  expect_true(all(out$reviewed))
  expect_false(any(is.na(out$soc_id)))
})

test_that("get_dplace_link can include unmatched populations", {
  out <- get_dplace_link(include_unmatched = TRUE)
  expect_true("none" %in% out$confidence)
  expect_true(all(is.na(out$soc_id[out$confidence == "none"])))
})

test_that("get_dplace_link never returns a match_score below 0 or above 1", {
  out <- get_dplace_link(include_unmatched = TRUE)
  scores <- out$match_score[!is.na(out$match_score)]
  expect_true(all(scores >= 0 & scores <= 1))
})

test_that("get_dplace_link min_marriage_vars filters on coded variable count", {
  out <- get_dplace_link(min_marriage_vars = 8)
  expect_true(all(out$n_marriage_vars_coded >= 8))
  expect_false(anyNA(out$soc_id))
  expect_true(nrow(out) < nrow(get_dplace_link()))
  expect_error(get_dplace_link(min_marriage_vars = "eight"), "single number")
})

test_that("get_dplace_link gives codes sharing a label the same society", {
  # Yoruba is carried by YorubaHGDP, YRI and YRIHapMap; the society link is a
  # property of the population, not of the sequencing project, so all three
  # resolve to the same soc_id
  out <- get_dplace_link(population = "Yoruba")
  expect_true(length(unique(out$pop)) > 1)
  expect_equal(length(unique(out$soc_id)), 1L)
})

test_that("get_dplace_link pops are all present in get_pop_info", {
  out <- get_dplace_link(include_unmatched = TRUE)
  expect_true(all(out$pop %in% get_pop_info()$pop))
})
