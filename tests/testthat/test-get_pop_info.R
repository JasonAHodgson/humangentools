test_that("get_pop_info returns all populations by default", {
  out <- get_pop_info()
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(c(
    "pop", "population_label", "source_dataset", "region", "lat", "lon",
    "origin_lat", "origin_lon", "sampling_lat", "sampling_lon", "coord_note"
  ) %in% names(out)))
})

test_that("get_pop_info canonical codes are unique", {
  out <- get_pop_info()
  expect_equal(anyDuplicated(out$pop), 0L)
})

test_that("get_pop_info filters by pop, population label, region and dataset", {
  out <- get_pop_info(pop = "YRI")
  expect_equal(nrow(out), 1L)
  expect_equal(out$pop, "YRI")

  out <- get_pop_info(population = "Brahui")
  expect_true(nrow(out) > 0)
  expect_true(all(vapply(
    strsplit(out$population_label, "|", fixed = TRUE),
    function(l) "Brahui" %in% l, logical(1)
  )))

  out <- get_pop_info(region = "Central_Asia")
  expect_true(all(out$region == "Central_Asia"))

  out <- get_pop_info(dataset = "HGDP")
  expect_true(all(out$source_dataset == "HGDP"))
})

test_that("get_pop_info separates the same group across source datasets", {
  # SGDP resequenced HGDP cell lines, so one group yields several codes
  out <- get_pop_info(population = "Yoruba")
  expect_true(length(unique(out$source_dataset)) > 1)
  expect_true(all(c("YorubaHGDP", "YRI") %in% out$pop))
})

test_that("get_pop_info location argument switches the lat/lon columns", {
  o <- get_pop_info(pop = "GIH", location = "origin")
  s <- get_pop_info(pop = "GIH", location = "sampling")
  # GIH was sampled in Houston but originates in Gujarat
  expect_false(isTRUE(all.equal(o$lon, s$lon)))
  expect_equal(o$lat, o$origin_lat)
  expect_equal(s$lat, s$sampling_lat)
})

test_that("get_pop_info leaves origin NA where origin is not a point", {
  out <- get_pop_info(pop = c("ASW", "ACB", "CEU", "MXL"))
  expect_true(all(is.na(out$origin_lat)))
  expect_false(any(is.na(out$sampling_lat)))
  expect_false(any(is.na(out$coord_note)))
})

test_that("get_pop_info filters by a character vector of sample ids", {
  out <- get_pop_info(samples = c("HGDP00001", "HGDP00003"))
  expect_true(all(out$pop %in%
    get_sample_information(c("HGDP00001", "HGDP00003"))$pop))
})

test_that("get_pop_info rejects a samples argument that isn't a gen_tibble or character vector", {
  expect_error(get_pop_info(samples = 1:3), "gen_tibble or a character vector")
})

test_that("get_pop_info includes/excludes columns as requested", {
  out <- get_pop_info(include = c("pop", "region"))
  expect_equal(names(out), c("pop", "region"))

  out <- get_pop_info(exclude = c("reference"))
  expect_false("reference" %in% names(out))
})
