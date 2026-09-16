test_that("get_pop_info returns all populations by default", {
  out <- get_pop_info()
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(c("population", "region", "dataset") %in% names(out)))
})

test_that("get_pop_info filters by region, population, and dataset", {
  out <- get_pop_info(region = "Central_Asia")
  expect_true(all(out$region == "Central_Asia"))

  out <- get_pop_info(population = "Brahui")
  expect_true(all(out$population == "Brahui"))

  out <- get_pop_info(dataset = "HGDP")
  expect_true(all(out$dataset == "HGDP"))
})

test_that("get_pop_info filters by a character vector of sample ids", {
  out <- get_pop_info(samples = c("HGDP00001", "HGDP00003"))
  expect_true(all(out$population %in% get_sample_information(c("HGDP00001", "HGDP00003"))$population))
})

test_that("get_pop_info rejects a samples argument that isn't a gen_tibble or character vector", {
  expect_error(get_pop_info(samples = 1:3), "gen_tibble or a character vector")
})

test_that("get_pop_info includes/excludes columns as requested", {
  out <- get_pop_info(include = c("population", "region"))
  expect_equal(names(out), c("population", "region"))

  out <- get_pop_info(exclude = c("reference"))
  expect_false("reference" %in% names(out))
})
