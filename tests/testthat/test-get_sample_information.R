test_that("get_sample_information returns population and region for known IDs", {
  out <- get_sample_information(c("HGDP00001", "HGDP00003"))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$id, c("HGDP00001", "HGDP00003"))
  expect_false(anyNA(out$population))
})

test_that("get_sample_information preserves input order", {
  out <- get_sample_information(c("HGDP00003", "HGDP00001"))
  expect_equal(out$id, c("HGDP00003", "HGDP00001"))
})

test_that("get_sample_information fills unknown IDs with NA when na.fill = TRUE", {
  expect_warning(
    out <- get_sample_information(c("HGDP00001", "not-a-real-id")),
    "not found"
  )
  expect_equal(nrow(out), 2)
  expect_true(is.na(out$population[out$id == "not-a-real-id"]))
})

test_that("get_sample_information drops unknown IDs when na.fill = FALSE", {
  expect_warning(
    out <- get_sample_information(c("HGDP00001", "not-a-real-id"), na.fill = FALSE),
    "not found"
  )
  expect_equal(nrow(out), 1)
  expect_equal(out$id, "HGDP00001")
})
