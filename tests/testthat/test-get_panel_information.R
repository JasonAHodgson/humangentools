test_that("get_panel_information returns all panels by default", {
  out <- get_panel_information()
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(c("panel", "population", "sample", "SNPs") %in% names(out)))
})

test_that("get_panel_information filters by panel", {
  out <- get_panel_information("panel1")
  expect_true(all(out$panel == "panel1"))
  expect_equal(nrow(out), 1)
})

test_that("get_panel_information warns and returns zero rows for an unknown panel", {
  expect_warning(out <- get_panel_information("not-a-real-panel"), "No valid panel names")
  expect_equal(nrow(out), 0)
})
