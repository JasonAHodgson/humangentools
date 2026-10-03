test_that("get_dataset_info returns every dataset by default", {
  out <- get_dataset_info()
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true(all(c("dataset", "description", "genotyping", "n_pops",
                    "n_samples", "reference") %in% names(out)))
  expect_equal(anyDuplicated(out$dataset), 0L)
})

test_that("get_dataset_info filters by dataset", {
  out <- get_dataset_info("HGDP")
  expect_equal(nrow(out), 1L)
  expect_equal(out$dataset, "HGDP")
})

test_that("get_dataset_info warns for an unknown dataset", {
  expect_warning(out <- get_dataset_info("not-a-dataset"), "not found")
  expect_equal(nrow(out), 0L)
})

test_that("the dataset vocabulary matches the other tables", {
  ds <- get_dataset_info()$dataset
  expect_setequal(ds, unique(get_pop_info()$source_dataset))
  expect_true(all(get_dataset_info("KGP")$n_samples ==
                    sum(get_pop_info(dataset = "KGP")$n_samples)))
})

test_that("counts in get_dataset_info agree with get_pop_info", {
  info <- get_dataset_info()
  for (i in seq_len(nrow(info))) {
    pops <- get_pop_info(dataset = info$dataset[i])
    expect_equal(nrow(pops), info$n_pops[i],
                 info = paste("n_pops mismatch for", info$dataset[i]))
  }
})
