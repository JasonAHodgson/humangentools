test_that("get_sample_information returns canonical pop and region for known IDs", {
  out <- get_sample_information(c("HGDP00001", "HGDP00003"))
  expect_s3_class(out, "data.frame")
  expect_true(all(c("id", "pop", "population", "region", "source_dataset") %in% names(out)))
  expect_true(all(c("HGDP00001", "HGDP00003") %in% out$id))
  expect_false(anyNA(out$pop))
})

test_that("get_sample_information preserves input order", {
  out <- get_sample_information(c("HGDP00003", "HGDP00001"))
  expect_equal(unique(out$id), c("HGDP00003", "HGDP00001"))
})

test_that("get_sample_information can return several rows for one sample", {
  # HGDP01414 was genotyped by HGDP, resequenced by SGDP, and regenotyped on
  # the 1240K panel for the AADR: one individual, three genotype datasets.
  out <- get_sample_information("HGDP01414")
  expect_true(nrow(out) > 1)
  expect_setequal(out$source_dataset, c("HGDP", "SGDP", "AADR"))
  expect_equal(length(unique(out$pop)), nrow(out))

  # and the dataset argument still narrows it to one
  expect_equal(nrow(get_sample_information("HGDP01414", dataset = "SGDP")), 1)
})

test_that("get_sample_information dataset argument gives one row per sample", {
  out <- get_sample_information("HGDP01414", dataset = "HGDP")
  expect_equal(nrow(out), 1L)
  expect_equal(out$source_dataset, "HGDP")
})

test_that("get_sample_information warns about unknown IDs and fills them with NA", {
  expect_warning(
    out <- get_sample_information(c("HGDP00001", "not-a-sample")),
    "were not found"
  )
  expect_true("not-a-sample" %in% out$id)
  expect_true(is.na(out$pop[out$id == "not-a-sample"]))
})

test_that("get_sample_information drops unknown IDs when na.fill = FALSE", {
  expect_warning(
    out <- get_sample_information(c("HGDP00001", "not-a-sample"), na.fill = FALSE),
    "were not found"
  )
  expect_false("not-a-sample" %in% out$id)
})

test_that("get_sample_information IDs all resolve to a known population", {
  out <- get_sample_information(c("HG00096", "NA18525", "HGDP00001"))
  expect_true(all(out$pop %in% get_pop_info()$pop))
})

test_that("one population code never carries two labels", {
  # `population` is a property of `pop`, not of the sample: a group that two
  # source datasets named differently (GBR as British and as English) must not
  # split into two populations when samples are counted.
  ids <- c("HG00126", "HG00127", "HG00096", "HG00171", "HG00174", "NA19648")
  info <- get_sample_information(ids, dataset = "KGP")
  by_pop <- tapply(info$population, info$pop, function(x) length(unique(x)))
  expect_true(all(by_pop == 1))
  expect_false(any(grepl("|", info$population, fixed = TRUE)))
})
