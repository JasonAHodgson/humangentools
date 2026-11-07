# The counts asserted here were computed from sample_information.Rtable as
# shipped. They are expected to change when the AADR `Individual ID` column
# reaches the table: matching is on the exact `id` string today, and 267
# S_-prefixed AADR ids that are SGDP individuals do not match any SGDP id, so
# the AADR-SGDP figure in particular is a lower bound.

test_that("get_sample_overlap finds the documented cross-dataset duplication", {
  # 35 individuals shared by HGDP and SGDP, one row per (id, source_dataset)
  expect_equal(nrow(get_sample_overlap(dataset = c("HGDP", "SGDP"))), 70L)

  expect_equal(
    length(unique(get_sample_overlap(dataset = c("AADR", "KGP"))$id)), 2514L
  )
  expect_equal(
    length(unique(get_sample_overlap(dataset = c("AADR", "HGDP"))$id)), 929L
  )
  expect_equal(length(unique(get_sample_overlap()$id)), 3471L)
})

test_that("get_sample_overlap returns only individuals above min_datasets", {
  out <- get_sample_overlap()
  expect_s3_class(out, "data.frame")
  expect_true(all(out$n_datasets >= 2))

  # HGDP01414: genotyped by HGDP, resequenced by SGDP, regenotyped for the AADR
  expect_true("HGDP01414" %in% out$id)
  expect_equal(unique(out$n_datasets[out$id == "HGDP01414"]), 3L)
})

test_that("get_sample_overlap returns the full column set", {
  out <- get_sample_overlap(dataset = c("HGDP", "SGDP"))
  expect_equal(
    names(out),
    c("id", "source_dataset", "source_id", "data_type", "pop", "population",
      "temporal", "n_datasets", "n_source_ids", "datasets")
  )
  expect_type(out$n_datasets, "integer")
  expect_type(out$n_source_ids, "integer")
  expect_type(out$datasets, "character")
})

test_that("a single dataset cannot overlap itself", {
  # within-dataset library duplication is a different problem and must not be
  # counted as an overlap; it is reported by n_source_ids instead
  out <- get_sample_overlap(dataset = "HGDP")
  expect_equal(nrow(out), 0L)
  expect_s3_class(out, "data.frame")

  # the empty return has to be usable exactly like the populated one
  expect_equal(names(out), names(get_sample_overlap()))
  expect_equal(
    vapply(out, typeof, character(1)),
    vapply(get_sample_overlap(), typeof, character(1))
  )
})

test_that("n_source_ids surfaces within-dataset duplication without inflating n_datasets", {
  # AADR individuals with both .AG and .SG libraries have two source_ids under
  # one id; at min_datasets = 1 the pool is just listed
  aadr <- get_sample_overlap(dataset = "AADR", min_datasets = 1L)
  expect_true(all(aadr$n_datasets == 1L))
  expect_true(any(aadr$n_source_ids > 1L))
})

test_that("dataset restricts the candidate pool before overlap is computed", {
  # not "the overall answer, filtered": HGDP shares 929 individuals with the
  # AADR but only 35 with SGDP
  hs <- get_sample_overlap(dataset = c("HGDP", "SGDP"))
  expect_true(all(hs$source_dataset %in% c("HGDP", "SGDP")))
  expect_setequal(unique(hs$datasets), "HGDP|SGDP")

  # the same individuals carry a third dataset when the pool is unrestricted
  all_ov <- get_sample_overlap()
  expect_true(any(all_ov$n_datasets[all_ov$id %in% hs$id] > 2L))
})

test_that("min_datasets is validated and 1L is legal", {
  expect_error(get_sample_overlap(min_datasets = 2.5), "whole number")
  expect_error(get_sample_overlap(min_datasets = c(2L, 3L)), "whole number")
  expect_error(get_sample_overlap(min_datasets = -1L), "whole number")
  expect_error(get_sample_overlap(min_datasets = "2"), "whole number")

  listing <- get_sample_overlap(dataset = "SGDP", min_datasets = 1L)
  expect_true(nrow(listing) > 0L)
  expect_true(all(listing$n_datasets == 1L))
})

test_that("get_sample_overlap warns on an unknown sample id rather than erroring", {
  expect_warning(
    out <- get_sample_overlap(samples = "NOT_A_SAMPLE"),
    "not found"
  )
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), names(get_sample_overlap()))
})

test_that("samples accepts a character vector and rejects other types", {
  # HGDP00112 is present only in HGDP, so it is in the pool -- no warning --
  # but has nothing to overlap with and is silently absent from the result
  out <- get_sample_overlap(samples = c("HGDP01414", "HGDP00112"))
  expect_setequal(unique(out$id), "HGDP01414")
  expect_equal(nrow(out), 3L)

  expect_error(get_sample_overlap(samples = 1:3), "gen_tibble")
})

test_that("samples and pop report the individual, not the filtered rows", {
  # every overlapping individual carries a different pop code in each dataset,
  # so a pop filter has to select individuals and then return all their rows
  out <- get_sample_overlap(pop = "BantuKenyaSGDP")
  expect_true(nrow(out) > 0L)
  expect_true("HGDP01414" %in% out$id)
  expect_true(any(out$source_dataset != "SGDP"))
  expect_true(all(out$n_datasets >= 2L))
})

test_that("an unknown dataset or population warns and narrows to nothing", {
  expect_warning(out <- get_sample_overlap(dataset = "NOT_A_DATASET"), "not found")
  expect_equal(nrow(out), 0L)

  expect_warning(out2 <- get_sample_overlap(pop = "NOT_A_POP"), "not found")
  expect_equal(nrow(out2), 0L)
  expect_equal(names(out2), names(get_sample_overlap()))
})

test_that("temporal filters overlapping individuals and validates its vocabulary", {
  modern <- get_sample_overlap(temporal = "modern")
  expect_true(nrow(modern) > 0L)
  expect_true(all(modern$id %in% get_sample_overlap()$id))

  expect_error(get_sample_overlap(temporal = "recent"), "modern")
})

test_that("include and exclude select columns, include first", {
  expect_equal(
    names(get_sample_overlap(dataset = c("HGDP", "SGDP"),
                             include = c("id", "n_datasets", "not_a_column"))),
    c("id", "n_datasets")
  )
  expect_false(
    "data_type" %in%
      names(get_sample_overlap(dataset = c("HGDP", "SGDP"), exclude = "data_type"))
  )
})

test_that("rows are grouped by id and ordered by descending n_datasets", {
  out <- get_sample_overlap()
  expect_equal(out$n_datasets, sort(out$n_datasets, decreasing = TRUE))
  # an id's rows are contiguous
  expect_equal(length(rle(out$id)$values), length(unique(out$id)))
  expect_equal(rownames(out), as.character(seq_len(nrow(out))))
})

test_that("datasets is the pipe-separated sorted set the individual appears in", {
  out <- get_sample_overlap(samples = "HGDP01414")
  expect_setequal(unique(out$datasets), "AADR|HGDP|SGDP")
  expect_setequal(out$source_dataset, strsplit("AADR|HGDP|SGDP", "|", fixed = TRUE)[[1]])
})
