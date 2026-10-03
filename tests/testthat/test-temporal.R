test_that("every population carries a temporal classification", {
  out <- get_pop_info()
  expect_true("temporal" %in% names(out))
  expect_setequal(unique(out$temporal), c("modern", "ancient"))
  expect_false(anyNA(out$temporal))
})

test_that("get_pop_info filters by temporal", {
  modern <- get_pop_info(temporal = "modern")
  ancient <- get_pop_info(temporal = "ancient")
  expect_true(all(modern$temporal == "modern"))
  expect_true(all(ancient$temporal == "ancient"))
  expect_equal(nrow(modern) + nrow(ancient), nrow(get_pop_info()))
  expect_equal(
    nrow(get_pop_info(temporal = c("modern", "ancient"))),
    nrow(get_pop_info())
  )
})

test_that("only AADR contributes ancient populations", {
  ancient <- get_pop_info(temporal = "ancient")
  expect_true(all(ancient$source_dataset == "AADR"))
  # and every non-AADR population is modern
  expect_true(all(get_pop_info(dataset = "HGDP")$temporal == "modern"))
  expect_true(all(get_pop_info(dataset = "KGP")$temporal == "modern"))
})

test_that("AADR contributes both modern and ancient populations", {
  aadr <- get_pop_info(dataset = "AADR")
  expect_true(nrow(aadr) > 0)
  expect_setequal(unique(aadr$temporal), c("modern", "ancient"))
})

test_that("get_pop_info rejects an invalid temporal value", {
  expect_error(get_pop_info(temporal = "neolithic"), "modern")
})

test_that("get_sample_information filters by temporal", {
  out <- get_sample_information(c("HGDP00001", "HG00096"), temporal = "modern")
  expect_true(all(out$temporal == "modern"))
  expect_warning(
    none <- get_sample_information("HGDP00001", temporal = "ancient", na.fill = FALSE),
    "were not found"
  )
  expect_equal(nrow(none), 0L)
})

test_that("get_sample_information rejects an invalid temporal value", {
  expect_error(get_sample_information("HGDP00001", temporal = "neolithic"), "modern")
})

test_that("sample rows are unique on source_id and pop, not id and pop", {
  # I1960 has .AG, .DG and .SG libraries in one AADR group
  aadr <- get_sample_information("I1960", dataset = "AADR")
  expect_equal(nrow(aadr), 3L)
  expect_setequal(aadr$data_type, c("AG", "DG", "SG"))
  expect_equal(length(unique(aadr$id)), 1L)
  expect_equal(length(unique(aadr$source_id)), nrow(aadr))
  expect_false(anyNA(aadr$data_type))
})

test_that("an AADR individual can share an id with another dataset", {
  # AADR regenotyped many HGDP and 1000 Genomes samples on the 1240K panel
  out <- get_sample_information("HGDP00001")
  expect_setequal(out$source_dataset, c("HGDP", "AADR"))
  expect_setequal(out$pop, c("BrahuiHGDP", "BrahuiAADR"))
  expect_equal(nrow(get_sample_information("HGDP00001", dataset = "HGDP")), 1L)
})
