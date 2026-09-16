test_that("get_axiom_snps returns all SNPs by default", {
  out <- get_axiom_snps("all")
  expect_type(out, "character")
  expect_true(length(out) > 0)
})

test_that("get_axiom_snps filters by panel", {
  out <- get_axiom_snps("panel1")
  expect_type(out, "character")
  expect_true(length(out) > 0)
  expect_true(length(out) < length(get_axiom_snps("all")))
})

test_that("get_axiom_snps returns unique RS ids", {
  out <- get_axiom_snps("panel1")
  expect_equal(out, unique(out))
})

test_that("get_axiom_snps warns and returns an empty vector for an unknown panel", {
  expect_warning(out <- get_axiom_snps("not-a-real-panel"), "No valid panel names")
  expect_equal(out, character(0))
})
