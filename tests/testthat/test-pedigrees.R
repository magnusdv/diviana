
library(pedtools)
source_app("utils.R", "pedutils.R")

test_that("pedigree edits rename references", {
  ped = nuclearPed(father = "fa", mother = "mo", children = "child", sex = 1)
  x = list(ped = ped, miss = "child", refs = "fa")

  y = editPeddata(x, idch = c(fa = "R1"), missing = "child")

  expect_true("R1" %in% labels(y$ped))
  expect_false("fa" %in% labels(y$ped))
  expect_equal(y$refs, "R1")
})

test_that("pedigree edits reject unsafe sex changes", {
  ped = nuclearPed(father = "fa", mother = "mo", children = "child", sex = 1)
  x = list(ped = ped, miss = "mo", refs = "fa")

  expect_error(
    editPeddata(x, sxch = c(fa = 2), missing = "mo"),
    "missing spouses",
    fixed = TRUE
  )
})

test_that("unknown pedigree sex can be set to known", {
  ped = singleton(id = "X", sex = 0)
  x = list(ped = ped, miss = character(), refs = character())

  y = editPeddata(x, sxch = c(X = 1), missing = character())

  expect_equal(getSex(y$ped, "X"), 1)
})
