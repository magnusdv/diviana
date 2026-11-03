
source_app("utils.R", "tables.R", "download.R")
library(gt)

test_that("missing rows and columns are added to result matrices", {
  m = matrix(1, nrow = 1, ncol = 1, dimnames = list("V1", "M1"))
  y = completeMatrix(m, rownames = c("V1", "V2"), colnames = c("M1", "M2"))

  expect_equal(dim(y), c(2, 2))
  expect_true(is.na(y["V2", "M1"]))
  expect_true(is.na(y["V1", "M2"]))
})

test_that("safe formatting leaves missing values blank", {
  expect_equal(safeFormat(c(1.25, NA)), c("1.25", ""))
})

test_that("result tables can be written to xlsx", {
  results = list(
    AM = data.frame(LR = 1, GLR = 2),
    PM = data.frame(LR = 3, GLR = 4),
    LR = matrix(1, nrow = 1, ncol = 1, dimnames = list("V1", "M1")),
    EX = matrix(0, nrow = 1, ncol = 1, dimnames = list("V1", "M1")),
    JT = list(data.frame(Missing = "M1", Sample = "V1"))
  )

  file = tempfile(fileext = ".xlsx")
  downloadTables(results, file)

  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 0)
})

test_that("result download also works without joint tables", {
  results = list(
    AM = data.frame(LR = 1, GLR = 2),
    PM = data.frame(LR = 3, GLR = 4),
    LR = matrix(1, nrow = 1, ncol = 1, dimnames = list("V1", "M1")),
    EX = matrix(0, nrow = 1, ncol = 1, dimnames = list("V1", "M1")),
    JT = list()
  )

  file = tempfile(fileext = ".xlsx")
  expect_no_error(downloadTables(results, file))
})

test_that("joint-table aliases do not rename statistical columns", {
  x = data.frame(V1 = "M1", loglik = -1, LR = 10)
  tab = formatJointTab(x, vics = "V1", miss = "M1", usealias = TRUE,
                       aliasPM = c(V1 = "LR"))

  expect_equal(names(gt::extract_body(tab, build_stage = "init")), names(x))
  expect_no_error(gt::as_raw_html(tab))
})
