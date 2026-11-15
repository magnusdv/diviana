source_app("utils.R", "tables.R", "download.R")
library(gt)

test_that("result matrices are completed in the requested order", {
  m = matrix(1, nrow = 1, dimnames = list("V1", "M1"))
  y = completeMatrix(m, rownames = c("V2", "V1"), colnames = c("M2", "M1"))
  expect_identical(dimnames(y), list(c("V2", "V1"), c("M2", "M1")))
  expect_equal(unname(y["V1", "M1"]), 1)
  expect_true(all(is.na(y[c(1, 2, 3)])))
})

test_that("joint views preserve columns and apply PM aliases", {
  x = data.frame(V1 = c("M1", "*"), loglik = c(-1, -2), LR = c(10, 1), M1 = c("V1", "*"))
  pm = formatJointTab(x, vics = "V1", miss = "M1", usealias = TRUE,
                      aliasPM = c(V1 = "LR"), columns = "PM")
  mp = formatJointTab(x, vics = "V1", miss = "M1", usealias = TRUE,
                      aliasPM = c(V1 = "LR"), columns = "Missing")
  expect_identical(names(extract_body(pm, build_stage = "init")), c("V1", "loglik", "LR"))
  y = extract_body(mp)
  expect_identical(names(y), c("M1", "loglik", "LR"))
  expect_equal(y$M1, c("LR", "*"))
  expect_no_error(as_raw_html(pm))
  expect_no_error(as_raw_html(mp))
})

test_that("result files include every joint table and allow none", {
  results = list(
    AM = data.frame(LR = 1, GLR = 2),
    PM = data.frame(LR = 3, GLR = 4),
    LR = matrix(1, nrow = 1, dimnames = list("V1", "M1")),
    EX = matrix(0, nrow = 1, dimnames = list("V1", "M1")),
    JT = list(F1 = data.frame(M1 = "V1", LR = 10), F2 = data.frame(M2 = "V2", LR = 20))
  )
  file = tempfile(fileext = ".xlsx")
  downloadTables(results, file)
  expect_length(openxlsx::getSheetNames(file), 6)
  expect_equal(openxlsx::read.xlsx(file, sheet = 5)$M1, "V1")
  expect_equal(openxlsx::read.xlsx(file, sheet = 6)$M2, "V2")
  results$JT = list()
  downloadTables(results, file)
  expect_identical(openxlsx::getSheetNames(file), c("AM", "PM", "LR", "EX"))
  expect_equal(safeFormat(c(1.25, NA)), c("1.25", ""))
})
