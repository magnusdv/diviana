source_app("utils.R", "genemapper.R", "module-data.R")

test_that("text files with split markers can contain a sex column", {
  txt = tempfile(fileext = ".txt")
  writeLines(c(
    "Sample\tSex\tD1S1656.1\tD1S1656.2\tTPOX.1\tTPOX.2",
    "R1\tF\t12\t12\t8\t11",
    "R2\tM\t14\t18.3\t8\t12"
  ), txt)

  x = readGenoFromTxt(txt)

  expect_equal(rownames(x), c("R1", "R2"))
  expect_equal(x$Sex, c("F", "M"))
  expect_equal(x$D1S1656, c("12/12", "14/18.3"))
  expect_equal(x$TPOX, c("8/11", "8/12"))
})

test_that("text files without sex keep sex unknown", {
  x = data.frame(D1S1656 = c("12/12", "14/18.3"), TPOX = c("8/11", "8/12"), row.names = c("R1", "R2"))
  y = standardiseGenoData(x)

  expect_equal(y$Sample, c("R1", "R2"))
  expect_equal(y$Sex, c("?", "?"))
  expect_true("Alias" %in% names(y))
})

test_that("saved dvi data is read using the original file name", {
  skip_if_not_installed("dvir")

  dvi = dvir::dviData(NULL, NULL, NULL)
  path = tempfile()
  saveRDS(dvi, path)

  expect_s3_class(readRdvi(path, name = "case.rds"), "dviData")
})
