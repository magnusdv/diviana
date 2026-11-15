source_app("utils.R", "read-text.R", "module-data.R", "module-import.R")

test_that("text imports detect separators and combine split alleles", {
  file = tempfile()
  txt = c("Sample\tSex\tD1S1656.1\tD1S1656.2\tTPOX.1\tTPOX.2",
          "R1\tF\t12\t12\t8\t11", "R2\tM\t14\t18.3\t8\t12")
  writeLines(txt, file)
  x = readTextImport(file)$data
  expect_equal(x$Sample, c("R1", "R2"))
  expect_equal(x$Sex, c("F", "M"))
  expect_equal(x$D1S1656, c("12/12", "14/18.3"))
  expect_equal(x$TPOX, c("8/11", "8/12"))
  writeLines(gsub("\t", ",", txt, fixed = TRUE), file)
  expect_identical(readTextImport(file)$data, x)
})

test_that("GeneMapper imports handle homozygotes and missing genotypes", {
  file = tempfile()
  writeLines(c("Sample Name\tMarker\tAllele 1\tAllele 2",
               "R1\tD1S1656\t12-NB\t", "R1\tTPOX\t8\t11",
               "R2\tD1S1656\t14\t18.3", "R2\tTPOX\t\t"), file)
  x = readTextImport(file)
  expect_equal(x$type, "gm")
  expect_equal(rownames(x$data), c("R1", "R2"))
  expect_equal(x$data$D1S1656, c("12/12", "14/18.3"))
  expect_equal(x$data$TPOX, c("8/11", ""))
})

test_that("genotypes and missing sample metadata are standardised", {
  x = data.frame(D1S1656 = c("12,12", "14/18.3"), row.names = c("R1", "R2"))
  y = standardiseGenoData(x)
  expect_equal(y$Sample, c("R1", "R2"))
  expect_equal(y$Sex, c("?", "?"))
  expect_true(all(is.na(y$Alias)))
  expect_equal(y$D1S1656, c("12/12", "14/18.3"))
  expect_equal(checkSex(c("F", "M", "", NA, "0", "1", "2")),
               c("F", "M", "?", "?", "?", "M", "F"))
})

test_that("AMEL is converted to sex", {
  x = data.frame(AMEL = c("X/Y", "X/X", ""), D1S1656 = c("12/12", "14/18.3", "15/16"))
  y = standardiseGenoData(x)
  expect_equal(y$Sex, c("M", "F", "?"))
  expect_false("AMEL" %in% names(y))
})

test_that("RDS imports use the original filename and remove legacy aliases", {
  dvi = dvir::dviData(NULL, NULL, NULL)
  ids = data.frame(Family = "F1", Missing = "M1", Sample = "V1",
                   Conclusion = "Provided", Comment = "", Alias = "old alias")
  dvi$identifications = ids
  file = tempfile()
  saveRDS(dvi, file)
  y = readDviFile(file, name = "case.rds")
  expect_s3_class(y, "dviData")
  expect_identical(y$identifications, ids[1:5])
})

test_that("invalid imports give useful errors", {
  file = tempfile()
  writeLines(c("Sample\tD1S1656.1", "R1\t12"), file)
  expect_error(readTextImport(file), "Unpaired allele columns", fixed = TRUE)
  saveRDS(list(), file)
  expect_error(readDviFile(file, name = "case.rds"), "dviData", fixed = TRUE)
})
