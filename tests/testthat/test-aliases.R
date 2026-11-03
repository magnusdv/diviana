source_app("utils.R", "genemapper.R", "module-data.R")

test_that("legacy identification aliases are removed on RDS import", {
  dvi = dvir::dviData(NULL, NULL, NULL)
  ids = data.frame(Family = "F1", Missing = "M1", Sample = "fixed",
                   Conclusion = "Provided", Comment = "", Alias = "old alias")
  dvi$identifications = ids
  path = tempfile(fileext = ".rds")
  saveRDS(dvi, path)
  expect_identical(readRdvi(path)$identifications, ids[1:5])
})

