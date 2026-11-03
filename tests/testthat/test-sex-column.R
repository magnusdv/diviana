source_app("utils.R", "module-data.R")

test_that("sex values are standardised", {
  x = checkSex(c("F", "M", "", NA, "0", "1", "2"))
  expect_equal(x, c("F", "M", "?", "?", "?", "M", "F"))
})

test_that("AMEL is converted to sex", {
  x = data.frame(AMEL = c("X/Y", "X/X", ""), D1S1656 = c("12/12", "14/18.3", "15/16"), row.names = c("R1", "R2", "R3"))
  y = standardiseGenoData(x)

  expect_equal(y$Sex, c("M", "F", "?"))
  expect_false("AMEL" %in% names(y))
})

test_that("unknown sex is shown but cannot be chosen", {
  x = data.frame(.rowid = 1:2, Sample = c("R1", "R2"), Alias = c("R1", "R2"), Sex = c("?", "F"))
  y = formatGenoView(x, mode = "edit")

  expect_match(y$Sex[1], "value='\\?' selected disabled")
  expect_match(y$Sex[1], "value='F'>Female")
  expect_match(y$Sex[2], "value='F' selected")
})

