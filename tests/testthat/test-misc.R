
test_that("data frame column alignment returns correct length", {
  target <- data.frame(matrix(NA, ncol = 5))
  template <- data.frame(matrix(NA, ncol = 6))
  names(target) <- c(letters[c(3, 1, 2, 4, 8)])
  names(template) <- c(letters[1:6])
  expect_equal(ncol(peatcollapse::align_dfcol(target =  target, template = template)), ncol(template))
})

test_that("incomplete date formatting returns valid posix",{
  dates <- c("51514", "101214", "8714", "1214", "81412")
  expect_equal(any(is.na(peatcollapse::date456posix(dates, century = "20"))), FALSE)
})

test_that("zero padding and century expansion on / dates",{
  x <- "5/5/15"
  expect_equal(nchar(peatcollapse::mdy2mmyyyy(x)), 10)
})

#mesokey
