context("ojs scraper auxiliary files")
library(ojsr)

test_that("auxiliary files are loaded", {
  expect_equal(class(url_sample),"character")
})
