context("url parsing")
library(ojsr)

url_sample_test <- c(
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/download/43/54',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/issue/view/2018-Vol3-2'
)
url_parsed_test <- ojsr::process_urls(url = url_sample_test)

test_that("result is a dataframe with the same number of rows as the input", {
  expect_is(url_parsed_test,"data.frame")
  expect_equal(length(url_sample_test), nrow(url_parsed_test))
})

test_that("parameters have been parsed correctly", {
  expect_equal(is.na(url_parsed_test$article_id[1]), TRUE)
  expect_equal(url_parsed_test$article_id[2], "43")
  expect_equal(url_parsed_test$issue_id[3], "2018-Vol3-2")
})
