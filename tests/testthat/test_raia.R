context("probando para raia")
library(ojsr)

url_sample_test <- c(
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/',
  'http://portalreviscien.uai.edu.ar/ojs/index.php/RAIA'
)

ojs_base_url <- ojsr::parse_base_url(input_url = url_sample_test)
ojs_oai_url <- ojsr::parse_oai_url(input_url = url_sample_test)

test_that("result is a dataframe with the same number of rows as the input", {
  expect_is(ojs_base_url,"character")
  expect_equal(length(url_sample_test), length(ojs_base_url))
  expect_equal(length(url_sample_test), length(ojs_oai_url))
})

# raia_issues <- get_issues_from_archive(input_url = url_sample_test, verbose = TRUE)
# raia_issues
#
# raia_art <- get_articles_from_issue(input_url = raia_issues$output_url, verbose = TRUE)
# raia_art

raia_art <- get_articles_from_issue(
  input_url =
    c(
      "http://portalreviscien.uai.edu.ar/ojs/index.php/RAIA/issue/view/28/showToc",
      "http://portalreviscien.uai.edu.ar/ojs/index.php/RAIA/issue/view/18/showToc"
      ),
  verbose = TRUE)

test_that("raia returns fine", {
  expect_equal(nrow(raia_art),4)
})
