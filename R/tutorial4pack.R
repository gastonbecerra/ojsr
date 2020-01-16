# this file is my log on using Hadley Wickham's “R packages” http://r-pkgs.had.co.nz/
# and it is supossed to be deleted once package is complete

# http://r-pkgs.had.co.nz/man.html ----------- documents
#roxygen2::roxygenise()

# http://r-pkgs.had.co.nz/tests.html ----------- tests
# instead of devtools::use_testthat() im using:
#usethis::use_testthat()

# http://r-pkgs.had.co.nz/namespace.html -------- namespaces
# search paths (attached packages):
#search()

# http://r-pkgs.had.co.nz/data.html -------- data
url_sample <- c( 'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/issue/view/2018-Vol3-2',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/view/44',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/view/43/61',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/download/43/54',
  'https://firstmonday.org/ojs/index.php/fm/article/view/9540',
  'http://imed.pub/ojs/index.php/iam/article/view/1650',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/oai',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/oai'
)
#usethis::use_data(... = url_sample, internal = FALSE, overwrite = TRUE)

# --------- this file!
#usethis::use_build_ignore("R\tutorial4pack.R")

# some tests ---------------------------
# these produce an error when building... not sure why, they're use_build_ignore'd

#library(ojsr)
#url_sample
#ojsr::process_URL(url = url_sample)
