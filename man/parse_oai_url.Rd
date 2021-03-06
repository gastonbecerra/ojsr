% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/url_parsers.R
\name{parse_oai_url}
\alias{parse_oai_url}
\title{Parses urls against OJS routing conventions and retrieves the OAI url}
\usage{
parse_oai_url(input_url)
}
\arguments{
\item{input_url}{Character vector.}
}
\value{
A vector of the same length of your input.
}
\description{
Takes a vector of urls and parses them according to OJS routing conventions, then retrieves OAI entry url.
}
\examples{

mix_links <- c(
  'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903'
)
oai_url <- ojsr::parse_oai_url(input_url = mix_links)

}
