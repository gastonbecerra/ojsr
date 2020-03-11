#' Parses URLs according to OJS conventions
#'
#' This function parse a string vector of OJS urls to check which (if any) page/function could be referring to,
#' if there are IDs for articles, issues or galleys.
#'
#' It also returns expected ulrs for the scraping functions.
#'
#' This works by parsing URLs strings against OJS routing conventions. It does not check anything against the actual pages!
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector of OJS url(s).
#' @return A dataframe wich indicates the type of OJS url expected, and the parameters.
#' Please refer to vignette ("Parse OJS urls" section).
#' @examples
#' process_urls(c('https://firstmonday.org/ojs/index.php/fm/article/view/9540',
#'     'http://imed.pub/ojs/index.php/iam/article/view/1650'))
#' @export
process_urls <- function( url ) {

  # 2do: remove the as.integer validation for IDs. i.e., https://fundacionmenteclara.org.ar/revista/index.php/RCA/issue/view/2019-Vol4-1
  # 2do: include a correction parameter (-1, -2) for ojs_url_basePosition + x, so you can skip journal name in single-OJS installations

  if( !is.character(url) ) { error("url must be a character string or vector") }

  ojs_url_dataframe <- data.frame()  # object to return

  for (i in 1:length(url)) { # in a loop for vectorized input

    ojs_url <- list() # object to populate. converts to df before returning

    urlposta <- url[i] # input parameter
    ojs_url$url <- urlposta # input parameter

    ojs_url$expect <- ojs_url$command <- ojs_url$page  <- ""
    ojs_url$galleyId <- ojs_url$articleId <- ojs_url$issueId <- ""
    ojs_url$baseUrl <- ojs_url$assume_archive <- ojs_url$assume_search <- ojs_url$assume_oai <- ojs_url$assume_issue <- ojs_url$assume_article <- ""

    ojs_url_parsed <- urltools::url_parse(urlposta) # parsing non-ojs url conventions
    ojs_url_path <- urltools::path(urlposta) # url - domain = /index.php/journal/page/function/arg1/arg2/arg3
    urlsplit <- unlist(strsplit(x = ojs_url_path, split = "/", fixed = TRUE)) # split url by "/"

    if (!grepl(pattern = "^https?://", x = urlposta)) { warning( paste(urlposta , "(element ", i ,")", " does not include http|https protocol") ) }
    if (!grepl(pattern = "index.php", x = urlposta, fixed = TRUE)) { warning( paste(urlposta , "(element ", i ,")", " does not include index.php") ) }
    if (!grepl(pattern = "/search", x = urlposta, fixed = TRUE) & !is.na(ojs_url_parsed$parameter)) { warning( paste(urlposta , "(element ", i ,")", " includes paramenters in url") ) }

    ojs_url_basePosition <- match("index.php", urlsplit) # position of "index.php" url segment, or NA

    ojs_url_baseUrl <- "" # base url to rewrite (the "assume" paramenters returned)
    ojs_url_directory <- ifelse( ojs_url_basePosition > 1, paste( urlsplit[1:ojs_url_basePosition-1] , collapse = "/"), "" ) # directory of installation
    ojs_url_journalName <- urlsplit[ ojs_url_basePosition + 1 ] # abv name of the jounal
    ojs_url_baseUrl <- paste0( ojs_url_parsed$scheme , "://" , ojs_url_parsed$domain)
    if ( !is.na(ojs_url_parsed$port )) { ojs_url_baseUrl <- paste0( ojs_url_baseUrl , ":" , ojs_url_parsed$port) }
    if ( ojs_url_directory != "" ) { ojs_url_baseUrl <- paste0( ojs_url_baseUrl , "/" , ojs_url_directory) }
    if ( !is.na( ojs_url_basePosition )) { ojs_url_baseUrl <- paste0( ojs_url_baseUrl , "/index.php") }
    if ( ojs_url_journalName != "" ) { ojs_url_baseUrl <- paste0( ojs_url_baseUrl , "/" , ojs_url_journalName) }
    ojs_url$baseUrl <- ojs_url_baseUrl

    if ( !is.na ( urlsplit[ ojs_url_basePosition + 2 ] ) ) { ojs_url$page <- urlsplit[ ojs_url_basePosition + 2 ] } # page/controller = about, user, article, issue, search, etc.
    if ( !is.na ( urlsplit[ ojs_url_basePosition + 3 ] ) ) { ojs_url$command <- urlsplit[ ojs_url_basePosition + 3 ] } # function = view, download, index, search, etc.

    if ( ojs_url$page == "article" & ( ojs_url$command == "view" | ojs_url$command == "download" ) ) {
      if ( !is.na ( urlsplit[ ojs_url_basePosition + 4 ] ) ) { ojs_url$articleId = as.integer(urlsplit[ ojs_url_basePosition + 4 ]) } # there is an article id
      if ( !is.na ( urlsplit[ ojs_url_basePosition + 5 ] ) ) { ojs_url$galleyId = as.integer(urlsplit[ ojs_url_basePosition + 5 ]) } # there is an galley id
      if ( ( ojs_url$articleId != "" ) & ( ojs_url$galleyId == "" ) ) { ojs_url$expect <- "view article" } # url points to article view (abstract)
      if ( ( ojs_url$articleId != "" ) & ( ojs_url$galleyId != "" ) & ( ojs_url$command == "view" ) ) { ojs_url$expect <- "view galley" } # url points to article galley view (probably inline reader)
      if ( ( ojs_url$articleId != "" ) & ( ojs_url$galleyId != "" ) & ( ojs_url$command == "download" ) ) { ojs_url$expect <- "download galley" } # url forces download of article galley (probably pdf)
    }

    if ( ojs_url$page == "issue" & ( ojs_url$command == "current" | ojs_url$command == "view" | ojs_url$command == "archive" ) ) {
      if ( !is.na ( urlsplit[ ojs_url_basePosition + 4 ] ) ) { ojs_url$issueId = urlsplit[ ojs_url_basePosition + 4 ] } # there is an issue id
      if ( ojs_url$command == "current" ) { ojs_url$expect <- "current issue coverpage" } # url may be directing to current issue cover page
      if ( ojs_url$command == "view" & ( ojs_url$issueId != "" ) ) {
        if ( !is.na(urlsplit[ ojs_url_basePosition + 5 ]) && (urlsplit[ ojs_url_basePosition + 5 ] == "showToC") ) {
          ojs_url$expect <- "view issue ToC" ; # forces issue ToC
        } else {
          ojs_url$expect <- "view issue coverpage" ; # url may be directing to issue cover page
        }
      }
      if ( ojs_url$command == "archive" ) { ojs_url$expect <- "view issue archive" } # url is redirecting issue archive list
    }

    if ( ojs_url$page == "oai" ) {
      ojs_url$expect <- "oai"; # url is redirecting to oai protocol
    }

    if ( ojs_url$page == "search" ) {
      ojs_url$expect <- "search"; # url is pointing to search form/ results
      # 2do: search form or result?
      # https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/search/search?simpleQuery=becerra&searchField=query
    }

    ojs_url$assume_article <- ifelse(ojs_url$articleId != "", paste0(ojs_url_baseUrl , "/article/view/", ojs_url$articleId), "")
    ojs_url$assume_issue <- ifelse(ojs_url$issueId != "", paste0(ojs_url_baseUrl , "/issue/view/", ojs_url$issueId , "/showToc"), "")
    ojs_url$assume_oai <- paste0(ojs_url_baseUrl , "/oai")
    ojs_url$assume_search <- paste0(ojs_url_baseUrl , "/search/search/query=")
    ojs_url$assume_archive <- paste0(ojs_url_baseUrl , "/issue/archive")

    ojs_url_row <- as.data.frame( t(unlist(ojs_url)), stringsAsFactors = FALSE)
    ojs_url_dataframe <- rbind(ojs_url_dataframe, ojs_url_row)
    #glimpse(ojs_url_row)

  }
  return(ojs_url_dataframe)
}
