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
  # 2do: include a correction parameter (-1, -2) for ojs_url_base_position + x, so you can skip journal name in single-OJS installations

  if( !is.character(url) ) { stop("url must be a character string or vector") }

  ojs_url_dataframe <- data.frame()  # object to return

  for (i in 1:length(url)) { # in a loop for vectorized input

    urlposta <- url[i] # input parameter
    if (!grepl(pattern = "^https?://", x = urlposta)) { warning( paste(urlposta , "(element ", i ,")", " does not include http|https protocol") ) }
    if (!grepl(pattern = "index.php", x = urlposta, fixed = TRUE)) { warning( paste(urlposta , "(element ", i ,")", " does not include index.php") ) }

    ojs_url <- list() # object to populate. converts to df before returning
    ojs_url$input_url <- urlposta # input parameter
    ojs_url$base_url <- "" # base url of the OJS
    ojs_url$issue_id <- NA # if an issue, its ID
    ojs_url$article_id <- NA # if an article, its ID
    ojs_url$galley_id <- NA # if a galley (full-content, supplementary materials, etc.), its ID
    ojs_url$conventional_archive <- "" # the *(conventional to be) right* url to scrap the list of issues
    ojs_url$conventional_issue <- "" # the *(conventional to be) right* url to scrap the ToC from an issue (if issue_id is present, otherwise "")
    ojs_url$conventional_article <- "" # the *(conventional to be) right* url to scrap metadata from an article (if article_id is present, otherwise "")
    ojs_url$conventional_oai <- "" # the *(conventional to be) right* url to the OAI records listing
    ojs_url$conventional_search <- "" # the *(conventional to be) right* url to a search result

    ojs_url_path <- ojs_url_page <- ojs_url_command <- ""

    tryCatch({

      ojs_url_parsed <- urltools::url_parse(urlposta) # parsing non-ojs url conventions
      ojs_url_path <- urltools::path(urlposta) # url - domain = /index.php/journal/page/function/arg1/arg2/arg3

    }, warning = function(war) { warning(paste("warning parsing ", url[i], " : ",war)) ;
    }, error = function(err) { stop(paste("error parsing ", url[i], " : ",err));
    })

    if ( ojs_url_path != "" ) {

      urlsplit <- unlist(strsplit(x = ojs_url_path, split = "/", fixed = TRUE)) # split url by "/"

      if (!grepl(pattern = "/search", x = urlposta, fixed = TRUE) & !is.na(ojs_url_parsed$parameter)) { warning( paste(urlposta , "(element ", i ,")", " includes paramenters in url") ) }

      ojs_url_base_position <- match("index.php", urlsplit) # position of "index.php" url segment, or NA
      ojs_url_base_url <- "" # base url to rewrite (the "conventional" paramenters returned)
      ojs_url_directory <- ifelse( ojs_url_base_position > 1, paste( urlsplit[1:ojs_url_base_position-1] , collapse = "/"), "" ) # directory of installation
      ojs_url_journal_name <- urlsplit[ ojs_url_base_position + 1 ] # abv name of the jounal
      ojs_url_base_url <- paste0( ojs_url_parsed$scheme , "://" , ojs_url_parsed$domain)
      if ( !is.na(ojs_url_parsed$port )) { ojs_url_base_url <- paste0( ojs_url_base_url , ":" , ojs_url_parsed$port) }
      if ( ojs_url_directory != "" ) { ojs_url_base_url <- paste0( ojs_url_base_url , "/" , ojs_url_directory) }
      if ( !is.na( ojs_url_base_position )) { ojs_url_base_url <- paste0( ojs_url_base_url , "/index.php") }
      if ( ojs_url_journal_name != "" ) { ojs_url_base_url <- paste0( ojs_url_base_url , "/" , ojs_url_journal_name) }
      ojs_url$base_url <- ojs_url_base_url

      if ( !is.na ( urlsplit[ ojs_url_base_position + 2 ] ) ) { ojs_url_page <- urlsplit[ ojs_url_base_position + 2 ] } # page/controller = about, user, article, issue, search, etc.
      if ( !is.na ( urlsplit[ ojs_url_base_position + 3 ] ) ) { ojs_url_command <- urlsplit[ ojs_url_base_position + 3 ] } # function = view, download, index, search, etc.

      if ( ojs_url_page == "article" & ( ojs_url_command == "view" | ojs_url_command == "download" ) ) {
        if ( !is.na ( urlsplit[ ojs_url_base_position + 4 ] ) ) { ojs_url$article_id = as.integer(urlsplit[ ojs_url_base_position + 4 ]) } # there is an article id
        if ( !is.na ( urlsplit[ ojs_url_base_position + 5 ] ) ) { ojs_url$galley_id = as.integer(urlsplit[ ojs_url_base_position + 5 ]) } # there is an galley id
      }

      if ( ojs_url_page == "issue" & ( ojs_url_command == "current" | ojs_url_command == "view" | ojs_url_command == "archive" ) ) {
        if ( !is.na ( urlsplit[ ojs_url_base_position + 4 ] ) ) { ojs_url$issue_id = urlsplit[ ojs_url_base_position + 4 ] } # there is an issue id
      }

      ojs_url$conventional_article <- ifelse( !is.na(ojs_url$article_id), paste0(ojs_url_base_url , "/article/view/", ojs_url$article_id), NA)
      ojs_url$conventional_issue <- ifelse( !is.na(ojs_url$issue_id), paste0(ojs_url_base_url , "/issue/view/", ojs_url$issue_id , "/showToc"), NA)
      ojs_url$conventional_oai <- paste0(ojs_url_base_url , "/oai")
      ojs_url$conventional_search <- paste0(ojs_url_base_url , "/search/?query=")
      ojs_url$conventional_archive <- paste0(ojs_url_base_url , "/issue/archive")
      ojs_url_row <- as.data.frame( t(unlist(ojs_url)), stringsAsFactors = FALSE)
      ojs_url_dataframe <- rbind(ojs_url_dataframe, ojs_url_row)
    }

  }
  return(ojs_url_dataframe)
}
