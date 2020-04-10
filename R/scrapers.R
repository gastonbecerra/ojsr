#' Scraps an OJS archive of issues and retrieves the issues' url
#'
#' Takes a vector of OJS urls and scraps their archive of issues to retrieve links to OJS issues.
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the url of issues found (output_url)
#'
#' @examples
#'
#' journals <- c(
#'   'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
#'   'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903'
#' )
#' issues <- ojsr::get_issues_from_archive(input_url = journals, verbose = TRUE)
#'
#' @export
get_issues_from_archive <- function ( input_url , verbose = FALSE ) {
  url_parsed <- process_urls(input_url)
  xpath <- '//a[contains(@href, "/issue/view/")]'
  output_names <- c('input_url', 'output_url')
  df <- ojsr_scrap_v3(input_url = input_url, verbose = verbose, from = "get_issue_url",
    conventional_url = url_parsed$conventional_archive, xpath = xpath, output_names = output_names)
  return(df)
}


#' Scraps an OJS issue and retrieves the articles' url
#'
#' Takes a vector of OJS urls and scraps them to retrieve links to OJS articles
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the articles url scrapped (output_url)
#'
#' @examples
#'
#' issues <- c(
#'    'https://revistas.ucn.cl/index.php/saludysociedad/issue/view/65',
#'    'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/view/31'
#' )
#' articles <- ojsr::get_articles_from_issue(input_url = issues, verbose = TRUE)
#'
#' @export
#'
get_articles_from_issue <- function ( input_url , verbose = FALSE ) {
  url_parsed <- process_urls(input_url)
  xpath <- '//a[contains(@href, "/article/view/")]'
  output_names <- c('input_url', 'output_url')
  df <- ojsr_scrap_v3(input_url = input_url, verbose = verbose, from = "get_article_url",
    conventional_url = url_parsed$conventional_issue, xpath = xpath, output_names = output_names)
  return(df)
}


#' Scraps an OJS article for galley links
#'
#' Takes a vector of OJS urls and scraps them to retrieve links to OJS galleys
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the articles url scrapped (output_url),
#' the format of the galley (format), and the url that forces download of the galley (download_url)
#'
#' @examples
#'
#' articles <- c(
#'   'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/55657',
#'   'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311'
#' )
#' galleys <- ojsr::get_galleys_from_article(input_url = articles,verbose = TRUE)
#'
#' @export
#'
get_galleys_from_article <- function ( input_url , verbose = FALSE ) {
  url_parsed <- process_urls(input_url)
  xpath <- '//a[contains(@href, "/article/view/")]'
  output_names <- c('input_url','output_url','format','download_url')
  df <- ojsr_scrap_v3(input_url = input_url, verbose = verbose, from = "get_galley_url",
    conventional_url = url_parsed$conventional_article, xpath = xpath, output_names = output_names)
  return(df)
}


#' Scraps OJS search results for a given criteria and retrieves the articles' url
#'
#' Takes a vector of OJS urls, process them to create search result pages (including pagination)
#' and scraps them to retrieve links to OJS articles
#'
#' @param input_url Character vector.
#' @param search_criteria Character string
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#'
#' @examples
#'
#' journals <- c(
#'    'https://revistapsicologia.uchile.cl/index.php/RDP/',
#'    'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/'
#' )
#' criteria <- "actitudes"
#' search_result_pages <- ojsr::get_articles_from_search(input_url = journals,
#'     search_criteria = criteria, verbose = TRUE)
#'
#' @export
get_articles_from_search <- function ( input_url , search_criteria, verbose = FALSE) {

  # basic validation
  if ( missing(search_criteria) | trimws(search_criteria) == "" | !is.character(search_criteria) | length(search_criteria) > 1 ) { stop("search criteria must be a non-empty character string", call. = FALSE) }
  if ( missing(input_url) | !is.character(input_url) ) { stop("url must be a character string/vector. maybe introduced a dataframe and forgot to point a column?", call. = FALSE) }
  if ( !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }

  search_criteria <- gsub(pattern = " ", replacement = "+", x = search_criteria)

  df <- data.frame() # object to collect
  url_parsed <- process_urls(input_url) # parsing the input
  url <- paste0(url_parsed$conventional_search, search_criteria) # url = conventional url to be scraped
  xpath <- '//a[contains(@href, "searchPage=")]'; # xpath = criteria to look for in the html: the pagination link
  output_names <- c('input_url', 'output_url'); # output_names = returning table headings

  if (length(url) < 1){ stop("empty url vector to scrap. aborting", call. = FALSE) }

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying search url ", i, "/" , length(url), ": ", url[i]) }

    if (!is.na(url[i])) {

      # reading webpage from url
      webpage_read = FALSE;
      tryCatch({ # reading the webpage
        webpage <- xml2::read_html(url[i]) # url page content
        webpage_read = TRUE;
      }, warning = function(war) { warning(paste("warning reading ", url[i], " : ",war)) ; closeAllConnections();
      }, error = function(err) { warning(paste("error reading ", url[i], " : ",err)); closeAllConnections();
      })

      if ( webpage_read ) {

        df <- rbind(df, data.frame(cbind( input_url[i], url[i]), stringsAsFactors = FALSE))

        links <- rvest::html_nodes(webpage, xpath = xpath) # processing scrapped links

        if (verbose) { message("looking for extra search pages in ", substr(url[i],1,15), " ... found  ", length(links), " elements using criteria ", xpath) }

        newdf <- data.frame() # object to return
        if (length(links)>0) {
          result_pages <- 0
          # preparing the row to return (usually the input, the conventional (parsed>processed), and etc.)
          search_pages <- urltools::param_get(urls = xml2::xml_attr(x = links, attr = "href"),"searchPage")
          if (!missing(search_pages)) {
            result_pages <- max(search_pages$searchPage)
            search_df <- data.frame()
            for (j in 2:result_pages) {
              search_df <- rbind(search_df, data.frame(cbind( input_url[i], paste0(url[i],'&searchPage=',j)), stringsAsFactors = FALSE))
            }
            newdf <- search_df
          }
          if (nrow(newdf)>0) { df <- rbind(df, newdf) }
        }
      }
    }
  }

  if (nrow(df)>0) {
    if (verbose) { message("retrieving articles from ", nrow(df), " search result pages") }
    names(df) <- output_names
    xpath <- '//a[contains(@href, "/article/view/")]'
    output_names <- c('input_url', 'output_url')
    articles <- data.frame() # object to collect
    articles <- ojsr_scrap_v3(
      input_url = df$input_url,
      verbose = verbose,
      from = "get_article_url",
      conventional_url = df$output_url,
      xpath = xpath,
      output_names = output_names )
  }
  return(articles)
}


#' Scraps metadata from the HTML of OJS articles
#'
#' Takes a vector of OJS urls and and scraps the metadata written in the html.
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the name of the metadata (meta_data_name),
#' the content of the metadata (meta_data_content), the standard in which the content is annotated (meta_data_scheme),
#' and the language in which the metadata was entered (meta_data_xmllang)
#'
#' @examples
#'
#' articles <- c(
#'   'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # article
#'   'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # xml galley
#' )
#' metadata <- ojsr::get_html_meta_from_article(articles, verbose = TRUE)
#'
#' @importFrom magrittr %>%
#' @export
#'
get_html_meta_from_article <- function ( input_url , verbose = FALSE) {

  # basic validation

  if ( missing(input_url) | !is.character(input_url) ) { stop("url must be a character string/vector. maybe introduced a dataframe and forgot to point a column?", call. = FALSE) }
  if ( !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }

  df <- data.frame() # object to collect

  # parsing the input

  url_parsed <- process_urls(input_url)

  # url = conventional url to be scrapped

  url <- url_parsed$conventional_article

  # xpath = criteria to look for in the html, depending on who is calling

  xpath <- './/meta'

  if (length(url) < 1){ stop("empty url vector to scrap. aborting", call. = FALSE) }

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }

    if (!is.na(url[i])) {

      # reading webpage from url

      webpage_read = FALSE;
      tryCatch({ # reading the webpage
        webpage <- xml2::read_html(url[i]) # url page content
        webpage_read = TRUE;
      }, warning = function(war) { warning(paste("warning reading ", url[i], " : ",war)) ; closeAllConnections();
      }, error = function(err) { warning(paste("error reading ", url[i], " : ",err)); closeAllConnections();
      })

      if ( webpage_read ) {

        # processing scrapped links

        meta_data_tags <- rvest::html_nodes(webpage, xpath = xpath)

        if (verbose) { message("scrapped ", substr(url[i],1,15), " ... found ", length(meta_data_tags), " elements using criteria ", xpath) }

        if (class(meta_data_tags)=="xml_nodeset"){
          meta_data_tags_list <- xml2::xml_attrs(meta_data_tags)
          meta_data_name <- meta_data_content <- meta_data_scheme <- meta_data_xmllang <- NA
          if (length(meta_data_tags_list)>0){
            for (j in 1:length(meta_data_tags_list)) { # iterate per metadata
              if ( "name" %in% names(meta_data_tags_list[[j]]) ) {
                meta_data_name <- unname(c(meta_data_name, meta_data_tags_list[[j]]["name"]))
                if ( "content" %in% names(meta_data_tags_list[[j]]) ) { meta_data_content<-unname(c(meta_data_content,meta_data_tags_list[[j]]["content"])) } else { meta_data_content<-c(meta_data_content, NA) }
                if ( "scheme" %in% names(meta_data_tags_list[[j]]) ) { meta_data_scheme<-unname(c(meta_data_scheme,meta_data_tags_list[[j]]["scheme"])) } else { meta_data_scheme<-c(meta_data_scheme, NA) }
                if ( "xml:lang" %in% names(meta_data_tags_list[[j]]) ) { meta_data_xmllang<-unname(c(meta_data_xmllang,meta_data_tags_list[[j]]["xml:lang"])) } else { meta_data_xmllang<-c(meta_data_xmllang, NA) }
              }
            }
            if (!( purrr::is_empty(meta_data_name) | purrr::is_empty(meta_data_content) )) {
              df <- rbind(df,
                as.data.frame(cbind(
                  input_url = input_url[i],
                  meta_data_name,meta_data_content,meta_data_scheme,meta_data_xmllang), stringsAsFactors = FALSE))
            }
          }
        }
      }
    }
  }
  return(df)
}


#' Get OAI metadata from an OJS article url
#'
#' This functions access OAI records (within OJS) for any article for which you provided an url.
#'
#' Several limitations are in place. Please refer to vignette.
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the name of the metadata (meta_data_name),
#' and the content of the metadata (meta_data_content).
#'
#' @examples
#'
#' articles <- c(
#'   'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # article
#'   'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # xml galley
#' )
#' metadata_oai <- ojsr::get_oai_meta_from_article(input_url = articles, verbose = TRUE)
#'
#' @importFrom magrittr %>%
#' @export
#'
get_oai_meta_from_article <- function ( input_url , verbose = FALSE ) {

  url <- input_url

  if ( !is.character(url) ) { stop("url must be a character string/vector", call. = FALSE) }
  if ( !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }

  oai_base_url = ""
  oai_identifier = ""
  article_id = NA

  df <- data.frame() # object to collect

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }

    if (!is.na(url[i])) {

      if (verbose) { message("pre-processing url ", url[i]) }
      process_url <- process_urls(url[i])
      oai_base_url <- process_url$conventional_oai[1]
      article_id <- process_url$article_id[1]
      identify_url <- paste0(oai_base_url, "/?verb=Identify")
      if (verbose) { message("identifying on ", identify_url) }

      tryCatch({
        identify_xml <- xml2::read_xml( identify_url )
        identify_list <- identify_xml %>% xml2::as_list()
      }, warning = function(war) { warning(paste("warning processing ", identify_url)) ;
      }, error = function(err) { warning(paste("error processing ", identify_url));
      })

      if ( 'error' %in% names(identify_list[[1]]) ) {
        warning("OAI identity records could not be found on ", identify_url)
      } else {
        identifier <- identify_list[[1]]$Identify$description$`oai-identifier` %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE, row.names = FALSE)
        baseIdentifier <- paste0(
          identifier$scheme,
          identifier$delimiter,
          identifier$repositoryIdentifier,
          identifier$delimiter,
          "article/",
          article_id
        )
        record_url <- paste0(
          oai_base_url,
          "/?verb=GetRecord&metadataPrefix=oai_dc&identifier=",
          baseIdentifier
        )
        if (verbose) { message("looking for record on ", record_url) }

        tryCatch({
          record <- xml2::read_xml(record_url) %>% xml2::as_list()

          # print(record)

        }, warning = function(war) { warning(paste("warning processing ", record_url)) ;
        }, error = function(err) { warning(paste("error processing ", record_url));
        })

        if ( ! "error" %in% names(record[[1]]) ) {
          tryCatch({
            registro <- record[[1]]$GetRecord$record$metadata$dc %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE)
            if (!missing(registro)){
              registro_tidy <- registro %>%
                cbind( input_url = as.character(url[i]) , deparse.level = TRUE) %>%
                tidyr::pivot_longer(-input_url, names_to = "meta_data_name", values_to = "meta_data_content")
              registro_tidy$meta_data_name <- sub('\\..*', '', registro_tidy$meta_data_name)
              registro_tidy$input_url <- as.character(registro_tidy$input_url)
              registro_tidy$meta_data_scheme <- NA
              registro_tidy$meta_data_xmllang <- NA
              df <- rbind(df,registro_tidy)
            } else {
              warning("OAI record could not be parsed for url ", url[i], "\n")
            }
          }, warning = function(war) { warning(paste("warning processing ", url[i]),war) ;
          }, error = function(err) { warning(paste("error processing ", url[i]),err);
          })
        } else {
          warning("OAI record not found on ", record_url, " for url ", url[i], "\n")
        }
      }
    }
  }
  return(df)
}


#' @importFrom magrittr %>%
ojsr_scrap_v3 <- function ( input_url, verbose, from, conventional_url, xpath, output_names ) {

  # basic validation
  if (missing(input_url) | !is.character(input_url) ) { stop("url must be a character string/vector", call. = FALSE) }
  if (!is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }
  if (length(url) < 1){ stop("empty url vector to scrap. aborting", call. = FALSE) }

  df <- data.frame() # object to collect

  url <- conventional_url

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying conventional url for element ", i, "/" , length(url), ": ", url[i]) }

    if (!is.na(url[i])) {

      # reading webpage from url
      webpage_read = FALSE;
      tryCatch({ # reading the webpage
        webpage <- xml2::read_html(url[i]) # url page content
        webpage_read = TRUE;
      }, warning = function(war) { warning(paste("warning reading ", url[i], " : ",war)) ; closeAllConnections();
      }, error = function(err) { warning(paste("error reading ", url[i], " : ",err)); closeAllConnections();
      })

      if ( webpage_read ) {

        links <- rvest::html_nodes(webpage, xpath = xpath) # processing scrapped links

        if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }

        if (length(links)>0) {

          # create a table for the scrapped links
          scrapped_links <- data.frame(
            cbind(
              input_url = as.character( xml2::xml_attr(x = links, attr = "href") ), # href of link
              format = trimws(as.character( rvest::html_text(x = links) ) ) # text of link ... only using this with galleys
            ), stringsAsFactors = FALSE
          ) %>% unique()

          # parsing the scrapped links
          parsed_links <- dplyr::left_join( scrapped_links, process_urls(scrapped_links$input_url) , by="input_url" )

          # returning the conventional form for the scrapped links
          conventional_links <- switch( from,
            get_issue_url = { parsed_links %>% dplyr::filter( !is.na(issue_id) ) %>% dplyr::select(conventional_issue) %>% unlist() %>% unique() },
            get_article_url = { parsed_links %>% dplyr::filter( !is.na(article_id), is.na(galley_id) ) %>% dplyr::select(conventional_article) %>% unlist() %>% unique() },
            get_galley_url = NA, # we need more than 1 variable; we do this in the next block
          )

          newdf <- data.frame() # object to return

          # preparing the row to return (usually the input, the conventional (parsed>processed), and etc.)

          ## if article or issue, return urls for input + output
          if (from=="get_issue_url" | from=="get_article_url") {
            newdf <- data.frame(cbind( input_url[i], conventional_links ), stringsAsFactors = FALSE)
          }

          ## if galley, preparing a second vector with formats
          if (from=="get_galley_url") {
            galley_links <- parsed_links %>% dplyr::filter(!is.na(article_id), !is.na(galley_id), galley_id != "0" ) %>% dplyr::select(input_url, format) %>% unique()
            if(nrow(galley_links)>0){
              conventional_links <- galley_links$input_url %>% as.character()
              links_formats <- galley_links$format %>% as.character()
              links_force <- gsub(pattern = "article/view", replacement = "article/download", x = conventional_links, fixed = TRUE)
              newdf <- data.frame(cbind( input_url[i], conventional_links, links_formats, links_force ), stringsAsFactors = FALSE)
            }
          }

          if (verbose ) { message("scrapped links processed; returning ", nrow(newdf), " elements") }

          if (nrow(newdf)>0) {
            names(newdf) <- output_names
            df <- rbind(df, newdf)
          }
        }
      }
    }
  }
  return(df)
}

