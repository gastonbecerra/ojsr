#' Scraps an OJS archive of issues and retrieves the issues' url
#'
#' Takes a vector of OJS urls and scraps their archive of issues to retrieve the links to OJS issues.
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the url of issues found (output_url)
#'
#' @examples
#'
#' socPsy_urls <- c( # argentinian social psychology journals
#'    'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
#'    'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903'
#' )
#' issues <- ojsr::get_issues_from_archive(socPsy_urls, verbose = TRUE)
#'
#' @export
get_issues_from_archive <- function ( input_url , verbose = FALSE ) {
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_issue_url")
  return(df)
}


#' Scraps an OJS issue and retrieves the articles' url
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS articles
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the articles url scrapped (output_url)
#'
#' @examples
#'
#' socPsy_issues_url <- c( #  initial issues from a few social psychology journals
#'    'https://revistas.ucn.cl/index.php/saludysociedad/issue/view/65', # includes ToC
#'    'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/view/31' # no ToC
#' )
#' articles <- ojsr::get_articles_from_issue(socPsy_issues_url, verbose = TRUE)
#'
#' @export
#'
get_articles_from_issue <- function ( input_url , verbose = FALSE ) {
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_article_url")
  return(df)
}


#' Scraps an OJS article for galley links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS galleys
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the articles url scrapped (output_url),
#' the format of the galley (format), and the url that forces download of the galley (download_url)
#' @examples
#'
#'     socPsy_articles <- c( # articles on social psychology
#'        'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/55657',
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137'
#'     )
#'     galleys <- ojsr::get_galley_url_from_article(socPsy_articles, verbose = TRUE)
#'
#' @export
#'
get_galley_url_from_article <- function ( input_url , verbose = FALSE ) {
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_galley_url")
  return(df)
}


#' Creates OJS search result urls
#'
#' Takes a vector of OJS urls and a string for search criteria to compose the search url.
#'
#' You may then pass these urls to any other get_*_url() function.
#'
#' @param input_url Character vector.
#' @param search_criteria Character string
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     socPsy_journals <- c( # 2 social psychology journals
#'         'https://revistapsicologia.uchile.cl/index.php/RDP/',
#'         'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/current'
#'     )
#'     search_criteria <- "social representations"
#'     search_results <- ojsr::get_search_url(socPsy_journals,
#'         search_criteria = "psicología social", verbose = TRUE)
#'
#' @export
get_paginated_search_url <- function ( input_url , search_criteria, verbose = FALSE) {
  if (missing(search_criteria) | trimws(search_criteria) == "" | !is.character(search_criteria) | length(search_criteria) > 1 ) {
    stop("search criteria must be a non-empty character string", call. = FALSE)
  } else {
    search_criteria <- gsub(pattern = " ", replacement = "+", x = search_criteria)
    df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_search_url", search_criteria = search_criteria)
    return(df)
  }
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
#' @examples
#'
#'     socPsy_articles <- c(
#'         'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137',
#'         'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311'
#'     )
#'     metadata <- ojsr::get_html_meta_from_article(socPsy_articles, verbose = TRUE)
#'
#' @export
#'
get_html_meta_from_article <- function ( input_url , verbose = FALSE) {
  df <- ojrs_scrap_meta(url_input = input_url, verbose = verbose)
  return(df)
}


#' @importFrom magrittr %>%
ojrs_scrap_v2 <- function ( url_input, verbose, from, search_criteria = "" ) {

  # basic validation

  if ( missing(url_input) | !is.character(url_input) ) { stop("url must be a character string/vector. maybe introduced a dataframe and forgot to point a column?", call. = FALSE) }
  if ( missing(verbose) | !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }

  df <- data.frame() # object to collect

  # parsing the input

  url_parsed <- ojsr::process_urls(url_input)

  # url = conventional url to be scraped

  url <- switch (from,
    get_issue_url = url_parsed$conventional_archive,
    get_article_url = url_parsed$conventional_issue,
    get_galley_url = url_parsed$conventional_article,
    get_meta_from_html = url_parsed$conventional_article,
    get_search_url = paste0(url_parsed$conventional_search, search_criteria)
  )

  # xpath = criteria to look for in the html, depending on who is calling

  xpath <- switch (from ,
    get_issue_url = '//a[contains(@href, "/issue/view/")]',
    get_article_url = '//a[contains(@href, "/article/view/")]',
    get_galley_url = '//a[contains(@href, "/article/view/")]',
    get_meta_from_html = './/meta',
    get_search_url = '//a[contains(@href, "searchPage=")]'
  )

  # output_names = returning table headings

  output_names <- switch (from ,
    get_issue_url = c('input_url', 'output_url'),
    get_article_url = c('input_url', 'output_url'),
    get_galley_url = c('input_url','output_url','format','download_url'),
    get_meta_from_html = NA, # this one returns a different table format
    get_search_url = c('input_url', 'output_url')
  )

  if (length(url) < 1){ stop("empty url vector to scrap. aborting", call. = FALSE) }

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

        # processing scrapped links

        links <- rvest::html_nodes(webpage, xpath = xpath)

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

          parsed_links <- dplyr::left_join( scrapped_links, ojsr::process_urls(scrapped_links$input_url) , by="input_url" )

          # returning the conventional form for the scrapped links

          conventional_links <- switch( from,
            get_issue_url = { parsed_links %>% dplyr::filter( !is.na(issue_id) ) %>% dplyr::select(conventional_issue) %>% unlist() %>% unique() },
            get_article_url = { parsed_links %>% dplyr::filter( !is.na(article_id), is.na(galley_id) ) %>% dplyr::select(conventional_article) %>% unlist() %>% unique() },
            get_galley_url = NA, # we need more than 1 variable; we do this in the next block
            get_meta_from_html = NA, # 2do: xxx
            get_search_url = { parsed_links %>% dplyr::select(conventional_search) %>% unlist() %>% unique() }
          )

          skip_row <- FALSE # error tracking before adding the row to the returning df
          newdf <- data.frame() # object to return

          # preparing the row to return (usually the input, the conventional (parsed>processed), and etc.)

          ## if article or issue, return urls for input + output

          if (from=="get_issue_url" | from=="get_article_url") {
            newdf <- data.frame(cbind( url_input[i], conventional_links ), stringsAsFactors = FALSE)
          }

          ## if galley, preparing a second vector with formats

          if (from=="get_galley_url") {
            galley_links <- parsed_links %>% dplyr::filter(!is.na(article_id), !is.na(galley_id) ) %>% dplyr::select(input_url, format) %>% unique()
            if(nrow(galley_links)>0){
              conventional_links <- galley_links$input_url %>% as.character()
              links_formats <- galley_links$format %>% as.character()
              links_force <- gsub(pattern = "article/view", replacement = "article/download", x = conventional_links, fixed = TRUE)
              newdf <- data.frame(cbind( url_input[i], conventional_links, links_formats, links_force ), stringsAsFactors = FALSE)
            }
          }

          ## if search, preparing the different pagination links

          if (from=="get_search_url") {
            search_pages <- urltools::param_get(urls = xml2::xml_attr(x = links, attr = "href"),"searchPage")
            if (!missing(search_pages)) {
              result_pages <- max(search_pages$searchPage)
              search_df <- data.frame()
              for (j in 1:result_pages) {
                search_df <- rbind(search_df, data.frame(cbind( url_input[i], paste0(conventional_links,search_criteria,'&searchPage=',j)), stringsAsFactors = FALSE))
              }
              newdf <- search_df
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


#' @importFrom magrittr %>%
ojrs_scrap_meta <- function ( url_input, verbose ) {

  # basic validation

  if ( missing(url_input) | !is.character(url_input) ) { stop("url must be a character string/vector. maybe introduced a dataframe and forgot to point a column?", call. = FALSE) }
  if ( missing(verbose) | !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }

  df <- data.frame() # object to collect

  # parsing the input

  url_parsed <- ojsr::process_urls(url_input)

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
                input_url = url_input[i],
                meta_data_name,meta_data_content,meta_data_scheme,meta_data_xmllang), stringsAsFactors = FALSE))
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
#' @examples
#'
#'     socPsy_articles <- c(
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137',
#'        'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311'
#'     )
#'     metadata_oai <- ojsr::get_oai_meta_from_article(socPsy_articles, verbose = TRUE)
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

    if (verbose) { message("pre-processing url ", url[i]) }
    process_url <- ojsr::process_urls(url[i])
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

  return(df)
}
