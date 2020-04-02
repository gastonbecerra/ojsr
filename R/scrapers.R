#' Scraps an OJS page for issue links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS issues.
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the url of issues found (output_url)
#'
#' @examples
#'
#'     socPsy_urls <- c( # argentinian social psychology journals
#'        'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive',
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903'
#'     )
#'     issues <- ojsr::get_issue_url(socPsy_urls, verbose = TRUE)
#'
#' @export
get_issue_url <- function ( input_url , verbose = FALSE ) {
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_issue_url")
  return(df)
}





#' Scraps an OJS page for article links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS articles
#'
#' Search criteria: links containing "/article/view” (method='scrap_by_href_convention_no_classes'),
#' and the same without filtering (method='scrap_by_href_convention'). (Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_href_convention_no_classes (default), scrap_by_href_convention
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the articles url scrapped (output_url)
#'
#' @export
#'
get_article_url <- function ( input_url , use_conventional_url = TRUE, method = "scrap_by_href_convention_no_classes", verbose = FALSE ) {
  # df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_article_url")
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_article_url")
  return(df)
}





#' Scraps an OJS page for galley links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS galleys
#'
#' Search criteria: links containing classes “file”, “download” or “obj_galley_link”
#' (method='scrap_by_class_convention')
#'
#' @param input_url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_class_convention
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the articles url scrapped (output_url),
#' the format of the galley (format), and the url that forces download of the galley (download_url)
#' @examples
#'
#'     socPsy_articles <- c( # articles on social psychology
#'        'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/55657',
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137'
#'     )
#'     galleys <- ojsr::get_galley_url(socPsy_articles, use_conventional_url = TRUE, verbose = TRUE)
#'
#' @export
#'
get_galley_url <- function ( input_url , use_conventional_url = TRUE, method =  "scrap_by_class_convention", verbose = FALSE ) {
  # df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_galley_url")
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_galley_url")
  return(df)
}




#' Creates OJS search result urls
#'
#' Takes a vector of OJS urls and a string for search criteria to compose the search url.
#'
#' You may then pass these urls to any other get_*_url() function.
#'
#' If check_pagination = TRUE, it runs the search in OJS to see if pagination is involved,
#' and returns the url for the search result pages.
#'
#' @param input_url Character vector.
#' @param search_criteria Character string
#' @param check_pagination Logical.
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
#'         search_criteria = "psicología social", check_pagination = TRUE, verbose = TRUE)
#'
#' @export
get_search_url <- function ( input_url , search_criteria, check_pagination = TRUE, verbose = FALSE) {
  # if (missing(search_criteria) | trimws(search_criteria) == "" | !is.character(search_criteria) | length(search_criteria)>1 ) {
  #   stop("search criteria must be a character string", call. = FALSE)
  # } else {
  #   search_criteria <- gsub(pattern = " ", replacement = "+", x = search_criteria)
  #   df <- ojrs_scrap(url_input = input_url, search_criteria, check_pagination = check_pagination,
  #     use_conventional_url = TRUE, verbose = verbose, method = "scrap_by_searchPage", from = "get_search_url")
  #   return(df)
  # }
  df <- ojrs_scrap_v2(url_input = input_url, verbose = verbose, from = "get_search_url", search_criteria = search_criteria)
  return(df)
}




#' Creates OJS search result urls
#'
#' Takes a vector of OJS urls and a string for search criteria to compose the search url.
#'
#' You may then pass these urls to any other get_*_url() function.
#'
#' If check_pagination = TRUE, it runs the search in OJS to see if pagination is involved,
#' and returns the url for the search result pages.
#'
#' @param input_url Character vector.
#' @param search_criteria Character string
#' @param check_pagination Logical.
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
#'         search_criteria = "psicología social", check_pagination = TRUE, verbose = TRUE)
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





#' Scraps metadata from html of OJS pages
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
#'     metadata <- ojsr::get_meta_from_html(socPsy_articles, verbose = TRUE)
#'
#' @export
#'
get_meta_from_html <- function ( input_url , verbose = FALSE) {
  df <- ojrs_scrap_meta(url_input = input_url, verbose = verbose)
  return(df)
}


#' @importFrom magrittr %>%
ojrs_scrap <- function (url_input, use_conventional_url, verbose, method, from, search_criteria = "", check_pagination = TRUE) {

  # basic validation

  if ( missing(url_input) | !is.character(url_input) ) { stop("url must be a character string/vector. maybe introduced a dataframe and forgot to point a column?", call. = FALSE) }
  if ( missing(verbose) | !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }
  if ( missing(method) | !is.character(method) ) { stop("method must be a character string", call. = FALSE) }
  if ( missing(use_conventional_url) | !is.logical(use_conventional_url) ) { stop("use_conventional_url must be logical", call. = FALSE) }

  df <- data.frame() # object to collect

  # should we (parse urls and) use the conventional url, or should we use user-input?

  if ( use_conventional_url ) {
    if (verbose) { message("parsing urls") }
      url_parsed <- ojsr::process_urls(url_input)
      url <- switch (from, # conventional url to be scrapped
        get_issue_url = url_parsed$conventional_archive,
        get_article_url = url_parsed$conventional_issue,
        get_galley_url = url_parsed$conventional_article,
        get_meta_from_html = url_parsed$conventional_article,
        get_search_url = paste0(url_parsed$conventional_search, search_criteria)
      )
      if (verbose) { message("urls parsed; using conventional format for urls instead of input") }
  } else {
    if (verbose) { message("using input urls") }
    url <- url_input
  }

  # loop for vectorized url input

  if (length(url)<1){ stop("empty url vector to scrap. aborting", call. = FALSE) }

  for (i in 1:length(url)) {

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

      from_method <- paste(from, method, sep = "@")

      if (webpage_read) {

        if( grepl( x = method, pattern = "scrap_by_href"  )  ) { # we are crawling (issue, article, galley) links ...

          xpath <- switch ( from , # xpath = criteria to look for in the html, depending on who is calling
            "get_issue_url" = '//a[contains(@href, "/issue/view/")]',
            "get_article_url" = {
              xpath <- switch (method,
                "scrap_by_href_convention" = '//a[contains(@href, "/article/view/")',
                "scrap_by_href_convention_no_classes" = '//a[contains(@href, "/article/view/") and not(contains(@class, "galley-link")) and not(contains(@class, "galley")) and not(contains(@class, "file")) and not(contains(@class, "pdf"))]',
                "scrap_by_href_convention_no_classes_long" = '//a[contains(@href, "/article/view/") and not(contains(@class, "galley-link")) and not(contains(@class, "galley")) and not(contains(@class, "file")) and not(contains(@class, "pdf")) and not(contains(@class, "btn"))]'
              )
            }
          )
          output_names <- c('input_url', 'output_url')
          links <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href") %>% unique()
          if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
          if (length(links)>0) {
            newdf <- data.frame(cbind( url_input[i], links ), stringsAsFactors = FALSE)
            names(newdf) <- output_names
            df <- rbind(df, newdf)
          }

        } else if ( from_method == "get_article_url@scrap_and_process" ) { # we are crawlling galleys (pdf, xml, mp3, etc.)

          xpath <- '//a[contains(@href, "/article/view/")]'
          output_names <- c('input_url', 'output_url')
          links <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href") %>% unique()
          if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
          if (length(links)>0) {

            links2 <- ojsr::process_urls(links)
            links3 <- links2$conventional_article %>% unlist() %>% unique()

            #links3 <- links2 %>% dplyr::filter(!is.na(article_id), is.na(galley_id))
            if (verbose & (length(links2)!=length(links3))) { message("links processed... kept only ", length(links3), " elements with article_id and NOT galley_id") }

            newdf <- data.frame(cbind( url_input[i], links3 ), stringsAsFactors = FALSE)
            names(newdf) <- output_names
            df <- rbind(df, newdf)
          }

        } else if ( from_method == "get_galley_url@scrap_by_class_convention" ) { # we are crawlling galleys (pdf, xml, mp3, etc.)

          xpath <- './/a[contains(@class, "file") or contains(@class, "obj_galley_link") or contains(@class, "download") or contains(@class, "btn")]'
          output_names <- c('input_url','output_url','format','download_url')
          links <- rvest::html_nodes(webpage, xpath = xpath)
          if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
          if (length(links)>0) {
            links_url <- links %>% xml2::xml_attr("href") %>% unique()
            links_formats <- links %>% rvest::html_text() %>% trimws()
            links_force <- gsub(pattern = "article/view", replacement = "article/download", x = links_url, fixed = TRUE)
            newdf <- data.frame(cbind( url_input[i] , links_url, links_formats , links_force ), stringsAsFactors = FALSE)
            names(newdf) <- output_names
            df <- rbind(df, newdf)
          }
          names(df) <- output_names

        } else if ( from_method == "get_meta_from_html@scrap_meta_in_head" ) { # we are scraping meta-data via html

          xpath <- './/meta'
          meta_data_tags <- rvest::html_nodes(webpage, xpath = xpath)
          if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(meta_data_tags), " elements using criteria ", xpath) }
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

        } else if ( from_method == "get_search_url@scrap_by_searchPage" ) { # we are composing search urls and checking pagination

          output_names <- c('input_url','output_url')
          if ( !check_pagination ) { # returning only 1 input = 1 output ulr from process_urls()
            df <- rbind(df, data.frame(cbind( url_input[i], url[i] ), stringsAsFactors = FALSE))
          } else { # checking pagination to return 1 input and several outputs
            xpath <- '//a[contains(@href, "searchPage=")]'
            links <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href") %>% unique()
            if (length(links)>0) {
              if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links)+1, " pagination links using criteria ", xpath) }
              links2 <- urltools::param_get(links,"searchPage")
              if (!missing(links2)) {
                result_pages <- max(links2$searchPage)
                for (j in 1:result_pages) {
                  df <- rbind(df, data.frame(cbind( url_input[i], paste0(url[i],'&searchPage=',j)), stringsAsFactors = FALSE))
                }
              }
            } else {
              if (verbose) { message("scraped ", substr(url[i],1,15), " ... no pagination found using criteria ", xpath) }
              newdf <- data.frame(cbind( url_input[i], url[i] ), stringsAsFactors = FALSE)
              names(newdf) <- output_names
              df <- rbind(df, newdf)
            }
          }

        } else {
          stop("no valid combination of function / method provided: ", from_method, call. = FALSE)
        }
      }


    }

  }
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

  if (verbose) { message("urls parsed; using conventional format for urls instead of input") }

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


#
#         } else if ( from_method == "get_meta_from_html@scrap_meta_in_head" ) { # we are scraping meta-data via html
#
#           xpath <- './/meta'
#           meta_data_tags <- rvest::html_nodes(webpage, xpath = xpath)
#           if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(meta_data_tags), " elements using criteria ", xpath) }
#           if (class(meta_data_tags)=="xml_nodeset"){
#             meta_data_tags_list <- xml2::xml_attrs(meta_data_tags)
#             meta_data_name <- meta_data_content <- meta_data_scheme <- meta_data_xmllang <- NA
#             for (j in 1:length(meta_data_tags_list)) { # iterate per metadata
#               if ( "name" %in% names(meta_data_tags_list[[j]]) ) {
#                 meta_data_name <- unname(c(meta_data_name, meta_data_tags_list[[j]]["name"]))
#                 if ( "content" %in% names(meta_data_tags_list[[j]]) ) { meta_data_content<-unname(c(meta_data_content,meta_data_tags_list[[j]]["content"])) } else { meta_data_content<-c(meta_data_content, NA) }
#                 if ( "scheme" %in% names(meta_data_tags_list[[j]]) ) { meta_data_scheme<-unname(c(meta_data_scheme,meta_data_tags_list[[j]]["scheme"])) } else { meta_data_scheme<-c(meta_data_scheme, NA) }
#                 if ( "xml:lang" %in% names(meta_data_tags_list[[j]]) ) { meta_data_xmllang<-unname(c(meta_data_xmllang,meta_data_tags_list[[j]]["xml:lang"])) } else { meta_data_xmllang<-c(meta_data_xmllang, NA) }
#               }
#             }
#             if (!( purrr::is_empty(meta_data_name) | purrr::is_empty(meta_data_content) )) {
#               df <- rbind(df,
#                 as.data.frame(cbind(
#                   input_url = url_input[i],
#                   meta_data_name,meta_data_content,meta_data_scheme,meta_data_xmllang), stringsAsFactors = FALSE))
#             }
#           }
#




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
#'     metadata_oai <- ojsr::get_meta_from_oai(socPsy_articles, verbose = TRUE)
#' @importFrom magrittr %>%
#' @export
#'
get_meta_from_oai <- function ( input_url , verbose = FALSE ) {

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
          registro <- record[[1]]$GetRecord$record$metadata$dc %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE, row.names = FALSE)
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

