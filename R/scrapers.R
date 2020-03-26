#' Scraps an OJS page for issue links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS issues.
#'
#' Search criteria: links containing “/issue/view” (method='scrap_by_href_convention', default).
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_href_convention
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the issues url scraped (output_url)
#'
#' @examples
#'
#'     socPsy_urls <- c( # argentinian social psychology journals
#'        'https://dspace.palermo.edu/ojs/index.php/psicodebate/issue/archive', # points at the archive of issues
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903' # points at an article; ojsr will use process_url() to form the right link to scrap
#'     )
#'     issues <- ojsr::get_issue_url(socPsy_urls, use_conventional_url = TRUE, verbose = TRUE)
#'
#' @export
get_issue_url <- function ( input_url , use_conventional_url = TRUE, method = "scrap_by_href_convention", verbose = FALSE ) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_issue_url")
  return(df)
}





#' Scraps an OJS page for article links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS articles
#'
#' Search criteria: links containing "/article/view” (method='scrap_by_href_convention_no_classes', default), and the same without filtering (method='scrap_by_href_convention'). (Please refer to vignette.)
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_href_convention_no_classes (default), scrap_by_href_convention
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url) and the articles url scraped (output_url)
#'
#' @export
#'
get_article_url <- function ( input_url , use_conventional_url = TRUE, method = "scrap_by_href_convention_no_classes", verbose = FALSE ) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_article_url")
  return(df)
}





#' Scraps an OJS page for galley links
#'
#' Takes a vector of OJS urls and scraps them to retrieve the links to OJS galleys
#'
#' Search criteria: links containing containing classes “file”, “download” or “obj_galley_link” (method='scrap_by_class_convention', default)
#'
#' Galleys are the final presentation version of the content of the articles.
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_class_convention
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the articles url scraped (output_url),
#' the format of the galley (format), and the url that forces download of the galley (download_url)
#' @examples
#'
#'     socPsy_articles <- c( # 3 articles on social psychology, specifically social representations theory
#'        'https://revistapsicologia.uchile.cl/index.php/RDP/article/view/55657', # 2 galleys: pdf and mp3
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # 1 galley: pdf
#'     )
#'     galleys <- ojsr::get_galley_url(socPsy_articles, use_conventional_url = TRUE, verbose = TRUE)
#'
#' @export
#'
get_galley_url <- function ( input_url , use_conventional_url = TRUE, method =  "scrap_by_class_convention", verbose = FALSE ) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_galley_url")
  return(df)
}




#' Creates OJS search result urls
#'
#' Takes a vector of OJS urls and a string for search criteria to compose the search url.
#'
#' If check_pagination = TRUE, it runs the search in OJS to see if pagination is involved, and returns the url for the search result pages. You may then pass these urls to any other get_*_url() function. Please refer to the vignette.
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param search_criteria Character string
#' @param check_pagination Logical.
#' @param method String. Available methods: scrap_by_href_convention
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     socPsy_journals <- c( # 2 social psychology journals
#'         'https://revistapsicologia.uchile.cl/index.php/RDP/', # home url
#'         'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/current' # current issue url
#'     )
#'     search_criteria <- "social representations"
#'     search_results <- ojsr::get_search_url(socPsy_journals, search_criteria = "psicología social", check_pagination = TRUE, verbose = TRUE)
#'
#' @export
get_search_url <- function ( input_url , search_criteria, check_pagination = TRUE, verbose = FALSE) {
  if (missing(search_criteria) | trimws(search_criteria) == "" | !is.character(search_criteria) | length(search_criteria)>1 ) {
    stop("search criteria must be a character string", call. = FALSE)
  } else {
    search_criteria <- gsub(pattern = " ", replacement = "+", x = search_criteria)
    df <- ojrs_scrap(url_input = input_url, search_criteria, check_pagination = check_pagination,
      use_conventional_url = TRUE, verbose = verbose, method = "scrap_by_searchPage", from = "get_search_url")
    return(df)
  }
}





#' Scraps metadata from html of OJS pages
#'
#' Takes a vector of OJS urls and and scraps the metadata written in the html.
#'
#' Search criteria: <meta> tags in the <head> section of the html (method='scrap_meta_in_head', default)
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_meta_in_head
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the name of the metadata (meta_data_name),
#' the content of the metadata (meta_data_content), the standard in which the content is annotated (meta_data_scheme),
#' and the language in which the metadata was entered (meta_data_xmllang)
#' @examples
#'
#'     socPsy_articles <- c( # 3 articles on social psychology, specifically social representations theory
#'         'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # url pointing at the article page
#'         'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # url pointing at a particular galley (xml)
#'     )
#'     metadata <- ojsr::get_meta_from_html(socPsy_articles, use_conventional_url = TRUE, verbose = TRUE)
#'
#' @export
#'
get_meta_from_html <- function ( input_url , use_conventional_url = TRUE, method =  "scrap_meta_in_head", verbose = FALSE) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_meta_from_html")
  return(df)
}



ojrs_scrap <- function (url_input, use_conventional_url, verbose, method, from, search_criteria = "", check_pagination = TRUE) {

  # basic validation

  if ( missing(url_input) | !is.character(url_input) ) { stop("url must be a character string/vector", call. = FALSE) }
  if ( missing(verbose) | !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }
  if ( missing(method) | !is.character(method) ) { stop("method must be a character string", call. = FALSE) }
  if ( missing(use_conventional_url) | !is.logical(use_conventional_url) ) { stop("use_conventional_url must be logical", call. = FALSE) }

  df <- data.frame() # object to collect

  # should we (parse urls and) use the conventional url, or should we use user input?

  if ( use_conventional_url ) {
    if (verbose) { message("parsing urls") }
      url_parsed <- ojsr::process_urls(url_input)
      url <- switch (from, # conventional url to be scraped
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
                "scrap_by_href_convention_no_classes" = '//a[contains(@href, "/article/view/") and not(contains(@class, "galley-link")) and not(contains(@class, "galley")) and not(contains(@class, "file")) and not(contains(@class, "pdf"))]'
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


        } else if ( from_method == "get_galley_url@scrap_by_class_convention" ) { # we are crawlling galleys (pdf, xml, mp3, etc.)

          xpath <- './/a[contains(@class, "file") or contains(@class, "obj_galley_link") or contains(@class, "download")]'
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






#' Get OAI metadata from an OJS article url
#'
#' This functions access (within the OJS) the OAI records for any article for which you provided an url.
#'
#' Several limitations are in place. Please refer to vignette.
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param input_url Character vector.
#' @param verbose Logical.
#' @return A long-format dataframe with the url you provided (input_url), the name of the metadata (meta_data_name),
#' and the content of the metadata (meta_data_content).
#' @examples
#'
#'     socPsy_articles <- c( # 2 articles on social psychology, specifically social representations theory
#'        'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2137', # url pointing to the article page
#'        'https://dspace.palermo.edu/ojs/index.php/psicodebate/article/view/516/311' # url pointing a particular galley (xml)
#'     )
#'     metadata_oai <- ojsr::get_meta_from_oai(socPsy_articles, verbose = TRUE)
#'
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
      }, warning = function(war) { warning(paste("warning processing ", record_url)) ;
      }, error = function(err) { warning(paste("error processing ", record_url));
      })

      if ( ! "error" %in% names(record[[1]]) ) {
        tryCatch({
          registro <- record[[1]]$GetRecord$record$metadata$dc %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE, row.names = FALSE)
          registro_tidy <- registro %>%
            cbind( input_url = as.character(url[i]) , deparse.level = TRUE) %>%
            # ,conventional_article = process_url$conventional_article[1], deparse.level = TRUE
            # cbind( baseUrl = process_url$baseUrl[1] ) %>%
            gather( key=meta_data_name , value=meta_data_content, -c(input_url))
          registro_tidy$meta_data_name <- sub('\\..*', '', registro_tidy$meta_data_name)
          registro_tidy$input_url <- as.character(registro_tidy$input_url)
        }, warning = function(war) { warning(paste("warning processing ", url[i])) ;
        }, error = function(err) { warning(paste("error processing ", url[i]));
        })
        df <- rbind(df,registro_tidy)
      } else {
        warning("OAI record not found on ", record_url, " for url ", url[i], "\n")
      }
    }

  }

  return(df)
}

