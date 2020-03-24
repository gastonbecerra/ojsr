#' Scraps an OJS archive page for issues urls
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_href_convention
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
#'
#' @export
get_issue_url <- function ( input_url , use_conventional_url = TRUE, verbose = FALSE, method = "scrap_by_href_convention" ) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_issue_url")
  return(df)
}





#' Scraps an OJS issue page for articles urls
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_href_convention_no_classes, scrap_by_href_convention_no
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
#'
#' @export
#'
get_article_url <- function ( input_url , use_conventional_url = TRUE, verbose = FALSE, method = "scrap_by_href_convention_no_classes" ) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_article_url")
  return(df)
}





#' Scraps OJS article pages for galleys urls
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_class_convention
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
#'
#' @export
#'
get_galley_url <- function ( input_url , use_conventional_url = TRUE, verbose = FALSE, method =  "scrap_by_class_convention" ) {
  df <- ojrs_scrap(url_input = input_url, use_conventional_url = use_conventional_url, verbose = verbose, method = method, from = "get_galley_url")
  return(df)
}




#' xxxx
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param use_conventional_url Logical. Should ojsr parse the urls given to form the conventional url to scrap?
#' @param method String. Available methods: scrap_by_href_convention
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
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




#' xxx
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param method String. Available methods: scrap_by_class_convention
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
#'
#' @export
#'
get_meta_from_html <- function ( input_url , use_conventional_url = TRUE, verbose = FALSE, method =  "scrap_meta_in_head") {
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
    tryCatch({
      url_parsed <- ojsr::process_urls(url_input)
      url <- switch (from, # conventional url to be scraped
        get_issue_url = url_parsed$conventional_archive,
        get_article_url = url_parsed$conventional_issue,
        get_galley_url = url_parsed$conventional_article,
        get_meta_from_html = url_parsed$conventional_article,
        get_search_url = paste0(url_parsed$conventional_search, search_criteria)
      )
      if (verbose) { message("urls parsed; using conventional format for urls instead of input") }
      }, warning = function(war) { warning(paste("warning parsing input urls: ",war)) ;
      }, error = function(err) { warning(paste("error parsing input urls: ",err));
      })
  } else {
    if (verbose) { message("using input urls") }
    url <- url_input
  }

  # loop for vectorized url input

  for (i in 1:length(url)) {

    if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }

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
              "scrap_by_href_convention_no_classes" = '//a[contains(@href, "/article/view/") and not(contains(@class, "file")) and not(contains(@class, "pdf"))]'
            )
          }
        )
        output_names <- c('input_url', 'output_url')
        links <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href") %>% unique()
        if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
        if (length(links)>0) {
          df <- rbind(df, data.frame(cbind( url_input[i], links ), stringsAsFactors = FALSE))
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
          df <- rbind(df, data.frame(cbind( url_input[i] , links_url, links_formats , links_force ), stringsAsFactors = FALSE))
        }

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
            df <- rbind(df, data.frame(cbind( url_input[i], url[i] ), stringsAsFactors = FALSE))
          }
        }

      } else {
        stop("no valid combination of function / method provided: ", from_method, call. = FALSE)
      }
    }
  }
  # closeAllConnections()
  if ( from_method != "get_meta_from_html@scrap_meta_in_head" ) { names(df) <- output_names }
  return(df)
}






#' Get OAI metadata for an article
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
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

