#' Scraps OJS article pages for galleys urls
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
get_metadata_from_article <- function ( url , method = "scrap_meta_in_head", verbose = FALSE ) {
  df <- ojrs_scrap(url = url, verbose = verbose, method = method, from = "get_metadata_from_article")
  return(df)
}

#' Scraps OJS article pages for galleys urls
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
get_galley_urls_from_article <- function ( url , method = "scrap_by_class_convention", verbose = FALSE ) {
  df <- ojrs_scrap(url = url, verbose = verbose, method = method, from = "get_galley_urls_from_article")
  return(df)
}

#' Scraps an OJS issue page for articles urls
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param method String. Available methods: scrap_by_href_convention_no_classes, scrap_by_href_convention_no
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
#'
#' @export
#'
get_article_urls_from_issue <- function ( url , method = "scrap_by_href_convention_no_classes", verbose = FALSE ) {
  df <- ojrs_scrap(url = url, verbose = verbose, method = method, from = "get_article_urls_from_issue")
  return(df)
}

#' Scraps an OJS archive page for issues urls
#'
#' This functions xxx
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param method String. Available methods: scrap_by_href_convention
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#'
#'     xxx
#'
#' @export
get_issue_urls_from_archive <- function ( url , method = "scrap_by_href_convention", verbose = FALSE ) {
  df <- ojrs_scrap(url = url, verbose = verbose, method = method, from = "get_issue_urls_from_archive")
  return(df)
}


ojrs_scrap <- function (url , verbose , method, from) {

  if ( !is.character(url) ) { stop("url must be a character string/vector", call. = FALSE) }
  if ( !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }
  if ( !is.character(method) ) { stop("method must be a character string", call. = FALSE) }

  from_method <- paste(from, method, sep = "@")

  df <- data.frame() # object to collect

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }

      tryCatch({

        webpage <- xml2::read_html(url[i]) # url page content

        if( grepl( x = method, pattern = "scrap_by_href"  )  ) { # crawling

          switch ( from_method ,
            "get_issue_urls_from_archive@scrap_by_href_convention" = xpath <- '//a[contains(@href, "/issue/view/")]',
            "get_article_urls_from_issue@scrap_by_href_convention_no_classes" = xpath <- '//a[contains(@href, "/article/view/") and not(contains(@class, "file")) and not(contains(@class, "pdf"))]',
            "get_article_urls_from_issue@scrap_by_href_convention" = xpath <- '//a[contains(@href, "/article/view/")',
          )

          links <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href") %>% unique()
          if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
          if (length(links)>0) {
            df <- rbind(df, data.frame(cbind( url = url[i] , links = links ), stringsAsFactors = FALSE))
          }

        } else if ( from_method == "get_galley_urls_from_article@scrap_by_class_convention" ) {

          xpath <- './/a[contains(@class, "file") or contains(@class, "obj_galley_link")]'
          links <- rvest::html_nodes(webpage, xpath = xpath)
          if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
          if (length(links)>0) {
            links_url <- links %>% xml2::xml_attr("href") %>% unique()
            links_formats <- links %>% rvest::html_text() %>% trimws()
            links_force <- gsub(pattern = "article/view", replacement = "article/download", x = links_url, fixed = TRUE)
            df <- rbind(df, data.frame(cbind( url = url[i] , links = links_url, format = links_formats , force = links_force ), stringsAsFactors = FALSE))
          }

        } else if ( from_method == "get_metadata_from_article@scrap_meta_in_head" ) {

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
              df <- rbind(df, as.data.frame(cbind(url = url[i],meta_data_name,meta_data_content,meta_data_scheme,meta_data_xmllang), stringsAsFactors = FALSE))
            }
          }

        } else {
          stop("no valid combination of function / method provided: ", from_method)
        }

      }, warning = function(war) { warning(paste("warning processing ", url[i], " : ",war)) ;
      }, error = function(err) { warning(paste("error processing ", url[i], " : ",err));
      })
  }

  # closeAllConnections()
  return(df)
}






#' Get OAI metadata for an article
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
get_oai_metadata_from_article <- function ( url , verbose = FALSE ) {

  if ( !is.character(url) ) { stop("url must be a character string/vector", call. = FALSE) }
  if ( !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }

  # oaiBase == en tamaño que url, o 1
  # oaiIdentifier == en tamaño que url, o 1
  # article_id == en tamaño que url, o 1

  oai_base_url = ""
  oai_identifier = ""
  article_id = NA

  df <- data.frame() # object to collect

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }

    if (verbose) { message("pre-processing url ", url[i]) }
    process_url <- ojsr::process_urls(url[i])
    oai_base_url <- process_url$assume_oai[1]
    article_id <- process_url$articleId[1]
    identify_url <- paste0(oai_base_url, "/?verb=Identify")
    if (verbose) { message("identifying on ", identify_url) }

    tryCatch({
      identify_xml <- xml2::read_xml( identify_url )
      identify_list <- identify_xml %>% xml2::as_list()
    }, warning = function(war) { warning(paste("warning processing ", identify_url)) ;
    }, error = function(err) { warning(paste("error processing ", identify_url));
    })

    if ( ! "error" %in% names(identify_list[[1]]) ) {
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
          registro_tidy <- registro %>% cbind(url = url[i], deparse.level = TRUE) %>% gather( key=meta_data_name , value=meta_data_content, -url)
          registro_tidy$meta_data_name <- sub('\\..*', '', registro_tidy$meta_data_name)
        }, warning = function(war) { warning(paste("warning processing ", url[i])) ;
        }, error = function(err) { warning(paste("error processing ", url[i]));
        })
        df <- rbind(df,registro_tidy)
      } else {
        warning("OAI record not found on ", record_url, " for url ", url[i])
      }

    } else {
      warning("OAI identity records could not be found on ", identify_url)
    }

  }

  return(df)
}
