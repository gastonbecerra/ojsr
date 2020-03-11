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

  # ---------- validation

  if ( !is.character(url) ) { stop("url must be a character string/vector", call. = FALSE) }
  if ( !is.logical(verbose) ) { stop("verbose must be logical", call. = FALSE) }
  if ( !is.character(method) ) { stop("method must be a character string", call. = FALSE) }
  from_method <- paste(from, method, sep = "@")

  # ---------- preparation

  df <- data.frame() # object to collect

  # ---------- iterate through url list

  for (i in 1:length(url)) { # loop for vectorized url input

    if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }

    # ---------- validate the url and warn

    url_type <- ojsr::process_urls(url[i])

    # 2do: aca faltan comprobaciones y ver que hacemos con esto...

    # ---------- read page

    tryCatch({
      webpage <- xml2::read_html(url[i]) # url page content

      # ---------- scrap and process content according to caller / method

      if ( from_method %in% c(
        "get_issue_urls_from_archive@scrap_by_href_convention",
        "get_article_urls_from_issue@scrap_by_href_convention_no_classes",
        "get_article_urls_from_issue@scrap_by_href_convention"
      )) {

        switch (from_method,
          "get_issue_urls_from_archive@scrap_by_href_convention" = xpath <- '//a[contains(@href, "/issue/view/")]',
          "get_article_urls_from_issue@scrap_by_href_convention_no_classes" = xpath <- '//a[contains(@href, "/article/view/") and not(contains(@class, "file")) and not(contains(@class, "pdf"))]',
          "get_article_urls_from_issue@scrap_by_href_convention" = xpath <- '//a[contains(@href, "/article/view/")',
        )

        links <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href") %>% unique()
        if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
        if (length(links)>0) {
          df <- rbind(df, data.frame(cbind( url = url[i] , links = links ), stringsAsFactors = FALSE))
        }

      } else if ( from_method %in% c(
        "get_galley_urls_from_article@scrap_by_class_convention"
      )) {

        switch (from_method,
          "get_galley_urls_from_article@scrap_by_class_convention" = xpath <- './/a[contains(@class, "file") or contains(@class, "obj_galley_link")]',
        )

        links <- rvest::html_nodes(webpage, xpath = xpath)
        if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(links), " elements using criteria ", xpath) }
        if (length(links)>0) {
          links_url <- links %>% xml2::xml_attr("href") %>% unique()
          links_formats <- links %>% rvest::html_text() %>% trimws() %>% tolower()
          links_force <- gsub(pattern = "article/view", replacement = "article/download", x = links_url, fixed = TRUE)
          df <- rbind(df, data.frame(cbind( url = url[i] , links = links_url, format = links_formats , force = links_force ), stringsAsFactors = FALSE))
        }

      } else if (from_method %in% c(
        "get_metadata_from_article@scrap_meta_in_head"
      )) {

        switch (from_method,
          "get_metadata_from_article@scrap_meta_in_head" = xpath <- './/meta'
        )

        meta_data_tags <- rvest::html_nodes(webpage, xpath = xpath)
        if (verbose) { message("scraped ", substr(url[i],1,15), " ... found ", length(meta_data_tags), " elements using criteria ", xpath) }
        if (class(meta_data_tags)=="xml_nodeset"){
          meta_data_tags_list <- xml2::xml_attrs(meta_data_tags)
          meta_data_names <- meta_data_content <- meta_data_scheme <- meta_data_xmllang <- NA
          for (j in 1:length(meta_data_tags_list)) { # iterate per metadata
            if ( "name" %in% names(meta_data_tags_list[[j]]) ) {
              meta_data_names <- unname(c(meta_data_names, meta_data_tags_list[[j]]["name"]))
              if ( "content" %in% names(meta_data_tags_list[[j]]) ) { meta_data_content<-unname(c(meta_data_content,meta_data_tags_list[[j]]["content"])) } else { meta_data_content<-c(meta_data_content, NA) }
              if ( "scheme" %in% names(meta_data_tags_list[[j]]) ) { meta_data_scheme<-unname(c(meta_data_scheme,meta_data_tags_list[[j]]["scheme"])) } else { meta_data_scheme<-c(meta_data_scheme, NA) }
              if ( "xml:lang" %in% names(meta_data_tags_list[[j]]) ) { meta_data_xmllang<-unname(c(meta_data_xmllang,meta_data_tags_list[[j]]["xml:lang"])) } else { meta_data_xmllang<-c(meta_data_xmllang, NA) }
            }
          }
          if (!( purrr::is_empty(meta_data_names) | purrr::is_empty(meta_data_content) )) {
            df <- rbind(df, as.data.frame(cbind(url = url[i],meta_data_names,meta_data_content,meta_data_scheme,meta_data_xmllang), stringsAsFactors = FALSE))
          }

        }

      } else {
        stop("no valid combination of function / method provided: ", from_method)
      }

    }, warning = function(war) { warning(paste("warning processing ", url[i], " : ",war)) ;
    }, error = function(err) { warning(paste("error processing ", url[i], " : ",err));
    })
  }

  # ---------- validate return?

  return(df)
}
