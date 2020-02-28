#' Scraps meta-data from article_abstract OJS pages
#'
#' This functions takes an array of OJS article_abstract URLs and browse them to scrap article metadata from their HTML source code.
#'
#' Before scraping, URL is checked to comply to an article_abstract URL convention.
#' Then, this function retrieves all <meta> elements within the <head> section.
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param verbose Logical.
#' @return A list with every meta-data in the xml_nodeset of the html page.
#'     You can convert this list to a dataframe using ojsr::metadatalist_to_df
#' @examples
#' get_metadata_from_page(c('https://firstmonday.org/ojs/index.php/fm/article/view/9540',
#'     'http://imed.pub/ojs/index.php/iam/article/view/1650'))
#' @export
get_metadata_from_page <- function ( url , verbose = FALSE) {
  ft <- list() # object to return
  for (i in 1:length(url)) { # loop for vectorized url input
    if (verbose) { message(paste("trying URL ", i, url[i])) }
    url_type <- ojsr::process_urls(url[i])
    if ( url_type[1,"type"]  == "article_abstract") { # proceeds only if in a article view page
      tryCatch({
        webpage <- xml2::read_html(url[i]) # url page content
        xpath <- './/meta'
        meta_data_tags <- rvest::html_nodes(webpage, xpath = xpath)
        if (class(meta_data_tags)=="xml_nodeset"){
          ft[[i]] <- xml2::xml_attrs(meta_data_tags)
        } else {
          warning(paste("non-xml (meta-tag set) in element",i, url[i]))
          ft[[i]] <- NA
        }
      }, warning = function(war) { print(paste("WARNING in element ", i, " : ",war)) ; ft[[i]] <- NA ;
      }, error = function(err) { print(paste("ERROR in element ", i, " : ",err)); ft[[i]] <- NA ;
      })
    } else {
      warning(paste("non-article url in element",i, url[i]))
      ft[[i]] <- NA
    }
  }
  return(ft)
}




#' Converts a list of scrapped meta-data to a dataframe
#'
#' @param metadataList A list generated with ojsr::get_metadata_from_page
#' @param url Character vector (same used in ojsr::get_metadata_from_page).
#' @return A dataframe
#' @examples
#' article_URL <- c('https://firstmonday.org/ojs/index.php/fm/article/view/9540',
#'     'http://imed.pub/ojs/index.php/iam/article/view/1650')
#' metadata_articles <- get_metadata_from_page( article_URL )
#' metadata_articles_df <- metadata_list_to_df( metadata_articles , url )
#' @export
metadata_list_to_df <- function( metadataList, url ){
  y<-list()
  for (j in 1:length(metadataList)) {
    name<-content<-scheme<-xmllang<-vector()
    z<-metadataList[[j]] # iterate per article
    for (i in 1:length(z)) { # iterate per metadata
      if ( "name" %in% names(z[[i]]) ) {
        name<-unname(c(name,z[[i]]["name"]))
        if ( "content" %in% names(z[[i]]) ) { content<-unname(c(content,z[[i]]["content"])) } else { content<-c(content,NA) }
        if ( "scheme" %in% names(z[[i]]) ) { scheme<-unname(c(scheme,z[[i]]["scheme"])) } else { scheme<-c(scheme,NA) }
        if ( "xml:lang" %in% names(z[[i]]) ) { xmllang<-unname(c(xmllang,z[[i]]["xml:lang"])) } else { xmllang<-c(xmllang,NA) }
      }
    }
    if (!( purrr::is_empty(name) | purrr::is_empty(content) )) {
      y[[j]]<-as.data.frame(cbind(name,content,scheme,xmllang,url[j]), stringsAsFactors = FALSE)
    }
  }
  y2<-do.call("rbind", y)
  names(y2)[5] <- "url"
  return(y2)
}



#' Scraps the galley(s) url(s) from article_abstract OJS pages
#'
#' This functions takes an array of OJS article_abstract URLs and scrap the galley(s) url from their links.
#'
#' "Galleys" are the public (and usually full-content) copies of the articles. Usual formats for galleys are pdf, xml, html, rtf, epub.
#' These may also include supplementary materials (dataset, charts, etc.).
#'
#' Before scraping, URL is checked to comply to an article_abstract URL convention.
#' Then, this function retrieves all the hrefs from links with usual OJS classes (e.g., .obj_galley_link)
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param verbose Logical.
#' @return A dataframe with the urls of the galleys linked from the OJS article page. (Please refer to vignette for structure)
#' @examples
#' get_galley_urls(c('https://firstmonday.org/ojs/index.php/fm/article/view/9540',
#'     'http://imed.pub/ojs/index.php/iam/article/view/1650'))
#' @export
get_galley_urls <- function ( url , verbose = FALSE) {
  galleyList <- list() # object to collect
  galleyDataframe <- data.frame() # object to return
  for (i in 1:length(url)) { # loop for vectorized url input
    if (verbose) { message(paste("trying URL ", i, url[i])) }
    url_type <- ojsr::process_urls(url[i])
    if ( url_type$type[1]  == "article_abstract") { # proceeds only if in a article view page
      tryCatch({
        webpage <- xml2::read_html(url[i]) # url page content
        xpath <- './/a[contains(@class, "file") or contains(@class, "obj_galley_link")]' # classes from ojs templates (v2.4.8, v3.1.1)
        galleyLinks <- rvest::html_nodes(webpage, xpath = xpath)
        galley_url <- galleyLinks %>% rvest::html_attr(name = "href")
        galley_format <-galleyLinks %>% rvest::html_text() %>% trimws() %>% tolower()
        galley_force_download <- gsub(pattern = "article/view", replacement = "article/download", x = galley_url, fixed = TRUE)
        galleyList[[i]] <- data.frame( cbind(galley_url , galley_force_download , galley_format, url[i] ) , row.names = NULL , stringsAsFactors = FALSE)
      }, warning = function(war) { print(paste("WARNING in element ", i, " : ",war)) ; galleyList[[i]] <- NA ;
      }, error = function(err) { print(paste("ERROR in element ", i, " : ",err)); galleyList[[i]] <- NA ;
      })
    } else {
      warning(paste("non-article url in element",i, url[i]))
      galleyList[[i]] <- NA
    }
    galleyDataframe <- rbind(galleyDataframe, galleyList[[i]])
  }
  names(galleyDataframe)[4] <- "url"
  return(galleyDataframe)
}




#' Scraps an OJS issue page for articles urls
#'
#' This functions takes an array of OJS issues URLs and scrap the articles urls from their links.
#'
#' Before scraping, URL is checked to comply to an OJS issue URL convention.
#' Then, this function retrieves all the hrefs from links with usual OJS classes (e.g., a.tocTitle)
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @param verbose Logical.
#' @return A dataframe with the urls of the articles linked from the OJS issue page.
#' @examples
#' get_articles_urls_from_issue(c('https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/view/319/showToc',
#'     'https://firstmonday.org/ojs/index.php/fm/issue/view/634'))
#' @export
get_articles_urls_from_issue <- function ( url , verbose = FALSE) {
  galleyList <- list() # object to collect
  galleyDataframe <- data.frame() # object to collect
  for (i in 1:length(url)) { # loop for vectorized url input
    if (verbose) { message(paste("trying URL ", i, url[i])) }
    url_type <- ojsr::process_urls(url[i])
    if ( url_type$type[1] == "issue") { # proceeds only if in an issue
      tryCatch({
        webpage <- xml2::read_html(url[i]) # url page content
        xpath <- '//a[contains(@href, "/article/view/") and not(contains(@class, "file"))]'
        meta_data_tags <- rvest::html_nodes(webpage, xpath = xpath)
        galleyLinks <- rvest::html_nodes(webpage, xpath = xpath) %>% xml2::xml_attr("href")
        galleyDataframe <- rbind( galleyDataframe , data.frame(cbind(galleyLinks, url[i] ), stringsAsFactors = FALSE) )
      }, warning = function(war) { print(paste("WARNING in element ", i, " : ",war)) ; galleyList[[i]] <- NA ;
      }, error = function(err) { print(paste("ERROR in element ", i, " : ",err)); galleyList[[i]] <- NA ;
      })
    } else {
      warning(paste("non-article url in element",i, url[i]))
      #galleyList[[i]] <- NA
      #2do: iria vacio?
    }
  }
  names(galleyDataframe)[2] <- "url"
  return(galleyDataframe)
}

