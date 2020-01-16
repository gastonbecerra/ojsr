get_fulltext_links <- function ( url ) {

  # scrap article page to retrieve full-text urls
  # this fn works by scraping a page, using ojs templates classes
  # libs: rvest
  # param: url = character or vector
  # returns: list

  ft <- list() # object to return
  for (i in 1:length(url)) { # loop for vectorized url input
    type <- process_URL(url[i])
    if ( type[1]$type  == "article_abstract") { # proceeds only if in a article view page (although we could proceed also on issue pages...)
      tryCatch({
        webpage <- xml2::read_html(url[i]) # url page content
        xpath <- './/a[contains(@class, "file") or contains(@class, "obj_galley_link")]' # classes from ojs templates (v2.4.8, v3.1.1)
        fullTextLinks <- rvest::html_nodes(webpage, xpath = xpath)
        ft_url <- fullTextLinks %>% rvest::html_attr(name = "href")
        ft_format <-fullTextLinks %>% rvest::html_text() %>% trimws() %>% tolower()
        ft[[i]] <- data.frame( cbind(ft_url , ft_format ) , row.names = NULL , stringsAsFactors = FALSE)
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

get_article_oai <- function ( articleId , oaiUrl ) {

  # looks for an OAI record
  # libs: oai
  # param: articleId = OJS id for article (eg, 9999) / oaiUrl = base URL of OAI (eg, http://imed.pub/ojs/index.php/iam/oai)
  # both params are returned by process_URL
  # returns: list

  x<-oai::id(oaiUrl) # let's create an oai URL for the article
  r2 <- unlist(strsplit(x = x$description, split = "oai:", fixed = TRUE)) # oai id for article .... horrible
  r3 <- unlist(strsplit(x = r2[2], split = "article/", fixed = TRUE))
  oaiId <- paste0("oai:",r3[1],"article/",articleId)
  print(oaiId)
  ArtData <- get_records(ids = oaiId, url = oaiUrl) # gets metadata from oai record
  return( ArtData ) # should be df
}

get_meta_from_page <- function ( url ) {

  # scraps the meta tags from an ojs page
  # libs: rvest, xml2
  # param: url = character or vector
  # returns: list

  ft <- list() # object to return
  for (i in 1:length(url)) { # loop for vectorized url input
    type <- process_URL(url[i])
    if ( type[1]$type  == "article_abstract") { # proceeds only if in a article view page
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

metadatalist_to_df <- function(x, urlList){

  # converts the list from get_meta_from_page to a readable table .... this should be a conversion step in get_meta_from_page
  # libs: purrr
  # param: x = metadata list; urlList vector of urls (parameter of get_meta_from_page)
  # returns: dataframe

  y<-list()
  for (j in 1:length(x)) {
    name<-content<-vector()
    z<-x[[j]] # iterate per article
    for (i in 1:length(z)) { # iterate per metadata
      if ( "name" %in% names(z[[i]]) ) {
        name<-unname(c(name,z[[i]]["name"]))
        if ( "content" %in% names(z[[i]]) ) { content<-unname(c(content,z[[i]]["content"])) } else { content<-c(content,NA) }
      }
    }
    if (!( purrr::is_empty(name) | purrr::is_empty(content) )) {
      y[[j]]<-as.data.frame(cbind(name,content,urlList[j]), stringsAsFactors = FALSE)
    }
  }
  y2<-do.call("rbind", y)
  return(y2)
}
