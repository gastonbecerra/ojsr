#' Parses URLs according to OJS conventions
#'
#' This function parse a string vector of URLs to check which (if any) OJS view they could be referring to,
#' if there are IDs for articles and/or galleys,
#' and the (expected) OAI address.
#'
#' This works by parsing URLs strings against OJS routing conventions. It does not check anything against the actual pages!
#'
#' (Warning: This will only work on OJS v1,v2 installations with none or min customization. Please refer to vignette.)
#'
#' @param url Character vector.
#' @return A dataframe wich indicates which type of OJS page the url refers to. Please refer to vignette ("Parse OJS urls" section).
#' @examples
#' process_urls(c('https://firstmonday.org/ojs/index.php/fm/article/view/9540',
#'     'http://imed.pub/ojs/index.php/iam/article/view/1650'))
#' @export
process_urls <- function( url ) {
  urldf <- data.frame()  # object to return
  for (i in 1:length(url)) { # in a loop for vectorized input
    type <- NA; articleid <- NA; galleyid <- NA; oai <- NA # default values
    tryCatch({
      ( urlparse <-  urltools::url_parse(url[i]) ) # parse url to check if proper url
      ( urlpath <- urlparse$path ) # path (-domain) of url provided
      ( urldomain <- substr(url[i] , 1 , (regexpr(pattern = urlpath, text = url[i], fixed = TRUE)[1] )-1) )
      ( urlsplit <- unlist(strsplit(x = urlpath, split = "/", fixed = TRUE)) ) # split url by "/"
      ( ojsbase <- match("index.php", urlsplit) ) # "index.php" url segment should indicate OJS installation
      ( oai <- paste0(urldomain , paste(urlsplit[1:(ojsbase+1)], collapse = "/"), "/oai") )
      # urlsplit[ojsbase+1] # OJS journal base folder
      # urlsplit[ojsbase+2] # OJS router/view == article, issue, index, about ....
      # urlsplit[ojsbase+4] # OJS article/issue id
      # urlsplit[ojsbase+5] # OJS galley id
      switch( urlsplit[ojsbase+2],
        "article" = { # if is an article ...
          switch( urlsplit[ojsbase+3],
            "view" = {  # ... viewing article == abstract, download, etc.
              articleid <- as.integer(urlsplit[ojsbase+4]) # article id
              galleyid <- as.integer(urlsplit[ojsbase+5]) # galley id (NA if none)
              if ( is.na(galleyid) && !is.na(articleid) ) { type <- "article_abstract" }
              else if ( !is.na(galleyid) && !is.na(articleid) ) { type <- "article_galley" }
              else { type <- NA } } ,
            "download" = {
              articleid <- as.integer(urlsplit[ojsbase+4]) # article id
              galleyid <- as.integer(urlsplit[ojsbase+5]) # galley id (NA if none)
              if ( is.na(galleyid) && !is.na(articleid) ) { type <- NA } # you can only download a galley
              else if ( !is.na(galleyid) && !is.na(articleid) ) { type <- "download_galley" }
              else { type <- NA } }
          )},
        "issue" = { type<-"issue" },
        "about" = { type<-"about" },
        "index" = { type<-"index" },
        "oai" = { type<-"oai" }
      )
    },
    error = function(cond) {
      message(paste("URL ", i, url[i], "caused error. NA assigned."))
    }
    )
    urldf <- rbind( urldf , data.frame(url=url[i],type, articleid, galleyid, oai, stringsAsFactors = FALSE, row.names = NULL) )
  }
  return(as.data.frame(urldf))
}
