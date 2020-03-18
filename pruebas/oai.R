
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






url_articulos <- c( 'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/download/43/54',
  'https://firstmonday.org/ojs/index.php/fm/article/view/9540',
  'http://imed.pub/ojs/index.php/iam/article/view/1650')

type_articulos <- ojsr::process_urls(url_articulos)

i = 2

# 1. ir al identify

paste0( type_articulos$assume_oai[i] , "/?verb=Identify" )

identify <- xml2::read_xml( paste0( type_articulos$assume_oai[i] , "/?verb=Identify" )) %>% xml2::as_list()
identifier <- identify[[1]]$Identify$description$`oai-identifier` %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE, row.names = FALSE)

# 2. armar el identifier del registro

baseIdentifier <- paste0(
  identifier$scheme,
  identifier$delimiter,
  identifier$repositoryIdentifier,
  identifier$delimiter,
  "article/",
  type_articulos$articleId[i]
  )
baseIdentifier

# 3. armar la url para ir al registro

record_url <- paste0(
  type_articulos$assume_oai[i],
  "/?verb=GetRecord&metadataPrefix=oai_dc&identifier=",
  baseIdentifier
)
record_url

# 4. ir a la url y tomar el registro

record <- xml2::read_xml(record_url) %>% xml2::as_list()
if ( ! "error" %in% names(record[[1]]) ) {
  registro <- record[[1]]$GetRecord$record$metadata$dc %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE, row.names = FALSE)
} else {
  message("no hay registro")
}

# 5. pasar a tidy


registro_total <- data.frame()
glimpse(registro)
registro_tidy <- registro %>% cbind(url = type_articulos$url[i], deparse.level = TRUE) %>% gather( key=meta , value=value, -url)
registro_total <- rbind(registro_total,registro_tidy)


# usar como id el articulo url??



url_sample <- c( 'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/download/43/54',
)

metadata <- ojsr::get_oai_metadata_from_article(url = url_sample)











#### -----------------------------------------------------------------------------------------------------






# se puede tomar todos los registros oai de una?

oai::list_identifiers(url = type_articulos$assume_oai[4])


url_articulos <- c( 'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/2903',
  'http://fundacionmenteclara.org.ar/revista/index.php/RCA/article/download/43/54',
  'https://firstmonday.org/ojs/index.php/fm/article/view/9540',
  'http://imed.pub/ojs/index.php/iam/article/view/1650')

type_articulos <- ojsr::process_urls(url_articulos)


type_articulos$assume_oai[1]


oai_identifiers <- oai::list_identifiers(url = type_articulos$assume_oai[1], set = "psicologiasocial:ART")
oai_identifiers

oai_records <- oai::get_records( ids =  oai_identifiers$identifier[1])
oai_identifiers$identifier

oai_record_ojsr <- ojsr::get_oai_metadata_from_article(url = "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/1234", verbose = T)





#---------------------

url <- c(
  "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/1234",
  "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/1234"
)

i=1


# if (verbose) { message("trying url ", i, "/" , length(url), " ", url[i]) }
#
# if (verbose) { message("pre-processing url ", url[i]) }
process_url <- ojsr::process_urls(url[i])
oai_base_url <- process_url$assume_oai[1]
article_id <- process_url$articleId[1]
identify_url <- paste0(oai_base_url, "/?verb=Identify")
# if (verbose) { message("identifying on ", identify_url) }

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
  # if (verbose) { message("looking for record on ", record_url) }


} else {
  warning("OAI identity records could not be found on ", identify_url)
}



tryCatch({
  record <- xml2::read_xml(record_url) %>% xml2::as_list()
}, warning = function(war) { warning(paste("warning processing ", record_url)) ;
}, error = function(err) { warning(paste("error processing ", record_url));
})

if ( ! "error" %in% names(record[[1]]) ) {
} else {
  warning("OAI record not found on ", record_url, " for url ", url[i])
}

registro <- record[[1]]$GetRecord$record$metadata$dc %>% unlist() %>% t() %>% data.frame(stringsAsFactors = FALSE, row.names = FALSE )
registro_tidy <- registro %>% cbind(url = url[i], deparse.level = TRUE) %>% gather( key=meta_data_name , value=meta_data_content , -url)
registro_tidy$meta_data_name <- sub('\\..*', '', registro_tidy$meta_data_name)
registro_tidy$meta_data_name






#### aca estoy jodiendo los nombres xq se repiten y mete .1 ***********************************

df <- rbind(df,registro_tidy)


data.frame(c(aaa="aaa",bbb="bbb"),)


names(registro)


x<-record[[1]]$GetRecord$record$metadata$dc %>% unlist() %>% t()
names(x)
str(x)
