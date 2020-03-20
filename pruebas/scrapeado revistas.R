setwd("C:/Users/GASTON/Desktop/r/ojsr/pruebas")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(ojsr)


## listado de revistas ------------------


url <- c(
  'http://sportsem.uv.es/j_sports_and_em/index.php/rips/',
  'https://dspace.palermo.edu/ojs/index.php/psicodebate',
  'https://publicacionescientificas.uces.edu.ar/index.php/desvapsico/',
  'https://revistapsicologia.uchile.cl/index.php/RDP/',
  'https://revistas.ucn.cl/index.php/saludysociedad',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial'
)
nombre <- c(
  'Revista Internacional de Psicología Social', # (UNLZ, Argentina)
  'Psicodebate', # (UP, Argentina)
  'Desvalimiento psicosocial', # (UCES, Argentina)
  'Revista de Psicología', # (UChile, Chile)
  'Salud y Sociedad', # (Universidad Católica del Norte, Chile)
  'PSocial' # (UBA, Argentina)
)
revistas <- as.data.frame(cbind(url=url,nombre=nombre),stringsAsFactors = FALSE)
rm(url,nombre)


## recuperacion de datos con ojsr ------------------

ps_numeros <- ojsr::get_issue_urls_from_archive(url = revistas$url, verbose = TRUE) # listamos los issues o numeros de las revistas
ps_numeros %>% inner_join(revistas, by=c("archive"="url")) %>% group_by(nombre) %>% tally() # cantidad de numeros por revista

ps_articulos <- ojsr::get_article_urls_from_issue(ps_numeros$issue, verbose = TRUE) # a partir de los issues, listamos los articulos
ps_articulos %>% inner_join(ps_numeros, by="issue") %>% inner_join(revistas, by=c("archive"="url")) %>% group_by(nombre) %>% tally() # cantidad de articulos por revista

ps_metadata <- ojsr::get_metadata_from_article(ps_articulos$article, verbose = TRUE) # buscamos la metada por articulo

ps <- list(revistas=revistas, numeros=ps_numeros, articulos=ps_articulos, metadata=ps_metadata) # por comodidad, guardamos todo en un objeto
saveRDS(ps,file=paste0("ps_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas
rm(ps_articulos,ps_metadata,ps_numeros)


## ... o arrancamos con todo procesado ------------------


ps <- readRDS("C:/Users/GASTON/Desktop/r/ojsr/pruebas/ps_200319_1055.rds")


## descriptivos de la muestra ------------------


glimpse(ps$revistas)

nrow(ps$revistas) # cantidad de revistas
ps$numeros %>% inner_join(ps$revistas, by=c("url"="assume_archive")) %>% group_by(nombre) %>% tally() # cantidad de numeros por revista
ps$articulos %>% inner_join(ps$numeros, by=c("url"="links")) %>% inner_join(ps$revistas, by=c("url.y"="assume_archive")) %>% group_by(nombre) %>% tally() # cantidad de articulos por revista


ojsr::get_article_urls_from_issue( url = ps$numeros %>% filter(url=='https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/issue/archive') %>% select(links) %>% unlist() , verbose = TRUE)


view(ps$articulos)

nrow(ps$articulos)

options(max.print=100)
glimpse(ps$articulos)

glimpse(metadata2)
sort(table(metadata2$name))
keywords <- metadata2 %>% filter(name=="citation_keywords") %>% select(content)
keywords$content <- gsub("." , "$", keywords$content, fixed = TRUE)
keywords$content <- gsub("," , "$", keywords$content, fixed = TRUE)
keywords$content <- gsub(";" , "$", keywords$content, fixed = TRUE)
keywords$content <- gsub("–" , "$", keywords$content, fixed = TRUE)
keywords$content <- gsub("  " , "$", keywords$content, fixed = TRUE)
keywords$content <- gsub("$" , " $ ", keywords$content, fixed = TRUE)
keywords$content <- tolower(keywords$content)
keywords$content
keywords2 <- strsplit(x = keywords$content, split = "$", fixed = TRUE)
keywords2 <- trimws(unlist(keywords2))
table(keywords2)
wordcloud::wordcloud(keywords2, min.freq = 3)




## BAJAR LOS PDF An example of this, using dplyr's filter on format, and download.file:

# url_download <- galleys %>% filter(format=="pdf") %>% select(force) %>% unlist(use.names = FALSE)
# for (i in 1:length(url_download)){
#    download.file( url_download[i], mode = 'wb' , destfile = paste0(getwd(),"/",i,".pdf"))
# }



