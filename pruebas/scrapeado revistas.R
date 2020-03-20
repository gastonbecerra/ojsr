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

ps_revistas <- revistas %>% left_join( ojsr::process_urls( revistas$url ))
ps_numeros <- ojsr::get_issue_urls_from_archive(url = revistas$url, verbose = TRUE) # listamos los issues o numeros de las revistas
ps_articulos <- ojsr::get_article_urls_from_issue(ps_numeros$issue, verbose = TRUE) # a partir de los issues, listamos los articulos
ps_metadata <- ojsr::get_metadata_from_article(ps_articulos$article, verbose = TRUE) # buscamos la metada por articulo

ps <- list(revistas=ps_revistas, numeros=ps_numeros, articulos=ps_articulos, metadata=ps_metadata) # por comodidad, guardamos todo en un objeto
saveRDS(ps,file=paste0("ps_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas
rm(ps_articulos,ps_metadata,ps_numeros,ps_revistas,revistas)



## ... o arrancamos con todo procesado ------------------


# ps <- readRDS("C:/Users/GASTON/Desktop/r/ojsr/pruebas/ps_200320_0218.rds")


## otras fuentes ------------------


### se pueden scrapear otras fuentes, tipo JSTOR?
### buscar rOpenSci
### buscar doc sobre luhmann y ciencia


## descriptivos de la muestra ------------------

nrow(ps$revistas) # cantidad de revistas
muestra <-
  left_join(
    ps$numeros %>% left_join(ps$revistas) %>% group_by(nombre) %>% summarize(numeros=n()), # cantidad de numeros por revista
    ps$articulos %>% left_join(ps$revistas) %>% group_by(nombre) %>% summarize(articulos=n()) # cantidad de articulos por revista
    ) %>%
  left_join(
    ps$metadata %>% left_join(ps$revistas) %>% group_by(nombre) %>% summarize(metadata=n()) # cantidad de metadata por revista
  )
muestra$metada_articulo <- muestra$metadata / muestra$articulos
muestra



# desvalimiento tiene solo 3 metadata x art... no los esta sirviendo

# vamos a ver si podemos tomarlos por oai
glimpse(ps$revistas)
ps$revistas %>% filter(nombre=="Desvalimiento psicosocial") %>% select(baseUrl) %>% unlist()
articulosDPS <- ps$articulos %>% filter(baseUrl ==  (ps$revistas %>% filter(nombre=="Desvalimiento psicosocial") %>% select(baseUrl) %>% unlist()))
oai_articulosDPS <- ojsr::get_oai_metadata_from_article(articulosDPS$article, verbose = TRUE)
oai_articulosDPS2 <- distinct(oai_articulosDPS)







# 2do: get_oai_metadata_from_article tiene que traer el idioma con el que se sirve el contenido





## bibliometricos ------------------

### cantidad de autores?
### paises?
### idiomas?
### sexo autores?
### instituciones?
### fondos?

## analisis contenido: de keywords ------------------




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

## analisis contenido: de resumenes ------------------

### tm? para que?

## analisis contenido: de contenido completo ------------------


## BAJAR LOS PDF An example of this, using dplyr's filter on format, and download.file:

# url_download <- galleys %>% filter(format=="pdf") %>% select(force) %>% unlist(use.names = FALSE)
# for (i in 1:length(url_download)){
#    download.file( url_download[i], mode = 'wb' , destfile = paste0(getwd(),"/",i,".pdf"))
# }

### unidad de analisis?
### metodologia?
### muestras?
### variables?
### problemas de referencia?
### xxx ?


## analisis contenido: referencias ------------------


### autores mas citados?
### redes de citado?

