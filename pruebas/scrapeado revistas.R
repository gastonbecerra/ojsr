setwd("C:/Users/GASTON/Desktop/r/ojsr/pruebas")
library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(ojsr)


## recuperacion de datos ------------------


### listado de revistas
url <- c(
  'https://dspace.palermo.edu/ojs/index.php/psicodebate',
  'https://publicacionescientificas.uces.edu.ar/index.php/desvapsico/',
  'https://revistapsicologia.uchile.cl/index.php/RDP/',
  'https://revistas.ucn.cl/index.php/saludysociedad',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial',
  'https://revistas.ucc.edu.co/index.php/pe/about',
  'https://revistas.javeriana.edu.co/index.php/revPsycho/index/',
  'https://revista.psico.edu.uy/index.php/revpsicologia'
)
nombre <- c(
  'Psicodebate', # (UP, Argentina)
  'Desvalimiento psicosocial', # (UCES, Argentina)
  'Revista de Psicología', # (UChile, Chile)
  'Salud y Sociedad', # (Universidad Católica del Norte, Chile)
  'PSocial', # (UBA, Argentina)  'Psocial', # (UBA, Argentina)
  'Pensando Psicología', # 'Pensando psicología', # (Universidad Cooperativa de Colombia, Colombia)
  'Universitas Psychologica', # (Pontificia Universidad Javeriana, Colombia)
  'Psicología, Conocimiento y Sociedad' # (Universidad de la República, Uruguay)
  )
revistas <- as.data.frame(cbind(url=url,nombre=nombre),stringsAsFactors = FALSE)
rm(url,nombre)

### descartadas
# 'http://revistas.innovacionumh.es/index.php?journal=rpsa' ,   'http://sportsem.uv.es/j_sports_and_em/index.php/rips/',


### recuperamos con ojsr
ps_revistas <- revistas %>% left_join( ojsr::process_urls( revistas$url ))
ps_numeros <- ojsr::get_issue_urls_from_archive(url = ps_revistas$url, verbose = TRUE) # listamos los issues o numeros de las revistas
ps_articulos <- ojsr::get_article_urls_from_issue(ps_numeros$issue, verbose = TRUE) # a partir de los issues, listamos los articulos
ps_metadata <- ojsr::get_metadata_from_article(ps_articulos$article, verbose = TRUE) # buscamos la metada por articulo

ps <- list(revistas=ps_revistas, numeros=ps_numeros, articulos=ps_articulos, metadata=ps_metadata) # por comodidad, guardamos todo en un objeto
saveRDS(ps,file=paste0("ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas
rm(ps_articulos,ps_metadata,ps_numeros,ps_revistas,revistas)

### 2do: revPsycho esta mostrando galeradas en article?

### 2do: otras fuentes

### se pueden scrapear otras fuentes, tipo JSTOR?
### buscar rOpenSci
### buscar doc sobre luhmann y ciencia
### buscar en oai
### buscar en doaj

# https://www.tandfonline.com/action/journalInformation?show=aimsScope&journalCode=rrps20
# ojs search:
# https://atheneadigital.net/search?subject=Psicolog%C3%ADa%20Social



## limpiamos, reemplazamos y normalizamos algunos ------------------



# desvalimiento tiene solo 3 metadata x art... vamos a reemplazarlos por oai
ps$revistas %>% filter(nombre=="Desvalimiento psicosocial") %>% select(baseUrl) %>% unlist()
articulosDPS <- ps$articulos %>% filter(baseUrl ==  (ps$revistas %>% filter(nombre=="Desvalimiento psicosocial") %>% select(baseUrl) %>% unlist()))
oai_articulosDPS <- ojsr::get_oai_metadata_from_article(articulosDPS$article, verbose = TRUE)
oai_articulosDPS_viejo <- ps$metadata %>% filter(baseUrl == "https://publicacionescientificas.uces.edu.ar/index.php/desvapsico")
ps$metadata <- ps$metadata %>% anti_join( oai_articulosDPS_viejo ) # los borro ...
ps$metadata <- ps$metadata %>% rbind(cbind(oai_articulosDPS,meta_data_scheme=NA,meta_data_xmllang=NA)) # ... y ahora los vuelvo a agregar

# hay que normalizar metadata?
glimpse(ps$metadata)
ps$metadata %>% filter( !grepl(x = meta_data_name, pattern = ":", fixed = TRUE) ) %>% group_by(meta_data_name, baseUrl) %>% tally() %>% ggplot(aes(y=meta_data_name,x=baseUrl,fill=n)) + geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#mapa_metadatos <- ps$metadata %>% group_by(meta_data_name, baseUrl) %>% tally() %>% pivot_wider(names_from = baseUrl, values_from = n) # para ver quien / como se usa cada meta

# normalizamos algunos metadata
ps$metadata %>% filter(baseUrl == "https://publicacionescientificas.uces.edu.ar/index.php/desvapsico") %>% group_by(meta_data_name) %>% tally() # e.g., metadata raros de desvapsico
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "creator", "DC.Creator.PersonalName", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "date", "citation_date", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "title", "citation_title", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "description", "DC.Description", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "language", "DC.Language", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "rights", "DC.Rights", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "source", "DC.Source", meta_data_name) )
ps$metadata <- ps$metadata %>% mutate(meta_data_name = ifelse(meta_data_name == "type", "DC.Type", meta_data_name) )
ps$metadata %>% filter(baseUrl == "https://publicacionescientificas.uces.edu.ar/index.php/desvapsico") %>% group_by(meta_data_name) %>% tally()

# limpiamos
rm(oai_articulosDPS, oai_articulosDPS_viejo,articulosDPS)



## ... o arrancamos con todo procesado ------------------



saveRDS(ps,file=paste0("ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas
ps <- readRDS("ps_r8_200321_2104.rds")



## descriptivos de la muestra ------------------



# cantidad de numeros, articulos, y metadatas
nrow(ps$revistas) # cantidad de revistas
muestra <-
  left_join(
    ps$numeros %>% left_join(ps$revistas) %>% group_by(nombre) %>% summarize(numeros=n()), # cantidad de numeros por revista
    ps$articulos %>% left_join(ps$revistas) %>% group_by(nombre) %>% summarize(articulos=n()) # cantidad de articulos por revista
    ) %>%
  left_join(
    ps$metadata %>% left_join(ps$revistas) %>% group_by(nombre) %>% summarize(metadata=n()) # cantidad de metadata por revista
  ) %>%
  transform(metadata_articulo = metadata / articulos) %>%
  left_join(ps$revistas %>% select(nombre,baseUrl))
muestra



## subtotales meta-datos ------------------



### cantidad de articulos?
sum(muestra$articulos)

### cantidad de articulos x año?
ps$metadata %>% filter(meta_data_name=="citation_date") %>% select(fecha = meta_data_content) %>%
  mutate(y=lubridate::ymd(fecha)) %>% mutate(y2=lubridate::year(y)) %>% group_by(y2) %>% tally() %>%
  ggplot(aes(x=y2,y=n)) + geom_line()

### cantidad de autores? # 2do: hay que elegir indicador
ps$metadata %>% filter(meta_data_name=="DC.Creator.PersonalName") %>% group_by(x = meta_data_content) %>% tally(sort=TRUE) # articulos x autor
ps$metadata %>% filter(meta_data_name=="DC.Creator.PersonalName") %>% distinct(x = meta_data_content) %>% count() # cantidad de autores
ps$metadata %>% filter(meta_data_name=="citation_author") %>% distinct(x = meta_data_content) %>% count() # cantidad de autores

### idiomas? # 2do: hay que normalizar idiomas y elegir indicador
ps$metadata %>% filter(meta_data_name=="citation_language") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
ps$metadata %>% filter(meta_data_name=="DC.Language") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)

### instituciones? # 2do: hay que normalizar
ps$metadata %>% filter(meta_data_name=="citation_author_institution") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)

### formatos # 2do: hay que normalizar
ps$metadata %>% filter(meta_data_name=="DC.Format") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)

### version OJS
ps$metadata %>% filter(meta_data_name=="generator") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)

### sexo autores? # 2do: libreria para parsear nombres



## analisis contenido: de keywords ------------------



limpiar <- function (x) {
  y <- x
  y <- tolower(y)
  y <- trimws(y)
  y <- gsub("á","a",y,fixed=TRUE)
  y <- gsub("é","e",y,fixed=TRUE)
  y <- gsub("í","i",y,fixed=TRUE)
  y <- gsub("ó","o",y,fixed=TRUE)
  y <- gsub("ú","u",y,fixed=TRUE)
  return (y)
} # funcion para normalizar caracteres
separadores <- function(x) {
  y <- x
  y <- gsub("." , "$", y, fixed = TRUE)
  y <- gsub("," , "$", y, fixed = TRUE)
  y <- gsub(";" , "$", y, fixed = TRUE)
  y <- gsub("– " , "$", y, fixed = TRUE)
  y <- gsub("  " , "$", y, fixed = TRUE)
  y <- gsub("$" , " $ ", y, fixed = TRUE)
  return(y)
} # funcion para ayudar a corregir los keywords todo pegados de psocial

view(ps$metadata)

ps$metadata %>% filter(meta_data_name=="citation_keywords") %>% group_by(baseUrl) %>% tally()
ps$metadata %>% filter(meta_data_name=="DC.Subject") %>% group_by(baseUrl) %>% tally()

### totales de keywords # 2do: hay que normalizar
ps$metadata %>% filter(meta_data_name=="citation_keywords") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)
ps$metadata %>% filter(meta_data_name=="DC.Subject") %>% select(x = meta_data_content) %>% group_by(x) %>% tally(sort=TRUE)

### ajusto los de psocial que estan en cualquiera
keywords_psocial <- ps$metadata %>% filter(baseUrl == "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial", meta_data_name=="citation_keywords" ) # e.g., metadata keywords juntos en psocial
keywords_psocial2 <- keywords_psocial %>% mutate(keywords=limpiar(separadores(meta_data_content)))
keywords_psocial2$id <- seq_along(1:nrow(keywords_psocial))
kps <- as.data.frame(str_split_fixed(keywords_psocial2$keywords, pattern = fixed("$"), n=6),stringsAsFactors = FALSE)
kps$id <- keywords_psocial2$id
kps <- pivot_longer(data = kps, cols = -id, names_to = "keyword", values_to = "keywords")
kps2 <- kps %>% filter(keywords!="") %>% select(id,keywords) %>% mutate(keywords=limpiar(keywords))
keywords_psocial2 <- keywords_psocial2 %>% select(article, baseUrl, meta_data_name, meta_data_scheme, meta_data_xmllang, id) %>% left_join(kps2, by="id") %>%
  select(article, baseUrl, meta_data_name, meta_data_content = keywords, meta_data_scheme, meta_data_xmllang)
rm(kps,kps2,keywords_psocial,keywords_psocial2)
ps$metadata <- ps$metadata %>% anti_join(keywords_psocial) # borro los keywords viejos
ps$metadata <- ps$metadata %>% rbind(keywords_psocial2) # sumo los keywords corregidos

saveRDS(ps,file=paste0("ps_","r",nrow(ps$revistas),"_",format(Sys.time(), "%y%m%d_%H%M"),".rds")) # guardamos en disco, para evitar este paso de ahora en mas

### armo una tabla de keywords

keywords <- ps$metadata %>% filter(meta_data_name=="citation_keywords", meta_data_xmllang=="es", trimws(meta_data_content)!="") %>% select(keywords = meta_data_content, baseUrl, article) %>% mutate(keywords=limpiar(keywords))
keywords <- keywords %>% anti_join(keywords %>% filter(baseUrl == "https://revistas.javeriana.edu.co/index.php/revPsycho"))

rm(limpiar,separadores)

### frecuencias

keywords %>% group_by(keywords) %>% tally(sort = TRUE) %>% top_n(30) %>% ggplot(aes(x=reorder(keywords,n),y=n)) + geom_col() + coord_flip() # distribucion de repeticion de keywords... esta bien?
keywords %>% group_by(keywords) %>% tally(sort = TRUE) %>% group_by(n) %>% tally(sort=TRUE) %>% ggplot(aes(x=n,y=nn)) + geom_col() # distribucion de repeticion de keywords... esta bien?
wordcloud::wordcloud( keywords$keywords , max.words = 100)
keywords %>% group_by(baseUrl, keywords) %>% tally(sort=TRUE) %>% top_n(wt = n, n = 20) %>%
  inner_join( keywords %>% group_by(baseUrl, keywords) %>% tally(sort=TRUE) %>% top_n(wt = n, n = 20) %>% group_by(keywords) %>% summarize(f=n()) ) %>%
  left_join( muestra ) %>%
  ggplot(aes(x=reorder(keywords,n),y=n, fill=as.factor(f))) + facet_wrap(~nombre, scales = "free") + geom_bar(stat = "identity") + coord_flip()
glimpse(keywords)

### correlaciones entre keywords (en un mismo doc) # https://www.tidytextmining.com/ngrams.html#
correlaciones <- keywords %>% group_by(keywords) %>% filter(n() > 2) %>% ungroup() %>%

    widyr::pairwise_cor(item = keywords, feature = article, sort = TRUE, method = "pearson")

library(igraph)
library(ggraph)

corr_graph <- correlaciones %>% filter(correlation>0.3) %>% igraph::graph_from_data_frame()
corr_graph
ggraph::ggraph(corr_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

### con que correlaciona representaciones vs. actitudes?

correlaciones %>% filter(item1=="personalidad")
correlaciones %>% filter(item1=="actitudes")
correlaciones %>% filter(item1=="representaciones sociales")
correlaciones %>% filter(item1=="psicologia")



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
