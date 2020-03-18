library(tidyverse) # we'll use dplyr and ggplot
library(lubridate) # we'll parse some years
library(ojsr)

revistas_ps <- c(
  'http://sportsem.uv.es/j_sports_and_em/index.php/rips/',
  'https://dspace.palermo.edu/ojs/index.php/psicodebate',
  'https://publicacionescientificas.uces.edu.ar/index.php/desvapsico/',
  'https://revistapsicologia.uchile.cl/index.php/RDP/',
  'https://revistas.ucn.cl/index.php/saludysociedad',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial'
)









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



