library('rvest')

# issues

url <- 'https://www.journals.uchicago.edu/loi/ajs'
webpage <- read_html(url)
links_issues <- webpage %>% html_nodes('a.loiLinkClass')
issues <- as.data.frame(cbind(
  links_issues %>% html_text(),
  links_issues %>% html_attr("href")
), stringsAsFactors = FALSE)
names(issues)<-c("numero","url")
rm(url,links_issues,webpage)

# articulos

keep_pdfplus <- function( charv ){
  r <- NULL
  for (i in 1:length(charv)) {
    if(grepl("pdfplus",charv[i],fixed=TRUE)){
      r<-c(r,charv[i])
    }
  }
  return(r)
}

articles <- list()
for (i in 1:nrow(issues[31:150,])) {
  print(paste0(i," - ",issues[i,"url"]))
  webpage <- read_html(issues[i,"url"])
  articulos <- webpage %>% html_nodes('.articleEntry') # deja afuera los book reviews
  articles[[i]] <- as.data.frame(cbind(
    issues[i,"url"],
    articulos %>% html_nodes('.hlFld-Title') %>% html_text(),
    articulos %>% html_nodes('a.ref.nowrap') %>% html_attr("href") %>% keep_pdfplus()
  ), stringsAsFactors = FALSE)
}
rm(i,articulos,webpage)

articles_df <- do.call("rbind", articles)
names(articles_df)<-c("issue_url","title","pdfplus")

write.csv(x = articles_df , file = "articulos_AJS.csv" , quote = TRUE)
