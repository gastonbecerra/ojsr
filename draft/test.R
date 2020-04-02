urls <- c('https://revistapsicologia.uchile.cl/index.php/RDP',
  'https://revistas.javeriana.edu.co/index.php/revPsycho/',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial')
urls <- c('https://revistas.javeriana.edu.co/index.php/revPsycho/',
  'https://publicaciones.sociales.uba.ar/index.php/psicologiasocial')

issues <- ojsr::get_issues_from_archive(input_url = urls, verbose = TRUE)
issues %>% dplyr::group_by(input_url) %>% dplyr::tally()

articles <- ojsr::get_articles_from_issue(input_url = issues$output_url, verbose = TRUE)
articles %>% dplyr::left_join(ojsr::process_urls(issues$input_url), by="input_url") %>% dplyr::select(base_url) %>% dplyr::group_by(base_url) %>% dplyr::tally()

galleys <- ojsr::get_galley_url_from_article(input_url = articles$output_url, verbose = TRUE)


ojsr::process_urls(url = "https://publicaciones.sociales.uba.ar/index.php/psicologiasocial/article/view/593/0")

search_paginated <- ojsr::get_paginated_search_url(input_url = urls, search_criteria = "psicologia",verbose = TRUE)


meta <- ojsr::get_html_meta_from_article(input_url = articles$output_url[180:190], verbose = TRUE)


meta2 <- ojsr::get_oai_meta_from_article(input_url = articles$output_url[185:190], verbose = TRUE)
