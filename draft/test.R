
bosque_issue2 <- ojsr::get_issue_url(input_url = 'https://revistas.unbosque.edu.co/index.php/CHP/', verbose = TRUE)

rm(bosque_art)

bosque_art1 <- ojsr::get_article_url(input_url = bosque_issue$output_url, verbose = TRUE, method = "scrap_by_href_convention_no_classes")
bosque_art2 <- ojsr::get_article_url(input_url = bosque_issue$output_url, verbose = TRUE, method = "scrap_by_href_convention_no_classes_long")
bosque_art3 <- ojsr::get_article_url(input_url = bosque_issue$output_url, verbose = TRUE, method = 'scrap_and_process')

bosque_art4 <- ojsr::get_article_url(input_url = bosque_issue2$output_url[1:10], verbose = TRUE)
warnings()


bosque_art4

bosque_gal1 <- ojsr::get_galley_url(input_url = bosque_art4$output_url[1:2], verbose = TRUE)
bosque_gal2 <- ojsr::get_galley_url(input_url = bosque_art4$output_url[5:7], verbose = TRUE)


ojsr::get_galley_url("https://revistas.javeriana.edu.co/index.php/revPsycho/article/view/29001",verbose = TRUE)


urls <- c('https://revistapsicologia.uchile.cl/index.php/RDP','https://revistas.javeriana.edu.co/index.php/revPsycho/')
issues <- ojsr::get_issue_url(input_url = urls, verbose = TRUE)
articles <- ojsr::get_article_url(input_url = issues$output_url, verbose = TRUE)
galleys <- ojsr::get_galley_url(input_url = articles$output_url, verbose = TRUE)
search_paginated <- ojsr::get_paginated_search_url(input_url = urls, search_criteria = "psicologia",verbose = TRUE)
meta <- ojsr::get_meta_from_html(input_url = articles$output_url, verbose = TRUE)

ojsr::get_search_url(input_url = urls, search_criteria = "psicologia", verbose = TRUE)


