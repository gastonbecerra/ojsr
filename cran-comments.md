## Test environments
* local Windows NT 10.0.17763 / x86_64-w64-mingw32 (64-bit)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

N  checking R code for possible problems (2.7s)
   ojsr_scrap_v3: no visible binding for global variable 'issue_id'
   ojsr_scrap_v3: no visible binding for global variable 'conventional_issue'
   ojsr_scrap_v3: no visible binding for global variable 'article_id'
   ojsr_scrap_v3: no visible binding for global variable 'galley_id'
   ojsr_scrap_v3: no visible binding for global variable 'conventional_article'
   Undefined global functions or variables:
     article_id conventional_article conventional_issue galley_id issue_id

All of these are not global variables but columns of a table created by a private function. They appear like undefined variables because they are mentioned in dplyr filters.
