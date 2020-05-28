---
title: 'ojsr: A R package for OJS scraping'
tags:
  - R
  - OJS
  - web-scraping
authors:
  - name: Gastón Becerra
    orcid: 0000-0001-9432-8848
    affiliation: "1, 2" 
affiliations:
 - name: CONICET, Argentina
   index: 1
 - name: Universidad Abierta Interamericana, Argentina
   index: 2
date: 24 April 2020
bibliography: paper.bib
---

# Summary

Open Journal Systems (OJS) is an open source software application for managing scientific journals, covering from manuscript submission, through evaluation and copyediting, and then online publication. It was originally developed at the University of British Columbia within a research initiative, named Public Knowledge Project (PKP) (<https://pkp.sfu.ca/ojs/>), and currently counts with the collaboration from other institutions and universities. OJS is a community maintained and supported project (<https://forum.pkp.sfu.ca/>), released under GNU General Public License.

OJS was designed to advance (free) open access to scientific knowledge by reducing publishing costs [@Willinsky2005; @Willinsky2006]. 
According to its website (<https://pkp.sfu.ca/ojs/ojs-usage/ojs-stats/>), as of 2010 OJS was being used by 10,000-11,000 journals worldwide, most of these from developing countries [@Edgar2010].

Here we introduce ojsr, an R package that allows you to crawl the content of OJS and scrap their articles' meta-data. 

As per version 0.1.0, ojsr include consistent functions to:

- Scraps issues' urls from OJS issue archive
- Scrap articles urls from the ToC of OJS issues
- Scraps OJS search results for a given criteria to retrieve articles' url
- Scrap galleys urls from OJS articles
- Scrap metadata from html of OJS articles
- Retrieve OAI records for OJS articles

ojsr composes the required url for different scrapping scenarios (e.g., if you are scrapping articles from an issue, it will parse and browse the issue's ToC), by parsing urls according to OJS routing conventions (<https://docs.pkp.sfu.ca/dev/documentation/en/architecture-routes>). This can be very handy when working with articles' or journals' urls retrieved from search engines. However, due this, ojsr is most likely to fail on OJS installments with customized routing.

Since OJS v3.1+ <https://docs.pkp.sfu.ca/dev/api/ojs/3.1> a Rest API is provided. We're positive a better R interface should use that API, instead of webscraping. 
However, currently this Rest API is in a very early stage of production (and also requires authentication). ojsr aims to help crawling and retrieving info from OJS during this legacy period. 

# References
