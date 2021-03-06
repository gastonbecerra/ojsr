---
title: 'ojsr: Crawler/scraper for Open Journal System'
authors:
- affiliation: 1, 2
  name: Gastón Becerra
  orcid: 0000-0001-9432-8848
date: "9 June 2020"
output:
  html_document:
    df_print: paged
bibliography: paper.bib
tags:
- R
- OJS
- web-scraping
affiliations:
- index: 1
  name: CONICET, Argentina
- index: 2
  name: Universidad Abierta Interamericana, Argentina
---

# Summary

Open Journal Systems (OJS) is an open source software application for managing scientific journals, covering from manuscript submission through evaluation and copyediting, and then online publication. OJS was originally developed at the University of British Columbia as a research initiative, named Public Knowledge Project (PKP) (<https://pkp.sfu.ca/ojs/>), and currently other institutions and universities work in collaboration with it. OJS is maintained and supported by an online community (<https://forum.pkp.sfu.ca/>) and is released under GNU General Public License.


OJS was designed to advance (free) open access to scientific knowledge by reducing publishing costs [@Willinsky2005; @Willinsky2006]. According to its website (<https://pkp.sfu.ca/ojs/ojs-usage/ojs-stats/>), as of 2010 OJS was being used by 10,000-11,000 journals worldwide, most of these from developing countries [@Edgar2010].

Here we introduce ``ojsr``, an R package that allows you to crawl the content of OJS and scrap their articles' metadata. 

As per version 0.1.0, ``ojsr`` includes consistent functions to:

- Scrap issues' URLs from OJS issue archive
- Scrap articles URLs from the ToC of OJS issues
- Scrap OJS search results for a given criteria to retrieve articles' URL
- Scrap galleys URLs from OJS articles
- Scrap metadata from HTML of OJS articles
- Retrieve OAI records for OJS articles

``ojsr`` composes the required url for different scrapping scenarios (e.g., if you are scrapping articles from an issue, it will parse and browse the issue's ToC), by parsing URLs according to OJS routing conventions (<https://docs.pkp.sfu.ca/dev/documentation/en/architecture-routes>). This can be very handy when working with articles' or journals' URLs retrieved from search engines. However, due to this, ``ojsr`` is most likely to fail on OJS installments with customized routing.

# Statement of need

``ojsr`` was designed to be used by anyone interested in retrieving content or metadata from an OJS *directly* (meaning, without looking for OJS content in third-party index or aggregators). This also allows you to retrieve information from non-indexed journals. 

Using ``ojsr`` it is fairly simple to retrieve the entire structure and metadata of a journal for bibliometric studies. I.e., the following case has been documented in the repository's example section (<https://github.com/gastonbecerra/ojsr#example>): *Let's say we want to scrap metadata from a collection of journals, in order to compare their top keywords. We have the journal names and urls; we can use ojsr to scrap their issues, articles and metadata.*

Finally, ``ojsr`` is also a convenient tool for particular *small* purposes, like desambiguating article/galleys links (usually served mixed in search results), or parsing urls (e.g., retrieving the base URL of an OJS, or its OAI protocol URL) for piping into different scrapping scenarios.

Since OJS v3.1+ <https://docs.pkp.sfu.ca/dev/api/ojs/3.1> a Rest API is provided. We're positive a better R interface should use that API, instead of webscraping. However, currently this Rest API is in a very early stage of production, and also requires authentication. ``ojsr`` aims to help crawling and retrieving information from OJS during the legacy period of massive update to v3.1+.

# References
