## Resubmission

### 20/09/21

Fixed in /R, /vignettes, /man:
- several spelling and grammar errors

Fixed in /R:
- warnings handled (e.g., when Internet resources fail to load) 
- unused connections closed

Fixed in DESCRIPTION:
- removed unused Imports

### 20/07/01

Fixed minor bug in R/scrapers.R 

### 20/04/16

Fixed in DESCRIPTION:
- OJS and OAI acronyms mentioned in undirected single quotes.

AlsO:
- deleted dev comments in test file

### 20/04/14

Fixed in DESCRIPTION:
- omit the redundant "for R" in Title
- mis-spelled words: "functiones" (11:18)
- The Description field should not start with the package name, 'This package' or similar.
- Finally, package names should be single quoted in the Description field. (removed name from description)

## Test environments

* local Windows NT 10.0.17763 / x86_64-w64-mingw32 (64-bit)
* Ubuntu 16.04.6 LTS (on Travis-CI)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (on R-hub)
* Fedora Linux, R-devel, clang, gfortran (on R-hub)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs
