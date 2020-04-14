## Test environments
* local Windows NT 10.0.17763 / x86_64-w64-mingw32 (64-bit)
* win-builder
* Ubuntu 16.04.6 LTS (on travis)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Resubmissions

### 20/04/14

Fixed in DESCRIPTION:
- omit the redundant "for R" in Title
- mis-spelled words: functiones (11:18)
- The Description field should not start with the package name, 'This package' or similar.
- Finally, package names should be single quoted in the Description field. (removed name from description)
