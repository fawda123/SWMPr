## Resubmission 

This is a resubmission to remove the dependency on the wq package that will be archived in the near future.  

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.2.4
* OS X (on travis-ci), R 3.2.4
* local Windows 7 install, R 3.3.0 
* local Windows 7 install, Current r-devel (2016-03-28 r71184)
* Windows install (on AppVeyor), R 3.2.4 Patched (2016-03-16 r70382)
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.  There was one NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Marcus W. Beck <mbafs2012@gmail.com>'

Possibly mis-spelled words in DESCRIPTION:
  Estuarine (9:66)
  
This is not misspelled.
  
## Downstream dependencies
None.
