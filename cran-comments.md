This is a resubmission to address processing time issues with some of the examples.  I have reduced examples for the functions `comb`, `ecometab`, `subset.swmpr`, and `import_remote`.

## Test environments
* ubuntu 12.04 (on travis-ci), R 3.1.3
* local Windows 7 install, R 3.1.3 
* local Windows 7 install, Current r-devel (2015-04-01 r68131)
* Windows install (on AppVeyor), r-devel (2015-01-04 r67323)
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Marcus Beck <mbafs2012@gmail.com>'
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    Estuarine (9:66)

This is not mis-spelled.
  
## Downstream dependencies
None.
