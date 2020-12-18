## Resubmission 

This is a version update to 2.4.0.

## Test environments
* ubuntu 20.04 (on GitHub Actions), R 4.0.3
* ubuntu 20.04 (on GitHub Actions), R devel
* OS X (on GitHub Actions), R 4.0.3
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)
* local Windows 7 install, R 4.0.0

## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE for changing the maintainer email.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Marcus W. Beck <mbeck@tbep.org>'

New maintainer:
  Marcus W. Beck <mbeck@tbep.org>
Old maintainer(s):
  Marcus W. Beck <marcusb@sccwrp.org>
  
## Downstream dependencies
I have also run R CMD check on the SWMPrExtension downstream dependency for SWMPr. There were no ERRORs, WARNINGs, or NOTEs.
