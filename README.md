MAVIS: Meta Analysis via Shiny v1.0.4 "Gobbling Turkey"
=====
[![DOI](https://zenodo.org/badge/9922/kylehamilton/MAVIS.svg)](http://dx.doi.org/10.5281/zenodo.14966)

![DOWLOADS](http://cranlogs.r-pkg.org/badges/MAVIS "Monthly Downloads")

#### News about MAVIS

* April 23, 2015
* Started to add some features to the effect size calculator
* Added support for calculating effect sizes for single case design research with the SCMA package


List of Packages Used 
```
library(shiny) 
library(shinyAce) 
library(metafor) 
library(meta) 
library(MAd) 
library(MAc) 
library(quantreg) 
library(ggplot2)
library(compute.es)
library(SCMA)
```
The first production release of MAVIS is now available on CRAN! v1.0 has been named "Gobbling Turkey"

Live demo of v1.0 can be found here http://kylehamilton.shinyapps.io/MAVIS_v1_0

![2063839197_ad08f77773_z](https://cloud.githubusercontent.com/assets/2274317/5225143/dc868ea4-7694-11e4-94bf-2c465f7d497c.jpg)

Photo by https://www.flickr.com/photos/rdeetz/

Image Licensed under https://creativecommons.org/licenses/by/2.0/

### Acknowledgments and Authors

#### Acknowledgments
William Kyle Hamilton would like to thank the [Health Communications and Interventions Lab at UC Merced](http://cameronhcilab.com/) for their comments and beta testing efforts on this application as well as [Kathleen Coburn](http://psychology.ucmerced.edu/content/kathleen-coburn) for her feedback and evaluation of the statistical methods related to this project.

Atsushi Mizumoto would like to thank [Dr. Luke Plonsky](http://oak.ucc.nau.edu/ldp3/) and [Dr. Yo In'nami](https://sites.google.com/site/yoinnami/) for their support and feedback to create this web application.


#### Authors


![alt text](http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg "Logo Title Text 1") [William Kyle Hamilton - University of California, Merced](http://www.kylehamilton.com)

William Kyle Hamilton maintains this application and has authored new features.

![alt text](http://kylehamilton.com/wp-content/uploads/2014/11/atsushi80.jpg "Logo Title Text 1")
[Atsushi Mizumoto, PhD - Kansai University](http://mizumot.com)

Atsushi Mizumoto wrote the first version of this application; this application is a fork of the original which can be found [here](https://github.com/mizumot/meta)
