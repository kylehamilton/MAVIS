MAVIS: Meta Analysis via Shiny v1.1.4
=====
[![CRAN Version](http://www.r-pkg.org/badges/version/MAVIS)](http://cran.rstudio.com/web/packages/MAVIS)
[![Monthly Downloads](http://cranlogs.r-pkg.org/badges/MAVIS)](http://cranlogs.r-pkg.org/badges/MAVIS)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/MAVIS)](http://cranlogs.r-pkg.org/badges/grand-total/MAVIS)

### Live demo of MAVIS v1.1.3 can be found here http://kylehamilton.net/shiny/MAVIS/


June 21, 2016
* Examples now include data from published papers
* More effect size calculators have been added

August 4, 2015
* Version 1.1.1 has been sent to CRAN
* Added one new effect size calculator and included the Turkish version of MAVIS (aRma) to the package.

April 29, 2015
* Version 1.1 has been sent to CRAN and can be found here https://cran.rstudio.com/web/packages/MAVIS/.


List of Packages Used 
```
library("shiny")
library("shinyAce")
library("shinyBS")
library("metafor")
library("MAd")
library("MAc")
library("quantreg")
library("ggplot2")
library("compute.es")
library("SCMA")
library("SCRT")
library("weightr")
library("irr")
```

### Acknowledgments and Authors

#### Acknowledgments
W. Kyle Hamilton would like to thank the [Health Communications and Interventions Lab at UC Merced](http://cameronhcilab.com/) for their comments and beta testing efforts on this application as well as [Kathleen Coburn](http://psychology.ucmerced.edu/content/kathleen-coburn) for her feedback and evaluation of the statistical methods related to this project.

Atsushi Mizumoto would like to thank [Dr. Luke Plonsky](http://oak.ucc.nau.edu/ldp3/) and [Dr. Yo In'nami](https://sites.google.com/site/yoinnami/) for their support and feedback to create this web application.


#### Authors


![alt text](http://kylehamilton.com/wp-content/uploads/2014/11/kyle80.jpg "Logo Title Text 1") [W. Kyle Hamilton - University of California, Merced](http://www.kylehamilton.com)

W. Kyle Hamilton maintains this application and has authored new features.

![alt text](http://oi59.tinypic.com/2mnrcci.jpg "Logo Title Text 1") [Burak Aydin, PhD - Recep Tayyip Erdoğan University](https://www.aydinburak.net/)

Burak Aydin is working on a Turkish version of MAVIS and contributed the dichotomous data entry feature.

![alt text](http://kylehamilton.com/wp-content/uploads/2014/11/atsushi80.jpg "Logo Title Text 1")
[Atsushi Mizumoto, PhD - Kansai University](http://mizumot.com)

Atsushi Mizumoto wrote the first version of this application; this application is a fork of the original which can be found [here](https://github.com/mizumot/meta)

Kathleen Coburn contributed technical advice on how to run a meta-analysis as well as information on publication bias.
