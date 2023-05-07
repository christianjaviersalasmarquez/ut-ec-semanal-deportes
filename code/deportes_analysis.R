## --------------------------------------------------------------------------- ##
## ----------------------------- Directorio ---------------------------------- ##
## --------------------------------------------------------------------------- ##

current.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current.path)
setwd("..")

## --------------------------------------------------------------------------- ##
## ------------------------------ Librerías ---------------------------------- ##
## --------------------------------------------------------------------------- ##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(radiant.data)) install.packages("radiant.data", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(srvyr)) install.packages("srvyr", repos = "http://cran.us.r-project.org")


## --------------------------------------------------------------------------- ##
## ------------------------------- Source ------------------------------------ ##
## --------------------------------------------------------------------------- ##

source('code/deportes_download.R')
