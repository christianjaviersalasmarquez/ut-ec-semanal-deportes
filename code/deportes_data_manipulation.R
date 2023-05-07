## --------------------------------------------------------------------------- ##
## ------------------------------ Librerías ---------------------------------- ##
## --------------------------------------------------------------------------- ##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(radiant.data)) install.packages("radiant.data", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")


## --------------------------------------------------------------------------- ##
## ---------------------------- Bases de datos ------------------------------- ##
## --------------------------------------------------------------------------- ##

# 2012

data_UT2012 <- read_sav(EUT2012.f.path)

# 2019

data_personas2019 <- read_csv2("https://raw.githubusercontent.com/christianjaviersalasmarquez/ut-ec-semanal-deportes/main/data/201912_multibdd_personas.sav.csv") # read_csv2 utiliza ';' como separador y ',' para punto decimal

# 2020

data_personas2020 <- read_csv2("https://raw.githubusercontent.com/christianjaviersalasmarquez/ut-ec-semanal-deportes/main/data/202012_multibdd_personas.csv") # read_csv2 utiliza ';' como separador y ',' para punto decimal

data_actfisica2020 <- read_csv2("https://raw.githubusercontent.com/christianjaviersalasmarquez/ut-ec-semanal-deportes/main/data/202012_multibdd_educaci%C3%B3n_actfisica_tics.csv") # read_csv2 utiliza ';' como separador y ',' para punto decimal


## --------------------------------------------------------------------------- ##
## ------------------------- Data Wrangling 2012 ----------------------------- ##
## --------------------------------------------------------------------------- ##




