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

# UT111: En la semana pasada... ¿Hizo ejercicios o practicó algún deporte?

UT111_UT2012 <- data_UT2012 %>%
  transmute(ciudad = CIUDAD,
            sexo = haven::as_factor(P02),
            edad = P03,
            UT111 = haven::as_factor(UT111), # ¿Hizo ejercicios? Si o No
            UT111A = UT111A, # Horas dedicadas (lunes a viernes)
            UT111B = UT111B, # Minutos dedicados (lunes a viernes)
            UT111C = UT111C, # Horas dedicadas (sábado y domingo)
            UT111D = UT111D, # Minutos dedicados (sábado y domingo)
            dominio = dominio, # Estrato
            id_upm = id_upm, # Unidad primaria de muestreo
            fexp = fexp # Factores de expansión
  ) %>% 
  as_tibble()


# Cálculo del total de horas semanales en hacer ejercicios o practicar algún deporte

UT111_UT2012[c('UT111A','UT111B','UT111C','UT111D')] <- sapply(UT111_UT2012[c('UT111A','UT111B','UT111C','UT111D')], 
                                                               FUN = function(x) ifelse(is.na(x), 0, x))

UT111_UT2012$t_horas_cocina <- UT111_UT2012$UT111A + 
  (UT111_UT2012$UT111B/60) + 
  UT111_UT2012$UT111C + 
  (UT111_UT2012$UT111D/60)


# Variable de grupos de edad

# 98: 98 y más
# 99: No informa

UT111_UT2012 <- UT111_UT2012 %>% 
  mutate(edad_rango = case_when(edad <= 11 ~ 'Edad entre 0 y 11 años',
                                between(edad, 12, 19) ~ 'Edad entre 12 y 19 años',
                                between(edad, 20, 29) ~ 'Edad entre 20 y 29 años',
                                between(edad, 30, 39) ~ 'Edad entre 30 y 39 años',
                                between(edad, 40, 49) ~ 'Edad entre 40 y 49 años',
                                between(edad, 50, 59) ~ 'Edad entre 50 y 59 años',
                                between(edad, 60, 69) ~ 'Edad entre 60 y 69 años',
                                edad >= 70 ~ 'Edad mayor a 70 años',
                                edad == 99 ~ 'No informa')) %>%
  mutate(edad_rango = as_factor(edad_rango))


# Variable de provincia

# Para la variable 'ciudad', el INEC maneja un código de 6 dígitos, donde el 1er y 2do dígito forman el Código de Provincia, el 3er y 4to dígito forman el Código de Cantón y el 5to y 6to dígito forman el Código de Parroquia. Por ejemplo, en el código 010150, 01 indica la provincia de Azuay, 0101 indica el cantón Cuenca y 010150 indica la parroquia 'Cuenca'
# No existen registros para Galápagos en el dataset del 2012

UT111_UT2012 <- UT111_UT2012 %>% 
  mutate(prov = ifelse(nchar(ciudad) == 5, paste("0", ciudad, sep = ""), ciudad)) %>%
  mutate(prov = substr(prov,1,2)) %>% 
  mutate(prov = as_factor(prov))

levels(UT111_UT2012$prov) <- 
  c('Azuay','Bolívar','Cañar','Carchi','Cotopaxi','Chimborazo','El Oro','Esmeraldas',
    'Guayas','Imbabura','Loja','Los Ríos','Manabí','Morona Santiago','Napo','Pastaza',
    'Pichincha','Tungurahua','Zamora Chinchipe','Sucumbíos','Orellana',
    'Santo Domingo de los Tsáchilas','Santa Elena','Zonas no delimitadas')


## --------------------------------------------------------------------------- ##
## ------------------------- Data Wrangling 2019 ----------------------------- ##
## --------------------------------------------------------------------------- ##

# s6p4: En la semana pasada... ¿Hizo ejercicios o practicó algún deporte?

S6P4_UT2019 <- data_personas2019 %>%
  transmute(id_per = id_per,
            ciudad = ciudad,
            sexo = fct_recode(factor(s1p2), "Hombre"="1","Mujer"="2" ),
            edad = s1p3,
            s6p4 = fct_recode(factor(s6p4), "Si"="1","No"="2" ), # ¿Hizo ejercicios? Si o No
            s6p4a = s6p4a, # Horas dedicadas (lunes a viernes)
            s6p4b = s6p4b, # Minutos dedicados (lunes a viernes)
            s6p4c = s6p4c, # Horas dedicadas (sábado y domingo)
            s6p4d = s6p4d, # Minutos dedicados (sábado y domingo)
            conglomerado = conglomerado,
            estrato = estrato, 
            upm = upm, # Unidad primaria de muestreo
            fexp = fexp # Factores de expansión
  ) %>%
  as_tibble()


# Cálculo del total de horas semanales en hacer ejercicios o practicar algún deporte

S6P4_UT2019[c('s6p4a','s6p4b','s6p4c','s6p4d')] <- sapply(S6P4_UT2019[c('s6p4a','s6p4b','s6p4c','s6p4d')], 
                                                          FUN = function(x) ifelse(is.na(x), 0, x))

S6P4_UT2019$t_horas_cocina <- (S6P4_UT2019$s6p4a) + 
  (S6P4_UT2019$s6p4b/60) + 
  (S6P4_UT2019$s6p4c) + 
  (S6P4_UT2019$s6p4d/60)


# Variable de grupos de edad

S6P4_UT2019 <- S6P4_UT2019 %>% 
  mutate(edad_rango = case_when(edad <= 11 ~ 'Edad entre 0 y 11 años',
                                between(edad, 12, 19) ~ 'Edad entre 12 y 19 años',
                                between(edad, 20, 29) ~ 'Edad entre 20 y 29 años',
                                between(edad, 30, 39) ~ 'Edad entre 30 y 39 años',
                                between(edad, 40, 49) ~ 'Edad entre 40 y 49 años',
                                between(edad, 50, 59) ~ 'Edad entre 50 y 59 años',
                                between(edad, 60, 69) ~ 'Edad entre 60 y 69 años',
                                edad >= 70 ~ 'Edad mayor a 70 años',
                                edad == 99 ~ 'No informa')) %>%
  mutate(edad_rango = as_factor(edad_rango))


# Variable de provincia

S6P4_UT2019$prov <- as.factor(substr(data_personas2019$ciudad, start = 1, stop = 2))

levels(S6P4_UT2019$prov) <- c('Azuay','Bolívar','Cañar','Carchi','Cotopaxi','Chimborazo','El Oro','Esmeraldas',
                               'Guayas','Imbabura','Loja','Los Ríos','Manabí','Morona Santiago','Napo','Pastaza',
                               'Pichincha','Tungurahua','Zamora Chinchipe','Galápagos','Sucumbíos','Orellana',
                               'Santo Domingo de los Tsáchilas','Santa Elena','Zonas no delimitadas')


## --------------------------------------------------------------------------- ##
## ------------------------- Data Wrangling 2020 ----------------------------- ##
## --------------------------------------------------------------------------- ##

df_info_personas2020 <- data.frame(id_per = data_personas2020$id_per, 
                                   s1p2 = data_personas2020$s1p2)

df_analisis_UT2020 <- merge(df_info_personas2020, 
                            data_actfisica2020, by = "id_per")


# s6p4: En la semana pasada... ¿Hizo ejercicios o practicó algún deporte?

S6P4_UT2020 <- df_analisis_UT2020 %>%
  transmute(id_per = id_per,
            ciudad = ciudad,
            sexo = fct_recode(factor(s1p2), "Hombre"="1","Mujer"="2" ),
            edad = s1p3,
            s6p4 = fct_recode(factor(s6p4), "Si"="1","No"="2" ), # ¿Hizo ejercicios? Si o No
            s6p4a = s6p4a, # Horas dedicadas (lunes a viernes)
            s6p4b = s6p4b, # Minutos dedicados (lunes a viernes)
            s6p4c = s6p4c, # Horas dedicadas (sábado y domingo)
            s6p4d = s6p4d, # Minutos dedicados (sábado y domingo)
            conglomerado = conglomerado,
            estrato = estrato, 
            upm = upm, # Unidad primaria de muestreo
            fexp = fexp # Factores de expansión
  ) %>%
  as_tibble()


# Cálculo del total de horas semanales en hacer ejercicios o practicar algún deporte

S6P4_UT2020[c('s6p4a','s6p4b','s6p4c','s6p4d')] <- sapply(S6P4_UT2020[c('s6p4a','s6p4b','s6p4c','s6p4d')], 
                                                          FUN = function(x) ifelse(is.na(x), 0, x))

S6P4_UT2020$t_horas_cocina <- (S6P4_UT2020$s6p4a) + 
  (S6P4_UT2020$s6p4b/60) + 
  (S6P4_UT2020$s6p4c) + 
  (S6P4_UT2020$s6p4d/60)


# Variable de grupos de edad

S6P4_UT2020 <- S6P4_UT2020 %>% 
  mutate(edad_rango = case_when(edad <= 11 ~ 'Edad entre 0 y 11 años',
                                between(edad, 12, 19) ~ 'Edad entre 12 y 19 años',
                                between(edad, 20, 29) ~ 'Edad entre 20 y 29 años',
                                between(edad, 30, 39) ~ 'Edad entre 30 y 39 años',
                                between(edad, 40, 49) ~ 'Edad entre 40 y 49 años',
                                between(edad, 50, 59) ~ 'Edad entre 50 y 59 años',
                                between(edad, 60, 69) ~ 'Edad entre 60 y 69 años',
                                edad >= 70 ~ 'Edad mayor a 70 años',
                                edad == 99 ~ 'No informa')) %>%
  mutate(edad_rango = as_factor(edad_rango))


# Variable de provincia

for (i in 1:dim(S6P4_UT2020)[1]) {
  if (nchar(S6P4_UT2020$ciudad[i]) == 5 ) {
    S6P4_UT2020$ciudad[i] <- paste("0", S6P4_UT2020$ciudad[i], sep = "")
  }
}

S6P4_UT2020$prov <- as.factor(substr(S6P4_UT2020$ciudad, start = 1, stop = 2))

levels(S6P4_UT2020$prov) <- c('Azuay','Bolívar','Cañar','Carchi','Cotopaxi','Chimborazo','El Oro','Esmeraldas',
                              'Guayas','Imbabura','Loja','Los Ríos','Manabí','Morona Santiago','Napo','Pastaza',
                              'Pichincha','Tungurahua','Zamora Chinchipe','Galápagos','Sucumbíos','Orellana',
                              'Santo Domingo de los Tsáchilas','Santa Elena','Zonas no delimitadas')
