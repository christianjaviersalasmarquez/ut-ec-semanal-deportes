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


## --------------------------------------------------------------------------- ##
## ---------------------- Diseño Muestral Encuestas -------------------------- ##
## --------------------------------------------------------------------------- ##

# Declaración del diseño muestral de cada encuesta utilizando la librería 'srvyr'

UT2012_survey_design <- UT111_UT2012 %>%
  as_survey_design(ids = id_upm,
                   strata = dominio,
                   weights = fexp,
                   nest = T)

UT2019_survey_design <- S6P4_UT2019 %>%
  as_survey_design(ids = upm,
                   strata = estrato,
                   weights = fexp,
                   nest = T)

UT2020_survey_design <- S6P4_UT2020 %>%
  as_survey_design(ids = upm,
                   strata = estrato,
                   weights = fexp,
                   nest = T)


## --------------------------------------------------------------------------- ##
## ----------------------------- Estimaciones -------------------------------- ##
## --------------------------------------------------------------------------- ##

# Promedio de horas semanales en hacer ejercicios o practicar algún deporte (2012)

df_t_deporte_UT2012_plot1 <- UT2012_survey_design %>% 
  group_by(sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2012_plot2 <- UT2012_survey_design %>% 
  group_by(edad_rango) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2012_plot3 <- UT2012_survey_design %>% 
  group_by(edad_rango, sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2012_plot4 <- UT2012_survey_design %>% 
  group_by(prov) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2012_plot5 <- UT2012_survey_design %>% 
  group_by(prov, sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))

# Promedio de horas semanales en hacer ejercicios o practicar algún deporte (2019)

df_t_deporte_UT2019_plot1 <- UT2019_survey_design %>% 
  group_by(sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2019_plot2 <- UT2019_survey_design %>% 
  group_by(edad_rango) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2019_plot3 <- UT2019_survey_design %>% 
  group_by(edad_rango, sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2019_plot4 <- UT2019_survey_design %>% 
  group_by(prov) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2019_plot5 <- UT2019_survey_design %>% 
  group_by(prov, sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))

# Promedio de horas semanales en hacer ejercicios o practicar algún deporte (2020)

df_t_deporte_UT2020_plot1 <- UT2020_survey_design %>% 
  group_by(sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2020_plot2 <- UT2020_survey_design %>% 
  group_by(edad_rango) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2020_plot3 <- UT2020_survey_design %>% 
  group_by(edad_rango, sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2020_plot4 <- UT2020_survey_design %>% 
  group_by(prov) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


df_t_deporte_UT2020_plot5 <- UT2020_survey_design %>% 
  group_by(prov, sexo) %>%
  summarise(media = survey_mean(t_horas_deporte, 
                                vartype = c("se"), 
                                na.rm = T))


## --------------------------------------------------------------------------- ##
## ------------------------------- Gráficos ---------------------------------- ##
## --------------------------------------------------------------------------- ##

p1_2012 <- 
  ggplot( df_t_deporte_UT2012_plot1 , aes(x = sexo , y = media, fill = sexo )) + 
  geom_bar(stat = 'identity', width = 0.5, position = 'dodge') + 
  geom_errorbar(aes(ymin = media - 1.96*media_se, ymax = media + 1.96*media_se), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  ylim(0,1.3) +
  labs(title = "2012", caption = "\n \n Fuente: Encuesta Específica de Uso del Tiempo (EUT) 2012", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)  ), color ="black", size = 4,position = position_dodge(1),vjust = -2.4) +
  xlab("Sexo") +
  ylab(" ") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position='none') 

p1_2019 <- 
  ggplot( df_t_deporte_UT2019_plot1 , aes(x = sexo , y = media, fill = sexo )) + 
  geom_bar(stat = 'identity', width = 0.5, position = 'dodge') + 
  geom_errorbar(aes(ymin = media - 1.96*media_se, ymax = media + 1.96*media_se), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  ylim(0,1.3) +
  labs(title = "2019", caption = "\n \n Fuente: Encuesta Multipropósito 2019", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)  ), color ="black", size = 4,position = position_dodge(1),vjust = -2.4) +
  xlab("Sexo") +
  ylab(" ") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position='none') 

p1_2020 <- 
  ggplot( df_t_deporte_UT2020_plot1 , aes(x = sexo , y = media, fill = sexo )) + 
  geom_bar(stat = 'identity', width = 0.5, position = 'dodge') + 
  geom_errorbar(aes(ymin = media - 1.96*media_se, ymax = media + 1.96*media_se), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  ylim(0,1.3) +
  labs(title = "2020", caption = "\n \n Fuente: Encuesta Multipropósito 2020", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)  ), color ="black", size = 4,position = position_dodge(1),vjust = -3.2) +
  xlab("Sexo") +
  ylab(" ") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position= c(0.84,0.95)) +
  theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="grey30"))

plot1_articulo_deporte <- 
  p1_2012 + 
  p1_2019 + 
  p1_2020 +
  plot_layout(ncol = 3) + 
  plot_annotation(title = 'Promedio de horas semanales en hacer ejercicio o practicar algún deporte en 2012, 2019, y 2020 (por sexo) \n',
                  caption = str_wrap('\n Nota: Las barras de error representan intervalos de confianza de 95% para la media', width = 100),
                  theme = theme(plot.title = element_text(hjust = 0.5, colour="grey20", size=14),
                                plot.caption = element_text(hjust = 0, colour="grey30")))

ggsave("images/grafico1-articulo-deporte.png", device = "png", width = 12.5, height = 7, dpi = 900)




p2_2012 <- 
  ggplot(df_t_deporte_UT2012_plot3 %>% filter(edad_rango != 'Edad entre 0 y 11 años'), 
         aes(x = media , y = edad_rango , fill = sexo )) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(xmin = media - 1.96*media_se, xmax = media + 1.96*media_se), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  xlim(0,2.5) +
  labs(title = "2012", caption = "\n \n Fuente: Encuesta Específica de Uso del Tiempo (EUT) 2012", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)), color ="black", size = 4,position = position_dodge(1), hjust = -0.8) +
  xlab("Promedio de horas") +
  ylab("Grupo de edad") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position='none')

p2_2019 <- 
  ggplot(df_t_deporte_UT2019_plot3 %>% filter(edad_rango != 'Edad entre 0 y 11 años'), 
         aes(x = media , y = edad_rango , fill = sexo )) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(xmin = media - 1.96*media_se, xmax = media + 1.96*media_se), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  xlim(0,2.5) +
  labs(title = "2019", caption = "\n \n Fuente: Encuesta Multipropósito 2019", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)), color ="black", size = 4,position = position_dodge(1), hjust = -0.8) +
  xlab("Promedio de horas") +
  ylab("Grupo de edad") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position='none')

p2_2020 <- 
  ggplot(df_t_deporte_UT2020_plot3 %>% filter(edad_rango != 'Edad entre 0 y 11 años'), 
         aes(x = media , y = edad_rango , fill = sexo )) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(xmin = media - 1.96*media_se, xmax = media + 1.96*media_se), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  xlim(0,2.5) +
  labs(title = "2020", caption = "\n \n Fuente: Encuesta Multipropósito 2020", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)), color ="black", size = 4,position = position_dodge(1), hjust = -0.8) +
  xlab("Promedio de horas") +
  ylab("Grupo de edad") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position= c(0.8,1)) +
  theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="grey30"))

plot2_articulo1 <- 
  p2_2012 + 
  p2_2019 + 
  p2_2020 +
  plot_layout(ncol = 3) + 
  plot_annotation(title = 'Promedio de horas semanales en hacer ejercicio o practicar algún deporte en 2012, 2019, y 2020 (por grupo de edad y por sexo) \n',
                  caption = str_wrap('\n Nota: Las barras de error representan intervalos de confianza de 95% para la media', width = 100),
                  theme = theme(plot.title = element_text(hjust = 0.5, colour="grey20", size=14),
                                plot.caption = element_text(hjust = 0, colour="grey30")))

ggsave("images/grafico2-articulo-deporte.png", device = "png", width = 12.5, height = 7, dpi = 900)




p3_2012 <- 
  ggplot(df_t_deporte_UT2012_plot5 %>% filter(prov == 'Pichincha' | prov == 'Guayas' | prov == 'Azuay'), 
         aes(x = prov, y = media, fill = sexo)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = media - 1.96*media_se, ymax = media + 1.96*media_se), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  ylim(0,2.4) +
  labs(title = "2012", caption = "\n \n Fuente: Encuesta Específica de Uso del Tiempo (EUT) 2012", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)  ), color ="black", size = 4,position = position_dodge(0.9), vjust = -3.4) +
  xlab("Provincia") +
  ylab(" ") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position='none')

p3_2019 <- 
  ggplot(df_t_deporte_UT2019_plot5 %>% filter(prov == 'Pichincha' | prov == 'Guayas' | prov == 'Azuay') , 
         aes(x = prov  , y = media , fill = sexo )) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = media - 1.96*media_se, ymax = media + 1.96*media_se), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  ylim(0,2.4) +
  labs(title = "2019", caption = "\n \n Fuente: Encuesta Multipropósito 2019", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)), color ="black", size = 4,position = position_dodge(0.9), vjust = -3.7) +
  xlab("Provincia") +
  ylab(" ") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position='none')

p3_2020 <- 
  ggplot(df_t_deporte_UT2020_plot5 %>% filter(prov == 'Pichincha' | prov == 'Guayas' | prov == 'Azuay') , 
         aes(x = prov  , y = media , fill = sexo )) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = media - 1.96*media_se, ymax = media + 1.96*media_se), position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values =c("#647A8F","#FFAC8E")) +
  ylim(0,2.4) +
  labs(title = "2020", caption = "\n \n Fuente: Encuesta Multipropósito 2020", fill = "Sexo") +
  geom_text(aes(label = round(media, digits = 2)), color ="black", size = 4,position = position_dodge(0.9), vjust = -5.8) +
  xlab("Provincia") +
  ylab(" ") +
  theme_classic() +
  theme(plot.title = element_text(colour = "grey20", size=20)) +
  theme(plot.caption = element_text(colour = "grey30")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line = element_line(colour = 'grey60')) +
  theme(legend.position= c(0.87,1)) +
  theme(legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="grey30"))

plot3_articulo1 <- 
  p3_2012 + 
  p3_2019 + 
  p3_2020 +
  plot_layout(ncol = 3) + 
  plot_annotation(title = 'Promedio de horas semanales en hacer ejercicio o practicar algún deporte en 2012, 2019, y 2020 (por provincia y por sexo) \n',
                  caption = str_wrap('\n Nota: Las barras de error representan intervalos de confianza de 95% para la media', width = 100),
                  theme = theme(plot.title = element_text(hjust = 0.5, colour="grey20", size=14),
                                plot.caption = element_text(hjust = 0, colour="grey30")))

ggsave("images/grafico3-articulo-deporte.png", device = "png", width = 12.5, height = 7, dpi = 900)

