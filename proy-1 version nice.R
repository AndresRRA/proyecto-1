library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggdark)
library(tidyverse)

#Carga de datos
dat <- read.csv("liberia_datos_climaticos.csv", na.strings = c("", " ", "NA"),
                dec = ",")
##Eliminando celdas vacías
dat <- na.omit(dat)

##Revisión estructura
str(dat)

#Definiendo variables como números
dat$Date <- as.Date(dat$Date, format = "%d/%m/%Y")
dat$Temperatura..Celsius. <- as.numeric(dat$Temperatura..Celsius.)
dat$HumedadRelativa.... <- as.numeric(dat$HumedadRelativa....)
dat$VelocidadViento..m.s. <- as.numeric(dat$VelocidadViento..m.s.)
dat$Lluvia..mm. <- as.numeric(dat$Lluvia..mm.)
dat$Irradiacion..W.m2. <- as.numeric(dat$Irradiacion..W.m2.)
dat$EvapoTranspiracion..mm. <- as.numeric(dat$EvapoTranspiracion..mm.)

# Histogramas

##Temperatura
ggplot(dat, aes(x = Temperatura..Celsius.)) +
  geom_histogram(binwidth = 0.5,
                 color = "#00A1B3",
                 fill = "#00A1B3", alpha = 0.8) +
  ggtitle("Histograma Temperatura") +
  xlab("Temperatura (°C)")  +
ylab("Frecuencia") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Humedad relativa
ggplot(dat, aes(x = HumedadRelativa....)) +
    geom_histogram(binwidth = 0.5,
                   color = "#66CC33",
                   fill = "#66CC33", alpha = 0.8) +
    ggtitle("Histograma Humedad Relativa") +
    xlab("Humedad")  +
    ylab("Frecuencia") +
    dark_theme_gray(base_size = 14) + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.background = element_rect(fill = "grey20"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = c(0.815, 0.27))

##Velocidad viento
ggplot(dat, aes(x = VelocidadViento..m.s.)) +
  geom_histogram(binwidth = 0.5,
                 color = "#87CEFA",
                 fill = "#87CEFA", alpha = 0.8) +
  ggtitle("Histograma Velocidad Viento") +
  xlab("Velocidad (m/s)")  +
  ylab("Frecuencia") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Lluvia
ggplot(dat, aes(x = Lluvia..mm.)) +
  geom_histogram(binwidth = 10,
                 color = "#0000BF",
                 fill = "#0000BF", alpha = 0.8) +
  ggtitle("Histograma Lluvia") +
  xlab("Lluvia (mm)")  +
  ylab("Frecuencia") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Irradiación
ggplot(dat, aes(x = Irradiacion..W.m2.)) +
  geom_histogram(binwidth = 10,
                 color = "#FFD700",
                 fill = "#FFD700", alpha = 0.8) +
  ggtitle("Histograma Irradiación") +
  xlab("Irradiación (W/m^2)")  +
  ylab("Frecuencia") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Evapotranspiración
ggplot(dat, aes(x = EvapoTranspiracion..mm.)) +
  geom_histogram(binwidth = 0.5,
                 color = "#704098",
                 fill = "#704098", alpha = 0.8) +
  ggtitle("Histograma Evapotranspiración") +
  xlab("Evapotranspiración (mm)")  +
  ylab("Frecuencia") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

#Sacando promedios y sumas mensuales
##Promedio mensual Temperatura
prom_mensual_temp <- data.frame(Mes = 1:12,
                                Temperatura = tapply
                                (dat$Temperatura..Celsius., 
                                            format(dat$Date, format = "%m"), 
                                            FUN = mean))

###Gráfico del promedio
ggplot(prom_mensual_temp, aes(x = Mes, y = Temperatura)) +
  geom_line(color = "#00A1B3",) +
  geom_point(color = "#00A1B3") +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  ylim(22.5,30) +
  ggtitle("Promedio mensual temperatura") +
  xlab("Mes")  +
  ylab("Temperatura (°C)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Promedio mensual Humedad relativa
prom_mensual_humedad <- data.frame(Mes = 1:12,
                                Humedad = tapply
                                (dat$HumedadRelativa...., 
                                  format(dat$Date, format = "%m"), 
                                  FUN = mean))

###Gráfico del promedio
ggplot(prom_mensual_humedad, aes(x = Mes, y = Humedad)) +
  geom_line(color = "#66CC33",) +
  geom_point(color = "#66CC33") +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  ylim(55,85) +
  ggtitle("Promedio mensual humedad relativa") +
  xlab("Mes")  +
  ylab("Humedad relativa") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Promedio mensual Velocidad viento
prom_mensual_viento <- data.frame(Mes = 1:12,
                                   Velocidad = tapply
                                   (dat$VelocidadViento..m.s., 
                                     format(dat$Date, format = "%m"), 
                                     FUN = mean))

###Gráfico del promedio
ggplot(prom_mensual_viento, aes(x = Mes, y = Velocidad)) +
  geom_line(color = "#87CEFA",) +
  geom_point(color = "#87CEFA") +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  ylim(0,10) +
  ggtitle("Promedio mensual velocidad viento") +
  xlab("Mes")  +
  ylab("Velocidad (m/s)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Suma mensual Lluvia
suma_mensual_lluvia <- data.frame(Mes = 1:12,
                                  Lluvia = tapply
                                  (dat$Lluvia..mm., 
                                    format(dat$Date, format = "%m"), 
                                    FUN = sum))

###Gráfico de la suma
ggplot(suma_mensual_lluvia, aes(x = Mes, y = Lluvia)) +
  geom_line(color = "#0000BF",) +
  geom_point(color = "#0000BF") +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  ylim(0,1800) +
  ggtitle("Suma mensual lluvia") +
  xlab("Mes")  +
  ylab("Cantidad lluvia (mm)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Promedio mensual Irradiación
prom_mensual_irrad <- data.frame(Mes = 1:12,
                                  Irradiación = tapply
                                  (dat$Irradiacion..W.m2., 
                                    format(dat$Date, format = "%m"), 
                                    FUN = mean))

###Gráfico del promedio
ggplot(prom_mensual_irrad, aes(x = Mes, y = Irradiación)) +
  geom_line(color = "#FFD700",) +
  geom_point(color = "#FFD700") +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  ylim(170,280) +
  ggtitle("Promedio mensual Irradiación") +
  xlab("Mes")  +
  ylab("Irradiación (W/m^2)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

##Suma mensual Evapotranspiración
suma_mensual_evapo <- data.frame(Mes = 1:12,
                                  Evapotranspiración = tapply
                                  (dat$EvapoTranspiracion..mm., 
                                    format(dat$Date, format = "%m"), 
                                    FUN = sum))

###Gráfico de la suma
ggplot(suma_mensual_evapo, aes(x = Mes, y = Evapotranspiración)) +
  geom_line(color = "#704098") +
  geom_point(color = "#704098") +
  scale_x_continuous(breaks = seq(1, 12, by = 2)) +
  ylim(360, 960) +
  ggtitle("Suma mensual evapotranspiración") +
  xlab("Mes")  +
  ylab("Evapotranspiración (mm)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))


# Gráficos de dispersión

## Relación temperatura-lluvia
ggplot(dat, aes(x = Temperatura..Celsius.)) +
  geom_point(
    aes(y = Lluvia..mm.)
  ) +
  ggtitle("Relación diaria entre Temperatura y Lluvia") +
  xlab("Temperatura (°C)") +
  ylab("Lluvia (mm)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

## Relación temperatura-irradiación
ggplot(dat, aes(x = Temperatura..Celsius.)) +
  geom_point(
    aes(y = Irradiacion..W.m2.)
  ) +
  ggtitle("Relación diaria entre Temperatura e Irradiación") +
  xlab("Temperatura (°C)") +
  ylab("Irradiación (W/m^2") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

## Relación lluvia-evapotranspiración
ggplot(dat, aes(x = Lluvia..mm.)) +
  geom_point(
    aes(y = EvapoTranspiracion..mm.)
  ) +
  ggtitle("Relación diaria entre Lluvia y Evapotranspiración") +
  xlab("Lluvia (mm)") +
  ylab("Evapotranspiración (mm)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

## Relación humedad-lluvia
ggplot(dat, aes(x = Lluvia..mm.)) +
  geom_point(
    aes(y = HumedadRelativa....)
  ) +
  ggtitle("Relación diaria entre Lluvia y Humedad") +
  xlab("Lluvia (mm)") +
  ylab("Humedad relativa") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

## Relación irradiación-evapotranspiración
ggplot(dat, aes(x = Irradiacion..W.m2.)) +
  geom_point(
    aes(y = EvapoTranspiracion..mm.)
  ) +
  ggtitle("Relación diaria entre Irradiación y Evapotranspiración") +
  xlab("Irradiación (W/m^2)") +
  ylab("Evapotranspiración") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))

## Relación temperatura-viento
ggplot(dat, aes(x = Temperatura..Celsius.)) +
  geom_point(
    aes(y = VelocidadViento..m.s.)
  ) +
  ggtitle("Relación diaria entre Temperatura y Velocidad del viento") +
  xlab("Temperatura (°C)") +
  ylab("Velocidad (m/s)") +
  dark_theme_gray(base_size = 14) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.815, 0.27))