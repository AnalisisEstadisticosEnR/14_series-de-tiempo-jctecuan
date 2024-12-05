#Analisi en lenguaje R
#28-04-2022
#Series de Tiempo

#Juan Carlos Martinez

library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)

setwd("C:/Users/jcjag/OneDrive/Documentos/MAESTRIA-JC/CLASES/Analisis en Lenguaje R/05 Series de Tiempo")

monthly_milk <- read.csv("monthly_milk.csv")

daily_milk <- read.csv("daily_milk.csv")

#Formato para series de tiempo AAAA-MM-DD HH:MM:SS 
#Si el las horas no son importantes, se pueden quitar

#Revisar base

head(monthly_milk)

class(monthly_milk)

class(monthly_milk$month)

#Cambiar a clase Fecha(Date) con as. Date()

monthly_milk$month_date <- as.Date(monthly_milk$month, format= "%Y-%m-%d")

class(monthly_milk$month_date)

#Diferentes codigos de fecha

format(monthly_milk$month_date, format = "%Y-%B-%u")
class(format(monthly_milk$month_date, format = "%Y-%B-%u"))

format(monthly_milk$month_date, format = "%Y-%j")
class(format(monthly_milk$month_date, format = "%Y-%j"))

format(monthly_milk$month_date, format = "%y-%B-%d-%a-%j")
class(format(monthly_milk$month_date, format = "%y-%B-%d-%a-%j"))

#FECHAS Y HORAS

head(daily_milk)

class(daily_milk)

#Cambiar clase POSIXct POSIXt, para formato FECHA/HORA

daily_milk$date_time_posix <- as.POSIXct(daily_milk$date_time,
                                         format = "%Y-%m-%d %H:%M:%S")

class(daily_milk$date_time_posix)

#Correcion de datos de fecha mal formateados

monthly_milk$bad_date <- format(monthly_milk$month_date, 
                                format = "%d/%b/%Y-%u")
head(monthly_milk$bad_date)

class(monthly_milk$bad_date)

#trasformar con as.Date() especificando formato

monthly_milk$good_date <- as.Date(monthly_milk$bad_date, format = "%d/%b/%Y-%u")
head(monthly_milk$good_date)
class(monthly_milk$good_date)

#VISUALIZACION DE DATOS DE SERIES TEMPORALES

#Uso de scale_x_date()

(time_plot <- ggplot(monthly_milk, aes(x = month_date, 
                                       y = milk_prod_per_cow_kg)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())

#01_T14_prod_milk.png

#Trazado de fecha y hora

(time_plot_2 <- ggplot(daily_milk, aes(x = date_time_posix, 
                                         y = milk_prod_per_cow_kg)) +
    geom_line() +
    scale_x_datetime(date_labels = "%p-%d", date_breaks = "72 hour") +
    theme_classic())

#02_T14_prod_milk_24hrs

## ANALISIS ESTADISTICO DE DATOS DE SERIES TEMPORALES

#Descomposicion

(decomp_1 <- ggplot(monthly_milk, aes(x = month_date, y = milk_prod_per_cow_kg)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())

#Trazar regresion de "Loess", que ajusta una curva entre dos variables

(decomp_2 <- ggplot(monthly_milk, aes(x = month_date, y = milk_prod_per_cow_kg)) +
    geom_line() +
    geom_smooth(method = "loess", se = F, span = 0.5) +
    theme_classic())

#03_T14_milk_prod_loess.png

#Patron estacional, trazar produccion de leche por año

#Extraer mes y año en columnas separadas

monthly_milk$year <- format(monthly_milk$month_date, format = "%Y")
monthly_milk$month_num <- format(monthly_milk$month_date, format = "%m")

#Crear paleta de colores usando colorstools

year_pal <- sequential(color = "#3B244F", percentage = 5, what = "value")



#Graficar

(seasonal <- ggplot(monthly_milk, aes(x = month_num, y = milk_prod_per_cow_kg, group = year)) +
    geom_line(aes(colour = year)) +
    theme_classic() +
    scale_color_manual(values = year_pal))

#Metodo 2

#Convertir DF de serie temporal en objeto de clase ts y descomponerlo con stl()

#Transforma a clase ts

library(stats)

monthly_milk_ts <- ts(monthly_milk$milk_prod, start = 1962, end = 1975, freq = 12)

#Descomponer usando stl()

monthly_milk_ts <- stl(monthly_milk_ts, s.window = "period")

plot(monthly_milk_ts)

#04_T14_prod_milk_stl.png

monthplot(monthly_milk_ts)

#05_T14_prod_milk_stl_mes.png

seasonplot(monthly_milk_ts)


#PRONOSTICO

monthly_milk_model <- window(x = monthly_milk_ts, start= c(1962), end = c(1970)) #Error

monthly_milk_test <- window(x = monthly_milk_ts, start = c(1970))

#Comparar los pronosticos de diferentes modelos ETS

#Crear objetos de modelo de cada tipo de modelo ETS

milk_ets_auto <- ets(monthly_milk_model)
milk_ets_mmm <- ets(monthly_milk_model, model = "MMM")
milk_ets_zzz <- ets(monthly_milk_model, model = "ZZZ")
milk_ets_mmm_damped <- ets(monthly_milk_model, model = "MMM", damped = TRUE)

#Crear objeto pronostico de cada objeto modelo

milk_ets_fc <- forecast(milk_ets_auto, h = 60)
milk_ets_mmm_fc <- forecast(milk_ets_mmm, h = 60)
milk_ets_zzz_fc <- forecast(milk_ets_zzz, h = 60)
milk_ets_mmm_damped_fc <- forecast(milk_ets_mmm_damped, h = 60)

#Convertir pronostico a base de datos

milk_ets_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_fc)), as.data.frame(milk_ets_fc))
#Quita espacios en blanco
names(milk_ets_fc_df) <- gsub(" ", "_", names(milk_ets_fc_df))

#antepon el dia del mes
milk_ets_fc_df$Date <- as.Date(paste("01-", milk_ets_fc_df$Month, sep = ""), format = "%d-%b %Y")
#Agrega columna del tipo modelo
milk_ets_fc_df$Model <- rep("ets") 


milk_ets_mmm_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_fc)), as.data.frame(milk_ets_mmm_fc))
names(milk_ets_mmm_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_fc_df))
milk_ets_mmm_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_mmm_fc_df$Model <- rep("ets_mmm")

milk_ets_zzz_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_zzz_fc)), as.data.frame(milk_ets_zzz_fc))
names(milk_ets_zzz_fc_df) <- gsub(" ", "_", names(milk_ets_zzz_fc_df))
milk_ets_zzz_fc_df$Date <- as.Date(paste("01-", milk_ets_zzz_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_zzz_fc_df$Model <- rep("ets_zzz")

milk_ets_mmm_damped_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_damped_fc)), as.data.frame(milk_ets_mmm_damped_fc))
names(milk_ets_mmm_damped_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_damped_fc_df))
milk_ets_mmm_damped_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_damped_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_mmm_damped_fc_df$Model <- rep("ets_mmm_damped")

#Combinar en una base de datos

forecast_all <- rbind(milk_ets_fc_df, milk_ets_mmm_fc_df, milk_ets_zzz_fc_df, milk_ets_mmm_damped_fc_df)

#Ahora grafica

(forecast_plot <- ggplot() +
    geom_line(data = monthly_milk, aes(x = month_date, y = milk_prod_per_cow_kg)) +
    geom_line(data = forecast_all, aes(x = Date, y = Point_Forecast, colour = Model)) +
    theme_classic())

#No salieron las lineas de los pronosticos y no supe por qué

#Comparar numericamente precision de diferentes modelos con accuracy()

accuracy(milk_ets_fc, monthly_milk_test)
accuracy(milk_ets_mmm_fc, monthly_milk_test)
accuracy(milk_ets_zzz_fc, monthly_milk_test)
accuracy(milk_ets_mmm_damped_fc, monthly_milk_test)


#Extraer valores de un pronostico

milk_ets_fc_df %>% filter(Month == "Jan 1975") %>% 
  select(Month, Point_Forecast)

milk_ets_zzz_fc_df %>% filter(Month == "Jan 1975") %>% 
  select(Month, Point_Forecast)


#Desafio de codificacion 

co2_data <- read.csv("co2_loa.csv", stringsAsFactors = FALSE)


#Error

co2_data$month <- as.factor(co2_data$month)

co2_data$month <- strptime(co2_data$month,format="%Y-%m") 

co2_data$month <- as.Date(co2_data$month, format = "%Y-%m")

ggplot(co2_data, aes(x = month, y = co2_conc)) +
  geom_line() +
  theme_classic()

                        