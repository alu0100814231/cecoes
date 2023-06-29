# cecoes

setwd("C:/Users/Bianca/Desktop/TFG/Redacci?n y scripts")
library(data.table)
library(xlsx)
library(readxl)
library(writexl)
library(ggplot2)

library(dplyr)
library(lubridate)
setwd("C:/Users/bianc/Desktop/TFG")
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(forecast)

load("data112_cargas_trabajo.RData")
data112

#data112$fecha<-strptime(data112$fecha,format="%Y/%m/%d %H:%M")
#data112$fecha_hora <- as.POSIXct(data112$fecha, format = "%d/%m/%Y %H:%M")
data112$fecha <- as.Date(data112$fecha)
data112$dia<-day(data112$fecha)
data112$mes<-month.name[month(data112$fecha)]
data112$mes <-month(data112$fecha,label=TRUE,abbr=FALSE)
data112$anio<-year(data112$fecha)

#contamos el número de llamadas que hay por día, por mes y por servicio
num_llamadas_servicio <- data112 %>%
  group_by(servicio,mes, dia) %>%
  summarise(num_llamadas_dia = n())

# Generar todas las combinaciones posibles de servicio, mes y día
combinaciones2 <- expand.grid(servicio = unique(data112$servicio),
                             mes = unique(data112$mes),
                             dia = unique(data112$dia))

# Unir con la tabla original y rellenar los valores faltantes con 0
num_llamadas_servicio <- combinaciones2 %>%
  left_join(num_llamadas_servicio, by = c("servicio", "mes", "dia")) %>%
  replace_na(list(num_llamadas_dia = 0))

#contamos el número de llamadas que hay por día, por mes y por trabajador
num_llamadas_trabajador <- data112 %>%
  group_by(login,mes, dia) %>%
  summarise(num_llamadas_dia = n())

# Generar todas las combinaciones posibles de trabajadores, días y meses
combinaciones <- expand.grid(login = unique(data112$login),
                             mes = unique(data112$mes),
                             dia = unique(data112$dia))

# Unir con la tabla original y rellenar los valores faltantes con 0
num_llamadas_trabajador <- combinaciones %>%
  left_join(num_llamadas_trabajador, by = c("login", "mes", "dia")) %>%
  replace_na(list(num_llamadas_dia = 0))

#Filtramos un solo servicio 'INFOCORONAVIRUS' e 'GESTION DE RECURSOS SANITARIOS'

num_llamadas_infocorona <- num_llamadas_servicio %>%
  filter(servicio == 'INFOCORONAVIRUS') 

num_llamadas_recursos_sanitarios <- num_llamadas_servicio %>%
  filter(servicio == 'GESTION DE RECURSOS SANITARIOS')

#Gráficas

ggplot(num_llamadas_infocorona, aes(x = dia, y = num_llamadas_dia)) +
  geom_line(col = 'hotpink') +
  ylim(0, 200) +
  xlab("Día") +
  ylab("Número de llamadas") +
  ggtitle("Número de llamadas de INFOCORONAVIRUS por día") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

#Agrupamos por mes cada tabla NO ME SIRVE

ggplot(num_llamadas_infocorona_2, aes(x = mes, y = num_llamadas_mes)) +
  geom_line(col = 'hotpink') +
  xlab("Mes") +
  ylab("Número de llamadas") +
  scale_x_discrete(labels = month.name) +
  theme_bw() +
  theme(legend.title = element_blank())
  
#Gráfica 
ggplot(num_llamadas_infocorona_2, aes(x = mes, y = num_llamadas_mes)) +
  geom_line(col = 'hotpink') +
  ylim(0, 5000) +
  xlab("Mes") +
  ylab("Número de llamadas") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(num_llamadas_recursos_sanitarios_2, aes(x = mes, y = num_llamadas_mes)) +
  geom_line(col = 'hotpink') +
  ylim(0, 250) +
  xlab("mes") +
  ylab("Número de llamadas") +
  scale_x_continuous(breaks = 1:31) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

#filtramos las dos tablas anteriores para el mes de marzo

num_llamadas_infocorona_marzo <- num_llamadas_infocorona %>%
  filter(mes == 'marzo')

num_llamadas_recursos_sanitarios_marzo <- num_llamadas_recursos_sanitarios %>%
  filter(mes=='marzo')

#Gráfica 
ggplot(num_llamadas_infocorona_marzo, aes(x = dia, y = num_llamadas_dia)) +
  geom_line(col = 'hotpink') +
  ylim(0, 250) +
  xlab("Día") +
  ylab("Número de llamadas") +
  scale_x_continuous(breaks = 1:31) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(num_llamadas_recursos_sanitarios_marzo, aes(x = dia, y = num_llamadas_dia)) +
  geom_line(col = 'hotpink') +
  ylim(0, 250) +
  xlab("Día") +
  ylab("Número de llamadas") +
  scale_x_continuous(breaks = 1:31) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

#En vez de coger dia y mes vamos a coger la fecha directamente

num_llamadas_servicio_3 <- data112 %>%
  group_by(servicio,fecha) %>%
  summarise(num_llamadas = n())

# Generar todas las combinaciones posibles de servicio y fecha
combinaciones <- expand.grid(servicio = unique(num_llamadas_servicio_3$servicio),
                             fecha = unique(num_llamadas_servicio_3$fecha))

# Unir con la tabla original y rellenar los valores faltantes con cero
num_llamadas_completas <- combinaciones %>%
  left_join(num_llamadas_servicio_3, by = c("servicio", "fecha")) %>%
  replace_na(list(num_llamadas = 0))
#filtramos por los servicios

num_llamadas_infocorona_3 <- num_llamadas_completas %>%
  filter(servicio == 'INFOCORONAVIRUS') %>%
  filter(fecha >= '2020-02-01' & fecha <= '2020-08-31') %>%
  group_by(fecha) %>%
  summarise(num_llamadas = sum(num_llamadas))

num_llamadas_infocorona_val <- num_llamadas_completas %>%
  filter(servicio == 'INFOCORONAVIRUS') %>%
  filter(fecha >= '2020-09-01' & fecha <= '2020-09-14') %>%
  group_by(fecha) %>%
  summarise(num_llamadas = sum(num_llamadas))

ggplot(num_llamadas_infocorona_val, aes(x=fecha, y=num_llamadas)) +
  geom_line(col = 'green') +
  ylim(0, 300) +
  xlab("Tiempo") + ylab ("Número de llamadas") +
  scale_x_date(date_labels = "%Y %m", date_breaks = "1 month") +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Llamadas para información COVID-19")




num_llamadas_sanitarios_3 <- num_llamadas_completas %>%
filter(servicio == 'GESTION DE RECURSOS SANITARIOS')  %>%
group_by(fecha)%>%
  summarise(num_llamadas = sum(num_llamadas))




# Gráfica 1: num_llamadas_infocorona_3
ggplot(num_llamadas_infocorona_3, aes(x=fecha, y=num_llamadas)) +
  geom_line(col = 'green') +
  ylim(0, 300) +
  xlab("Tiempo") + ylab ("Número de llamadas") +
  scale_x_date(date_labels = "%Y %m", date_breaks = "1 month") +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Llamadas para información COVID-19")

# Gráfica 2: num_llamadas_sanitarios_3
ggplot(num_llamadas_sanitarios_3, aes(x=fecha, y=num_llamadas)) +
  geom_line(col = 'hotpink') +
  ylim(100, 175) +
  xlab("Tiempo") + ylab ("Número de llamadas") +
  scale_x_date(date_labels = "%Y %m", date_breaks = "1 month") +
  theme_bw() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  ggtitle("Llamadas para recursos sanitarios")

#Filtramos los meses de marzo a junio

num_llamadas_infocorona_4 <- num_llamadas_infocorona_3 %>%
  filter(fecha > '2020-03-01' & fecha < '2020-05-31') 
  


num_llamadas_sanitarios_4 <- num_llamadas_sanitarios_3 %>%
  filter(fecha > '2020-03-01' & fecha < '2020-05-31')

#Analizar y visualizar nuestras tablas
ggplot(num_llamadas_infocorona_4, aes(x=fecha,y=num_llamadas)) + 
  geom_line(col = 'hotpink') + 
  ylim(0,300) + xlab("Tiempo") + ylab ("Número de llamadas") + 
  scale_x_date(date_labels = "%Y %m", date_breaks = "1 month") + 
  theme_bw() + theme(legend.title = element_blank(), 
                     axis.text.x = element_text(angle = 45, vjust = 0,5))

ggplot(num_llamadas_sanitarios_4, aes(x=fecha,y=num_llamadas)) + 
  geom_line(col = 'hotpink') + 
  ylim(100,175) + xlab("Tiempo") + ylab ("Número de llamadas") + 
  scale_x_date(date_labels = "%Y %m", date_breaks = "1 month") + 
  theme_bw() + theme(legend.title = element_blank(), 
                     axis.text.x = element_text(angle = 45, vjust = 0,5),
                     text = element_text(family = "Arial"))



#verifiquemos el ACF y el PACF de los datos

par(mfrow=c(1,2)) 
acf(as.ts(num_llamadas_infocorona_3$num_llamadas), main="Nº llamadas") 
pacf(as.ts(num_llamadas_infocorona_3$num_llamadas), main="Nº llamadas" )


par(mfrow=c(1,2)) 
acf(as.ts(num_llamadas_sanitarios_3$num_llamadas), main="Nº llamadas") 
pacf(as.ts(num_llamadas_sanitarios_3$num_llamadas), main="Nº llamadas" )


adfTest(num_llamadas_sanitarios_3$num_llamadas)

#ARIMA para número de llamadas para info coronavirus todo el año
# Dividir el conjunto de datos para entrenamiento y prueba
indice_entrenamiento <- 30
n_total <- nrow(num_llamadas_infocorona_3)
num_llamadas_tren <- num_llamadas_infocorona_3[1:indice_entrenamiento, ]
num_llamadas_test <- num_llamadas_infocorona_3[(indice_entrenamiento + 1):n_total, ]
predicho <- numeric(n_total - indice_entrenamiento)

# Entrenar los datos
for (i in 1:(n_total - indice_entrenamiento)) {
  num_llamadas_tren_1 <- num_llamadas_infocorona_3[1:(indice_entrenamiento - 1 + i), ]
  arima_model <- auto.arima(as.ts(num_llamadas_tren_1$num_llamadas))
  pred <- forecast(arima_model, h = 1)
  predicho[i] <- pred$mean
}

# Crear el tibble df_pred
df_pred <- tibble(obs = c(num_llamadas_tren$num_llamadas, num_llamadas_test$num_llamadas),
                  predicho = c(num_llamadas_tren$num_llamadas, predicho),
                  tiempo = num_llamadas_infocorona_3$fecha)


# Trazar el resultado
ggplot(df_pred, aes(x = tiempo, y = predicho, linetype = "predicted")) +
  geom_line() +
  geom_line(aes(y = obs, col = "obs", linetype = "obs")) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("predicted" = "black", "obs" = "hotpink")) +
  scale_linetype_manual(values = c("predicted" = 2 , "obs" = 1)) +
  scale_x_date(date_labels = "%y %b", date_breaks = "1 month") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.key = element_blank(),
        legend.box.background = element_blank())


#de marzo a mayo

indice_entrenamiento_2 <- 30
n_total_2 <- nrow(num_llamadas_infocorona_4)
num_llamadas_tren_2 <- num_llamadas_infocorona_4[1:indice_entrenamiento_2, ]
num_llamadas_test_2 <- num_llamadas_infocorona_4[(indice_entrenamiento_2 + 1):n_total_2, ]
predicho_2 <- numeric(n_total_2 - indice_entrenamiento_2)

# Entrenar los datos
for (i in 1:(n_total_2 - indice_entrenamiento_2)) {
  num_llamadas_tren_1_2 <- num_llamadas_infocorona_4[1:(indice_entrenamiento_2 - 1 + i), ]
  arima_model_2 <- auto.arima(as.ts(num_llamadas_tren_1_2$num_llamadas))
  pred_2 <- forecast(arima_model_2, h = 1)
  predicho_2[i] <- pred_2$mean
}

# Crear el tibble df_pred
df_pred_2 <- tibble(obs = c(num_llamadas_tren_2$num_llamadas, num_llamadas_test_2$num_llamadas),
                  predicho_2 = c(num_llamadas_tren_2$num_llamadas, predicho_2),
                  tiempo_2 = num_llamadas_infocorona_4$fecha)

# Trazar el resultado
ggplot(df_pred_2, aes(x = tiempo_2, y = predicho_2, col = "predicted", linetype = "predicted")) +
  geom_line() +
  geom_line(aes(y = obs, col = "obs", linetype = "obs")) +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("predicted" = "black", "obs" = "hotpink")) +
  scale_linetype_manual(values = c("predicted" = 2, "obs" = 1)) +
  scale_x_date(date_labels = "%y %b", date_breaks = "1 month") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5))


## ARIMA 2 ##

library(ggplot2)
library(ggfortify)
library(gridExtra)



# Gráfico 1: num_llamadas_infocorona_3
 ggplot(num_llamadas_infocorona_3, aes(x = fecha, y = num_llamadas)) +
  geom_line(col = 'green') +
  xlab("Fecha") +
  ylab("Número de llamadas") +
  ggtitle("Número de llamadas para información COVID-19")

# Convertir la serie de llamadas en un objeto de clase "ts"
serie_llamadas <- ts(num_llamadas_infocorona_3$num_llamadas)

serie_llamadas
# Crear un marco de datos con la serie diferenciada y la columna "fecha"
df_diferenciado <- data.frame(fecha = num_llamadas_infocorona_3$fecha[-1],
                              diferencia = diff(serie_llamadas))

# Graficar la serie diferenciada
ggplot(df_diferenciado, aes(x = fecha, y = diferencia)) +
  geom_line(col = 'hotpink') +
  xlab("Fecha") +
  ylab("Diferencia en el número de llamadas") +
  ggtitle("Número de llamadas para información COVID-19 (diferenciado)")

# Gráfico 3: ACF de num_llamadas_infocorona_3
ggAcf(num_llamadas_infocorona_3$num_llamadas)

# Gráfico 4: ACF del número de llamadas diferenciadas
ggAcf(diff(serie_llamadas))

par(mfrow=c(1,2)) 
acf(as.ts(serie_llamadas), main="Nº llamadas") 
pacf(as.ts(serie_llamadas), main="Nº llamadas" )


# Ajuste automáticamente múltiples modelos y encuentre el AICc más bajo
fit_serie_llamadas <- auto.arima(serie_llamadas, seasonal = FALSE)

# Verificar el resumen del informe del modelo
summary(fit_serie_llamadas)

# 4. Plot ACF of the residuals and do a portmanteau test to make sure they look like white noise. 
checkresiduals(fit_serie_llamadas)

checkresiduals(fit_serie_llamadas)
ylab("Nuevo Label del Eje Y")

# 5. Una vez que los residuos parezcan ruido blanco, calcule los pronósticos. 
autoplot(forecast(fit_serie_llamadas, h=10)) + 
  labs(x = "Días", y="Nº Llamadas")

forecast(fit_serie_llamadas, h=10)


# Ajuste automáticamente múltiples modelos y encuentre el AICc más bajo
fit_serie_llamadas <- auto.arima(serie_llamadas, seasonal = FALSE)

# Verificar el resumen del informe del modelo
summary(fit_serie_llamadas)

# Ajustar el modelo ARIMA a la serie de tiempo
fit <- auto.arima(df_diferenciado)

# Calcular el pronóstico para 10 períodos hacia adelante
forecast <- forecast(fit, h = 10)

# Generar la gráfica del pronóstico
autoplot(forecast) +
  labs(x = "Fecha", y = "Número de llamadas", title = "Pronóstico de llamadas para información COVID-19") 




load("data112_cargas_trabajo.RData")
data112

#data112$fecha<-strptime(data112$fecha,format="%Y/%m/%d %H:%M")
data112$dia<-day(data112$fecha)
#data112$mes<-month.name[month(data112$fecha)]
data112$mes <-month(data112$fecha,label=TRUE,abbr=FALSE)
data112$anio<-year(data112$fecha)


#Veamos el n?mero de contactos que un trabajador realiza a lo largo del a?o
data112_1 <- data112 %>%
  group_by(login) %>%
  summarize(n=n())

#suma total de la duraci?n de las llamadas por empleado
data112_2 <- data112 %>%
  group_by(login) %>%
  summarize(l= sum(duracion.total))
data112_2$lhoras<- (data112_2$l/60)

#filter(login=="TCVV") %>%
  
#suma total de la duraci?n de las llamadas por servicio
data112_3 <- data112 %>%
  group_by(servicio) %>%
  summarize(s= sum(duracion.total))
data112_3$shoras<- (data112_3$s/60)


library(ggplot2)

ggplot(data112_3) +
 aes(x = servicio, weight = shoras) +
 geom_bar(fill = "#1f9e89") +
 labs(x = "Sectores", y = "Tiempo en horas", title = "Duraci?n de las llamadas por sector") +
 coord_flip() +
 theme_minimal()


#suma total de la duraci?n de las llamadas por provincia
data112_4 <- data112 %>%
  group_by(prov) %>%
  summarize(p= sum(duracion.total))
data112_4$phoras<- (data112_4$p/60)



library(ggplot2)

ggplot(data112_4) +
 aes(x = prov, y = phoras) +
 geom_bar(stat="identity", fill = "#0c4c8a") +
 labs(x = "Provincia ", y = "Tiempo en horas", title = "Duraci?n del total de llamadas por provincia") +
 theme_minimal()


ggplot(data112_4) +
  aes(x = "", y = phoras, fill=prov) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0)+
  labs(x = "Provincia ", y = "Tiempo en horas", title = "Duraci?n del total de llamadas por provincia") +
  theme_minimal()

#suma total de la duraci?n de las llamadas a lo largo del a?o
data112_2 %>%
summarize(p=sum(l))

#Suma del total de duraci?n de las llamadad por dia y por mes#
data112_6 <- data112 %>%
  select(duracion.total, mes) %>%
  group_by(mes) %>%
  summarize(Total_m=sum(duracion.total))
data112_6$total_h <- (data112_6$Total_m/60)

data112_7 <-data112_6 %>%
  group_by(mes) %>%
  summarize(m=(Total_m/341649)*100)

library(ggplot2)

ggplot(data112_7) +
 aes(x = mes, y = m, colour = mes) +
 geom_boxplot(fill = "#fa9e3b") +
 scale_color_hue() +
 labs(x = "Meses", y = "Minutos", title = "Media de la duraci?n total de las llamadas", subtitle = "Tiempo en minutos") +
 theme_minimal()


ggplot(data112) +
  aes(x = mes, y = duracion.total, colour = mes) +
  geom_boxplot(fill = "#fa9e3b") +
  scale_color_hue() +
  ylim(c(0,100))+
  labs(x = "Meses", y = "Minutos", title = "Media de la duraci?n total de las llamadas", subtitle = "Tiempo en minutos") +
  theme_minimal()

#Suma de la duraci?n de ACW por empleado en minutos


data112_8.2 <- data112 %>%
  select(ACW.duracion, mes) %>%
  group_by(mes) %>%
  summarize(Total_ACW=sum(ACW.duracion))
data112_8.2$horas8.2 <- (data112_8.2$Total_ACW/60)

library(ggplot2)

ggplot(data112_8.2) +
 aes(x = mes, weight = horas8.2) +
 geom_bar(fill = "#d875af") +
 labs(x = "Meses", y = "Horas", title = "Tiempo de trabajo antes de la llamada") +
 theme_minimal()


data112_8 <- data112 %>%
  select(ACW.duracion, login, mes) %>%
  group_by(login, mes) %>%
  summarize(Total_ACW=sum(ACW.duracion))
data112_8$horas8 <- (data112_8$Total_ACW/60)



ggplot(data112_8) +
 aes(x = mes, weight = horas8) +
 geom_bar(fill = "#6dcd59") +
 labs(x = "Meses", y = "Tiempo en minutos", title = "Tiempo de trabajo posterior a la llamada") +
 coord_flip() +
 theme_minimal()




data112_9.2 <- data112 %>%
  select(BCW.duracion, mes) %>%
  group_by(mes) %>%
  summarize(Total_BCW2=sum(BCW.duracion))
data112_9.2$horas9.2 <- (data112_9.2$Total_BCW2/60)

library(ggplot2)

ggplot(data112_9.2) +
 aes(x = mes, weight = horas9.2) +
 geom_bar(fill = "#f4b46f") +
 labs(x = "Meses", y = "Horas", title = "Tiempo de trabajo después de la llamada") +
 theme_minimal()


data112_14.2 <- data112 %>%
  group_by(mes) %>%
  summarize(loc_m= sum(locucion.duracion))
data112_14.2$loc_h<- (data112_14.2$loc_m/60)

Tabla89.2 <- merge( data112_8.2, data112_9.2, by="mes", all=TRUE)

Tabla1489.2 <- merge( Tabla89.2, data112_14.2, by="mes", all=TRUE)


totales <- Tabla1489.2 %>%
  select(horas8.2,horas9.2,loc_h, mes) %>%
  summarize(totalACW=sum(horas8.2),
            totalBWC=sum(horas9.2),
            totalLOC=sum(loc_h))



#Suma de la duraci?n de BCW por empleado en minutos
data112_9 <- data112 %>%
  select(BCW.duracion, login) %>%
  group_by(login, mes) %>%
  summarize(Total_BCW=sum(BCW.duracion))

library(ggplot2)

ggplot(data112_9) +
 aes(x = mes, weight = Total_BCW) +
 geom_bar(fill = "#0c4c8a") +
 labs(x = " Meses", y = "Minutos", title = "Total de la duraci?n del trabajo antes de la llamada") +
 coord_flip() +
 theme_minimal()

#suma total de la duraci?n de las llamadas por empleado
data112_10 <- data112 %>%
  group_by(login) %>%
  summarize(lm= sum(duracion.total))
data112_2$lmhoras<- (data112_2$lm/60)


#Unimos las tablas de duraci?n total, duracion de ACW y BCW

Tabla89 <- merge( data112_8, data112_9, by="mes", all=TRUE)

Tabla1089 <- merge( Tabla89, data112_10, by="login", all=TRUE)


#Quiero filtrar un empleado para ver el tiempo total que invierte en la llamada, en EL ACW y el BCW
tabla.f1089 <- Tabla1089 %>%
  group_by(l, Total_ACW, Total_BCW)%>%
  filter(login=="PSSM")

#lo que intentaba hacer se debe hacer por POWERBI
#Suma de la duraci?n de ACW por empleado en minutos

data112_11 <- data112 %>%
  select(ACW.duracion, login, dia) %>%
  group_by(login, dia) %>%
  summarize(Total_ACW=sum(ACW.duracion))

#Suma de la duraci?n de BCW por empleado en minutos
data112_12 <- data112 %>%
  select(BCW.duracion, login, dia) %>%
  group_by(login, dia) %>%
  summarize(Total_BCW=sum(BCW.duracion))

#suma total de la duraci?n de las llamadas por empleado
data112_13 <- data112 %>%
  group_by(login,dia) %>%
  summarize(ld= sum(duracion.total))
data112_2$ldhoras<- (data112_2$ld/60)

#suma total de la duraci?n de las llamadas por empleado
data112_14 <- data112 %>%
  group_by(login,mes) %>%
  summarize(lt= sum(locucion.duracion))
data112_2$lthoras<- (data112_2$lt/60)

library(ggplot2)

ggplot(data112_14) +
 aes(x = mes, y = lt) +
 geom_boxplot(fill = "#0c4c8a") +
 labs(x = "Meses", y = "Tiempo en horas", title = "Duraci?n de la llamada por mes") +
 theme_minimal()




#Unimos las tablas de duraci?n total, duracion de ACW y BCW

Tabla89 <- merge( data112_8, data112_9, by="login, dia", all=TRUE)

Tabla1089 <- merge( Tabla89, data112_10, by="login", all=TRUE)

#Quiero filtrar un empleado para ver el tiempo total que invierte en la llamada, en EL ACW y el BCW
tabla.f289 <- Tabla289 %>%
  group_by(l, Total_ACW, Total_BCW)%>%
  filter(login=="PSSM")
