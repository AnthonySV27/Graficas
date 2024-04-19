library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)

datos <- read_xlsx("C:\\Anthony\\Anthony\\Tec\\Semestre I 2024\\Visualización de Información\\Proyecto I\\proyecto1.xlsx")

# GRAFICA 1
datos_contados <- summarise(group_by(datos, `TIPO SERVICIO`), Cantidad = n())
ggplot(datos_contados, aes(x = `TIPO SERVICIO`, y = Cantidad)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Tipo de Servicio", y = "Cantidad", title = "Cantidad por Tipo de Servicio")

# GRAFICA 2
datos_contados1 <- summarise(group_by(datos, `CHOFER`), Cantidad1 = n())
ggplot(datos_contados1, aes(x = `CHOFER`, y = Cantidad1)) +
       geom_bar(stat = "identity", fill = "steelblue") +
       labs(x = "CHOFERES", y = "Cantidad Viajes", title = "Cantidad de viajes por Chofer") +
       theme(axis.text.x = element_text(angle = -90))

# GRAFICA 3
datos_contados2 <- summarise(group_by(datos, `DIPUTADO (A)`), Cantidad2 = n())
ggplot(datos_contados2, aes(x = `DIPUTADO (A)`, y = Cantidad2)) +
       geom_bar(stat = "identity", fill = "steelblue") +
       labs(x = "DIPUTADOS", y = "Cantidad Viajes", title = "Cantidad de viajes por Diputado")+
       theme(axis.text.x = element_text(angle = -90))

# GRAFICA 4
sum_km_por_chofer <- datos %>%
     group_by(CHOFER) %>%
     summarise(total_km = sum(`TOTAL KM`))

# Crear un dataframe con los datos de suma por chofer
df <- data.frame(Chofer = sum_km_por_chofer$CHOFER, Total_KM = sum_km_por_chofer$total_km)

# Crear el gráfico de barras con ggplot2 y etiquetas en ángulo diagonal
ggplot(df, aes(x = Chofer, y = Total_KM, fill = Chofer)) +
  geom_bar(stat = "identity") +
  labs(x = "Choferes", y = "Total de KM", title = "Total de KM por Chofer") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(fill = FALSE)


#GRAFICA 5
# Crear un dataframe con los datos de suma por diputado
df_diputados <- data.frame(Diputado = datos$`DIPUTADO (A)`, Total_KM = datos$`TOTAL KM`)

# Crear el gráfico de barras con ggplot2 y etiquetas en ángulo diagonal
ggplot(df_diputados, aes(x = Diputado, y = Total_KM, fill = Diputado)) +
  geom_bar(stat = "identity") +
  labs(x = "Diputados", y = "Total de KM", title = "Total de KM por Diputado") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  guides(fill = FALSE)





#GRAFICA FACETAS

 # Convertir la columna de fecha a formato de fecha
   datos$`FECHA INICIAL` <- as.Date(datos$`FECHA INICIAL`, format = "%d/%m/%Y")
 
   # Filtrar los datos por los años 2018, 2019 y 2020
   datos_filtrados <- datos %>%
       filter(year(`FECHA INICIAL`) %in% c(2018, 2019, 2020))
 
   # Contar la cantidad de viajes por chofer para cada año
   datos_cont <- datos_filtrados %>%
       group_by(CHOFER, year(`FECHA INICIAL`)) %>%
       summarise(Cantidad_Viajes = n(), .groups = "drop")
 ggplot(datos_cont, aes(x = CHOFER)) +
       geom_bar(aes(y = Cantidad_Viajes, fill = factor(`year(\`FECHA INICIAL\`)`)), stat = "identity") +
       labs(x = "CHOFERES", y = "Cantidad Viajes", title = "Cantidad de viajes por Chofer (2018-2020)") +
       facet_wrap(~ `year(\`FECHA INICIAL\`)`, scales = "free_x") +
       scale_fill_manual(values = c("steelblue", "salmon", "darkgreen")) +
       theme(axis.text.x = element_text(angle = -90))

#GRAFICA COMPUESTA 
 
 library(ggplot2)
 library(gridExtra)
 
 # GRAFICA 2
 datos_contados1 <- summarise(group_by(datos, `CHOFER`), Cantidad1 = n())
 grafica_2 <- ggplot(datos_contados1, aes(x = `CHOFER`, y = Cantidad1)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   labs(x = "CHOFERES", y = "Cantidad Viajes", title = "Cantidad de viajes por Chofer") +
   theme(axis.text.x = element_text(angle = -50))
 
 # GRAFICA 3
 datos_contados2 <- summarise(group_by(datos, `DIPUTADO (A)`), Cantidad2 = n())
 grafica_3 <- ggplot(datos_contados2, aes(x = `DIPUTADO (A)`, y = Cantidad2)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   labs(x = "DIPUTADOS", y = "Cantidad Viajes", title = "Cantidad de viajes por Diputado") +
   theme(axis.text.x = element_text(angle = -90))
 
 # Combinar las dos gráficas en una sola
 grid.arrange(grafica_2, grafica_3, nrow = 1)



