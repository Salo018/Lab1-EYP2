# Importar librerias solo si no se han instalado
install.packages("ggplot2") #Visualizacion
install.packages("naniar")  #Datos faltantes
install.packages("corrplot") #Matriz de correlaciones
install.packages("dplyr")    #Manejo de datos
install.packages("ggcorrplot") #Visualizacion 

#Se cargan las librerias 
library(ggplot2)
library(naniar)
library(corrplot)
library(ggcorrplot)
library(dplyr)
# Librerias para cargar datos
library(readr)
library(purrr)
library(dplyr)
library(stringr)

# URL de cada csv
urls <- c(
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2021_Sem1.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2021_Sem2.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2022_Sem1.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/Rendimiento_2022_Sem2.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2023_Sem1.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2023_Sem2.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2024_Sem1.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2024_Sem2.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/rendimiento_2025_Sem1.csv",
"https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_1/Rendimiento_2025_Sem2.csv"
)

# Combinar csv en un df
lista_csv <- map(urls, read_csv)
df <- bind_rows(lista_csv)

# Ver valores unicos de una columna
unique(df$genero)
unique(df$carrera)
unique(df$acceso_internet)
unique(df$trabaja)
unique(df$modalidad)


# Normalización de datos de columnas

#Columna genero
df$genero <- tolower(df$genero) # Connvertir a minusculas los datos
#unique(df$genero)

# Columna carrera
df$carrera <- str_replace(df$carrera, "Busines$", "Business") 
# Se coloca Busines$ para que solo reemplace en la palabra exacta y no queden como Businesss 
df$carrera <- str_replace(df$carrera, "Ingenieria", "Engineering")
df$carrera <- str_replace(df$carrera, "CS", "Computer Science")
#unique(df$carrera)

# Columna acceso_internet
df$acceso_internet <- tolower(df$acceso_internet) 

# Columna trabaja
df$trabaja<- tolower(df$trabaja) 
df$trabaja <- str_replace(df$trabaja, "si", "sí") 
df$trabaja <- str_replace(df$trabaja, "sí", "yes")

# Columna modalidad
df$modalidad<- tolower(df$modalidad) 
df$modalidad <- str_replace(df$modalidad, "presencial", "in_person") 


#' Se crea la siguiente funcion para sacar el numero de negativos en cualquier columna
negativos_columna <- function(columna, nombre = "columna"){
  negativos <- sum(columna < 0, na.rm = TRUE)
  
  columna[columna < 0] <- NA # Convertir  menores a 0 a nulos
  
  cat("Se convirtieron",negativos,"en la columna:",nombre)
  return(columna)
}

df$horas_estudio <- negativos_columna(df$horas_estudio, "horas_estudio")
df$asistencia <- negativos_columna(df$asistencia, "asistencia")
df$horas_sueno <- negativos_columna(df$horas_sueno, "horas_sueno")
df$estres <- negativos_columna(df$estres, "estres")
df$uso_redes <- negativos_columna(df$uso_redes, "uso_redes")
df$ingresos_familiares <- negativos_columna(df$ingresos_familiares, "ingresos_familiares")

#' Se crea la siguiente funcion para sacar el porcentaje de nulos en cualquier columna
nulos_columna <- function(columna, nombre = "columna"){
  
    nulos <- (sum(is.na(columna)))
    
    cat("Porcentaje de nulos en la columna",nombre,"es de:", ((nulos/dim(df)[1])*100) ,"%")
}
nulos_columna(df$horas_estudio, "horas_estudio")
nulos_columna(df$asistencia, "asistencia")
nulos_columna(df$promedio_previo, "promedio_previo")
nulos_columna(df$estres, "estres")
nulos_columna(df$carrera, "carrera")

#Ver estadisticas basicas de todo el df
summary(df)

# Limpieza de valores fuera de rango 
#Columna de asistencia. Se borran los valores fuera de rango 
# Representan un porcentaje pequeño en el total de los datos 
((sum(df$asistencia > 100, na.rm = TRUE))/dim(df)[1])*100
df <- subset(df, is.na(asistencia) | asistencia <= 100)


#Columna de promedio previo
((sum(df$promedio_previo > 10, na.rm = TRUE))/dim(df)[1])*100
# Valores por encima de 10 representan el 14.1% de los datos
summary(df$promedio_previo)
hist(df$promedio_previo)


# Columna de horas_estudio
#Ver porcentaje de valores por encima del rango definido
((sum(df$horas_estudio > 35, na.rm = TRUE))/dim(df)[1])*100


ggplot(df, aes(x = horas_estudio)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribución de Horas de Estudio",
       x = "Horas de estudio",
       y = "Frecuencia") +
  theme_minimal()


# Columna horas_sueño
# Distribucion de las horas de sueño
ggplot(df, aes(x = horas_sueno)) +
  geom_histogram(bins = 5) +
  labs(title = "Distribución de Horas de sueño",
       x = "Horas de sueño",
       y = "Frecuencia") +
  theme_minimal()
# Porcentaje de horas de sueño menores a 2 y mayores a 12
((sum(df$horas_sueno < 2 | df$horas_sueno > 10, na.rm = TRUE))/dim(df)[1])*100
# Al representar el 0.5% y ser valores tan extremos, se decidió eliminar estos registros
#Como no hay menores a 2, solo eliminamos los mayores a 10
df <- subset(df, is.na(horas_sueno) | horas_sueno < 10)

# Columna edad
# Distribucion de edades
ggplot(df, aes(x = edad)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribución de edades",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()
# Porcentaje de edades menores a 15 y mayores a 30
((sum(df$edad < 15 | df$edad > 30, na.rm = TRUE))/dim(df)[1])*100


#Columna uso_redes
# Porcentaje de valores menores a 0 y mayores a 24
((sum(df$uso_redes < 0 | df$uso_redes > 24, na.rm = TRUE))/dim(df)[1])*100
# Eliminar mayores a 24
df <- subset(df, is.na(uso_redes) | uso_redes < 24)

# Columna de estres
# Ver distribucion de datos de estres
ggplot(df, aes(x = estres)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribución de datos de estres",
       x = "estres",
       y = "Frecuencia") +
  theme_minimal()
# Porcentaje de datos mayores a 10
((sum(df$estres > 10, na.rm = TRUE))/dim(df)[1])*100
boxplot(df$estres)

# Analisis de media y mediana con todos los datos y solo entre 0 y 10 
summary(df$estres) # Todos los valores 
mean(df$estres[df$estres >= 0 & df$estres <= 10], na.rm = TRUE) # solo entre 0 y 10 
median(df$estres[df$estres >= 0 & df$estres <= 10], na.rm = TRUE) # solo entre 0 y 10
# Se vio que los outliers no estaban afectando tanto la distribucion
# Entonces asumimos escala de 1 a 10 y los valores mayores a 10, se dejan en 10
df$estres[df$estres > 10] <- 10


# Columna edad
# Ver distribucion de datos de edad
ggplot(df, aes(x = edad)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribución de datos de edad",
       x = "estres",
       y = "Frecuencia") +
  theme_minimal()

# Columna de promedio_previo
# Ver distribucion de datos de edad
ggplot(df, aes(x = promedio_previo)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribución de datos de promedio_previo",
       x = "estres",
       y = "Frecuencia") +
  theme_minimal()


# Columna de carrera
(((sum(is.na(df$carrera))))/dim(df)[1])*100 # Ver porcentaje de nulos
# Ver grafico de distribucion de los datos con nulos
df %>%
  count(carrera) %>%
  ggplot(aes(x = reorder(carrera, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#4C72B0") +
  labs(title = "Distribución de estudiantes por carrera (incluyendo nulos)",
       x = "Carrera",
       y = "Frecuencia") +
  theme_minimal()
# Ver promedio de puntaje_final por carrera para sacar un tipo de relacion entre ellas
aggregate(puntaje_final ~ carrera, 
          data = df, 
          FUN = mean, 
          na.rm = TRUE)
# Al ver que no hay una relacion marcada entre ellas, se crea una nueva categoria
df$carrera[is.na(df$carrera)] <- "not_registered"

#Matriz de correlacion
#selecciono variables cuantitativashttp://127.0.0.1:45263/graphics/f55e4b74-6631-4315-9c55-045aff1527ac.png
df_num <- df %>% select(horas_estudio, asistencia, promedio_previo,
                        horas_sueno, edad, uso_redes,
                        ingresos_familiares, puntaje_final)

# Maneja nulos automáticamente
R <- cor(df_num, use = "complete.obs")
#Define caracteristicas visuales de la matriz
ggcorrplot(R,
           type = "upper",
           lab = TRUE,
           colors = c("#6D9EC1", "white", "#E31246"))



















