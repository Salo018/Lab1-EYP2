# Importar librerias solo si no se han instalado
install.packages("ggplot2") #Visualizacion
install.packages("naniar")  #Datos faltantes
install.packages("corrplot") #Matriz de correlaciones
install.packages("dplyr")    #Manejo de datos


#Se cargan las librerias 
library(ggplot2)
library(naniar)
library(corrplot)
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

# Analisis de nulos
sum(is.na(df$horas_estudio))
dim(df)[1]


#' Se crea la siguiente funcion para sacar el porcentaje de nulos en cualquier columna
nulos_columna <- function(columna, nombre = "columna"){
    nulos <- (sum(is.na(columna)))
    cat("Porcentaje de nulos en la columna",nombre,"es de:", ((nulos/dim(df)[1])*100) ,"%")
}

nulos_columna(df$puntaje_final, "puntaje_final")
nulos_columna(df$horas_estudio, "horas_estudio")
nulos_columna(df$asistencia, "asistencia")
nulos_columna(df$promedio_previo, "promedio_previo")
nulos_columna(df$horas_sueno, "horas_sueno")
nulos_columna(df$edad, "edad")
nulos_columna(df$uso_redes, "uso_redes")
nulos_columna(df$ingresos_familiares, "ingresos_familiares")
nulos_columna(df$genero, "genero")
nulos_columna(df$carrera, "carrera")
nulos_columna(df$acceso_internet, "acceso_internet")
nulos_columna(df$trabaja, "trabaja")
nulos_columna(df$modalidad, "modalidad")





