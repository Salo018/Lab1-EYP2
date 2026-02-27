install.packages("ggplot2") #Visualización
install.packages("naniar")  #Datos faltantes
install.packages("corrplot") #Matriz de correlaciones
install.packages("dplyr")    #Manejo de datos


#Se cargan las librerias con library()
library(ggplot2)
library(naniar)
library(corrplot)
library(dplyr)
library(readr)
library(purrr)
library(dplyr)

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
datos_finales <- bind_rows(lista_csv)












