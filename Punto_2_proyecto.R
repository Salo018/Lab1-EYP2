# Punto 2-proyecto

# Importar librerias solo si no se han instalado
install.packages("magick") #Leer imagenes 

# Se cargan las librerias 
library(magick)

# Carga de datos
# URL de github 
df_imagenes <- read.csv("https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_2/labels_imagenes.csv")
# Pasar a 0 y 1
df_imagenes$respuesta <- as.numeric(df_imagenes$respuesta == "No conforme") 
# Carga de imagenes
# URL de github 
url_imagenes <- "https://raw.githubusercontent.com/Salo018/Lab1-EYP2/main/Caso_2/imagenes_espuma/"
# Leer una imagen de prueba 
image <- image_read("https://raw.githubusercontent.com/Salo018/Lab1-EYP2/main/Caso_2/imagenes_espuma/img_0001-1.png")
print(image)
# Ver los detalles
info <- image_info(image)
print(info)

#Convertir a matriz de pixeles 
image_matrix <- as.integer(image_data(image, channels = "rgb"))

# Ver dimensiones
dim(image_matrix)

# Separar los 3 colores y convertir a escala 0-1
R <- image_matrix[,,1] / 255  # canal rojo
G <- image_matrix[,,2] / 255  # canal verde
B <- image_matrix[,,3] / 255  # canal azul




