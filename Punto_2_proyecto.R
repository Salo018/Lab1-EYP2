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

# PASO 4: Calcular las metricas del profesor
# Intensidad en escala de grises
Igray <- 0.2989*R + 0.5870*G + 0.1140*B

# Brillo medio
N <- length(Igray)
brillo_medio <- sum(Igray) / N

# Contraste global
contraste <- sqrt(sum((Igray - brillo_medio)^2) / (N-1))

# Segmentacion binaria
B_mask <- ifelse(Igray < 0.95, 1, 0)

# Area del objeto
area <- sum(B_mask)

# Rugosidad superficial
Igray_obj <- Igray[B_mask == 1]
G_obj     <- G[B_mask == 1]
M         <- length(Igray_obj)
mu_omega  <- mean(Igray_obj)
rugosidad <- sqrt(sum((Igray_obj - mu_omega)^2) / (M-1))

# Variacion canal verde
mu_G            <- mean(G_obj)
variacion_verde <- sqrt(sum((G_obj - mu_G)^2) / (M-1))

# PASO 5: Ver resultados de la imagen de prueba
cat("Brillo medio:   ", brillo_medio, "\n")
cat("Contraste:      ", contraste, "\n")
cat("Area:           ", area, "\n")
cat("Rugosidad:      ", rugosidad, "\n")
cat("Variacion verde:", variacion_verde, "\n")



