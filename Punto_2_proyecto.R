# Punto 2-proyecto

# Importar librerias solo si no se han instalado
install.packages("magick") # Leer imagenes 
install.packages("brglm2")

# Se cargan las librerias 
library(magick)
library(ggcorrplot)
library(brglm2)



# Carga de datos
# URL de github 
df_imagenes <- read.csv("https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_2/labels_imagenes.csv")
# Pasar variable de respuesta a 0 y 1 (No conforme= 1, Conforme= 0)
df_imagenes$respuesta <- as.numeric(df_imagenes$respuesta == "No conforme") 

# Carga de todas las imagenes
# URL de github 
url_imagenes <- "https://raw.githubusercontent.com/Salo018/Lab1-EYP2/main/Caso_2/imagenes_espuma/"

# Leer una imagen de prueba 
url_prueba <- "https://raw.githubusercontent.com/Salo018/Lab1-EYP2/main/Caso_2/imagenes_espuma/img_0001-0.png"
image <- image_read(url_prueba)
print(image)
# Ver los detalles de esa imagen 
info <- image_info(image)
print(info)

# Lista para guardar canales de todas las imagenes
canales <- list()

# Loop para leer todas las imagenes
for (i in 1:nrow(df_imagenes)) {
  
  # Obtener el id de la imagen actual
  imagen_id <- df_imagenes$imagen_id[i]
  
  for (sufijo in c(0, 1, 2)) {
    
    # Leer imagen
    # Construir URL de la imagen
    sufijo_texto <- as.character(sufijo)
    nombre_archivo <- paste0(imagen_id, "-", sufijo_texto, ".png")
    url <- paste0(url_imagenes, nombre_archivo)
    img <- suppressMessages(image_read(url))
    
    # Convertir a matriz de pixeles y separar canales
    img_data <- image_data(img, channels = "rgb")
    img_matrix <- as.integer(img_data)
    
    # Separar canales y convertir a escala 0-1
    canal_R <- img_matrix[,,1]
    canal_G <- img_matrix[,,2]
    canal_B <- img_matrix[,,3]
    R <- canal_R / 255
    G <- canal_G / 255
    B <- canal_B / 255
    
    # Guardar canales con nombre unico
    nombre_clave <- paste0(imagen_id, "_", sufijo_texto)
    canales_imagen <- list(R = R, G = G, B = B)
    canales[[nombre_clave]] <- canales_imagen
  }
  
  if (i %% 100 == 0) cat("Procesadas", i, "de", nrow(df_imagenes), "\n") # mostrar el progreso de leer las imagenes sin llenar la consola
}

# Loop para calcular metricas
resultados <- data.frame()  # crear tabla para guardar los resultados 

for (i in 1:nrow(df_imagenes)) {
  
  # Obtener el id de la imagen actual
  imagen_id <- df_imagenes$imagen_id[i]
  fila <- data.frame(imagen_id = imagen_id)  # crea la fila vacia para guardar las metricas 
  
  for (sufijo in c(0, 1, 2)) {   
    
    # Recuperar canales guardados
    sufijo_texto    <- as.character(sufijo)
    nombre_clave    <- paste0(imagen_id, "_", sufijo_texto)
    canales_imagen  <- canales[[nombre_clave]]
    R               <- canales_imagen$R
    G               <- canales_imagen$G
    B               <- canales_imagen$B
    
    # Metricas
    # Conversion a escala de grises
    coef_R              <- 0.2989
    coef_G              <- 0.5870
    coef_B              <- 0.1140
    canal_R_ponderado   <- coef_R * R
    canal_G_ponderado   <- coef_G * G
    canal_B_ponderado   <- coef_B * B
    Igray               <- canal_R_ponderado + canal_G_ponderado + canal_B_ponderado
    
    # Total de pixeles de la imagen
    N            <- length(Igray)
    # Promedio de intensidad de todos los pixeles
    suma_pixeles <- sum(Igray)
    brillo_medio <- suma_pixeles / N
    # Desviacion estandar de la intensidad
    diferencias_cuadradas <- (Igray - brillo_medio)^2
    suma_diferencias      <- sum(diferencias_cuadradas)
    contraste             <- sqrt(suma_diferencias / (N-1))
    # Segmentacion binaria 
    B_mask <- ifelse(Igray < 0.95, 1, 0)
    # Numero de pixeles que pertenecen a la espuma
    area <- sum(B_mask)
    # Pixeles de intensidad que pertenecen a la espuma 
    pixeles_espuma <- B_mask == 1
    Igray_obj      <- Igray[pixeles_espuma]
    # Pixeles del canal verde que pertenecen a la espuma
    G_obj <- G[pixeles_espuma]
    # Total de pixeles de la espuma
    M <- length(Igray_obj)
    # Media de intensidad solo en la espuma
    mu_omega <- mean(Igray_obj)
    # Desviacion estandar de intensidad solo en la espuma
    diff_rugosidad <- (Igray_obj - mu_omega)^2
    suma_rugosidad <- sum(diff_rugosidad)
    rugosidad      <- sqrt(suma_rugosidad / (M-1))
    # Media del canal verde solo en la espuma
    mu_G <- mean(G_obj)
    # Desviacion estandar del canal verde en la espuma
    diff_verde      <- (G_obj - mu_G)^2
    suma_verde      <- sum(diff_verde)
    variacion_verde <- sqrt(suma_verde / (M-1))
    
    # Guardar metricas con sufijo
    nombre_brillo   <- paste0("brillo_medio_",    sufijo_texto)
    nombre_contrast <- paste0("contraste_",       sufijo_texto)
    nombre_area     <- paste0("area_",            sufijo_texto)
    nombre_rugos    <- paste0("rugosidad_",       sufijo_texto)
    nombre_verde    <- paste0("variacion_verde_", sufijo_texto)
    
    fila[[nombre_brillo]]   <- round(brillo_medio,    6)
    fila[[nombre_contrast]] <- round(contraste,       6)
    fila[[nombre_area]]     <- as.integer(area)
    fila[[nombre_rugos]]    <- round(rugosidad,       6)
    fila[[nombre_verde]]    <- round(variacion_verde, 6)
  }
  
  resultados <- rbind(resultados, fila)
  
  if (i %% 100 == 0) cat("Procesadas", i, "de", nrow(df_imagenes), "\n")
}

# Agregar metricas a df_imagenes
df_imagenes <- cbind(df_imagenes, resultados[, -1])
# Reordenar columnas para mejor organización
df_imagenes <- df_imagenes[, c("imagen_id", "respuesta",
                               "brillo_medio_0", "brillo_medio_1", "brillo_medio_2",
                               "contraste_0",    "contraste_1",    "contraste_2",
                               "area_0",         "area_1",         "area_2",
                               "rugosidad_0",    "rugosidad_1",    "rugosidad_2",
                               "variacion_verde_0", "variacion_verde_1", "variacion_verde_2")]


# Verificar el balance 
cat("Conforme (0):", mean(df_imagenes$respuesta == 0) * 100, "%\n")
cat("No conforme (1):", mean(df_imagenes$respuesta == 1) * 100, "%\n")


#Distribucion de variables 

par(mfrow = c(2, 3))  #  graficos para imagenes_0

hist(df_imagenes$brillo_medio_0,    main = "Brillo medio",     xlab = "")
hist(df_imagenes$contraste_0,       main = "Contraste",        xlab = "")
hist(df_imagenes$area_0,            main = "Área objeto",      xlab = "")
hist(df_imagenes$rugosidad_0,       main = "Rugosidad",        xlab = "")
hist(df_imagenes$variacion_verde_0, main = "Variación verde",  xlab = "")

par(mfrow = c(2, 3))  # graficos para imagenes_1

hist(df_imagenes$brillo_medio_1,    main = "Brillo medio",     xlab = "")
hist(df_imagenes$contraste_1,       main = "Contraste",        xlab = "")
hist(df_imagenes$area_1,            main = "Área objeto",      xlab = "")
hist(df_imagenes$rugosidad_1,       main = "Rugosidad",        xlab = "")
hist(df_imagenes$variacion_verde_1, main = "Variación verde",  xlab = "")

par(mfrow = c(2, 3))  # graficos para imagenes_2

hist(df_imagenes$brillo_medio_2,    main = "Brillo medio",     xlab = "")
hist(df_imagenes$contraste_2,       main = "Contraste",        xlab = "")
hist(df_imagenes$area_2,            main = "Área objeto",      xlab = "")
hist(df_imagenes$rugosidad_2,       main = "Rugosidad",        xlab = "")
hist(df_imagenes$variacion_verde_2, main = "Variación verde",  xlab = "")



#Matriz de correlacion

df_numericas2 <- df_imagenes[, !names(df_imagenes) %in% c("respuesta","imagen_id")]

# Calcular correlación
correlacion <- cor(df_numericas2)
ggcorrplot(correlacion,
           type = "upper",
           lab = TRUE,
           colors = c("#6D9EC1", "white", "#E31246"))


#Grafico de Boxplot

par(mfrow = c(2, 3), bg = "#f9f9f9")

variables <- list(
  list(x = df_imagenes$brillo_medio_0,    nombre = "Brillo Medio"),
  list(x = df_imagenes$contraste_0,       nombre = "Contraste"),
  list(x = df_imagenes$area_0,            nombre = "Área Objeto"),
  list(x = df_imagenes$rugosidad_0,       nombre = "Rugosidad"),
  list(x = df_imagenes$variacion_verde_0, nombre = "Variación Verde")
)

for (v in variables) {
  boxplot(v$x ~ df_imagenes$respuesta,
          main   = paste(v$nombre, "por Clase"),
          xlab   = "0 = Conforme     1 = No conforme",
          ylab   = v$nombre,
          col    = c("#3498DB", "#E74C3C"),
          border = c("#2171A8", "#C0392B"),
          frame  = FALSE)
}



# PRIMER MODELO DE REGRESION LOGISTICA 
# Variable respuesta: respuesta (0 = Conforme, 1 = No conforme)
# Variables predictoras: las 15 metricas extraidas de las imagenes

modelo_logistico1<- glm(respuesta ~ . - imagen_id,
                        family = binomial(link = logit),
                        data   = df_imagenes)

# Ver resumen del modelo
summary(modelo_logistico1)

#Implementar VIF 
library(car)
vif(modelo_logistico1)

#Mirar cuales tienen correlacion mas baja 
# La que tenga correlaciones más bajas con las otras dos se conserva 
#Rugosidad
cor(df_imagenes[, c("rugosidad_0", "rugosidad_1", "rugosidad_2")])
#Nos quedamos con rugosidad_1
#Brillo medio
cor(df_imagenes[, c("brillo_medio_0", "brillo_medio_1", "brillo_medio_2")])
#Nos quedamos con brillo_medio2
#Contraste 
cor(df_imagenes[, c("contraste_0", "contraste_1", "contraste_2")])
#Nos quedamos con contraste_2
#Area
cor(df_imagenes[, c("area_0", "area_1", "area_2")])
#Nos quedamos con area_1
#Variacion verde 
cor(df_imagenes[, c("variacion_verde_0", "variacion_verde_1", "variacion_verde_2")])
#Nos quedamos con varacion_verde_1



#Modelo de regresiom logistica 2
#Descartamos porque quitar las variables con correlacion mas alta no mejoro las 
#metricas del modelo 
modelo_logistico2 <- glm(respuesta ~ brillo_medio_2 + contraste_2 + 
                       area_1 + rugosidad_1 + variacion_verde_1,
                     family = binomial(link = logit),
                     data = df_imagenes)

summary(modelo_logistico2)
vif(modelo_logistico2)

#Modelo de regresion logistica 3: Brillo medio 
modelo_logistico3 <- glm(respuesta ~ brillo_medio_0 + brillo_medio_1 + 
                           brillo_medio_2,family = binomial (link = logit),
                         data = df_imagenes)

summary(modelo_logistico3)
vif(modelo_logistico3)

#Modelo de regresion logistica 4: contraste
modelo_logistico4 <- glm(respuesta ~ contraste_0 + contraste_1 + 
                           contraste_2,family = binomial (link = logit),
                         data = df_imagenes)

summary(modelo_logistico4)
vif(modelo_logistico4)

#Modelo de regresion logistica 5: area
modelo_logistico5 <- glm(respuesta ~ area_0 + area_1 + 
                           area_2,family = binomial (link = logit),
                         data = df_imagenes)

summary(modelo_logistico5)
vif(modelo_logistico5)

#Modelo de regresion logistica 6: rugosidad
modelo_logistico6 <- glm(respuesta ~ rugosidad_0 + rugosidad_1 + 
                           rugosidad_2,family = binomial (link = logit),
                         data = df_imagenes)

summary(modelo_logistico6)
vif(modelo_logistico6)

#Modelo de regresion logistica 7: variacion_verde
modelo_logistico7 <- glm(respuesta ~ variacion_verde_0 + variacion_verde_1 + 
                           variacion_verde_2,family = binomial (link = logit),
                         data = df_imagenes)

summary(modelo_logistico7 )
vif(modelo_logistico7 )



#Comprobar Separacion perfecta: rugosidad_1 y variacion_verde_1 causan Cuasi-separación

variables <- c("brillo_medio_1", "contraste_1", 
               "area_1", "rugosidad_1", "variacion_verde_1")

par(mfrow = c(2, 3))
for (v in variables) {
  boxplot(df_imagenes[[v]] ~ df_imagenes$respuesta,
          main = v,
          xlab = "Respuesta",
          col = c("lightblue", "salmon"))
}




#Modelo de regresion logistica 8: Juntas
modelo_logistico8 <- glm(respuesta ~ brillo_medio_0 + contraste_0 + 
                           area_0 ,family = binomial (link = logit),
                         data = df_imagenes)

summary(modelo_logistico8 )
vif(modelo_logistico8 )




#Modelo 9

modelo_logistico9 <- glm(respuesta ~ brillo_medio_2 + contraste_2,
                 family = binomial(link = "logit"),
                 data = df_imagenes)

summary(modelo_logistico9)
vif(modelo_logistico9)



#Modelo 10

modelo_logistico10 <- glm(respuesta ~  area_2 + contraste_2 ,
                          family = binomial(link = "logit"),
                          data = df_imagenes)

summary(modelo_logistico10)
vif(modelo_logistico10)

#Modelo 11

modelo_logistico11 <- glm(respuesta ~  area_0 + contraste_0 ,
                          family = binomial(link = "logit"),
                          data = df_imagenes)

summary(modelo_logistico11)
vif(modelo_logistico11)


#COMPROBAR SUPUESTO DE LINEALIDAD
#Probar la linealidad del logit (Box-Tidwell)
boxTidwell(respuesta ~ area_0 + contraste_0, data = df_imagenes)
#Contraste no cumple debe transformarse 
df_imagenes$log_contraste_0 <- log(df_imagenes$contraste_0)











