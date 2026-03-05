# Importar librerias solo si no se han instalado
install.packages("ggplot2") #Visualizacion
install.packages("naniar")  #Datos faltantes
install.packages("corrplot") #Matriz de correlaciones
install.packages("dplyr")    #Manejo de datos
install.packages("ggcorrplot") #Visualizacion 
install.packages("fastDummies") # One-hot encoding
install.packages("VIM") # knn


#Se cargan las librerias 
library(ggplot2)
library(naniar)
library(corrplot)
library(ggcorrplot)
library(dplyr)
library(fastDummies)
library(VIM)
# Librerias para cargar datos
library(readr)
library(purrr)
library(dplyr)
library(stringr)


# Carga de datos
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

# Desde la linea 40 hasta la linea 334 se hace EDA y limpieza. 

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
df$carrera <- str_replace(df$carrera, "CS", "Computer_Science")
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


# Se crea la siguiente funcion para sacar el numero de negativos en cualquier columna
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
# Nose alteraron datos de la columna


# Columna de horas_estudio
#Ver porcentaje de valores por encima del rango definido
((sum(df$horas_estudio > 35, na.rm = TRUE))/dim(df)[1])*100


ggplot(df, aes(x = horas_estudio)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribución de Horas de Estudio",
       x = "Horas de estudio",
       y = "Frecuencia") +
  theme_minimal()

# Calcular límites con Rango intercuartilico 
Q1 <- quantile(df$horas_estudio, 0.25, na.rm = TRUE)
Q3 <- quantile(df$horas_estudio, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

limite_superior <- Q3 + 1.5 * IQR
limite_inferior <- Q1 - 1.5 * IQR

cat("Límite superior:", limite_superior, "\n")
cat("Límite inferior:", limite_inferior, "\n")

# Ver cuántos outliers tenemos
sum(df$horas_estudio > limite_superior, na.rm = TRUE)

# Ver los valores exactos
df$horas_estudio[df$horas_estudio > limite_superior]

# Porcentaje de outliers
total_outliers <- sum(df$horas_estudio > limite_superior, na.rm = TRUE)
total_datos <- sum(!is.na(df$horas_estudio))
porcentaje <- (total_outliers / total_datos) * 100

cat("Porcentaje:", round(porcentaje, 2), "%\n")


# Reemplazar outliers por el limite intercuartilico superior 
df$horas_estudio <- pmin(df$horas_estudio, limite_superior)

ggplot(df, aes(x = horas_estudio)) +
  geom_histogram(bins = 20) +
  xlim(0, 25) +
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
# Imputacion de nulos con la mediana
mediana_sueño <- median(df$horas_sueno, na.rm = TRUE) # Calcular la mediana
df$horas_sueno[is.na(df$horas_sueno)] <- mediana_sueño
#sum(is.na(df$horas_sueno))


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
boxplot(df$edad)
# Porcentaje de outliers muy extremos como se puede ver en el boxplot
(sum(df$edad > 74)/dim(df)[1])*100
# Al representar 0.5% se decidió eliminarlos
df <- subset(df, edad <= 30)


#Columna uso_redes
# Porcentaje de valores menores a 0 y mayores a 24
((sum(df$uso_redes < 0 | df$uso_redes > 24, na.rm = TRUE))/dim(df)[1])*100
# Eliminar mayores a 24
df <- subset(df, is.na(uso_redes) | uso_redes < 24)
# Imputacion de nulos uso_redes por mediana
mediana_uso_redes <- median(df$uso_redes, na.rm = TRUE) # Calcular la mediana
df$uso_redes[is.na(df$uso_redes)] <- mediana_uso_redes
sum(is.na(df$uso_redes))


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


# Columna de ingresos familiares
mediana_ingresos <- median(df$ingresos_familiares, na.rm = TRUE) # Calcular la mediana
# Imputacion de nulos con la mediana
df$ingresos_familiares[is.na(df$ingresos_familiares)] <- mediana_ingresos
#sum(is.na(df$ingresos_familiares))

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
# Al ver que no hay una relacion marcada entre ellas, se busca otro metodo de imputacion
# Para esto se elegió el metodo knn (K-Nearest Neighbors o K-Vecinos más Cercanos)
df_imputado <- kNN(df, 
                   variable = "carrera",  # solo imputar carrera
                   k = 5)                 # usa los 5 registros más cercanos

# Agregar una columna que despues se elimina
df_imputado <- df_imputado[, !grepl("_imp", names(df_imputado))]

# Grafico de Braras: conteo despues de la imputacion por K vecinos mas cercanos
df_original_distribucion_carrera <- df %>%
  count(carrera) %>%
  mutate(dataset = "Antes de imputar")

df_post_conteo <- df_imputado %>%
  count(carrera) %>%
  mutate(dataset = "Después de imputar")

# Unir
df_comparacion <- bind_rows(df_original_distribucion_carrera, df_post_conteo)

# Gráfico
ggplot(df_comparacion, aes(x = carrera, y = n, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Carrera antes y después de imputar por K-NN",
       x = "Carrera",
       y = "Frecuencia",
       fill = "") +
  theme_minimal() +
  scale_fill_manual(values = c("#90EE90", "#C8A2C8"))

table(df_imputado$carrera)



#Matriz de correlacion
#selecciono variables cuantitativashttp://127.0.0.1:45263/graphics/f55e4b74-6631-4315-9c55-045aff1527ac.png
df_num <- df_imputado %>% select(horas_estudio, asistencia, promedio_previo,
                        horas_sueno, edad, uso_redes,
                        ingresos_familiares, puntaje_final, estres)

# Maneja nulos automáticamente
R <- cor(df_num, use = "complete.obs")
#Define caracteristicas visuales de la matriz
ggcorrplot(R,
           type = "upper",
           lab = TRUE,
           colors = c("#6D9EC1", "white", "#E31246"))



#Grafico de dispercion para verificar la linealidad de las variables
#(puntaje_final y horas_estudio)

plot(df_imputado$puntaje_final, df_imputado$horas_estudio,
     xlab="Puntaje final",
     ylab="Horas estudio",
     main = "Disperción entre puntaje final y horas de estudio",
     col = "#5978D4", # azul con transparencia
     cex = 0.9,
)      

abline(lm(horas_estudio ~ puntaje_final, data=df_imputado),
       col="red", lwd = 2) 

#Grafica 2: #Grafico de dispercion para verificar la linealidad de las variables
#(puntaje_final y promedio_previo)

plot(df_imputado$puntaje_final, df_imputado$promedio_previo,
     xlab="Puntaje final",
     ylab="promedio_previo",
     main = "Disperción entre puntaje final y promedio_previo",
     col = "#5978D4", # azul con transparencia
     cex = 0.9,
)      

abline(lm(promedio_previo ~ puntaje_final, data=df_imputado),
       col="red", lwd = 2)

# Rellenar valores nulos de horas estudio respecto a puntaje_final
modelo_horas_estudio <- lm(horas_estudio ~ puntaje_final, data = df_imputado)
summary(modelo_horas_estudio)

prediccion_horas_estudio <- predict(modelo_horas_estudio,
                        newdata = df_imputado)
# Aplicacion del anterior modelo de regresion lineal simple
df_imputado$horas_estudio[is.na(df_imputado$horas_estudio)] <- prediccion_horas_estudio[is.na(df_imputado$horas_estudio)]
sum(is.na(df_imputado$horas_estudio))


# Rellenar valores nulos de promedio_previo respecto a puntaje_final
modelo_prom_previo <- lm(promedio_previo ~ puntaje_final, data = df_imputado)
summary(modelo_prom_previo)

prediccion_prom_previo <- predict(modelo_prom_previo,
                                    newdata = df_imputado)
# Aplicacion del anterior modelo de regresion lineal simple
df_imputado$promedio_previo[is.na(df_imputado$promedio_previo)] <- prediccion_prom_previo[is.na(df_imputado$promedio_previo)]
sum(is.na(df_imputado$promedio_previo))
summary(df_imputado$promedio_previo)



# Ver distribucion de datos de asistencia
ggplot(df_imputado, aes(x = asistencia)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribución de datos de asistencia",
       x = "asistencia",
       y = "Frecuencia") +
  theme_minimal()

# Ver distribucion de datos de estres
ggplot(df_imputado, aes(x = estres)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribución de datos de estres",
       x = "estres",
       y = "Frecuencia") +
  theme_minimal()

# Columna de asistencia
mediana_asistencia <- median(df_imputado$asistencia, na.rm = TRUE) # Calcular la mediana
# Imputacion de nulos con la mediana
df_imputado$asistencia[is.na(df_imputado$asistencia)] <- mediana_asistencia
#sum(is.na(df$ingresos_familiares))

# Columna de estres
mediana_estres <- median(df_imputado$estres, na.rm = TRUE) # Calcular la mediana
# Imputacion de nulos con la mediana
df_imputado$estres[is.na(df_imputado$estres)] <- mediana_estres
#sum(is.na(df$ingresos_familiares))


# Evaluacion de distribucion de la variable de respuesta 
# Ver distribucion de datos de puntaje final
hist(df_imputado$puntaje_final, main = "Distribución de puntaje final", col = "steelblue")

boxplot(df_imputado$puntaje_final, main = "Boxplot Puntaje Final")
boxplot.stats(df_imputado$puntaje_final)$out # Ver outliers del anterior boxplot 


# Dicotomizacion de las columnas cualitativas 
# La palabra dentro de las comillas se convierten en 0, cualquier otra en 0
df_imputado$semestre <- as.numeric(df_imputado$semestre == "Sem2") # columna semestre
df_imputado$genero <- as.numeric(df_imputado$genero == "male") # columna genero
df_imputado$acceso_internet <- as.numeric(df_imputado$acceso_internet == "yes") # columna acceso_internet
df_imputado$trabaja <- as.numeric(df_imputado$trabaja == "yes") # columna trabaja
df_imputado$modalidad <- as.numeric(df_imputado$modalidad == "in_person") # columna modalidad
# Como en carrera hay mas de 2 categorias, se aplica one-hot encoding
df_imputado <- dummy_cols(df_imputado, select_columns = "carrera")


# Excluir las dummies de carrera
# Excluir columnas cualitativas y las de one-hot encoding
df_numericas <- df_imputado[, !names(df_imputado) %in% c("carrera", "puntaje_final", "semestre", 
                                       "genero", "modalidad", "trabaja", "acceso_internet",
                                       "carrera_Business", "carrera_Data", "carrera_Engineering",
                                        "carrera_Computer_Science")]

# Calcular correlación
correlacion <- cor(df_numericas)
ggcorrplot(correlacion,
           type = "upper",
           lab = TRUE,
           colors = c("#6D9EC1", "white", "#E31246"))

# Obtener todos los pares de variables
variables <- colnames(df_numericas)
resultados <- data.frame()

for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    test <- cor.test(df_numericas[[i]], df_numericas[[j]])
    resultados <- rbind(resultados, data.frame(
      Variable1   = variables[i],
      Variable2   = variables[j],
      Correlacion = round(test$estimate, 3),
      P_valor     = round(test$p.value, 4)
    ))
  }
}

# Se asume un alpha = 0.05
# Filtrar solo los pares con p-valor < 0.05
significativos <- resultados[resultados$P_valor < 0.05, ]
print(significativos)

#Crear copia del df_imputado
df_copia <- subset(df_imputado, select = -c(carrera))

# Interacciones 
variables <- colnames(df_copia)

for (i in 1:(length(variables) - 1)) {
  for (j in (i + 1):length(variables)) {
    nombre_col <- paste0(variables[i], "_x_", variables[j])
    df_imputado[[nombre_col]] <- df_copia[[i]] * df_copia[[j]]
  }
}

#Eliminar columna de carrera en df_imputado 
df_imputado$carrera <- NULL

# Para solucionar el problema de multicolinealidad perfecta entre variables dummy
# Las interacciones de dummy van a ser perfectas entonces por eso se eliminan
cols_eliminar <- grep("carrera_.*_x_carrera_", names(df_imputado), value = TRUE)

# Verificar cuales son
print(cols_eliminar)

# Eliminarlas del dataframe
df_imputado <- df_imputado[, !names(df_imputado) %in% cols_eliminar]

# Modelo con todas las variables
modelo <- lm(puntaje_final ~ ., data = df_imputado)

# Se deja la categoria bussiness de carrera como referencia
vars_limpias <- setdiff(names(coef(modelo)), c("(Intercept)", "carrera_Business"))

formula_limpia <- as.formula(paste("puntaje_final ~", 
                                   paste(vars_limpias, collapse = " + ")))

# modelo
modelo_limpio <- lm(formula_limpia, data = df_imputado)

# Verificar que ya no haya NAs
coefs_na <- coef(modelo_limpio)[is.na(coef(modelo_limpio))]
print(coefs_na)


# Funcion para quitar variables de VIF mayores a un valor definido
remueve_VIF_grande <- function(modelo, u){ 
  require(car)
  # extrae el dataframe
  data <- modelo$model
  # Calcula todos los VIF
  all_vifs <- car::vif(modelo)
  # extraer el nombre de todas las variables X 
  names_all <- names(all_vifs)
  # extraer el nombre de la variables y 
  dep_var <- all.vars(formula(modelo))[1]
  
  # Lista para guardar todos los modelos intermedios
  lista_modelos <- list()
  lista_modelos[["modelo_inicial"]] <- modelo
  iteracion <- 1
  
  # Remover las variables con  VIF > u 
  # y reestimar el modelo con las otras variables
  
  while(any(all_vifs > u)){
    # elimina variable con max vif
    var_max_vif <- names(which(all_vifs == max(all_vifs)))
    # remueve la variable
    names_all <- names_all[!(names_all) %in% var_max_vif]
    # nueva fórmula
    myForm <- as.formula(paste(paste(dep_var, "~ "),
                               paste(names_all, collapse=" + "), sep=""))
    # creando el nuevo modelo con nueva fórmula
    modelo.prueba <- lm(myForm, data = data)
    all_vifs <- car::vif(modelo.prueba)
    
    # Guardar cada modelo intermedio con su iteracion
    nombre <- paste0("modelo_iter_", iteracion)
    lista_modelos[[nombre]] <- modelo.prueba
    iteracion <- iteracion + 1
  }
  
  return(lista_modelos)
}

#Aplicar la funcion para quitar VIF mayores a 10
lista_modelos <- remueve_VIF_grande(modelo_limpio, u = 10)

coefs <- coef(modelo)
coefs[is.na(coefs)]
#Prueba






