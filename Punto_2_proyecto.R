# Punto 2-proyecto

# Carga de datos
# URL de github 
df_imagenes <- read.csv("https://raw.githubusercontent.com/Salo018/Lab1-EYP2/refs/heads/main/Caso_2/labels_imagenes.csv")

unique(df_imagenes$respuesta)
sum(is.na(df_imagenes$respuesta))
