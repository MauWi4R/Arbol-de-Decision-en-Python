# Cargar librerías necesarias para trabajar en Collaboratory
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Archivo CSV
filename <- "/content/iris.csv"
iris_data <- read.csv(filename)

# Estructura del dataset
cat("Estructura del dataset cargado:\n")
str(iris_data)

set.seed(123)

# Entrenamiento y prueba (70%-30%)
train_index <- sample(1:nrow(iris_data), size = 0.7 * nrow(iris_data))
train_data <- iris_data[train_index, ]
test_data <- iris_data[-train_index, ]

cat("\nDimensiones del conjunto de entrenamiento:", dim(train_data), "\n")
cat("Dimensiones del conjunto de prueba:", dim(test_data), "\n")

colnames(iris_data) <- c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm", "Species")

# Creación del modelo de árbol de decisión
decision_tree <- rpart(Species ~ SepalWidthCm + PetalWidthCm, data = train_data, method = "class")

# Sección 1: Mostrar probabilidades de clasificación
cat("\nProbabilidades de clasificación para las primeras observaciones del conjunto de prueba:\n")
probabilities <- predict(decision_tree, test_data, type = "prob")
head(probabilities)

# Sección 2: Predicciones e interpretación de resultados
cat("\nPredicciones categóricas para las primeras observaciones:\n")
predictions <- predict(decision_tree, test_data, type = "class")
head(predictions)

# Tabla de confusión
cat("\nMatriz de confusión:\n")
confusion_matrix <- table(Predicted = predictions, Actual = test_data$Species)
print(confusion_matrix)

# Sección 3: Ilustración del árbol de decisión
cat("\nResumen del árbol de decisión:\n")
summary(decision_tree)
cat("\nÁrbol de decisión completo:\n")
rpart.plot(decision_tree, type = 3, extra = 104, fallen.leaves = TRUE)

# Sección 4: Poda del árbol para evitar sobreajuste
# Evaluar complejidad del árbol
cat("\nEvaluación de complejidad del árbol:\n")
printcp(decision_tree)
plotcp(decision_tree)

# Seleccionar un árbol con 4 hojas
pruned_tree <- prune(decision_tree, cp = 0.01)  # Ajustar cp para obtener 4 hojas
cat("\nÁrbol podado con 4 hojas:\n")
print(pruned_tree)

# Visualizar el árbol podado
rpart.plot(pruned_tree, type = 3, extra = 104, fallen.leaves = TRUE)

# Fin del programa
cat("\n¡Programa completado con éxito!\n")
