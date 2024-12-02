library(LogRegSISEM2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data = read.csv("data/Sample_DPE.csv")

# Categorical Encoding
verifier <- CategoricalVerifier$new(data, target_var='Etiquette_DPE')
verifier$apply_encoding()
data_clean <- verifier$get_dataset()

#Diviser les données en ensembles d'entraînement et de test
set.seed(123)  # Pour la reproductibilité
train_index <- sample(1:nrow(data_clean), size = 0.7 * nrow(data_clean))
train_data <- data_clean[train_index, ]
test_data <- data_clean[-train_index, ]

# Séparer les variables explicatives (X) et la cible (y)
X_train <- as.matrix(train_data[, -ncol(data_clean)])  # Toutes les colonnes sauf la dernière
y_train <- train_data$Etiquette_DPE
X_test <- as.matrix(test_data[, -ncol(data_clean)])
y_test <- test_data$Etiquette_DPE

is_numeric <- sapply(X_train, is.numeric)
if(all(is_numeric)){
  print("yes")
}

# Vérification des valeurs manquantes dans X_train et X_test
cat("Nombre de NA dans X_train :", sum(is.na(X_train)), "\n")
cat("Nombre de NA dans X_test :", sum(is.na(X_test)), "\n")


# Entraîner le modèle avec MLN
model <- LogisticRegression$new(learning_rate = 1, num_iterations = 1000)
#model$set_transformation()
model$fit(X_train, y_train)

#Faire des prédictions sur l'ensemble de test
predictions <- model$predict(X_test)

##Vérifier les prédictions
cat("Prédictions :\n")
print(head(predictions))

#Évaluation
accuracy <- mean(predictions == y_test)
cat("Précision sur l'ensemble de test :", accuracy, "\n")

#Matrice de confusion
confusion_matrix <- table(Predicted = predictions, Actual = y_test)
cat("Matrice de confusion :\n")
print(confusion_matrix)

print(table(y_train))


model$summary()
