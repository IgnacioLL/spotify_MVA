## DECISION TREE IMPLEMENTATION

library(rpart)
library(rpart.plot)
library(tidyverse)
library(AICcmodavg)

setwd("C:/Users/usuario/Desktop/MVA/spotify_MVA")
df_wk_i <- readRDS("../report/preprocessing.Rdata") #el que tiene el merge

dim(df_wk_i)
names(df_wk_i)

missingGenre<-which(is.na(df_wk_i[,24]));length(missingGenre) #0

#df_wk_i<-df_wk_i[-missingGenre,]

# df_ <- df_wk_i %>% 
#   select("Album_type","Danceability","Energy","Key","Loudness",
#          "Speechiness","Acousticness","Instrumentalness", "Liveness", "Valence",
#          "Tempo","Duration_ms","scaled_stream", "scaled_views", "genre","mode","time_signature",
#          "type")

df_ <- df_wk_i %>% 
  select("Album_type","Danceability","Energy","Key","Loudness",
         "Speechiness","Acousticness","Instrumentalness", "Liveness", "Valence",
         "Tempo","Duration_ms","scaled_stream", "scaled_views", "genre","mode","time_signature")

# Our response variable is genre but our dataset it's unbalanced, so we decided to discriminate by a threshold of 100 observations.
# In this way we can take only the modalities that are more relevant
genre_counts <- table(df_$genre)
selected_genres <- names(genre_counts[genre_counts > 100])
df_ <- df_[df_$genre %in% selected_genres, ]
df_$genre <- factor(df_$genre)

df_$genre()

levels(df_wk_i$genre)

attach(df_)
sapply(df_, class)
summary(df_)

#ASSUMING PREPROCESSED DATA
# WHICH RESPONSE? -> GENRE
# WHICH ARE CATEGORICAL AND WHICH ARE CONTINUOUS
#check in "environment" window the correct type of all variables in df_

#declare type of response variable and all factors
# DECISION TREES   CART
#by default
# dtot <- data.frame(Album_type,Danceability,Energy,Key,Loudness,Speechiness,Acousticness,Instrumentalness, Liveness, Valence,
#                    Tempo,Duration_ms,scaled_stream,scaled_views,genre,mode,time_signature, type)
dtot <- data.frame(Album_type,Danceability,Energy,Key,Loudness,Speechiness,Acousticness,Instrumentalness, Liveness, Valence,
                   Tempo,Duration_ms,scaled_stream,scaled_views,genre,mode,time_signature)

#don't predict genre=NA, missing data on the response variable makes no sense
# HOLDOUT OF A 1/3 OF DATA TO ESTIMATE THE MISCLASSIFICATION PROB. OF THE CHOSEN TREE 

n <- nrow(dtot)
learn <- sample(1:n, round(0.67*n))
head(learn)
nlearn <- length(learn)
nlearn
ntest <- n - nlearn

#verify that all modalities of all qualitative explanatory variables are appearing 
#in the training set, if not, rerun the sampling or sample with stratification


#build a dt by default parameters


tree1 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
              Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,])
class(tree1)

print(tree1)

summary(tree1)


plot(tree1)
text(tree1, use.n=TRUE, all=TRUE, cex=0.8) 
#click zoom

prp(tree1)
#diferents formats estetics
prp(tree1, type=1)


#diferent informacio presentada
prp(tree1, type=1, extra=4)


# Analyze control parameters: maxdepth

tree2 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(maxdepth=3))

prp(tree2, type=1, extra= 4)
print(tree2)

#complexity parameter
tree3 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.005))

prp(tree3, type=1, extra=4)

#please take your tree and place in extra window 
#and check another cp value


tree4 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.02))
prp(tree4, type=1, extra=4)



par(mfrow=c(2,3))

tree4.1 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                  Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.001))
prp(tree4.1, type=1,main = paste("cp =", 0.01))

tree4.2 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.02))
prp(tree4.2, type=1,  main = paste("cp =", 0.02))

tree4.3 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.03))
prp(tree4.3, type=1,  main = paste("cp =", 0.03))

tree4.4 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.04))
prp(tree4.4, type=1,  main = paste("cp =", 0.04))

tree4.5 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.05))
prp(tree4.5, type=1, main = paste("cp =", 0.05))

tree4.6 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.06))
prp(tree4.6, type=1,  main = paste("cp =", 0.06))

# Restablecer los parámetros gráficos a su valor por defecto
par(mfrow=c(1,1))




tree4 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(cp=0.02))
prp(tree4, type=1, extra=4)#minsplit
#minimum number of observations required to split an internal node of the tree
tree5 = rpart(genre~Album_type+Danceability+Energy+Key+Loudness+Speechiness+Acousticness+Instrumentalness+Liveness+Valence+
                Tempo+Duration_ms+scaled_stream+scaled_views+mode+time_signature, data=dtot[learn,], control=rpart.control(minsplit=200))
prp(tree5, type=1, extra= 3)
#this are the basics. Other criteria available, complexity parameter and cross-validation not introduced


################## CACULATE THE ERROR RATE IN THE LEARNING SAMPLE

predictions=predict(tree4,data=dtot[learn,])
tpredictions=predict(tree4,newdata=dtot[-learn,])
head(predictions)

# LETS HAVE A LOOK ON THE PREDICTION (conditional probability of LeafLabel)

dim(predictions)
predictions[1:10]
predictions[1:10,1]
predictions[1:10,2]
length(predictions[,1])
dim(dtot)
nlearn
dim(dtot[learn,])

table(genre,exclude=FALSE)
table(df_[learn,15])


table(df_[,15])
names(table(dtot$genre))
predictions
# CACULATE THE ERROR RATE IN THE LEARNING SAMPLE
l<-0.5
l<-0.3

########### Coge el maximo
# Calcula la clase con la mayor predicción para cada fila
predClass <- apply(predictions, 1, function(x) {
  labels <- c("pred_Emo", "pred_Hiphop", "pred_Pop", "pred_Rap", "pred_RnB", "pred_Underground_Rap")
  maxLabel <- labels[which.max(x)]
  return(maxLabel)
})

confusionMatrix <- table(genre[learn],predClass)
error_rate.learn <- 100*sum(diag(confusionMatrix))/nlearn 
error_rate.learn

# ERROR RATE IN THE TEST SAMPLE

summary(tpredictions)
dim(tpredictions)

tpredClass <- apply(tpredictions, 1, function(x) {
  labels <- c("pred_Emo", "pred_Hiphop", "pred_Pop", "pred_Rap", "pred_RnB", "pred_Underground_Rap")
  maxLabel <- labels[which.max(x)]
  return(maxLabel)
})
table(genre[-learn],tpredClass)
TestConfusionMatrix <- table(genre[-learn],tpredClass)
error_rate.test <- 100*sum(diag(TestConfusionMatrix))/ntest

######## Validation

print(paste("Error rate train", error_rate.learn))
print(paste("Error rate test set", error_rate.test))

# Suma de los verdaderos positivos (diagonal principal)
true_positives <- sum(diag(TestConfusionMatrix))

# Número total de observaciones
total_observations <- sum(TestConfusionMatrix)

# Calculando el accuracy
accuracy <- true_positives / total_observations * 100
accuracy

AIC.rpart <- function(fit) {
  n <- fit$frame$n[1]  # número de observaciones
  p <- sum(fit$frame$var != "<leaf>")  # número de parámetros (nodos no terminales)
  rss <- fit$cptable[which.min(fit$cptable[,"xerror"]),"xerror"]  # suma de cuadrados del error residual
  aic <- n * log(rss/n) + 2 * p
  print(n)
  print(p)
  print(rss)
  return(aic)
}

aic_value <- AIC.rpart(tree1)
aic_value

aic_value <- AIC.rpart(tree2)
aic_value

aic_value <- AIC.rpart(tree3)
aic_value

aic_value <- AIC.rpart(tree4)
aic_value

#---------------- ROC Iñigo
genre.test <- genre[-learn]
# Obten las clases únicas de genre.test
unique_classes <- unique(genre.test)
num_classes <- length(unique_classes)

# ----------------- ROC MULTILINEA

par("mar")
par(mar=c(1,1,1,1))

# Asegúrate de que la paleta de colores tenga suficientes colores para todas las clases
color_palette <- rainbow(length(unique_classes) + 1)

# Inicializar el gráfico con límites adecuados
plot(1, type = "n", xlim = c(0, 100), ylim = c(0, 100), xlab = "Falsos Positivos (%)", ylab = "Verdaderos Positivos (%)", main = "Curvas ROC para Todas las Clases")

for (i in 1:length(unique_classes)) {
  class <- unique_classes[i]
  
  # Calcula el número de positivos y negativos para la clase actual
  npos <- sum(genre.test == class)
  nneg <- length(genre.test) - npos
  
  # Extraer las predicciones para la clase actual
  pred.test <- tpredictions[, class]
  rank_pred.test <- rank(pred.test)
  
  # Calcular las acumulaciones para falsos positivos y verdaderos positivos
  binary_class <- ifelse(genre.test == class, "positiu", "negatiu")
  acum_fals.pos <- 100 * cumsum(rev(as.numeric(tapply(binary_class == "negatiu", rank_pred.test, sum)))) / nneg
  acum_true.pos <- 100 * cumsum(rev(as.numeric(tapply(binary_class == "positiu", rank_pred.test, sum)))) / npos
  
  # Trazar la curva ROC para la clase actual
  lines(acum_fals.pos, acum_true.pos, col = color_palette[i], type = "l")
  
  print(class)
  print(acum_fals.pos)
  print(acum_true.pos)
}
lines(acum_fals.pos, acum_fals.pos, col = color_palette[length(unique_classes) + 1])

# Agregar una leyenda si es necesario
legend("bottomright", legend = unique_classes, col = color_palette, lty = 1, cex = 0.35, pt.cex = 0.9)

# --------------- ROC multimagen

par("mar")
par(mar=c(1,1,1,1))

# Preparar un área de trazado con múltiples paneles
par(mfrow = c(ceiling(length(unique_classes) / 2), 2))

for (class in unique_classes) {
  # Calcula el número de positivos y negativos para la clase actual
  npos <- sum(genre.test == class)
  nneg <- length(genre.test) - npos
  
  # Extraer las predicciones para la clase actual
  pred.test <- tpredictions[, class]
  rank_pred.test <- rank(pred.test)
  
  # Calcular las acumulaciones para falsos positivos y verdaderos positivos
  binary_class <- ifelse(genre.test == class, "positiu", "negatiu")
  acum_fals.pos <- 100 * cumsum(rev(as.numeric(tapply(binary_class == "negatiu", rank_pred.test, sum)))) / nneg
  acum_true.pos <- 100 * cumsum(rev(as.numeric(tapply(binary_class == "positiu", rank_pred.test, sum)))) / npos

  # Trazar la curva ROC para la clase actual
  plot(acum_fals.pos, acum_true.pos, type = "l", main = paste("Curva ROC para", class))
  lines(acum_fals.pos, acum_fals.pos, col = "red")
  
  # sensitivity<-1-acum_fals.pos

  acum_fals.neg<- 100*cumsum(rev(as.numeric(tapply(binary_class=="positiu",rank_pred.test,sum))))/npos
  plot(acum_fals.neg,acum_fals.pos,type="l", main = paste("Curva ROC para", class))
  lines(acum_fals.neg,acum_fals.pos,  col="red")
  
  print(class)
  print(acum_fals.pos)
  print(acum_true.pos)
  print(acum_fals.neg)
  
  # OR
  ord_pred.test <- order(pred.test,decreasing=T)
  ac_neg_test <- 100*cumsum(binary_class[ord_pred.test]=="negatiu")/nneg
  ac_pos_test <- 100*cumsum(binary_class[ord_pred.test]=="positiu")/npos

  plot(ac_neg_test,ac_pos_test,type="l", main="Concentration Curve")
  lines(ac_neg_test, ac_neg_test, col="red")

  # CONCENTRATION CURVE

  # THE TOTAL NUMBER OF TEST INDIVIDUALS IN EACH LEAVE

  totn <- table(-pred.test)/ntest
  ac_totn <- 100*cumsum(as.numeric(totn))

  # ANOTHER WAY OF DOING THE CONCENTRATION CURVE

  ac_tot <- 100*(1:ntest)/ntest

  ord_pred.test <- order(pred.test,decreasing=T)


  plot(ac_tot,ac_pos_test,type="l", main=paste("Cocentrartion Curve para", class))
  lines(ac_tot, ac_tot, col="red")
  }

#----------------

# ROC CURVE

genre
genre.test <- genre[-learn]
table(genre.test)
npos <- table(genre.test)[1]
nneg <- ntest - npos

pred.test <- tPredictions[,1]
tpredictions
# RANKING THE PREDICTIONS

# Cambiar para que en vez de ser positivo y negativo sea multiclase
rank_pred.test <- rank(pred.test)

acum_fals.pos <- 100*cumsum(rev(as.numeric(tapply(genre.test=="negatiu",rank_pred.test,sum))))/nneg

# COMPUTING HOW MANY POSITIVE ARE IN EACH LEAVE (LEAVE = EQUAL RANK INDIVIDUALS)

acum_true.pos <- 100*cumsum(rev(as.numeric(tapply(genre.test=="positiu",rank_pred.test,sum))))/npos


# PLOT

plot(acum_fals.pos,acum_true.pos,type="l", main="ROC curve")
lines(acum_fals.pos, acum_fals.pos, col="red")

sensitivity<-1-acum_fals.pos

acum_fals.neg<- 100*cumsum(rev(as.numeric(tapply(genre.test=="positiu",rank_pred.test,sum))))/npos
plot(acum_fals.neg,acum_fals.pos,type="l", main="ROC curve")
lines(acum_fals.neg,acum_fals.pos,  col="red")

# OR
ord_pred.test <- order(pred.test,decreasing=T)
ac_neg_test <- 100*cumsum(genre.test[ord_pred.test]=="negatiu")/nneg
ac_pos_test <- 100*cumsum(genre.test[ord_pred.test]=="positiu")/npos

plot(ac_neg_test,ac_pos_test,type="l", main="Concentration Curve")
lines(ac_neg_test, ac_neg_test, col="red")

# CONCENTRATION CURVE

# THE TOTAL NUMBER OF TEST INDIVIDUALS IN EACH LEAVE 

totn <- table(-pred.test)/ntest
ac_totn <- 100*cumsum(as.numeric(totn))

# ANOTHER WAY OF DOING THE CONCENTRATION CURVE

ac_tot <- 100*(1:ntest)/ntest

ord_pred.test <- order(pred.test,decreasing=T)


plot(ac_tot,ac_pos_test,type="l", main="Concentration Curve")
lines(ac_tot, ac_tot, col="red")
