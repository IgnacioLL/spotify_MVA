# install.packages("MASS")
library(MASS)

df <- readRDS("preprocessing.Rdata")
names(df)

# [1] "Artist"           "Track"            "Album"            "Title"            "Channel"          "Album_type"       "Danceability"     "Energy"          
# [9] "Key"              "Loudness"         "Speechiness"      "Acousticness"     "Instrumentalness" "Liveness"         "Valence"          "Tempo"           
# [17] "Duration_ms"      "Licensed"         "official_video"   "genre"  (20)          "mode"             "time_signature"   "scaled_views"     "scaled_stream"   
# [25] "scaled_likes"     "scaled_comments" 

#get numerical variables (with no missing values)
numeriques<-which(sapply(df,is.numeric))
df_n<-df[,c(20,numeriques)]
sapply(df_n,class)
#remove none scaled variables + transformed to categorical variables + Likes and Comments very correlated to Views
df_n <- df_n[, !(names(df_n) %in% c("Views", "Stream", "Likes", "Comments","Speechiness","Instrumentalness", "scaled_likes", "scaled_comments"))]
sapply(df_n,class)
colnames(df_n)[colnames(df_n) == "scaled_views"] <- "Views"
colnames(df_n)[colnames(df_n) == "scaled_stream"] <- "Streams"

names(df_n)

# Our response variable is genre but our dataset it's unbalanced, so we decided to discriminate by a threshold of 100 observations.
# In this way we can take only the modalities that are more relevant
genre_counts <- table(df_n$genre)
selected_genres <- names(genre_counts[genre_counts >= 100])
df_n <- df_n[df_n$genre %in% selected_genres, ]
df_n$genre <- factor(df_n$genre)

names(table(df_n$genre))

df_n.lda <- lda(genre ~ Danceability + Energy + Loudness + Acousticness + Liveness + Valence + Tempo + Duration_ms + Views + Streams, data=df_n)
df_n.lda

#coeficients de la funcio discriminant
df_n.lda$scaling[,1:5]

#valors de cada cas per la primera funcio discriminant
df_n.lda.values <- predict(df_n.lda, df_n[2:11])

df_n[,dim(df_n)[2]+1]<- df_n.lda.values$x[,5]
df_n.lda.values$x[,5]
df_n[,dim(df_n)[2]+1]<- df_n.lda.values$x[,4]
df_n.lda.values$x[,4]
df_n[,dim(df_n)[2]+1]<- df_n.lda.values$x[,3]
df_n.lda.values$x[,3]
df_n[,dim(df_n)[2]+1]<- df_n.lda.values$x[,2]
df_n.lda.values$x[,2]
df_n[,dim(df_n)[2]+1]<- df_n.lda.values$x[,1]
df_n.lda.values$x[,1]

names(df_n)[dim(df_n)[2]-4]<-"LDA5"
names(df_n)[dim(df_n)[2]-3]<-"LDA4"
names(df_n)[dim(df_n)[2]-2]<-"LDA3"
names(df_n)[dim(df_n)[2]-1]<-"LDA2"
names(df_n)[dim(df_n)[2]]<-"LDA1"

calcWithinGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the standard deviation for group i:
    sdi <- sd(levelidata)
    numi <- (levelilength - 1)*(sdi * sdi)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the within-groups variance
  Vw <- numtotal / (denomtotal - numlevels)
  return(Vw)
}


groupStandardise <- function(variables, groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the group-standardised version of each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablei_name <- variablenames[i]
    variablei_Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    variablei_mean <- mean(as.matrix(variablei))  
    variablei_new <- (variablei - variablei_mean)/(sqrt(variablei_Vw))
    data_length <- nrow(variablei)
    if (i == 1) { variables_new <- data.frame(row.names=seq(1,data_length)) }
    variables_new[`variablei_name`] <- variablei_new
  }
  return(variables_new)
}
#si s'estandarditzen les variables es solen obtenir valors mes interpretables
groupstandardisedconcentrations <- groupStandardise(df_n[2:13], df_n[1])

calcBetweenGroupsVariance <- function(variable,groupvariable)
{
  # find out how many values the group variable can take
  groupvariable2 <- as.factor(groupvariable[[1]])
  levels <- levels(groupvariable2)
  numlevels <- length(levels)
  # calculate the overall grand mean:
  grandmean <- mean(as.matrix(variable) )         
  # get the mean and standard deviation for each group:
  numtotal <- 0
  denomtotal <- 0
  for (i in 1:numlevels)
  {
    leveli <- levels[i]
    levelidata <- variable[groupvariable==leveli,]
    levelilength <- length(levelidata)
    # get the mean and standard deviation for group i:
    meani <- mean( as.matrix(levelidata) )
    sdi <- sd(levelidata)
    numi <- levelilength * ((meani - grandmean)^2)
    denomi <- levelilength
    numtotal <- numtotal + numi
    denomtotal <- denomtotal + denomi
  }
  # calculate the between-groups variance
  Vb <- numtotal / (numlevels - 1)
  Vb <- Vb[[1]]
  return(Vb)
}



calcSeparations <- function(variables,groupvariable)
{
  # find out how many variables we have
  variables <- as.data.frame(variables)
  numvariables <- length(variables)
  # find the variable names
  variablenames <- colnames(variables)
  # calculate the separation for each variable
  for (i in 1:numvariables)
  {
    variablei <- variables[i]
    variablename <- variablenames[i]
    Vw <- calcWithinGroupsVariance(variablei, groupvariable)
    Vb <- calcBetweenGroupsVariance(variablei, groupvariable)
    sep <- Vb/Vw
    print(paste("variable",variablename,"Vw=",Vw,"Vb=",Vb,"separation=",sep))
  }
}

#separacio que donen les dues funcions discriminants (ratio de variancia entre respecte v intra)
calcSeparations(df_n.lda.values$x,df_n[1])

#total separation (la suma de les dues)
#percentatge que separa cada una, coincideix amb la proportion of trace del model discriminant
par(mfrow=c(2, 3))
hist(df_n.lda.values$x[,1])
hist(df_n.lda.values$x[,2])
hist(df_n.lda.values$x[,3])
hist(df_n.lda.values$x[,4])
hist(df_n.lda.values$x[,5])

#histograma multiple entre la funcio discriminant i la resposta
#par("mar")
#par(mar=c(1,1,1,1))
#par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1, 1))
par(mar=c(3,2.5,1.5,1))

ldahist(data = df_n.lda.values$x[,1], g=df_n$genre)
ldahist(data = df_n.lda.values$x[,2], g=df_n$genre)
ldahist(data = df_n.lda.values$x[,3], g=df_n$genre)
ldahist(data = df_n.lda.values$x[,4], g=df_n$genre)
ldahist(data = df_n.lda.values$x[,5], g=df_n$genre)

#plot de les dues components discriminants (etiquetem els grups)
plot(df_n.lda.values$x[,1],df_n.lda.values$x[,2]) # make a scatterplot

plot(df_n$LDA1, df_n$LDA2)
text(df_n.lda.values$x[,1],df_n.lda.values$x[,2],df_n$genre,cex=0.7,pos=4,col = rainbow(length(unique(df_n$genre)))[as.factor(df_n$genre)])#  "red") # add labels

# matriz de gráficos de dispersión para las 5 lda
pairs(df_n.lda.values$x[, 1:5], col = rainbow(length(unique(df_n$genre)))[as.factor(df_n$genre)], pch = 19)

plot(df_n$LDA1, df_n$LDA2, type="n")
text(df_n.lda.values$x[,1],df_n.lda.values$x[,2],df_n$genre,cex=0.7,pos=4,col="red") # add labels


df_n.lda$scaling[,2]
df_n.lda$scaling[,1]

#utilitzar les regles per estimar el grup de cada cas
par(mfrow=c(1,2))
ldahist(data = df_n$LDA2, g=df_n$genre)
ldahist(data = df_n$LDA1, g=df_n$genre)

n<-dim(df_n)[1]
for(i in 1:n){
  if(df_n[i,"LDA2"]<0){   #1,2,5,6
    if(df_n[i,"LDA1"]>2){ #1
      df_n[i,"PredictedClass"]<-1
    }else if(df_n[i,"LDA1"]>-1){  #5
      df_n[i,"PredictedClass"]<-5
    }else{
      if(df_n[i,"LDA3"]>0){   #2
        df_n[i,"PredictedClass"]<-2
      }else{  #6
        df_n[i,"PredictedClass"]<-6
      }
    }
  }else{  #3,4
    if(df_n[i,"LDA1"]>0.5){   #3
      df_n[i,"PredictedClass"]<-3
    }else{  #4
      df_n[i,"PredictedClass"]<-4
    }
  }
}


#matriu de confusio
MC1 <- table(df_n[,"genre"], df_n[,"PredictedClass"]);MC1 #cortes manuales
MC2 <- table(df_n.lda.values$class, df_n$genre);MC2 #funcion lda

#accuracy
accuracy1 <- sum(diag(MC1))/dim(df_n)[1];accuracy1
accuracy2 <- sum(diag(MC2))/dim(df_n)[1];accuracy2


#compute missclassification rate
MR <- 1-accuracy2
MR

#buscar punts intermedis de les mitjanes i utilitzarlos per definir les regles de classificacio


printMeanAndSdByGroup <- function(variables,groupvariable)
{
  # find the names of the variables
  variablenames <- c(names(groupvariable),names(as.data.frame(variables)))
  # within each group, find the mean of each variable
  groupvariable <- groupvariable[,1] # ensures groupvariable is not a list
  means <- aggregate(as.matrix(variables) ~ groupvariable, FUN = mean)
  names(means) <- variablenames
  print(paste("Means:"))
  print(means)
  # within each group, find the standard deviation of each variable:
  sds <- aggregate(as.matrix(variables) ~ groupvariable, FUN = sd)
  names(sds) <- variablenames
  print(paste("Standard deviations:"))
  print(sds)
  # within each group, find the number of samples:
  samplesizes <- aggregate(as.matrix(variables) ~ groupvariable, FUN = length)
  names(samplesizes) <- variablenames
  print(paste("Sample sizes:"))
  print(samplesizes)
}

#mitjanes de les funcions discriminants per grups
printMeanAndSdByGroup(df_n.lda.values$x,df_n[1])
