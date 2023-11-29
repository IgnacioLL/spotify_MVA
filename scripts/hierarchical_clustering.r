library(ggplot2)
library(factoextra)
library(FactoMineR)
library(ggfortify)

df_wk_i <- readRDS("preprocessing.Rdata")
df_ <- df_wk_i %>% 
  select("Album_type","Danceability","Energy","Key","Loudness",
         "Speechiness","Acousticness","Instrumentalness", "Liveness", "Valence",
         "Tempo","Duration_ms","scaled_stream", "scaled_views", "genre","mode","time_signature",
         "type"
         )
df_wk_i %>% names()
d <- dist(df_)
h1 <- hclust(d,method="ward")  # NOTICE THE COST
plot(h1)


c1 <- cutree(h1,6)

df_$cluster1 <- c1 %>% as.factor

c2 <- cutree(h1,3)

df_$cluster2 <- c2 %>% as.factor

catdes(df_, num.var=which(names(df_)=='cluster1'))
catdes(df_, num.var=which(names(df_)=='cluster2'))



### Understanding cluster 1

df_pca_analisis <- df_ %>% select(!c("mode","time_signature","cluster2","Album_type","Key","type"))


df_pca_analisis$cluster1 <- df_pca_analisis$cluster1 %>% as.factor()

pca_data <- prcomp(df_pca_analisis %>% select(!c(cluster1,"genre")), scale. = TRUE)$x
pca_result <- prcomp(df_pca_analisis %>% select(!c(cluster1,"genre")), scale. = TRUE)


viz <- cbind(pca_data, df_pca_analisis$cluster1) %>% as.data.frame()
viz$cluster1 <- viz$V13
viz$V13 <- NULL;



viz2 <- cbind(pca_data, df_pca_analisis$genre) %>% as.data.frame()
viz2$genre <- viz2$V13
viz2$V13 <- NULL;


centroids_clusters <- viz %>% group_by(cluster1) %>% summarise_all("mean")


viz2[1:12]<- viz2[1:12] %>% apply(2,as.numeric)
centroids_genre <- viz2 %>% group_by(genre) %>% summarise_all("mean", na.rm=TRUE)


# Assuming 'pca_result$rotation' is your data
rotation <- pca_result$rotation %>% as.data.frame()

# Convert the data to long format
rotation_long <- rotation %>% 
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Component", values_to = "Value")

rotation_long$Component <- factor(rotation_long$Component, levels = sort(unique(rotation_long$Component)))
# Plot the data
ggplot(rotation_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Component, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(ggnewscale)
viz$cluster1 <- viz$cluster1 %>% as.factor
autoplot(pca_result, data = viz, loadings = TRUE, loadings.label = TRUE, size = 0, type = "n", scale = 0, y=4) +
  geom_point(data = centroids_clusters, aes(x = PC1, y = PC4, color = factor(cluster1)), size = 3, shape = 17) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_genre, aes(x = PC1, y = PC4, color = factor(genre)), size = 3, shape = 8) +
  theme_minimal()
  

  
cat_analisis <- df_ %>% select(c("mode","time_signature","cluster1","Album_type","Key","type","genre"))

cat_analisis$Album_type <- cat_analisis$Album_type %>% as.factor
cat_analisis$time_signature <- cat_analisis$time_signature  %>% as.factor
cat_analisis$mode <- cat_analisis$mode %>% as.factor
cat_analisis$type <- cat_analisis$type %>% as.factor
cat_analisis$genre <- cat_analisis$genre%>% as.factor

cat_analisis %>% glimpse


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = genre), stat = "count")


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = time_signature), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = mode), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = type), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = Album_type), stat = "count")

### Any distribution seems to be differ so it is pretty difficult to infer how the algorithm segmented cluster1


### Understanding cluster 2

df_pca_analisis <- df_ %>% select(!c("mode","time_signature","cluster1","Album_type","Key","type"))


df_pca_analisis$cluster2 <- df_pca_analisis$cluster2 %>% as.factor()

pca_data <- prcomp(df_pca_analisis %>% select(!c(cluster2,"genre")), scale. = TRUE)$x
pca_result <- prcomp(df_pca_analisis %>% select(!c(cluster2,"genre")), scale. = TRUE)


viz <- cbind(pca_data, df_pca_analisis$cluster2) %>% as.data.frame()
viz$cluster2 <- viz$V13
viz$V13 <- NULL;



viz2 <- cbind(pca_data, df_pca_analisis$genre) %>% as.data.frame()
viz2$genre <- viz2$V13
viz2$V13 <- NULL;


centroids_clusters <- viz %>% group_by(cluster2) %>% summarise_all("mean")


viz2[1:12]<- viz2[1:12] %>% apply(2,as.numeric)
centroids_genre <- viz2 %>% group_by(genre) %>% summarise_all("mean", na.rm=TRUE)





library(ggnewscale)
viz$cluster2 <- viz$cluster2 %>% as.factor
autoplot(pca_result, data = viz, loadings = TRUE, loadings.label = TRUE, size = 0, type = "n", scale = 0) +
  geom_point(data = centroids_clusters, aes(x = PC1, y = PC2, color = factor(cluster2)), size = 3, shape = 17) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_genre, aes(x = PC1, y = PC2, color = factor(genre)), size = 3, shape = 8) +
  theme_minimal()

autoplot(pca_result, data = viz, loadings = TRUE, loadings.label = TRUE, size = 0, type = "n", scale = 0, y=3) +
  geom_point(data = centroids_clusters, aes(x = PC1, y = PC3, color = factor(cluster2)), size = 3, shape = 17) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_genre, aes(x = PC1, y = PC3, color = factor(genre)), size = 3, shape = 8) +
  theme_minimal()


autoplot(pca_result, data = viz, loadings = TRUE, loadings.label = TRUE, size = 0, type = "n", scale = 0, y=4) +
  geom_point(data = centroids_clusters, aes(x = PC1, y = PC4, color = factor(cluster2)), size = 3, shape = 17) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_genre, aes(x = PC1, y = PC4, color = factor(genre)), size = 3, shape = 8) +
  theme_minimal() ## This one is better and PC4-AXIS

# Assuming 'pca_result$rotation' is your data
rotation <- pca_result$rotation %>% as.data.frame()

# Convert the data to long format
rotation_long <- rotation %>% 
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Component", values_to = "Value")

rotation_long$Component <- factor(rotation_long$Component, levels = sort(unique(rotation_long$Component)))

# Plot the data
ggplot(rotation_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Component, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


autoplot(pca_result, data = viz, loadings = TRUE, loadings.label = TRUE, size = 0, type = "n", scale = 0, x=4, y=5) +
  geom_point(data = centroids_clusters, aes(x = PC4, y = PC5, color = factor(cluster2)), size = 3, shape = 17) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_genre, aes(x = PC4, y = PC5, color = factor(genre)), size = 3, shape = 8) +
  theme_minimal() ## This one is better and PC4-AXIS


autoplot(pca_result, data = viz, loadings = TRUE, loadings.label = TRUE, size = 0, type = "n", scale = 0, x=4, y=8) +
  geom_point(data = centroids_clusters, aes(x = PC4, y = PC8, color = factor(cluster2)), size = 3, shape = 17) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_genre, aes(x = PC4, y = PC8, color = factor(genre)), size = 3, shape = 8) +
  theme_minimal() ## This one is better and PC4-AXIS



cat_analisis <- df_ %>% select(c("mode","time_signature","cluster2","Album_type","Key","type","genre"))

cat_analisis$Album_type <- cat_analisis$Album_type %>% as.factor
cat_analisis$time_signature <- cat_analisis$time_signature  %>% as.factor
cat_analisis$mode <- cat_analisis$mode %>% as.factor
cat_analisis$type <- cat_analisis$type %>% as.factor
cat_analisis$genre <- cat_analisis$genre%>% as.factor

cat_analisis %>% glimpse


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = genre), stat = "count")


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = time_signature), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = mode), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = type), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = Album_type), stat = "count")

### Any distribution seems to be differ so it is pretty difficult to infer how the algorithm segmented cluster2

### We will try again but in this case we will not include duration_ms

rm(list = ls())

df_wk_i <- readRDS("preprocessing.Rdata")
df_ <- df_wk_i %>% 
  select("Album_type","Danceability","Energy","Key","Loudness",
         "Speechiness","Acousticness","Instrumentalness", "Liveness", "Valence",
         "Tempo","scaled_stream", "scaled_views", "genre","mode","time_signature",
         "type"
  )
df_wk_i %>% names()
d <- dist(df_)
h1 <- hclust(d,method="ward")  
plot(h1) # NOTICE THE height this time


c1 <- cutree(h1,6)

df_$cluster1 <- c1 %>% as.factor

c2 <- cutree(h1,3)

df_$cluster2 <- c2 %>% as.factor

catdes(df_, num.var=which(names(df_)=='cluster1'))
catdes(df_, num.var=which(names(df_)=='cluster2'))



### Understanding cluster 1

df_pca_analisis <- df_ %>% select(!c("mode","time_signature","cluster2","Album_type","Key","type"))


df_pca_analisis$cluster1 <- df_pca_analisis$cluster1 %>% as.factor()

pca_data <- prcomp(df_pca_analisis %>% select(!c(cluster1,"genre")), scale. = TRUE)$x
pca_result <- prcomp(df_pca_analisis %>% select(!c(cluster1,"genre")), scale. = TRUE)


viz <- cbind(pca_data, df_pca_analisis$cluster1) %>% as.data.frame()
viz$cluster1 <- viz$V12
viz$V12 <- NULL;



viz2 <- cbind(pca_data, df_pca_analisis$genre) %>% as.data.frame()
viz2$genre <- viz2$V12
viz2$V12 <- NULL;


centroids_clusters <- viz %>% group_by(cluster1) %>% summarise_all("mean")


viz2[1:11]<- viz2[1:11] %>% apply(2,as.numeric)
centroids_genre <- viz2 %>% group_by(genre) %>% summarise_all("mean", na.rm=TRUE)


# Assuming 'pca_result$rotation' is your data
rotation <- pca_result$rotation %>% as.data.frame()

# Convert the data to long format
rotation_long <- rotation %>% 
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Component", values_to = "Value")

rotation_long$Component <- factor(rotation_long$Component, levels = sort(unique(rotation_long$Component)))
# Plot the data
ggplot(rotation_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Component, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(ggnewscale)
viz$cluster1 <- viz$cluster1 %>% as.factor
autoplot(pca_result, data = viz2, loadings = TRUE, loadings.label = TRUE, size = 1, type = "n", scale = 0, colour="genre",y=4) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_clusters, aes(x = PC1, y = PC4, color = factor(cluster1)), size = 5, shape = 17) +
  theme_minimal()



cat_analisis <- df_ %>% select(c("mode","time_signature","cluster1","Album_type","Key","type","genre"))

cat_analisis$Album_type <- cat_analisis$Album_type %>% as.factor
cat_analisis$time_signature <- cat_analisis$time_signature  %>% as.factor
cat_analisis$mode <- cat_analisis$mode %>% as.factor
cat_analisis$type <- cat_analisis$type %>% as.factor
cat_analisis$genre <- cat_analisis$genre%>% as.factor

cat_analisis %>% glimpse


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = genre), stat = "count")


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = time_signature), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = mode), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster1, fill = Album_type), stat = "count")

### Most important variables tempo, genre.


### Understanding cluster 1

df_pca_analisis <- df_ %>% select(!c("mode","time_signature","cluster1","Album_type","Key","type"))


df_pca_analisis$cluster2 <- df_pca_analisis$cluster2 %>% as.factor()

pca_data <- prcomp(df_pca_analisis %>% select(!c(cluster2,"genre")), scale. = TRUE)$x
pca_result <- prcomp(df_pca_analisis %>% select(!c(cluster2,"genre")), scale. = TRUE)


viz <- cbind(pca_data, df_pca_analisis$cluster2) %>% as.data.frame()
viz$cluster2 <- viz$V12
viz$V12 <- NULL;



viz2 <- cbind(pca_data, df_pca_analisis$genre) %>% as.data.frame()
viz2$genre <- viz2$V12
viz2$V12 <- NULL;


centroids_clusters <- viz %>% group_by(cluster2) %>% summarise_all("mean")


viz2[1:11]<- viz2[1:11] %>% apply(2,as.numeric)
centroids_genre <- viz2 %>% group_by(genre) %>% summarise_all("mean", na.rm=TRUE)


# Assuming 'pca_result$rotation' is your data
rotation <- pca_result$rotation %>% as.data.frame()

# Convert the data to long format
rotation_long <- rotation %>% 
  rownames_to_column(var = "Variable") %>%
  pivot_longer(cols = -Variable, names_to = "Component", values_to = "Value")

rotation_long$Component <- factor(rotation_long$Component, levels = sort(unique(rotation_long$Component)))
# Plot the data
ggplot(rotation_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Component, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


library(ggnewscale)
viz$cluster2 <- viz$cluster2 %>% as.factor
autoplot(pca_result, data = viz2, loadings = TRUE, loadings.label = TRUE, size = 1, type = "n", scale = 0, colour="genre",x=1, y=4) +
  new_scale_color() +  # This is where you add the new color scale
  geom_point(data = centroids_clusters, aes(x = PC1, y = PC4, color = factor(cluster2)), size = 5, shape = 17) +
  theme_minimal()



cat_analisis <- df_ %>% select(c("mode","time_signature","cluster2","Album_type","Key","type","genre"))

cat_analisis$Album_type <- cat_analisis$Album_type %>% as.factor
cat_analisis$time_signature <- cat_analisis$time_signature  %>% as.factor
cat_analisis$mode <- cat_analisis$mode %>% as.factor
cat_analisis$type <- cat_analisis$type %>% as.factor
cat_analisis$genre <- cat_analisis$genre%>% as.factor

cat_analisis %>% glimpse


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = genre), stat = "count")


cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = time_signature), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = mode), stat = "count")

cat_analisis %>%
  ggplot() +
  geom_bar(aes(x = cluster2, fill = Album_type), stat = "count")

### Most important variables tempo, genre.




