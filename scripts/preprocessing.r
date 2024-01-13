library(tidyverse); library(stringr);library(ggplot2);library(gridExtra);library(mice)


df_wk <- df %>% select(!c("Uri","Url_youtube","...1","Url_spotify", "Description","type")) ## this columns won't be used.

((df_wk$Artist %>% is.na)==FALSE) %>% sum
## If VEVO is in the WORD then the channel will be VEVO
df_wk$Channel %>% table %>% sort(decreasing = TRUE)
x <- (df_wk$Channel %>% stringr::str_match(pattern = "VEVO") %>% is.na)==FALSE
df_wk$Channel[x] <- "VEVO"


df_wk$genre <- df_wk$genre %>% as.factor()
df_wk$mode <- df_wk$mode %>% as.factor()
df_wk$time_signature <- df_wk$time_signature %>% as.factor()

g1 <- ggplot(data = df_wk) + geom_histogram(aes(x=Views)) + theme_minimal()
g2 <- ggplot(data = df_wk) + geom_histogram(aes(x=Stream)) + theme_minimal()
g3 <- ggplot(data = df_wk) + geom_histogram(aes(x=Likes)) + theme_minimal()
g4 <- ggplot(data = df_wk) + geom_histogram(aes(x=Comments)) + theme_minimal()

grid.arrange(g1, g2, g3, g4, ncol = 2)

### Create logarithmic of Views, Stream, Likes and comments 
df_wk$scaled_views <- log(df_wk$Views)
df_wk$scaled_stream <- log(df_wk$Stream)
df_wk$scaled_likes <- log(df_wk$Likes)
df_wk$scaled_comments <- log(df_wk$Comments)


g1 <- ggplot(data = df_wk) + geom_histogram(aes(x=scaled_views)) + theme_minimal()
g2 <- ggplot(data = df_wk) + geom_histogram(aes(x=scaled_stream)) + theme_minimal()
g3 <- ggplot(data = df_wk) + geom_histogram(aes(x=scaled_likes)) + theme_minimal()
g4 <- ggplot(data = df_wk) + geom_histogram(aes(x=scaled_comments)) + theme_minimal()

grid.arrange(g1, g2, g3, g4, ncol = 2)

## Outliers
# Univariate outliers

## It looks like we have an outlier at 0 that must be investigated and some mild outliers with have a 200 - 250 range which they look okay
ggplot(data = df_wk) + geom_histogram(aes(x=Tempo)) + theme_minimal() 
ggplot(data = df_wk) + geom_histogram(aes(x=Duration_ms)) + theme_minimal() 


## Delete songs with Noise in the track and Tempo < 15 and in the channel White Noise - Topic
cond_Noise <- (df_wk$Track %>% stringr::str_match("Noise") %>% is.na()==FALSE)
cond_tempo <- (df_wk$Tempo <15)

df_wk <- df_wk[which((cond_Noise & cond_tempo)==FALSE),]
df_wk <- df_wk[(df_wk$Channel != 'White Noise - Topic') | is.na(df_wk$Channel),]

## We impute missings for the rest
df_wk$Tempo <- ifelse(df_wk$Tempo <15, NA, df_wk$Tempo)


## We have outliers of songs which last very long duration and some with a short duration capping might be a good solution
ggplot(data = df_wk) + geom_boxplot(aes(x=Duration_ms)) + theme_minimal() 
df_wk[df_wk$Duration_ms > 1e6,] ## Seems that are properly created and fit the objective of the analysis so we won't change them
df_wk$Duration_ms %>% min(na.rm = TRUE) ## We will consider this a valid Duration so all the above too



## We have outliers of songs with low DB
ggplot(data = df_wk) + geom_boxplot(aes(x=Loudness)) + theme_minimal() 
df_wk[df_wk$Loudness < -40,] %>% drop_na()  ## Classical music seems to be the cause. Will keep it as it is. 


ggplot(df_wk, aes(x = Artist)) +
  geom_bar() +
  labs(title = "Count of Categorical Values", x = "Artist", y = "Count")
## All Artist are represented 10 times.
df_wk$Artist %>% unique %>% length()
## As we cannot study 2.079 some grouping is needed. 

## Convert numeric those variables that have a clearly not-normal distribution
df_wk$Key <- df_wk$Key %>% as.factor()

df_wk$scaled_views <- ifelse(df_wk$scaled_views %>% is.infinite, 0, df_wk$scaled_views)
df_wk$scaled_likes <- ifelse(df_wk$scaled_likes %>% is.infinite, 0, df_wk$scaled_likes)
df_wk$scaled_comments <- ifelse(df_wk$scaled_comments %>% is.infinite, 0, df_wk$scaled_comments)
df_wk$scaled_stream <- ifelse(df_wk$scaled_stream %>% is.infinite, 0, df_wk$scaled_stream)


## Normalization of youtube related columns
## 1. Convert both official_video and Licensed to factor and encode its NA's to "NO_VIDEO"
## 2. Similarly, NA's of Title and Channel columns also encoded to "NO_VIDEO"
df_wk <- df_wk %>%
  mutate(
    official_video = factor(
      case_when(
        is.na(official_video) ~ "NO_VIDEO",
        official_video == TRUE ~ "OFFICIAL",
        official_video == FALSE ~ "NON_OFFICIAL"
      ),
      levels = c("OFFICIAL", "NON_OFFICIAL", "NO_VIDEO")
    ),
    Licensed = factor(
      case_when(
        is.na(Licensed) ~ "NO_VIDEO",
        Licensed == TRUE ~ "LICENSED",
        Licensed == FALSE ~ "UNLICENSED"
      ),
      levels = c("LICENSED", "UNLICENSED", "NO_VIDEO")
    ),
    Title = ifelse(is.na(Title), "NO_VIDEO", Title),
    Channel = ifelse(is.na(Channel), "NO_VIDEO", Channel),
  )
df_wk %>% apply(2, is.na) %>% apply(2, sum)


df_wk_impute <- df_wk %>% select(!c("Artist","Track","Album","Title","Channel"))

df_wk_impute <- mice(df_wk_impute, method="cart", m=1) ## We use CART method as it gives no problem for Computationally singular errors.
df_wk_impute <- mice::complete(df_wk_impute)
add_df_wk <- df_wk %>% select(c("Artist","Track","Album","Title","Channel"))

df_wk_i <- cbind(add_df_wk, df_wk_impute)


df_wk_i$scaled_comments <- ifelse(is.na(df_wk_i$scaled_comments), 0, df_wk_i$scaled_comments) # We will impute log(0) values as 0.
df_wk_i$scaled_likes <- ifelse(is.na(df_wk_i$scaled_likes), 0, df_wk_i$scaled_likes) # We will impute log(0) values as 0.
df_wk_i$scaled_views <- ifelse(is.na(df_wk_i$scaled_views), 0, df_wk_i$scaled_views) # We will impute log(0) values as 0.
df_wk_i$Stream

df_wk_i <- df_wk_i %>% select(!c("Stream","Views","Comments","Likes")) ## this columns won't be used.


saveRDS(df_wk_i, file = "preprocessing.Rdata")
