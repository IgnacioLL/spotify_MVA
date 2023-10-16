library(tidyverse); library(stringr);library(ggplot2);library(gridExtra)



df_wk <- df %>% select(!c("Uri","Url_youtube","...1","Url_spotify", "Description")) ## this columns won't be used.
df_wk %>% glimpse
((df_wk$Artist %>% is.na)==FALSE) %>% sum
## If VEVO is in the WORD then the channel will be VEVO
df_wk$Channel %>% table %>% sort(decreasing = TRUE)
x <- (df_wk$Channel %>% stringr::str_match(pattern = "VEVO") %>% is.na)==FALSE
df_wk$Channel[x] <- "VEVO"


g1 <- ggplot(data = df_wk) + geom_histogram(aes(x=Views)) + theme_minimal()
g2 <- ggplot(data = df_wk) + geom_histogram(aes(x=Stream)) + theme_minimal()
g3 <- ggplot(data = df_wk) + geom_histogram(aes(x=Likes)) + theme_minimal()
g4 <- ggplot(data = df_wk) + geom_histogram(aes(x=Comments)) + theme_minimal()

grid.arrange(g1, g2, g3, g4, ncol = 2)

### Create logarithmic of Views, Stream, Likes and comments 
df_wk$log_views <- log(df_wk$Views+1)
df_wk$log_stream <- log(df_wk$Stream+1)
df_wk$log_likes <- log(df_wk$Likes+1)
df_wk$log_comments <- log(df_wk$Comments+1)


g1 <- ggplot(data = df_wk) + geom_histogram(aes(x=log_views)) + theme_minimal()
g2 <- ggplot(data = df_wk) + geom_histogram(aes(x=log_stream)) + theme_minimal()
g3 <- ggplot(data = df_wk) + geom_histogram(aes(x=log_likes)) + theme_minimal()
g4 <- ggplot(data = df_wk) + geom_histogram(aes(x=log_comments)) + theme_minimal()


## Outliers
# Univariate outliers

## It looks like we have an outlier at 0 that must be investigated and some mild outliers with have a 200 - 250 range which they look okay
ggplot(data = df_wk) + geom_boxplot(aes(x=Tempo)) + theme_minimal() 


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





