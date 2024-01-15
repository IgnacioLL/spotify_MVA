#library of Association rules
library(arules)
library("arulesViz")
library(dplyr)
library("FactoMineR")
library("factoextra")

######################################## Data preparation ####################################

df_wk_i <- readRDS("preprocessing.Rdata")
df_wk_i %>% names()

# We perform this analysis with all categorical columns we have currently.
df_wk_i$Album_type <- factor(df_wk_i$Album_type)
df_wk_i$Key <- factor(df_wk_i$Key)
df_wk_i$Licensed <- factor(df_wk_i$Licensed)
df_wk_i$official_video <- factor(df_wk_i$official_video)
df_wk_i$genre <- factor(df_wk_i$genre)

#Selecting interesting categorical variables
dcat<-df_wk_i[,c(6,9,18,19,21,20)]

#Checking levels
length(levels(dcat$Album_type))
length(levels(dcat$Key))
length(levels(dcat$Licensed))
length(levels(dcat$official_video))
length(levels(dcat$genre))

foo<-function(x){length(levels(x))}
sum(sapply(dcat, foo))

# Convert the dataframe to a transactional dataset
df_trans <- as(dcat, "transactions")
summary(df_trans)
inspect(head(df_trans,10)) #list top 10 transactions
itemFrequencyPlot(df_trans, topN=10, xlab="Items")
title("Top 10 frequent items explored by ECLAT Algorithm")
itemFrequencyPlot(df_trans, topN=15, xlab="Items")

#Generate itemsets of size 1 and count their frequencies
item_freq <- itemFrequency(df_trans, type = "absolute")
#Rank the items based on frequency
top_items <- names(sort(item_freq, decreasing = TRUE))[1:15]
#Find the minimum support by taking the support of the least frequent item among the top 10
min_support <- min(itemFrequency(df_trans[, top_items], type = "relative"))

######################################## Apriori Rules mining ####################################

# Apply Apriori algorithm
rules <- apriori(df_trans, parameter = list(support = min_support, confidence = 0.75, minlen=2))

# General Rules set
summary(rules)
inspect(head(rules, n=10,  by="lift"))  

# Create a matrix to check for redundant rules
subset.matrix <- is.subset(rules, rules, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA

# Identify redundant rules
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
which(redundant)
# Prune out redundant rules
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by = "lift")
inspect(head(rules.pruned, n = 10))
inspect(head(rules, n = 10))

###Visualizing Results
plot(rules.pruned, measure = c("support", "lift"), shading = "confidence")
#order == number of items inside the rules
plot(rules, method = "grouped")

######################################## ECLAT algorithm ####################################

# Apply ECLAT algorithm
eclatDTrans <- eclat(df_trans, parameter = list(support = min_support, minlen = 2, maxlen = 5))

# Inspect the top itemsets
inspect(head(sort(eclatDTrans)))
