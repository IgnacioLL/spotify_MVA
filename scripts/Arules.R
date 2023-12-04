#library of Association rules
library(arules)
library("arulesViz")
library(dplyr)
library("FactoMineR")
library("factoextra")

######################################## Data preparation ####################################

df_wk_i <- readRDS("report/preprocessing.Rdata")
df_wk_i %>% names()

# We perform this analysis with all categorical columns we have currently.
df_cat <- df_wk_i %>%
  select("Artist", "Track", "Album", "Key", "instrumental_band", 
         "speech_band", "Album_type",  "Licensed", "official_video", "genre", "type", "time_signature",
         "mode") %>%
  mutate(across(everything(), as.factor))



# Compute "Popularity" column, which sums up "scaled_views" with "scaled_stream"
df_cat$popularity <- df_wk_i$scaled_views + df_wk_i$scaled_stream

# Categorize the 'popularity' column into four categories, defining by their quantiles
quantiles <- quantile(df_cat$popularity, probs = c(0, 0.25, 0.5, 0.75, 1))

df_cat$popularity_category <- cut(df_cat$popularity, 
                              breaks = quantiles, 
                              labels = c("Low", "Medium", "High", "Very High"),
                              include.lowest = TRUE)

table(df_cat$popularity_category)
df_cat$popularity <- NULL


# Convert the dataframe to a transactional dataset
df_trans <- as(df_cat, "transactions")
summary(df_trans)
inspect(head(df_trans,10)) #list top 10 transactions
itemFrequencyPlot(df_trans, topN=10, xlab="Top 10 Frequent Items")
itemFrequencyPlot(df_trans, topN=20, cex.names=0.8, xlab="Top 20 Frequent Items")


######################################## Apriori Rules mining ####################################

# Apply Apriori algorithm
rules <- apriori(df_trans, parameter = list(support = 0.10, confidence = 0.30, minlen=3))

# General Rules set
summary(rules)
inspect(head(rules, n=20,  by="lift"))  

# Create subsets of rules, taking into account for the rhs there are 4 different levels of popularity
# Subset for popularity_category = Medium is empty


# Subset of rules when popularity_category = Low
popularityLow_subset <- subset(rules, subset = rhs %in% "popularity_category=Low")
inspect(head(popularityLow_subset,by="lift"))


# Subset of rules when popularity_category = High
popularityHigh_subset <- subset(rules, subset = rhs %in% "popularity_category=High")
inspect(head(popularityHigh_subset,by="lift"))

# Subset of rules when popularity_category = Very High
popularityVeryHigh_subset <- subset(rules, subset = rhs %in% "popularity_category=Very High")
inspect(head(popularityVeryHigh_subset,by="lift"))


######################################## ECLAT algorithm ####################################

# Apply ECLAT algorithm
eclat_itemsets <- eclat(df_trans, parameter = list(supp = 0.30, minlen = 3))

# Inspect the top itemsets
inspect(head(sort(eclat_itemsets)))


# Create a matrix to check for redundant rules
subset.matrix <- is.subset(rules, rules, sparse = FALSE)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA

# Identify redundant rules
redundant <- colSums(subset.matrix, na.rm = TRUE) >= 1
redundant_indices <- which(redundant)
# Prune out redundant rules
rules_pruned <- rules[!redundant]

rules_pruned <- sort(rules_pruned, by = "lift")
inspect(head(rules_pruned, n = 20))

