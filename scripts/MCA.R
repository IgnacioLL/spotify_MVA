##############################################
### MCA Multiple Correspondence Analysis   ###
###                                        ###
##############################################


### Required packages ###
library(FactoMineR)
library(Matrix)
library(factoextra)
library(corrplot)

spoty <- readRDS("../preprocessing.RData")
dim(spoty)
summary(spoty)
names(spoty)

##### FIRST ANALYSIS WITH LICENSED VARIABLE

#Running MCA Analysis
spoty$genre <- as.factor(spoty$genre)
spoty$time_signature <- as.factor(spoty$time_signature)
spoty$mode <- as.factor(spoty$mode)
spoty$Album_type <- as.factor(spoty$Album_type)
qualitative_variables <- which(sapply(spoty, is.factor))
qualitative_variables_list <- as.list(qualitative_variables)

quali <- c(6,9,21,22,24,25,26,28,30,32,33)
quali_no_lis <- c(6,9,22,24,25,26,28,30,32,33)
# numeriques<-which(sapply(spoty,is.numeric))
# #remove none scaled variables + transformed to categorical variables + Likes and Comments very correlated to Views
# df_n<-spoty[,numeriques]
# df_n <- df_n[, !(names(df_n) %in% c("Views", "Stream", "Likes", "Comments","Speechiness","Instrumentalness", "scaled_likes", "scaled_comments"))]
# 
# quanti_sup_indices <- which(names(spoty) %in% names(df_n))
# quanti.sup=c(17,23,24),

res.mca0 <- MCA(spoty[, quali_no_lis],quanti.sup = c(7,8),graph = FALSE) # Pass numerical features as extra information
# res.mca0 will be calculated by using the LOGICAL TABLE or Indicator Table, Method by default = "Indicator"
# At the end after getting good knowledge of the MCA R function, you can run MCA analysis by selecting graph=TRUE

# RESULTS
attributes(res.mca0)
# Eigenvalues and Inertia
res.mca0$eig
mar=c(5, 5, 4, 6)
par(mfrow = c(1,2))
barplot(res.mca0$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.mca0$eig),
        ylab = "Eigenvalues", xlab = "Dimensions") ## CHECK P-VALUE Licensed + Official_video
barplot(res.mca0$eig[,2],main="% of Inertia",names.arg=1:nrow(res.mca0$eig),
        ylab = "% of Inertia", xlab = "Dimensions")
par(mfrow = c(1,1))

# Inertia:
round(res.mca0$eig,2)
#Quick summary of the results (Try to use it once a good knowledge of the MCA function
# is obained)
dimdesc(res.mca0)

# Contributions of the individuals and variables
res.mca0$ind$contrib
res.mca0$var$contrib

(res.mca0$var$contrib[,1]) #Contributions of the variables for Dim1
sum(res.mca0$var$contrib[,1])

#plotting individuals
plot(res.mca0,invisible=c("var","quali.sup"),cex=0.7)
# Plot (agregar plots de MCA con factoextra y factominer)


#Coordinates of the individuals on factorial plane (transformed space)
#
res.mca0$ind$coord
dim(res.mca0$ind$coord)

#plot ACTIVE MODALITIES 
plot(res.mca0,invisible=c("ind","quali.sup"), cex=0.9)

#Coordinates of the modalities on factorial plane (transformed space)
res.mca0$var$coord ## TAKE A LOOK INTO OFFICIAL_VIDEO + LICENSED (SAME COORDINATES, DIMENSIONS ACCORDING ONLY TO THESE TWO VARIABLES)
## REDUNDANCY BETWEEN OFFICIAL VIDEO + LICENSED
dim(res.mca0$var$coord)
class(res.mca0$var$coord)
#Saving these coordinates in a new system variable as data frame
MatriuFactors<-as.data.frame(res.mca0$var$coord)
# Only using two first dimensions
plot(MatriuFactors[,1],MatriuFactors[,2])
#With lines shown above you can use Karina Gibert PCA script to plot variables and 
# modalities

##########################Factoextra options for PLOTS##############
fviz_mca_var(res.mca0, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#The plot above helps to identify variables that are the most correlated
# with each dimension. The squared correlations between variables and the dimensions
# are used as coordinates.

### GRAPHICAL ANALYSIS - BIPLOTS
### BIPLOT
#fviz_mca_biplot(res.mca0,repel = TRUE, # Avoid text overlapping (slow if many point)
#                ggtheme = theme_minimal())
#The plot above shows a global pattern within the data. Rows (individuals) are represented
# by blue points and columns (variable categories) by red triangles
# The distance between any row points or column points gives a measure of their similarity
# (or dissimilarity). Row points with similar profile are closed on
# the factor map. The same holds true for column points.Supplementary variables are shown in green color.


fviz_mca_var(res.mca0, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#fviz_mca_var(res.mca0, col.var="black", shape.var = 15,
#             repel = TRUE)
#Variable categories with a similar profile are grouped together.
#Negatively correlated variable categories are positioned on opposite sides of the plot
#origin (opposed quadrants).
#The distance between category points and the origin measures the quality
#of the variable category on the factor map. Category points that are away from
# the origin are well represented on the factor map.

#Contribution of variable categories to the dimensions
# Showing the first four variables contributions
head(round(res.mca0$var$contrib,2), 9)
# Showing all variable contributions
round(res.mca0$var$contrib,2)
# Total contribution to dimension 1 and 2
nrow(res.mca0$var$v.test) # Number of modalities
fviz_contrib(res.mca0, choice = "var", axes = 1:2, top = 15)
fviz_mca_var(res.mca0, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
########### CANVIAR habillage
##With Individuals you can perform the same previous analysis
fviz_mca_ind(res.mca0, 
             label = "none", # hide individual labels
             habillage = "genre", # color by groups 
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca0, 
             label = "none", # hide individual labels
             habillage = "instrumental_band", # color by groups 
             palette = c("#00AFBB", "#E7B800","red","grey"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca0, c("official_video", "speech_band"),
              geom = "point")

##########################################****#
#### SECOND ANALYSIS WITHOUT LICENSED

#Running MCA Analysis
# res.mca0 will be calculated by using the LOGICAL TABLE or Indicator Table,
# Method by default = "Indicator"
qualitative_variables <- c(6,9,22,24,25,28,29)
res.mca0<-MCA(spoty[,qualitative_variables], quanti.sup=c(4,5), graph = FALSE) # Pass numerical features as extra information
# At the end after getting good knowledge of the MCA R function, you can run
# MCA analysis by selecting graph=TRUE)

# RESULTS
attributes(res.mca0)
# Eigenvalues and Inertia
res.mca0$eig
mar=c(5, 5, 4, 6)
par(mfrow = c(1,2))
barplot(res.mca0$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.mca0$eig),
        ylab = "Eigenvalues", xlab = "Dimensions") ## CHECK P-VALUE Licensed + Official_video
barplot(res.mca0$eig[,2],main="% of Inertia",names.arg=1:nrow(res.mca0$eig),
        ylab = "% of Inertia", xlab = "Dimensions")
par(mfrow = c(1,1))

# Inertia:
round(res.mca0$eig,2)
#Quick summary of the results (Try to use it once a good knowledge of the MCA function
# is obained)
dimdesc(res.mca0)

# Contributions of the individuals and variables
res.mca0$ind$contrib
res.mca0$var$contrib

(res.mca0$var$contrib[,1]) #Contributions of the variables for Dim1
sum(res.mca0$var$contrib[,1])

#plotting individuals
plot(res.mca0,invisible=c("var","quali.sup"),cex=0.7)
# Plot (agregar plots de MCA con factoextra y factominer)


#Coordinates of the individuals on factorial plane (transformed space)
#
res.mca0$ind$coord
dim(res.mca0$ind$coord)

#plot ACTIVE MODALITIES 
plot(res.mca0,invisible=c("ind","quali.sup"), cex=0.9)

#Coordinates of the modalities on factorial plane (transformed space)
res.mca0$var$coord ## TAKE A LOOK INTO OFFICIAL_VIDEO + LICENSED (SAME COORDINATES, DIMENSIONS ACCORDING ONLY TO THESE TWO VARIABLES)
## REDUNDANCY BETWEEN OFFICIAL VIDEO + LICENSED
dim(res.mca0$var$coord)
class(res.mca0$var$coord)
#Saving these coordinates in a new system variable as data frame
MatriuFactors<-as.data.frame(res.mca0$var$coord)
# Only using two first dimensions
plot(MatriuFactors[,1],MatriuFactors[,2])
#With lines shown above you can use Karina Gibert PCA script to plot variables and 
# modalities

##########################Factoextra options for PLOTS##############
fviz_mca_var(res.mca0, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#The plot above helps to identify variables that are the most correlated
# with each dimension. The squared correlations between variables and the dimensions
# are used as coordinates.

### GRAPHICAL ANALYSIS - BIPLOTS
### BIPLOT
#fviz_mca_biplot(res.mca0,repel = TRUE, # Avoid text overlapping (slow if many point)
 #               ggtheme = theme_minimal())
#The plot above shows a global pattern within the data. Rows (individuals) are represented
# by blue points and columns (variable categories) by red triangles
# The distance between any row points or column points gives a measure of their similarity
# (or dissimilarity). Row points with similar profile are closed on
# the factor map. The same holds true for column points.Supplementary variables are shown in green color.


fviz_mca_var(res.mca0, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
#fviz_mca_var(res.mca0, col.var="black", shape.var = 15,
#             repel = TRUE)
#Variable categories with a similar profile are grouped together.
#Negatively correlated variable categories are positioned on opposite sides of the plot
#origin (opposed quadrants).
#The distance between category points and the origin measures the quality
#of the variable category on the factor map. Category points that are away from
# the origin are well represented on the factor map.

#Contribution of variable categories to the dimensions
head(round(res.mca0$var$contrib,2), 4)
# Showing all variable contributions
round(res.mca0$var$contrib,2)
# Total contribution to dimension 1 and 2
nrow(res.mca0$var$v.test) # Number of modalities
fviz_contrib(res.mca0, choice = "var", axes = 1:2, top = 15)
fviz_mca_var(res.mca0, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)
########### CANVIAR habillage
##With Individuals you can perform the same previous analysis
fviz_mca_ind(res.mca0, 
             label = "none", # hide individual labels
             habillage = "official_video", # color by groups 
             palette = c("#00AFBB", "#E7B800","darkgreen"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca0, 
             label = "none", # hide individual labels
             habillage = "speech_band", # color by groups 
             palette = c("#00AFBB", "#E7B800","red","grey"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca0, c("official_video", "speech_band"),
              geom = "point")


###############********************************************####################
#Approaching MCA with the Burt Table, method="Burt"

res.mca<-MCA(spoty[,qualitative_variables], quanti.sup=c(4,5), method="Burt",graph=FALSE)
#Comparing results
plot(res.mca,invisible=c("ind","quali.sup"), cex=0.9)

par(mfrow = c(1,1))
barplot(res.mca$eig[,3],main="% of Inertia",names.arg=1:nrow(res.mca0$eig),
        ylab = "% of Inertia", xlab = "Dimensions")
abline(h=80, col="red", lty=2)

res.mca$eig
#plot individuals & modalities
plot(res.mca,invisible="quali.sup", cex=0.5) #You can change cex parameter to improve labels size

#Active Modalities & Suplementary Modalities
plot(res.mca,invisible="ind", cex=0.75)

#Illustratives(Suplementary) Modalities only
#plot(res.mca,invisible=c("ind","var"), cex=0.70)


#vaps

res.mca$eig
round(res.mca$eig,2)

barplot(res.mca$eig[,2],main="% of Inertia",names.arg=1:nrow(res.mca$eig),
        xlab = "Dimensions", ylab = "% of Inertia")

totalIner<- sum(res.mca$eig[,1])
pinerEix<- 100*res.mca$eig[,1]/totalIner
barplot(cumsum(pinerEix))

# Individuals coordinates
res.mca$ind$coord

#Modalities coordinates
c<-res.mca$var$coord

#contributions for the burt plot
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)

#res.mca$ind$contrib
res.mca$var$contrib

# Plotting by speech_band & official_video
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "official_video", # color by groups 
             palette = c("#00AFBB", "#E7B800","darkgreen"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = "speech_band", # color by groups 
             palette = c("#00AFBB", "#E7B800","red","grey"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca, c("official_video", "speech_band"),
              geom = "point")

#contribucions a cada eix i test-values significatius
dimdesc(res.mca)

a<-dimdesc(res.mca) 
a
a[1]
a[2]
a[[1]]$quali

#Confidence Regions "Ellipses"
plotellipses(res.mca,keepvar=c("speech_band","official_video","Key","instrumental_band"), cex=0.4)
plotellipses(res.mca,keepvar=c("quali"))
#plotellipses(res.mca,keepvar=c("quali.sup"))