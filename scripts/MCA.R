##############################################
### MCA Multiple Correspondence Analysis   ###
###                                        ###
##############################################


#setwd("C:\\Users\\MVA\\Desktop\\UPC") #Set your Working
# Directory
setwd("/Users/maxtico/Documents/Master Data Science/MVA/PROJECT2/spotify_MVA/")
### Required packages ###
library(FactoMineR)
library(Matrix)
library(factoextra)
library(corrplot)

?MCA
### Lectura de la base de dades ###
spoty <- readRDS("preprocessing.RData")
dim(spoty)
summary(spoty)
names(spoty)

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
barplot(res.mca0$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.mca0$eig)) ## CHECK P-VALUE Licensed + Official_video

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
fviz_mca_biplot(res.mca0,repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
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
head(round(var$contrib,2), 4)
# Total contribution to dimension 1 and 2
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
             habillage = "Album_type", # color by groups 
             palette = c("#00AFBB", "#E7B800","red"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

fviz_ellipses(res.mca0, c("official_video", "Album_type"),
              geom = "point")
###############********************************************####################
#Approaching MCA with the Burt Table, method="Burt"

res.mca<-MCA(spoty[,qualitative_variables], quanti.sup=c(4,5), method="Burt",graph=FALSE)
#Comparing results
plot(res.mca0,invisible=c("ind","quali.sup"), cex=0.9)
plot(res.mca,invisible=c("ind","quali.sup"), cex=0.9)

#plot individuals & modalities
plot(res.mca,invisible="quali.sup", cex=0.5) #You can change cex parameter to improve labels size

#Active Modalities & Suplementary Modalities
plot(res.mca,invisible="ind", cex=0.75)

#Illustratives(Suplementary) Modalities only
plot(res.mca,invisible=c("ind","var"), cex=0.70)


#vaps

res.mca$eig
round(res.mca$eig,2)

barplot(res.mca$eig[,1],main="EigenValues",names.arg=1:nrow(res.mca$eig))

totalIner<- sum(res.mca$eig[,1])
pinerEix<- 100*res.mca$eig[,1]/totalIner
barplot(cumsum(pinerEix))

# Individuals coordinates
res.mca$ind$coord

#Modalities coordinates
c<-res.mca$var$coord


#contributions
#res.mca$ind$contrib
res.mca$var$contrib

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
plotellipses(res.mca,keepvar=c("quali.sup"))
