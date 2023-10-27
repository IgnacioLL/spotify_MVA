df_wk_i <- readRDS("../preprocessing.Rdata")

#get numerical variables (with no missing values)
numeriques<-which(sapply(df_wk_i,is.numeric))
numeriques

df_wk_num<-df_wk_i[,numeriques]
sapply(df_wk_num,class)
#remove none scaled variables + transformed to categorical variables
df_wk_num <- df_wk_num[, !(names(df_wk_num) %in% c("Views", "Stream", "Likes", "Comments","Speechiness","Instrumentalness"))]
sapply(df_wk_num,class)
colnames(df_wk_num)[colnames(df_wk_num) == "scaled_views"] <- "Views"
colnames(df_wk_num)[colnames(df_wk_num) == "scaled_comments"] <- "Comments"
colnames(df_wk_num)[colnames(df_wk_num) == "scaled_likes"] <- "Likes"
colnames(df_wk_num)[colnames(df_wk_num) == "scaled_stream"] <- "Streams"

# PRINCIPAL COMPONENT ANALYSIS
pc1 <- prcomp(df_wk_num, scale=TRUE)
print(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

inerProj<- pc1$sdev^2 #eigenvalues
inerProj
totalIner<- sum(inerProj)
pinerEix<- 100*inerProj/totalIner
barplot(pinerEix, names.arg = 1:length(pinerEix), 
        main = "Scree Plot", xlab = "Principal Component", 
        ylab = "Variance Explained")

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
cumulative_variance <- cumsum(inerProj) / totalIner * 100
barplot(cumulative_variance, names.arg = 1:length(cumulative_variance),
        main = "Cumulative Variance Explained (Scree Plot)",
        xlab = "Principal Component", ylab = "Cumulative Variance Explained")
# Add a red horizontal line at y = 80%
abline(h = 80, col = "red")

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 6

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(df_wk_num)
etiq = names(df_wk_num)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

#Influence of each variable on each dimension
loadings <- pc1$rotation
loadings_dimension1 <- loadings[, 1]
loadings_dimension2 <- loadings[, 2]
loadings_dimension3 <- loadings[, 3]
loadings_dimension4 <- loadings[, 4]
loadings_dimension5 <- loadings[, 5]
loadings_dimension6 <- loadings[, 6]

barplot(loadings_dimension1, names.arg = colnames(loadings_dimension1), col = "blue", ylab = "Loadings", main = "Loadings for Dimension 1", las = 2)
barplot(loadings_dimension2, names.arg = colnames(loadings_dimension2), col = "blue", ylab = "Loadings", main = "Loadings for Dimension 2", las = 2)
barplot(loadings_dimension3, names.arg = colnames(loadings_dimension3), col = "blue", ylab = "Loadings", main = "Loadings for Dimension 3", las = 2)
barplot(loadings_dimension4, names.arg = colnames(loadings_dimension4), col = "blue", ylab = "Loadings", main = "Loadings for Dimension 4", las = 2)
barplot(loadings_dimension5, names.arg = colnames(loadings_dimension5), col = "blue", ylab = "Loadings", main = "Loadings for Dimension 5", las = 2)
barplot(loadings_dimension6, names.arg = colnames(loadings_dimension6), col = "blue", ylab = "Loadings", main = "Loadings for Dimension 6", las = 2)

# PLOT OF INDIVIDUALS

#select your axis
#eje1<-2
eje1<-1
#eje2<-3
eje2<-2

#Projection of variables

Phi = cor(df_wk_num,Psi) #correlation between principal components and numerical

#select your axis

X<-Phi[,eje1]
Y<-Phi[,eje2]

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1), xlab = paste("PC", eje1), ylab = paste("PC", eje2), main = "Factorial Map - PC1 vs PC2")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X[1:8],Y[1:8],labels=etiq[1:8],col="darkblue", cex=0.7, pos=2)
text(X[9:10],Y[9:10],labels=etiq[9:10],col="darkblue", cex=0.7, pos=3)
text(X[11],Y[11],labels=etiq[11],col="darkblue", cex=0.7, pos=1)
text(X[12],Y[12],labels=etiq[12],col="darkblue", cex=0.7, pos=3)

#Qualitative

df_wk_i$Album_type <- as.factor(df_wk_i$Album_type)

#nominal qualitative variables

dcat<-c(6,9,21,22)
#divide categoricals in several graphs if joint representation saturates

#build a palette with as much colors as qualitative variables 

colors<-c("blue","red","darkgreen","orange")

#use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
fm = round(max(abs(Psi[,1]))) 
fm=20

#scale the projected variables
#X<-fm*U[,eje1]
#Y<-fm*U[,eje2]

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1.2), ylim=c(-1,1), xlab = paste("PC", eje1), ylab = paste("PC", eje2), main = "Factorial Map (Qualitative Variables)")
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)

#adf_wk_i projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X[1:8],Y[1:8],labels=etiq[1:8],col="gray", cex=0.7)
text(X[9:10],Y[9:10],labels=etiq[9:10],col="gray", cex=0.7, pos=3)
text(X[11],Y[11],labels=etiq[11],col="gray", cex=0.7, pos=1)
text(X[12],Y[12],labels=etiq[12],col="gray", cex=0.7, pos=3)

#adf_wk_i centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],df_wk_i[,k],mean)
  fdic2 = tapply(Psi[,eje2],df_wk_i[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df_wk_i[,k]))
  text(fdic1,fdic2,labels=levels(factor(df_wk_i[,k])),col=seguentColor, cex=0.8)
  c<-c+1
}
legend("bottomleft",names(df_wk_i)[dcat],pch=1,col=colors, cex=0.8)

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1.2), ylim=c(-1,1), xlab = paste("PC", eje1), ylab = paste("PC", eje2), main = "Factorial Map (Ordinal Variables)")
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)

#adf_wk_i projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X[1:8],Y[1:8],labels=etiq[1:8],col="gray", cex=0.7)
text(X[9:10],Y[9:10],labels=etiq[9:10],col="gray", cex=0.7, pos=3)
text(X[11],Y[11],labels=etiq[11],col="gray", cex=0.7, pos=1)
text(X[12],Y[12],labels=etiq[12],col="gray", cex=0.7, pos=3)

#add ordinal variables
dordi<-c(28,29)

colors<-c("green","purple")

c<-1
col<-1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eje1],df_wk_i[,k],mean)
  fdic2 = tapply(Psi[,eje2],df_wk_i[,k],mean) 
  
  # points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df_wk_i[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(df_wk_i[,k]),col=seguentColor, cex=0.8)
  c<-c+1
  col<-col+1
}
legend("bottomleft",names(df_wk_i)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.8)


