df_wk_i <- readRDS("../preprocessing.Rdata")

#get numerical variables (with no missing values)
numeriques<-which(sapply(df_wk_i,is.numeric))
numeriques

df_wk_num<-df_wk_i[,numeriques]
sapply(df_wk_num,class)
#remove scaled, because they have infinite values
df_wk_num <- df_wk_num[, !(names(df_wk_num) %in% c("scaled_views", "scaled_stream", "scaled_likes", "scaled_comments"))]


# PRINCIPAL COMPONENT ANALYSIS
pc1 <- prcomp(df_wk_num, scale=TRUE)
print(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?

pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix #percentage of represented data (NOT REALLY GOOD)
barplot(pinerEix)

#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(df_wk_num)[2]]^2)/dim(df_wk_num)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(df_wk_num)[2]]^2)/dim(df_wk_num)[2]
percInerAccum


# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 8

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(df_wk_num)
etiq = names(df_wk_num)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS

# PLOT OF INDIVIDUALS

#select your axis
#eje1<-2
eje1<-1
#eje2<-3
eje2<-2

plot(Psi[,eje1],Psi[,eje2], type="n")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
#library(rgl)
#plot3d(Psi[,1],Psi[,2],Psi[,3])

#Projection of variables

Phi = cor(df_wk_num,Psi) #correlation between principal components and numerical
View(Phi)

#select your axis

X<-Phi[,eje1]
Y<-Phi[,eje2]

plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)

#Qualitative

# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)
df_wk_i$Album_type <- as.factor(df_wk_i$Album_type)

varcat=factor(df_wk_i[,29])
plot(Psi[,1],Psi[,2])
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)

#select your qualitative variable
k<-6 #dictamen in credsco

varcat<-factor(df_wk_i[,k])
fdic1 = tapply(Psi[,eje1],varcat,mean)#centroid
fdic2 = tapply(Psi[,eje2],varcat,mean) 
#points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=0.7)

#if we have many variables, we can split every variables with many variables, but when we represent centroids, we represent all the numerical and categorical variables


#Now we project both cdgs of levels of a selected qualitative variable without
#representing the individual anymore

plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#select your qualitative variable
k<-6 #dictamen in credsco

#varcat<-df_wk_i[,k]
#fdic1 = tapply(Psi[,eje1],varcat,mean)
#fdic2 = tapply(Psi[,eje2],varcat,mean) 

#points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)


#all qualitative together
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#nominal qualitative variables

dcat<-c(6,9,21,22)
#divide categoricals in several graphs if joint representation saturates

#build a palette with as much colors as qualitative variables 

#colors<-c("blue","red","green","orange","darkgreen")
#alternative
colors<-rainbow(length(dcat))

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],df_wk_i[,k],mean)
  fdic2 = tapply(Psi[,eje2],df_wk_i[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(factor(df_wk_i[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df_wk_i)[dcat],pch=1,col=colors, cex=0.6)

#determine zoom level
#use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
fm = round(max(abs(Psi[,1]))) 
fm=20

#scale the projected variables
#X<-fm*U[,eje1]
#Y<-fm*U[,eje2]

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1), ylim=c(-3,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf_wk_i projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.7)

#adf_wk_i centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],df_wk_i[,k],mean)
  fdic2 = tapply(Psi[,eje2],df_wk_i[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df_wk_i[,k]))
  text(fdic1,fdic2,labels=levels(factor(df_wk_i[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df_wk_i)[dcat],pch=1,col=colors, cex=0.6)


#add ordinal variables
dordi<-c(28,29)

c<-1
col<-1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eje1],df_wk_i[,k],mean)
  fdic2 = tapply(Psi[,eje2],df_wk_i[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df_wk_i[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(df_wk_i[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(df_wk_i)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)


