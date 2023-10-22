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

