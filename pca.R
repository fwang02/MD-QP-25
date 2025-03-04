dd <- read.csv("data_preprocessed.csv")
library(ggplot2)
attach(dd)

#set a list of numerical variables
numeriques<-which(sapply(dd,is.numeric))
numeriques

dcon<-dd[,numeriques]
sapply(dcon,class)

# PRINCIPAL COMPONENT ANALYSIS OF dcon
pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)
print(pc1)
str(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)

#Cummulated Inertia in subspaces
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd = 3

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE 3 DIMENSIONS
View(pc1$x)
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]

Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]
# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(dcon)
etiq = names(dcon)
ze = rep(0,length(etiq))

# PLOT OF INDIVIDUALS and Projection of variables
Phi = cor(dcon,Psi)
View(Phi)
k <-1

for(i in 1:2){
  k = k + 1
  for(j in k:3){
    #eje1<-i
    #eje2<-j
    
    png(filename = paste0("Individuals", i, " vs ", j, ".png"), width = 1000, height = 600)
    p <- plot(Psi[,i],Psi[,j], type="n")
    text(Psi[,i],Psi[,j],labels=iden, cex=0.5)
    axis(side=1, pos= 0, labels = F, col="cyan")
    axis(side=3, pos= 0, labels = F, col="cyan")
    axis(side=2, pos= 0, labels = F, col="cyan")
    axis(side=4, pos= 0, labels = F, col="cyan")
    dev.off()
    print(p)
    
    X<-Phi[,i]
    Y<-Phi[,j]
    
    png(filename = paste0("Projection_of_variables", i, " vs ", j, ".png"), width = 1000, height = 600)
    p1 <- plot(Psi[,i],Psi[,j],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
    axis(side=1, pos= 0, labels = F)
    axis(side=3, pos= 0, labels = F)
    axis(side=2, pos= 0, labels = F)
    axis(side=4, pos= 0, labels = F)
    arrows(ze, ze, X, Y, length = 0.07,col="blue")
    text(X,Y,labels=etiq,col="darkblue", cex=0.7)
    dev.off()
    print(p1)
  }
}



detach(dd)
