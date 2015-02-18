##################Bivariate LISA-Conditional Randomization####################

##A function to implement a permutation test for LISAs. Look at the companion 
# R script file 'Lisa Permutation Test.R'. The code on that scrpit forms the
# basis to this function.

bilisa.perm<-function(x,y,listw,perm=999){

zx<-x-mean(x)
zy<-y-mean(y)
n<-length(x)
Sxy<-sqrt(sum(zx^2)/(n-1))*sqrt(sum(zy^2)/(n-1))
lzy<-lag.listw(listw,zy)
Ii<-(zx/Sxy)*lzy

lisas<-matrix(0,n,perm)
rids<-matrix(0,max(card(listw$neighbours)),perm)
n_1<-n-1

for(j in 1:perm){
	rids[,j]<-sample.int(n_1,size=max(card(listw$neighbours)),replace=FALSE)
    }

for(i in 1:n){
#	x=sample(zy[-i],replace=FALSE) 
	x<-zy[-i]
	rz<-x[as.numeric(rids[1:length(listw$weights[[i]]),])]
	rz<-matrix(rz,nrow=length(listw$weights[[i]]),ncol=perm,byrow=FALSE)
	lisas[i,]=zx[i]*(listw$weights[[i]]%*%rz)
    }

rlisas<-(1/Sxy)*lisas

above<-rlisas>=Ii
larger<-rowSums(above)
low.extreme<-(perm-larger)<larger
larger[low.extreme]<-perm-larger[low.extreme]

p.sim<-(larger+1)/(perm+1)

y<-cbind(Ii,p.sim,zx)
colnames(y)<-c('Ii','p-val','Zi')
class(y)<-'lisaPerm'
return(y)
	}

#save(lisa.perm,plot.lisaPerm,file='lisa.perm.q')
