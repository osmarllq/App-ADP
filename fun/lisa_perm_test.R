lisa.perm<-function(x,listw,perm=999){
    
    z<-x-mean(x)
    n<-length(x)
    s2<-sum(z^2)/(n-1)
    lz<-lag.listw(listw,z)
    Ii<-(z/s2)*lz
    
    lisas<-matrix(0,n,perm)
    rids<-matrix(0,max(card(listw$neighbours)),perm)
    n_1<-n-1
    
    for(j in 1:perm){
        rids[,j]<-sample.int(n_1,size=max(card(listw$neighbours)),replace=FALSE)
    }
    
    for(i in 1:n){
        #	x=sample(z[-i],replace=FALSE) 
        x<-z[-i]
        rz<-x[as.numeric(rids[1:length(listw$weights[[i]]),])]
        rz<-matrix(rz,nrow=length(listw$weights[[i]]),ncol=perm,byrow=FALSE)
        lisas[i,]=z[i]*(listw$weights[[i]]%*%rz)
    }
    
    rlisas<-(1/s2)*lisas
    
    above<-rlisas>=Ii
    larger<-rowSums(above)
    low.extreme<-(perm-larger)<larger
    larger[low.extreme]<-perm-larger[low.extreme]
    
    p.sim<-(larger+1)/(perm+1)
    
    y<-cbind(Ii,p.sim,z)
    colnames(y)<-c('Ii','p-val','Zi')
    class(y)<-'lisaPerm'
    return(y)
}

