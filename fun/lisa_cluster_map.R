###############################LISA Cluster Map################################

plot.lisaPerm<-function(lmoran,shape,signif=.05,legend.title='',lty=1,lwd=1){
    if(class(lmoran)!='lisaPerm')stop('object class is not lisa.perm')
    
    quadrant <- vector(mode="numeric",length=nrow(lmoran))
    
    quadrant[lmoran[,3] >0 & lmoran[,1]>0] <- 1 #H-H   
    quadrant[lmoran[,3] <0 & lmoran[,1]>0] <- 2 #L-L
    quadrant[lmoran[,3] <0 & lmoran[,1]<0] <- 3 #L-H     
    quadrant[lmoran[,3] >0 & lmoran[,1]<0] <- 4 #H-L
    quadrant[lmoran[,2]>signif] <- 0    
    
    brks <- c(0,1,2,3,4)
    palette <- c("white","red","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4))
    colors<-palette[findInterval(quadrant,brks,all.inside=FALSE)]
    
    plot(shape@bbox,t='n',axes=FALSE,ylab='',xlab='')
    for(i in 1:nrow(shape)){
        polygon(shape@polygons[[i]]@Polygons[[1]]@coords,col=colors[i],lty=lty,lwd=lwd)
    }
    box()
    legend("bottomright",legend=c("insignificant","high-high","low-low","low-high","high-low"),
           fill=palette,bty="n",cex=0.9,y.intersp=1,x.intersp=1,title=legend.title)
    title("LISA Cluster Map")
    
}


