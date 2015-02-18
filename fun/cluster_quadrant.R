clusterQuadrant <- function(lmoran,signif=.05){
 if(class(lmoran)!='lisaPerm')stop('object class is not lisa.perm')

  quadrant <- vector(mode="numeric",length=nrow(lmoran))

  quadrant[lmoran[,3] >0 & lmoran[,1]>0] <- 1 #H-H   
  quadrant[lmoran[,3] <0 & lmoran[,1]>0] <- 2 #L-L
  quadrant[lmoran[,3] <0 & lmoran[,1]<0] <- 3 #L-H     
  quadrant[lmoran[,3] >0 & lmoran[,1]<0] <- 4 #H-L
  quadrant[lmoran[,2]>signif] <- 0    
  return(quadrant)
}

