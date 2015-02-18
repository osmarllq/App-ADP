plot.classInt<-function(x,shape,n=5,palette='rainbow',transparency=1,start=4/6,end=1/6,style='jenks',
main='',legend.title='',dataPrecision=1){

stopifnot(is.null(library(classInt))==FALSE)

if(palette=='rainbow'){palette<-rainbow(n,start=start,end=end,alpha=transparency)
	}else{palette<-get(palette)(n,alpha=transparency)}
palette<-palette
fj<-classIntervals(x, n=n, style=style,dataPrecision=dataPrecision)
fjc<-findColours(fj, palette) 

		names(attr(fjc,'table'))<-		#Se manipula la leyenda de los intervalos
			gsub(',',' - ',names(attr(fjc,'table')))
		legtext<-paste(names(attr(fjc,'table')),' (',attr(fjc,'table'),')',sep='')

plot(shape@bbox,t='n',axes=FALSE,ylab='',xlab='')
    for(i in 1:nrow(shape)){
        polygon(shape@polygons[[i]]@Polygons[[1]]@coords,col=fjc[i],lty=3)
    }
box()
legend("bottomright", fill=palette, legend=legtext,
 bty="n", cex=0.8, title=legend.title, title.col='black')
title(main, sub='',cex.sub=.6)
}

