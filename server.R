#################################################################################

source('fun/lisa_perm_test.R')
source('fun/bi_lisa_perm_test.R')
source('fun/lisa_cluster_map.R')
source('fun/cluster_quadrant.R')
source('fun/plot_classInt.R')

require(boot);require(maptools);require(spdep);require(Matrix)
require(RColorBrewer);require(maptools);require(foreign)
require(sp);require(grid);require(lattice);require(rgeos)
require(rgdal);require(leaflet)



#---------------------------Total Energy Consumption----------------------------#

#setwd('C:/Users/Osmar/Documents/App ADP/shiny')

#odh <- read.table('data/odh_col_0.csv',sep=';',encoding='WINDOWS-1252')

#data <- read.table('data/data.csv',sep=';',header=TRUE,encoding='WINDOWS-1252')
#data <- data[order(data$COD),]
#data[,-c(1,2,63)] <- scale(data[,-c(1,2,63)]) ## Data scaling

#ant<-readShapePoly('data/antioquia.shp')
#row.names(ant)<-as.character(ant$ID_ESPAC_2) # Set rownames
#identical(row.names(ant),sapply(ant@polygons, function(x) slot(x,"ID"))) ## Check rownames
#ant<-ant[order(row.names(ant)),] ## Re-order

# ----- Set Coordinate System 
#proj4string(ant) = CRS("+proj=tmerc +lat_0=4.596200417 +lon_0=-74.07750791700001 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +units=m +no_defs")

# ----- Transform to EPSG 4326 - WGS84 (required)
#map0 <- spTransform(ant, CRS("+proj=longlat +datum=WGS84"))

# ----- simplification yields a SpatialPolygons class
#map1<-gSimplify(map0,tol=0.01, topologyPreserve=TRUE)

#options(save.defaults = list(encoding='Latin1'))
#save(odh, data, ant, map1, ascii=FALSE, file = "ShinyProj.RData")

load(con <- gzfile('data/ShinyProj.RData',encoding='Latin1'))#;close(con)

# function to obtain the pearson correlation from the data
corr <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- cor(d[,1],d[,2])
  return(fit)
}

# Setting Palette
col <- palette()
palette(c(col,'darkorange1'))

# Cluster Map Palette
brks <- c(0,1,2,3,4)
colors <- c("white","red","blue",rgb(0,1,1),rgb(1,.55,0))

#The following four lines code purpose is to generate the legend. For the
#leaflet map. I export the legend to a jpeg file to insert it manually 
#inside the shiny
#bmp('legend.bmp')
#plot(c(0,.5),c(0,.2),type='n',ylab='',xlab='',axes=FALSE)
#legend('center',fill=c(colors[1],'white',colors[-1]),border=c('grey50','white',colors[-1]),bty='n',ncol=3,
#       legend=c("Not Significant","","High-High","Low-Low","Low-High","High-Low"))
#dev.off()



# Labels for titles 

labs <- list(
  'Desempeno Institucional','Acciones Armadas','Luchas Sociales','Pobreza (NBI)',
  'Gini Tierra','Calidad de Vida','Saldo Migratorio','Crecimiento Pob. Rural',
  'Poblacion Joven','Acces. Est. Educativos','Acces. Est. Salud','Acces. lugares Esparcimiento'  ,
  'Acces. Global Servicios','IPD','IVR','IDEco','IDERca','IDERuso','IDEPca','IDEPuso',
  'CBVNat','CBUso','IFP UAF','APax','GCrg','ITran','DBVialTot','CEngTot','DBVialTerc',
  'CEngTerc','DCTViv','DCLViv','CSPAseoUR','CSPAcueUR','CSPAlcanUR','IFPreRur',
  'CCREne','ICFun','ICTur','Acces. Abs.','Acces. Rel.','Entrecruzamiento','Rectitud'  ,
  'Proximidad','Alcance','Gravedad','IA','VA tot','VA pc'
)

names(labs) <-c(
  'IDIM_PROM_2005_2013','VIOLENCIA_ACCIONES_2000_2013','LUCHAS_SOCIALES_1990_2013',
  'NBI_TOT_2005','GINI_TIERRA_2011_ANUARIO','ICV_2005_DANE','MIGRACIONES_2011_UDEA',
  'TASA_CRECI_RUR_2005_DANE','POR_DANE_2005','EDUCACION_OFERTA_90_MIN','SALUD_FLUJO_90_MIN',
  'ESPARCIMIENTO_90_MIN','DESTINOS_90_MIN',
  'IPD.2007.2013.','IVR.dic._7_2014.','IDEcologica','IDR_CA','IDR_COB','IDP_CA',
  'IDP_COB','SAP.SAR.CA.','SAP.SAR.COB.','IFP.UAF',
  'Atractividad.de.Pasajeros','Generacion.de.Carga','Transitabilidad','DENSIDAD.Bruta.Total',
  'Coeficiente.Engels.Total','DENSIDAD.Bruta.Terciarias','Coeficiente.Engels.Terciarias',
  'DEFICIT.CUANTITATIVO.VIV','DEFICIT.CUALITATIVO.VIV','COBERTURA.ASEO.URBANA.RESIDENCIAL',
  'COBERTURA.ACUEDUCTO.URBANA.RESIDENCIAL','COBERTURA.ALCANTARILLADO.URBANA.RESIDENCIAL',
  'IFP.2HA','COEFICIENTE.CONSUMO.DE.ENERGIA','COMPLEJIDAD.FUNCIONAL','CAPACIDAD.TURISTICA'  ,
  'IAA.Absoluto.','IAA.Relativo.','Betweenness','Straightnes','Closeness','Reach',
  'Gravity','IA','va_tot','va_pc'  
)


#################################################################################

library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  #Calculate Spatial Weights
  lw <- reactive({
    W<-as.matrix(odh[1:125,1:125]);colnames(W)<-rownames(W)
    W<-(W>=0 & W<input$hdist)
    W<-W/rowSums(W)
    lw<-mat2listw(W,row.names=rownames(W),style='W')    
  })
  
  #Calculate bivariete LISA's
  bilisa <- reactive({    
    bilisa <- bilisa.perm(data[,input$var1],data[,input$var2],lw(),perm=input$perm)    
    return(bilisa)
  })
  
  #Set up data for download
  dwnld <- reactive({
    wrt <- cbind(data$COD,bilisa(),clusterQuadrant(bilisa(),input$signif))
    wrt[,5] <- as.character(
      factor(wrt[,5],levels=c(0,1,2,3,4),labels=c(
        "No Significativo","Alto-Alto","Bajo-Bajo","Bajo-Alto","Alto-Bajo") )
    )
    colnames(wrt)[c(1,5)] <- c('Divipola','Cluster')    
    return(wrt)
  })
  
  cor <- reactive({    
    # bootstrapping with 1000 replications
    results <- boot(data[,c(input$var1,input$var2)], statistic=corr,
                    R=input$nsim)
    return(results)
  })
  
  #Labels for titles
  val <- reactive({
    lab1 <- as.character(labs[input$var1])
    lab2 <- as.character(labs[input$var2])
    return(c(lab1,lab2))
  })
  
  # Create a spot where we can store additional reactive values for this session
  pt <- reactiveValues(x=NULL, y=NULL)    
  
  # Listen for clicks
  observe({
    # Initially will be empty
    if (is.null(input$Click)){
      return()
    }
    
    isolate({
      pt$x <- c(input$Click$x,pt$x)
      pt$y <- c(input$Click$y,pt$y)
    })
  })
  
  ids <- reactiveValues()
  
  bool <- reactive({
    boolean <- abs(data[,input$var2]-pt$x[1])<0.09 & 
      abs(data[,input$var1]-pt$y[1])<0.09
    return(ifelse(sum(boolean)==0,9999,as.character(data[boolean,'COD']) ) )
  })
  
  observe({
    # Initially will be empty
    if (is.null(pt$x)){
      return()
    }
    isolate({
      ids$ids <- c(bool(),ids$ids) 
    })    
  })
  
  # Clear the points on button click
  observe({    
    if (input$clear > 0){
      pt$x <- NULL
      pt$y <- NULL
      ids$ids <- NULL
    }    
  })
  
  #Legend for Bilisa plot
  output$legend <- renderPlot({
    
    par(mai=c(0,0,0,0),pin=c(5,.9))
    
    plot(c(0,.5),c(0,.2),type='n',ylab='',xlab='',axes=FALSE)
    legend('center',fill=c(colors[1],'white',colors[-1]),border=c('grey50','white',colors[-1]),bty='n',ncol=3,
           legend=c("Not Significant","","High-High","Low-Low","Low-High","High-Low"))
    title(sub=paste(val()[1],' vs. ',val()[2]),line=-0.9, font.sub=2)
    
    })
  
  #Plot bi-LISA cluster map
  output$plot1 <- renderLeaflet({    
    
    fillCol <- colors[findInterval(clusterQuadrant(bilisa(),input$signif),brks,all.inside=FALSE)]
    leaflet(data = map1) %>% addTiles() %>%
      addPolygons(fillColor = fillCol, popup=data$MUNCIPIOS, fillOpacity=0.7,
                  stroke=TRUE, weight=1, color='grey')
    
  })
  
  #Plot scatterplot
  output$plot2 <- renderPlot({    
    pch <- c(15:20,15:19)
    
    par(mar = c(10,5,2,5),bg='grey95')
    plot(data[,input$var2],data[,input$var1],xlab=val()[2],ylab=val()[1],
         col=data$Subregion,pch=pch[data$Subregion],bty='n',fg='red')
    title(paste(val()[1],' vs. ',val()[2]),cex.main=.9)    
    
    if(input$fit=='Lineal'){
      abline( lsfit(data[,input$var2],data[,input$var1]) )
    }
    else{
      lines( lowess(data[,input$var2],data[,input$var1], f=1/4) )
    }
    
    legend('bottom',legend=levels(data$Subregion),cex=0.9,pt.cex=1.4,bty='n',
           col=palette(),pch=pch,xpd=TRUE,inset=-0.4,ncol=3)
    title(sub="Osmar Loaiza (c)",line=8.5,cex.sub=.6)
    title(sub="olloaizaq@unal.edu.co",line=9,cex.sub=.6)
    
    labels <- as.character(ids$ids); labels <- labels[labels!=9999]
    labels <- labels[order(labels)]
    
    coords <- data[data$COD %in% labels,c('MUNCIPIOS',input$var2,input$var1)]
    
    tryCatch({      
      text(coords[,2:3],pch=23,labels=coords[,1],cex=.6)
      }, error=function(warn){
        points(pt$x,pt$y,cex=0)
        }
      )
    })
  
  #Print pearson correlation results
  output$values <- renderPrint({
    bca <- boot.ci(cor(),type='bca',conf=input$conf)$bca
    coef <- boot.ci(cor(),type='bca',conf=input$conf)$t0
    boolean <- (bca[4]<=0.0 & 0.0<=bca[5])
    
    cat(paste('Correlación de Pearson:',
              round(coef, 3) ), '\n','\n',
        'Intervalo de Confianza Bootstrap:','\n','\n',
        '   Nivel de Confianza:',paste(bca[1]*100,'%',sep=''),'\n',
        '   Límite Inferior:',bca[4],'\n',
        '   Límite Superior:',bca[5],'\n','\n',
        
        'Correlación', ifelse(boolean==TRUE, ' NO ',''), 'significativa,',
        'es decir,','\n',
        'estadísticamente',ifelse(boolean==TRUE, 'igual a','distinta de'), 'cero'                
    )
  })  
  
  #Causes data to be downloaded
  output$downloadData <- downloadHandler(    
    filename = function() { paste('lmoran', '.csv', sep='') },
    content = function(file) {
      write.table(dwnld(), file, row.names=FALSE, sep=';', col.names=TRUE,
                  quote=TRUE, fileEncoding = 'UTF-8')
    })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
})
