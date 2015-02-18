library(shiny);library(leaflet)

choices <- list(
  Institucionales = c('Desempeño Institucional'="IDIM_PROM_2005_2013",
                      'Acciones Armadas'="VIOLENCIA_ACCIONES_2000_2013",
                      'Luchas Sociales'="LUCHAS_SOCIALES_1990_2013"),
  Sociales = c('Pobreza (NBI)'="NBI_TOT_2005", 'Gini Tierra'="GINI_TIERRA_2011_ANUARIO",
               'Calidad de Vida'="ICV_2005_DANE",
               'Saldo Migratorio' = "MIGRACIONES_2011_UDEA",
               'Crecimiento Pob. Rural'="TASA_CRECI_RUR_2005_DANE",
               'Población Joven'="POR_DANE_2005",
               'Acces. Est. Educativos'="EDUCACION_OFERTA_90_MIN",
               'Acces. Est. Salud'="SALUD_FLUJO_90_MIN",
               'Acces. lugares Esparcimiento'="ESPARCIMIENTO_90_MIN",
               'Acces. Global Servicios'="DESTINOS_90_MIN"),
  Económica = c('VA tot'='va_tot','VA pc'='va_pc'),
  #Generales = c("Población.total.2013","Población.Cabecera.2013","Población.Rural.2013",
  #              "Area.total.ha.", "Area.cabecera.ha.","Area.Rural.ha.","Ingreso_Promedio"),
  'Soporte Territorial' = c('IPD'="IPD.2007.2013.",
                            'IVR'="IVR.dic._7_2014.",
                            'IDEco'="IDEcologica",
                            'IDERca'="IDR_CA",
                            'IDERuso'="IDR_COB",
                            'IDEPca'="IDP_CA",
                            'IDEPuso'="IDP_COB",
                            'CBVNat'="SAP.SAR.CA.",
                            'CBUso'="SAP.SAR.COB.",
                            'IFP UAF'="IFP.UAF"),  
  'Dinámica Territorial' = c('APax'="Atractividad.de.Pasajeros",
                             'GCrg'="Generacion.de.Carga",
                             'ITran'="Transitabilidad",
                             'DBVialTot'="DENSIDAD.Bruta.Total",
                             'CEngTot'="Coeficiente.Engels.Total",
                             'DBVialTerc'="DENSIDAD.Bruta.Terciarias",
                             'CEngTerc'="Coeficiente.Engels.Terciarias"),
  'Tensión Territorial' = c('DCTViv'="DEFICIT.CUANTITATIVO.VIV",
                            'DCLViv'="DEFICIT.CUALITATIVO.VIV",
                            'CSPAseoUR'="COBERTURA.ASEO.URBANA.RESIDENCIAL",
                            'CSPAcueUR'="COBERTURA.ACUEDUCTO.URBANA.RESIDENCIAL",
                            'CSPAlcanUR'="COBERTURA.ALCANTARILLADO.URBANA.RESIDENCIAL",
                            'IFPreRur'="IFP.2HA",
                            'CCREne'="COEFICIENTE.CONSUMO.DE.ENERGIA",
                            'ICFun'="COMPLEJIDAD.FUNCIONAL",
                            'ICTur'="CAPACIDAD.TURISTICA"),
  'Conectividad Vial' = c('Acces. Abs.'="IAA.Absoluto.",
                          'Acces. Rel.'="IAA.Relativo.",
                          'Entrecruzamiento'="Betweenness",
                          'Rectitud'="Straightnes",
                          'Proximidad'="Closeness",
                          'Alcance'="Reach",
                          'Gravedad'="Gravity","IA")  
)

# Define UI for application that plots random distributions
shinyUI(navbarPage('Análisis Correlacional',
                   tabPanel('Mapa de Clusters (LISA)',
                            fluidRow(
                              column(3,
                                     div(class='well',             
                                         selectInput("var1", label=h6("Variable 1:"), choices = choices
                                                     ,  selected='NBI_TOT_2005'),
                                         
                                         selectInput("var2", label=h6("Variable 2:"),  choices = choices
                                                     , selected='Closeness'),
                                         br(),
                                         sliderInput("hdist", label = h6("Radio de Búsqueda (horas):"),
                                                     min=0.5,max=4, value=1,step = 0.5,animate=TRUE),                    
                                         br(),
                                         p(em("Descargar Datos")),
                                         downloadButton('downloadData', 'Descargar Clusters'),
                                         br(),hr(),br(),
                                         h5('Análisis Exploratorio de Datos Espaciales'),
                                         p('por ',a('Osmar Loaiza'),a('<olloaizaq@unal.edu.co>'))
                                     )                    
                              ),
                              column(9,                    
                                     fluidRow(
                                       column(12,                             
                                              leafletOutput("plot1", width="445px",height = "500px")
                                              
                                       )
                                     ),
                                     fluidRow( tagList(tags$div(
                                       plotOutput("legend", width="480px",height = "85px"),style="text-align: center;" ))
                                                     ),
                                     br(),
                                     fluidRow(                    
                                       column(3,
                                              radioButtons("signif", label=h6("Umbral de Significancia:"),
                                                           c("1%" = 0.01,
                                                             "5%" = 0.05,
                                                             "10%"=0.1),selected=0.05)
                                              
                                       ),                      
                                       column(3,
                                              numericInput('perm',label=h6('Numero de Permutaciones:'),
                                                           value=1000,min=100,max=10000,step=100)                                                     
                                       )                                  
                                     )
                              )
                            )
                   ),
                   
                   tabPanel('Diagrama de dispersión', 
                            fluidRow(
                              column(5,
                                     fluidRow(               
                                       div(class='well',
                                           radioButtons("fit", label=h6("Línea de Ajuste:"),
                                                        c("Lineal" = 'Lineal',
                                                          "Lowess" = 'Lowess'),
                                                        selected='Lowess')                           
                                       )
                                     ),
                                     
                                     fluidRow(
                                       conditionalPanel("input.var1 != input.var2",
                                                        verbatimTextOutput('values')
                                       )
                                     ),
                                     
                                     fluidRow(                    
                                       div(class='well',p(em('Parámetros Test Boostrap de correlación')),
                                           numericInput('nsim',label=h6('Numero de Simulaciones:'),
                                                        value=1000,min=100,max=10000,step=100),
                                           numericInput('conf',label=h6('Nivel de Confianza:'),
                                                        value=0.95,min=0.8,max=0.99,step=.01)
                                       )
                                     )                            
                              ),             
                              column(5,
                                     conditionalPanel("input.var1 != input.var2",
                                                      plotOutput("plot2", width="450px",height = "450px",
                                                                 clickId='Click'),
                                                      br(),                      
                                                      actionButton("clear", "Limpiar Nombres")
                                                      
                                     )
                              )
                            )
                   )
                   
                   
))