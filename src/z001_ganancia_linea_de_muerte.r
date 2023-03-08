# Este script calcula mediante montecarlo la ganancia 
# estimada de la línea de muerte


#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")
require("parallel")


#cargo los datasets
setwd( "~/buckets/b1/datasets/")

dataset_generacion  <- fread( "./data/ibarra_generacion.txt.gz") 
# dataset_aplicacion  <- fread( paste0( data_folder, alumno_apellido,"_aplicacion.txt.gz") )

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

dataset <- copy(dataset_generacion)

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

Entrenar.Obtener.Ganancia  <- function(sema)
{
  sprintf("\nExperimento con semilla: %d", sema)
  
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(7,3), agrupa="clase01", seed = sema) 
  
  dtrain  <- xgb.DMatrix( data=  data.matrix( dataset[fold == 1 , !c("numero_de_cliente","clase","clase01"), with=FALSE]),
                               label= dataset[fold ==1 , clase01 ])
  
  set.seed( 102191)  #SemillaGustavo
  modelo  <- xgb.train(data= dtrain,
                       objective= "binary:logistic",
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= mean( getinfo(dtrain, "label") ),
                       eta= 0.04,
                       nrounds= 300,
                       colsample_bytree= 0.6 )
  
  #aplico a los datos de aplicacion, que NO TIENE CLASE
  dval  <- xgb.DMatrix( data= data.matrix( dataset[fold==2 , !c("numero_de_cliente","clase","clase01"), with=FALSE]) )
 
   #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dval) 
  
  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, sum( ifelse(prediccion  >  0.025, ifelse( clase01==1, 78000, -2000 ),0 )) ]
  
  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3
  
  return( list( "testing"=       dataset[ fold==2, .N],
                "testing_pos"=   dataset[ fold==2 & clase01==1, .N],
                "envios"=        dataset[ fold==2 , sum( prediccion > 0.025)],
                "aciertos"=      dataset[ fold==2, sum( prediccion > 0.025 & clase01 ==1 )],
                "pifies" =       dataset[ fold ==2, sum(prediccion > 0.025 & clase01  == 0 )],
                "perdidos" =       dataset[ fold ==2, sum(prediccion <= 0.025 & clase01 == 1 )],
                "ganancia_test"= ganancia_test_normalizada ))
}


ksemillas  <- 1:100


salidasMC <- mcmapply( Entrenar.Obtener.Ganancia, 
                       ksemillas,   
                       SIMPLIFY= FALSE,
                       mc.cores= -1) 


#paso la lista a df
mc_salida  <- as.data.table(data.frame(rbindlist(salidasMC)))

# Guardo nuevos dataframes en carpeta
setwd( "~/buckets/b1/")
output_folder <- "./exp/LINEA_MUERTE_MC"
dir.create( output_folder, showWarnings = FALSE )
fwrite(mc_salida, paste0(output_folder,"salidas_MC.csv.gz"), row.names = F)


