#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

ArbolEstimarGanancia  <- function(dataset, semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(7,3), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !
  
  modelo  <- xgb.train(data= dataset,
                       objective= "binary:logistic",
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= mean( getinfo(dgeneracion, "label") ),
                       eta= 0.04,
                       nrounds= 300,
                       colsample_bytree= 0.6 )
  
  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad
  
  #prediccion es una matriz con dos columnas
  
  
  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  0.025,
                                          ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ),
                                          0 ) )]
  
  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3
  
  return( list( "testing"=       dataset[ fold==2, .N],
                "testing_pos"=   dataset[ fold==2 & clase_ternaria=="BAJA+2", .N],
                "envios"=        dataset[ fold==2 , sum( prediccion[ , "BAJA+2"] > 0.025)],
                "aciertos"=      dataset[ fold==2, sum( prediccion[ , "BAJA+2"] > 0.025 & clase_ternaria=="BAJA+2" )],
                "ganancia_test"= ganancia_test_normalizada ))
}

alumno_apellido  <- "ibarra"

#cargo los datasets
# setwd( "~/buckets/b1/recuperatorio/uba2022/ibarra/")

data_folder <- "./data/"

# dataset_generacion  <- fread( paste0( alumno_apellido, "_generacion.txt.gz") )
# dataset_aplicacion  <- fread( paste0( alumno_apellido, "_aplicacion.txt.gz") )

dataset_generacion  <- fread( paste0( data_folder, alumno_apellido ,"_generacion.txt.gz") )
dataset_aplicacion  <- fread( paste0( data_folder, alumno_apellido,"_aplicacion.txt.gz") )

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

dgeneracion  <- xgb.DMatrix( data=  data.matrix( dataset_generacion[ , !c("numero_de_cliente","clase","clase01"), with=FALSE]),
                             label= dataset_generacion[ , clase01 ]
                            )

#llamo al XGBoost,  notar lo frugal de los hiperparametros
set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles







modelo  <- xgb.train(data= dgeneracion,
                     objective= "binary:logistic",
                     tree_method= "hist",
                     max_bin= 31,
                     base_score= mean( getinfo(dgeneracion, "label") ),
                     eta= 0.04,
                     nrounds= 300,
                     colsample_bytree= 0.6 )




#aplico a los datos de aplicacion, que NO TIENE CLASE
daplicacion  <- xgb.DMatrix( data= data.matrix( dataset_aplicacion[ , !c("numero_de_cliente"), with=FALSE]) )


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, daplicacion )

#uno las columnas de numero_de_cliente y la probabilidad recien calculada
prediccion_final  <- cbind(  dataset_aplicacion[ ,c("numero_de_cliente")], aplicacion_prediccion )

#le doy nombre a las columnas
colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )


#Genero las TRES salidas
#grabo todas las probabilidad, simplemente para tenerlo
fwrite( prediccion_final[ order( -prob_positivo) ], 
        file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_probabilidades.txt"), 
        sep= "\t", 
        eol= "\r\n")

#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ] ), 
        file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_entregar.txt"),
        col.names=FALSE, 
        sep= "\t", 
        eol= "\r\n")

#grabo la importancia de las variables
write.table(  xgb.importance( model = modelo )
              , file= paste0( alumno_apellido, "_lineademuerte_recuperatorio_importancia.txt")
              , sep= "\t"
              , eol= "\r\n"
              , row.names= FALSE
           )