#Este script utiliza la salida de xgb.create.features y aplica una 
# regresión logística.

#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("parallel")
require("dplyr")
#======================================================

#cargo los datasets
# setwd( "~/buckets/b1/datasets/")

# Cargo data
orig.df <- fread("./data/ibarra_generacion.txt.gz", select = c("numero_de_cliente", "clase"))

# xgb features
# setwd( "~/buckets/b1/")
xgb.df <- fread("./exp/FE_XGB/fe_xgb_GENERACION.csv.gz")
xgb.df <- xgb.df %>% rename(numero_de_cliente = id_cli_gen) 

dataset <- merge(xgb.df, orig.df, by="numero_de_cliente")
#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase=="SI", 1L, 0L) ]

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

Entrenar.Obtener.Ganancia  <- function(sema)
{
  cat(sprintf("\nExperimento con semilla: %d", sema))
  
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(7,3), agrupa="clase01", seed = sema) 
  
  nro_cliente.train <- dataset[fold == 1 , c("numero_de_cliente"), with=FALSE]
  train.data <- dataset[fold == 1 , !c("numero_de_cliente","clase"), with=FALSE]
  
  nro_cliente.val <- dataset[fold == 2 , c("numero_de_cliente"), with=FALSE]
  x.val.data <- dataset[fold == 2 , !c("numero_de_cliente","clase","clase01"), with=FALSE]
  
  modelo <-  glm( clase01 ~., data = train.data, family = binomial)
  
  #aplico el modelo a los datos de testing
  prediccion  <- modelo %>% predict(x.val.data, type = "response")
  
  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[fold == 2 , sum( ifelse(prediccion  >  0.025, ifelse( clase01==1, 78000, -2000 ),0 )) ]
  
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
salidasLogReg.MC <-  mcmapply( Entrenar.Obtener.Ganancia, 
                               ksemillas,   
                               SIMPLIFY= FALSE,
                               mc.cores= 1) 

#paso la lista a df
mc_salida  <- as.data.table(data.frame(rbindlist(salidasLogReg.MC)))

# Guardo nuevos dataframes en carpeta
# setwd( "~/buckets/b1/")
output_folder <- "./exp/XGB_FE_LOG_REG/"
dir.create( output_folder, showWarnings = FALSE )
fwrite(mc_salida, paste0(output_folder,"salidas_MC.csv.gz"), row.names = F)



