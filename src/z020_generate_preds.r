# De la salida de multiple seeds elijo el mejor modelo

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("lightgbm")
require("data.table")
require("stringr")


date <- format(Sys.Date(), "%d%m")

PARAM_EXP <- list()

PARAM_EXP$BayOpt$input_code <- "BO0803b"
PARAM_EXP$BayOpt$topn <- 5
PARAM_EXP$BayOpt$cluster <- c(4)
PARAM_EXP$split_seed <- 123
PARAM_EXP$n_seeds <- 50
PARAM_EXP$mult_seeds_exp <- "MULT_SEEDS_B1503"
PARAM_EXP$datasets$generacion <- "./exp/FE_MERGE_0803/fe_merge_GENERACION.csv.gz"
PARAM_EXP$datasets$aplicacion <- "./exp/FE_MERGE_0803/fe_merge_APLICACION.csv.gz"
PARAM_EXP$salida <- paste0("OUT_", date)

get_params <- function(bo = "", 
                       topn=10, 
                       cluster = c(0,1,2,3),
                       exp = ""){
  
  calcular.ganancia <- function(probs, corte=0.025, correccion=0.3){
    return(sum( (probs >  corte) * ifelse( yval==1, 78000, -2000 ))/correccion)
  }
  
  
  #Leer archivo
  input.file <- paste0("./eval_BO/",bo,"/",bo,"_hclusters.csv")
  eval_BO <- fread(input.file)
  params <- colnames(eval_BO)[2:(length(colnames(eval_BO))-3)]
  
  #Order data.table
  setorder(eval_BO, -ganancia)
  bsp <- eval_BO[cluster_label %in% cluster, head(.SD,topn), .SDcols = params]
  
  f <- sprintf("./exp/%s/semillerio_topn_models.csv.gz", exp)
  
  data <- fread(f)
  
  #Calcular ganancia tomando probs y trues
  yval <- data$yval
  
  idx <- which.max(data[, lapply(.SD, calcular.ganancia), .SDcols = str_subset(names(data), "probs.model") ])
  
  return(bsp[idx,])
}

#=====================================
# get model predictions 

get_model_prediction <- function(params){
  params <- as.list(params)
  model <- lgb.train(params,
                     data = dtrain)
  
  #predicción de validación
  prediction  <- predict(  model , dtest )
  
  return(prediction)
}

#===================================

#promediar predicciones de semillas
promediar.semillas <- function(params, N=10){
  ksemillas <- 1:N
  predictions <- data.table()
  for (sema in ksemillas){
    cat("\nSemilla ",sema,"\n")
    params$seed <- sema
    preds <- get_model_prediction(params)
    predictions[, paste0("preds_",sema) := preds]
  }
  return(rowMeans(predictions))
}


#======================================

params <- get_params(bo = PARAM_EXP$BayOpt$input_code, 
                     topn = PARAM_EXP$BayOpt$topn, 
                     cluster = PARAM_EXP$BayOpt$cluster,
                     exp = PARAM_EXP$mult_seeds_exp)

#======================================

dataset_generacion <- fread(PARAM_EXP$datasets$generacion)
dataset_aplicacion <- fread(PARAM_EXP$datasets$aplicacion)

#paso la clase a binaria que tome valores {0,1}  enteros
dataset_generacion[ , clase01 := ifelse( clase=="SI", 1L, 0L) ]
#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset_generacion), c("numero_de_cliente","clase","clase01") )


#creo training set
dtrain  <- lgb.Dataset( data= data.matrix(  dataset_generacion[ , campos_buenos, with=FALSE]),
                        label= dataset_generacion[ , clase01 ],
                        free_raw_data= FALSE  )

dtest <- data.matrix(dataset_aplicacion[, !c("numero_de_cliente"), with=FALSE])


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- promediar.semillas(params, N=PARAM_EXP$n_seeds)

#uno las columnas de numero_de_cliente y la probabilidad recien calculada
prediccion_final  <- cbind(  dataset_aplicacion[ ,c("numero_de_cliente")], aplicacion_prediccion )

#le doy nombre a las columnas
colnames( prediccion_final )  <- c( "numero_de_cliente", "prob_positivo" )


#Guardo probs
output_folder <- paste0("./salida/", PARAM_EXP$salida )
dir.create( "./salida/",  showWarnings = FALSE ) 
dir.create( output_folder, showWarnings = FALSE )


#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > 0.025
fwrite( as.data.table( prediccion_final[ prob_positivo > 0.025  , "numero_de_cliente" ] ), 
        file= paste0(output_folder,"/",date,"_ibarra_entrega.txt"),
        col.names=FALSE, 
        sep= "\t", 
        eol= "\r\n")


#Genero las TRES salidas
#grabo todas las probabilidad, simplemente para tenerlo
fwrite( prediccion_final[ order( -prob_positivo) ], 
        file= paste0( output_folder,"/",date,"_ibarra_entrega_probs.txt"), 
        sep= "\t", 
        eol= "\r\n")



