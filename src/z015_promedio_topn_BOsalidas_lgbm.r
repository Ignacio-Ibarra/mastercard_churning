# Obtengo varios modelos con los mejores parámetros. 

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("lightgbm")
require("data.table")

#===================================================
#Cargo data

dataset_generacion <- fread("./exp/FE_MERGE_0803/fe_merge_GENERACION.csv.gz")
#paso la clase a binaria que tome valores {0,1}  enteros
dataset_generacion[ , clase01 := ifelse( clase=="SI", 1L, 0L) ]
#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset_generacion), c("clase","clase01") )

#=================================================

date <- format(Sys.Date(), "%d%m")

PARAM_EXP <- list()

PARAM_EXP$BayOpt$input_code <- "BO0803b"
PARAM_EXP$BayOpt$topn <- 5
PARAM_EXP$BayOpt$cluster <- c(4)

PARAM_EXP$split_seed <- 123

PARAM_EXP$n_seeds <- 50

PARAM_EXP$OutputFolder <- paste0("MULT_SEEDS", date)



#=================================================
#Consulto mejores parámetros. 

get_params <- function(bo = "", topn=10, cluster = c(0,1,2,3)){
  
  #Leer archivo
  input.file <- paste0("./eval_BO/",bo,"/",bo,"_hclusters.csv")
  eval_BO <- fread(input.file)
  params <- colnames(eval_BO)[2:(length(colnames(eval_BO))-3)]
  
  #Order data.table
  setorder(eval_BO, -ganancia)
  bsp <- eval_BO[cluster_label %in% cluster, head(.SD,topn), .SDcols = params]
  
  return(bsp)
}

#================================================
# random split

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

#====================================

# Imprime lista 

printList <- function(list) {
  vars <- names(list)
  
  for (i in 1:length(list)) {
    cat(vars[i],":",list[[i]],"\t")
    
  }
  cat("\n")
}

#=====================================
# get model predictions 

get_model_prediction <- function(params){
  params <- as.list(params)
  model <- lgb.train(params,
                     data = dtrain)
  
  #predicción de validación
  prediction  <- predict(  model , data.matrix(dval) )
  
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
    predictions[, paste0("preds_",i) := preds]
  }
  return(rowMeans(predictions))
}

#===================================

#Calcular ganancia tomando probs y trues
calcular.ganancia <- function(probs, trues, corte=0.025, correccion=0.3){
  return(sum( (probs >  corte) * ifelse( trues==1, 78000, -2000 ))/correccion)
}


#==========================================
# Acá empieza el programa

#Obtengo los primero 10 modelos
models.params <- get_params(bo = PARAM_EXP$BayOpt$input_code, 
                            topn=PARAM_EXP$BayOpt$topn, 
                            cluster = PARAM_EXP$BayOpt$cluster) #cluster 3 estaba mejor. 

#particiono
particionar( dataset_generacion, division=c(7,3), agrupa="clase01", seed = PARAM_EXP$split_seed) 

#creo training set
dtrain  <- lgb.Dataset( data= data.matrix(  dataset_generacion[ fold==1L, campos_buenos, with=FALSE]),
                        label= dataset_generacion[ fold==1L, clase01 ],
                        free_raw_data= FALSE  )

#Creo validation set
dval <- dataset_generacion[ fold==2L, campos_buenos, with=FALSE]
yval <- dataset_generacion[fold==2L, clase01]

results <- data.table()
results[, yval:=yval]
for (i in 1:nrow(models.params)){
  cat(sprintf("\nGenerando Semillerío con modelo %s\n", i))
  params <- models.params[i]
  print.params <- params
  print.params$seed <- NULL
  probs <- promediar.semillas(params, N=PARAM_EXP$n_seeds)
    cat("\n",printList(print.params))
  gan <- calcular.ganancia(probs, yval)
  cat("GANANCIA: ", gan, "\n")
  results[, paste0("probs.model_",i):= probs]
}


#Guardo probs
output_folder <- paste0("./exp/", PARAM_EXP$OutputFolder )
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( output_folder, showWarnings = FALSE )
fwrite(results, paste0(output_folder,"/","semillerio_topn_models.csv.gz"), row.names=FALSE)
