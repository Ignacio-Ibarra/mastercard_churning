#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

#Working Directory GCP
setwd( "~/buckets/b1/")

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.005, upper=    0.05),
         makeNumericParam("feature_fraction", lower=    0.2  , upper=    0.8),
         makeIntegerParam("min_data_in_leaf", lower=    100L   , upper=  1500L),
         makeIntegerParam("num_leaves",       lower=    200L   , upper=  1024L)
         )

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

day.month <- strftime(Sys.Date(),"%d%m")

PARAM$experimento  <- paste0("BO","0803b")

PARAM$input$dataset_generacion       <- "./exp/FE_MERGE_0803/fe_merge_GENERACION.csv.gz"
# PARAM$input$dataset_aplicacion       <- "./data/ibarra_aplicacion.txt.gz"

# PARAM$input$training <- "generacion"
# PARAM$input$testign <- "aplicacion"

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 102191  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 102191  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_LGBM  <- function( probs, datos) 
{
  labels   <- get_field(datos, "label")

  #vector de ganancias
  vgan  <- ifelse( labels == 1, PARAM$hyperparametertuning$POS_ganancia, 
                          PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) 


  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  ganancia <- tbl[ vprobs > 1/40, sum( vgan )] 
  
  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria

  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )
  
  corridas <- list()
  for (seed in 1:10){
    set.seed(seed)
    modelocv  <- lgb.cv( data= dtrain,
                         eval= fganancia_LGBM,
                         stratified= TRUE, #sobre el cross validation
                         nfold= kfolds,    #folds del cross validation
                         param= param_completo,
                         verbose= -100)
    
    #obtengo la ganancia
    
    ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
    ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia
    
    corrida.cv <- list("ganancia" = ganancia_normalizada,
                       "num_iterations" = modelocv$best_iter)
    
    cat("corrida.cv tiene la dimensión ", dim(corrida.cv), "\n")
    
    corridas <- append(corridas, corrida.cv)
  }
  
  corrida.media <- list("ganancia_media" = mean(corridas$ganancia), 
                        "num_iter_media" = mean(corridas$num_iterations))
  

  param_completo$num_iterations <- corrida.media$num_iter_media  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  ganancia_media <- corrida.media$ganancia_media
  #Voy registrando la importancia de variables
  if( ganancia_media >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_media
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
                         )

    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }


  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_media ,"extras" )  <- list("num_iterations"= corrida.media$num_iter_media)  #esta es la forma de devolver un parametro extra

  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_media   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )

  return( ganancia_media )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa


#cargo el dataset donde voy a entrenar el modelo
dataset <- fread( PARAM$input$dataset_generacion  )


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase=="SI", 1L, 0L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase","clase01") )

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ ( azar <= PARAM$trainingstrategy$undersampling | clase01 == 1L ),
         training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        # weight=  dataset[ training == 1L, ifelse( clase01==1, 1.0000002, 1.0000001 )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


quit( save="no" )

