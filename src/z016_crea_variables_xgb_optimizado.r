# Obtengo varios modelos con los mejores parámetros. 

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("lightgbm")
require("data.table")
require("stringr")
require("xgboost")
require("lightgbm")

#=================================================

date <- format(Sys.Date(), "%d%m")

PARAM_EXP <- list()

PARAM_EXP$dataset_generacion <-  "./data/ibarra_generacion.txt.gz"
PARAM_EXP$dataset_aplicacion <-  "./data/ibarra_aplicacion.txt.gz"

PARAM_EXP$BayOpt$input_code <- "XGB_BO1303"
PARAM_EXP$BayOpt$cluster <- c(16)

PARAM_EXP$semilla_azar  <- 999983
PARAM_EXP$OutputFolder <- paste0("FE_XGB", date)

#===================================================

#cargo los datasets
setwd( "~/buckets/b1/datasets/")

# Cargo data
dataset_generacion <- fread(PARAM_EXP$dataset_generacion)

# Cargo data
dataset_aplicacion <- fread(PARAM_EXP$dataset_aplicacion)

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

x.cols <- setdiff(names(copy(dataset_generacion)), c("numero_de_cliente", "clase","clase01"))

#guardo los nro de cliente
id_cli_gen <- dataset_generacion$numero_de_cliente
id_cli_apl <- dataset_aplicacion$numero_de_cliente


#=================================================
#Consulto mejores parámetros. 

get_params <- function(bo = "", topn=1, corte="1.5", cluster = c(0,1,2,3)){
  
  corte = ifelse(grepl("\\.", corte), str_replace(corte, "\\.","_"), corte)
  
    #Leer archivo
  input.file <- sprintf("./eval_BO/%s/%s_hclusters_corte%s.csv", PARAM_EXP$BayOpt$input_code, PARAM_EXP$BayOpt$input_code, corte)
  eval_BO <- fread(input.file)
  params <- colnames(eval_BO)[2:(length(colnames(eval_BO))-3)]
  
  #Order data.table
  setorder(eval_BO, -ganancia)
  bsp <- eval_BO[cluster_label %in% cluster, head(.SD,topn), .SDcols = params]
  
  return(bsp)
}

#================================================================
# Entrenamiento con XGB

dgeneracion  <- xgb.DMatrix( data=  data.matrix( dataset_generacion[ , x.cols, with=FALSE]),
                             label= dataset_generacion[ , clase01 ])


hclust.corte = "1.5"
cluster = c(16)
setwd( "~/mastercard_churning/")
params <- get_params(bo = PARAM_EXP$BayOpt$input_code,
                     topn = 1,
                     corte = hclust.corte,
                     cluster = cluster)

#llamo al XGBoost
set.seed( PARAM_EXP$semilla_azar ) #misma seed que XGB_BO1303
modelo  <- xgb.train(data= dgeneracion,
                     params = params,
                     base_score= mean( dataset_generacion[ , clase01 ]),
                     )

# Me crea una matriz de 105000 x 14072
new_features <- xgb.create.features(model = modelo, 
                                    data.matrix(dataset_generacion[ , x.cols, with=FALSE]))
new_features <- as.data.table(as.data.frame(as.matrix(new_features)))

#Le sumamos canaritos
set.seed(612337)
for (i in 1:20)  {
  new_features[, paste0("canarito", i) := runif(nrow(new_features))]
}

#================================
# Uso un LGBM con canaritos para hacer selección de variables

dtrain_lgb  <- lgb.Dataset(
  data = data.matrix(new_features),
  label = dataset_generacion[ , clase01 ])

mlgb <- lgb.train(
  dtrain_lgb,
  params = list(
    objective = "binary",
    max_bin = 31,
    min_data_in_leaf = 4000,
    learning_rate = 0.005),
  nrounds = 1024,
  verbose = -1)

var.importance.features <- lgb.importance(mlgb)$Feature
list.canaritos <- grepl("canarito", var.importance.features)
idx <- seq(length(list.canaritos))
tolerancia <- 5
id.canarito.tol <- idx[list.canaritos][tolerancia]
important.features <- var.importance.features[1:id.canarito.tol]
important.new.features <- important.features[grep("V\\d+", important.features)]

#=========================================================

new_features.apl <- xgb.create.features(model = modelo, 
                                        data.matrix(dataset_aplicacion[ , x.cols, with=FALSE]))
new_features.apl <- as.data.table(as.data.frame(as.matrix(new_features.apl)))[, important.new.features, with = FALSE]


#==========================================================
#guardo dataset generación y aplicacion sólo con nuevos features
numero_de_cliente <- id_cli_gen
gen <- cbind(numero_de_cliente, new_features[ ,important.new.features, with=FALSE])
rm(new_features)
numero_de_cliente <- id_cli_apl
apl <- cbind(numero_de_cliente, new_features.apl)
rm(new_features.apl)


# Guardo nuevos dataframes en carpeta
setwd( "~/buckets/b1/")
output_folder <- sprintf("./exp/%s/", PARAM_EXP$OutputFolder)
dir.create( output_folder, showWarnings = FALSE )
fwrite(gen, paste0(output_folder,"fe_xgb_GENERACION.csv.gz"), row.names = F)
fwrite(apl, paste0(output_folder,"fe_xgb_APLICACION.csv.gz"), row.names = F)
