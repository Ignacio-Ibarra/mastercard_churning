#Creo features con xgboost

#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")
require("lightgbm")
#======================================================

#cargo los datasets
setwd( "~/buckets/b1/datasets/")

# Cargo data
dataset_generacion <- fread("./data/ibarra_generacion.txt.gz")

# Cargo data
dataset_aplicacion <- fread("./data/ibarra_aplicacion.txt.gz")

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

x.cols <- setdiff(names(copy(dataset_generacion)), c("numero_de_cliente", "clase","clase01"))

#guardo los nro de cliente
id_cli_gen <- dataset_generacion$numero_de_cliente
id_cli_apl <- dataset_aplicacion$numero_de_cliente
#=====================================================
#Uso un modelo que tenga un colsample_bytree bajo
#que fitee 300 árboles

dgeneracion  <- xgb.DMatrix( data=  data.matrix( dataset_generacion[ , x.cols, with=FALSE]),
                             label= dataset_generacion[ , clase01 ])

#llamo al XGBoost
set.seed( 102191 ) #seed de Gustavo

modelo  <- xgb.train(data= dgeneracion,
                     objective= "binary:logistic",
                     tree_method= "hist",
                     nrounds = 300,
                     max_bin= 31,
                     base_score= mean( dataset_generacion[ , clase01 ]),
                     eta= 0.04,
                     max_depth = 3,
                     colsample_bytree= 0.3)

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
    learning_rate = 0.05),
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
output_folder <- "./exp/FE_XGB/"
dir.create( output_folder, showWarnings = FALSE )
fwrite(gen, paste0(output_folder,"fe_xgb_GENERACION.csv.gz"), row.names = F)
fwrite(apl, paste0(output_folder,"fe_xgb_APLICACION.csv.gz"), row.names = F)