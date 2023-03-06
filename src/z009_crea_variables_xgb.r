#Creo features con xgboost

#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")

#======================================================

#cargo los datasets
setwd( "~/buckets/b1/datasets/")

# Cargo data
dataset_generacion <- fread("./data/ibarra_generacion.txt.gz")

# Cargo data
dataset_aplicacion <- fread("./data/ibarra_aplicacion.txt.gz")

#======================================================

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

x.cols <- setdiff(names(copy(dataset_generacion)), c("numero_de_cliente", "clase","clase01"))


#=====================================================
#Entreno con modelo Línea de Muerte
dgeneracion  <- xgb.DMatrix( data=  data.matrix( dataset_generacion[ , x.cols, with=FALSE]),
                             label= dataset_generacion[ , clase01 ])

#llamo al XGBoost
set.seed( 102191 ) #seed de Gustavo

modelo  <- xgb.train(data= dgeneracion,
                     objective= "binary:logistic",
                     tree_method= "hist",
                     max_bin= 31,
                     base_score= mean( getinfo(dgeneracion, "label") ),
                     eta= 0.04,
                     nrounds= 300,
                     colsample_bytree= 0.6 )

# Me crea una matriz de 105000 x 14072
new_features <- xgb.create.features(model = modelo, data.matrix(dataset_generacion[ , x.cols, with=FALSE]))
new_features <- as.data.table(as.data.frame(as.matrix(new_features)))

#Le sumamos canaritos
set.seed(612337)
for (i in 1:20)  {
  new_features[, paste0("canarito", i) := runif(nrow(new_features))]
}

## ---------------------------
## Variable Importance !
## ---------------------------

dtrain_lgb  <- lgb.Dataset(
  data = data.matrix(new_features),
  label = dataset_generacion[ , clase01 ])

mlgb <- lgb.train(
  dtrain_lgb,
  params = list(
    objective = "binary",
    max_bin = 15,
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

