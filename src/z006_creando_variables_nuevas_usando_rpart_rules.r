rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("rattle")
require("ggplot2")
require("dplyr")
require("rpart.plot")
require("stringr")

# require("lightgbm")
# require("xgboost")
# require("Hmisc")


# Semas
# semillas <- c(17, 19, 23, 29, 31)


# Cargo data
df <- fread("./data/ibarra_generacion.txt.gz")

df[, clase01 :=  ifelse( clase=="SI", 1, 0 )]

# Rpart no tan grande

max.depth <- 6
num.leaves.teorico <- max.depth**2
kfolds=5
reg = ((kfolds-1)/kfolds)*nrow(df)
avg_minbucket = reg / num.leaves.teorico
lower.bound <- avg_minbucket*0.25

modelo  <- rpart(formula=    "clase01 ~ . -clase",  
                 data=     df,  
                 xval=      5,
                 cp=       -0.3,     
                 minbucket= lower.bound,     
                 maxdepth=  max.depth )    

output_folder <- paste0("./exp/ARBOL_SIMPLE", format(Sys.Date(),"%d%m"),"/")
dir.create( output_folder, showWarnings = FALSE )

rules <- as.data.table(rpart.rules(modelo, roundint = F))

nrules <- nrow(rules)


get_string = function(x){
  s = str_replace_all(str_squish(paste(rules[x, 3:73], collapse = " ")), " is "," == ")
  return(paste0("as.integer(",s,")"))
}
  

j <- seq_len( nrules )
df[ , paste0("campo", j) := lapply( j, function(x) eval(parse(text = get_string(x)))) ]

