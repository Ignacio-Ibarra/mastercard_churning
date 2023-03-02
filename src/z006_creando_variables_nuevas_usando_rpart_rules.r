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

#=======================================================
# VARIABLES BOOLEANAS DERIVADAS DE RPART.RULES

#Con un árbol pequeño. 

max.depth <- 10
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
  s = str_squish(paste(rules[x, 3:73], collapse = " "))
  # cat(s,"(1)\n")
  splitted = unlist(str_split(s, pattern = " "))
  to_ids = which("to" == splitted)
  # cat(to_ids, "to_ids\n")
  for (to_id in to_ids){
    value1 = splitted[to_id - 1]
    value2 = splitted[to_id + 1]
    to = splitted[to_id]
    is = splitted[to_id - 2]
    variable = splitted[to_id - 3]
    sub_to_reeplace = paste(variable, is, value1, to, value2, collapse = " ")
    # cat(sub_to_reeplace, "\n")
    new_sub = paste(variable,">", value1, "&", variable, "<", value2, collapse = " ")
    s = str_replace(s, sub_to_reeplace, new_sub)
    # cat(s,"(2)\n")
  }
  s = str_replace_all(s, " is "," == ")
  # cat(s,"(3)\n")
  return(paste0("as.integer(",s,")"))
}
  
#Para cada regla creadas armo una variable booleana. 
j <- seq_len( nrules )
df[ , paste0("campo", j) := lapply( j, function(x) eval(parse(text = get_string(x)))) ]


#==========================================================




