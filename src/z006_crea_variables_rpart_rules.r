rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("dplyr")
require("rpart.plot")
require("stringr")

#======================================================
setwd( "~/buckets/b1/datasets/")

# Cargo data
df_gen <- fread("./data/ibarra_generacion.txt.gz") #data original
df_gen[, clase01 :=  ifelse( clase=="SI", 1, 0 )]

# Cargo data
df_apl <- fread("./data/ibarra_aplicacion.txt.gz") #data original

#=======================================================
# VARIABLES BOOLEANAS DERIVADAS DE RPART.RULES

#Con un árbol pequeño. 

max.depth <- 10
num.leaves.teorico <- max.depth**2
kfolds=5
reg = ((kfolds-1)/kfolds)*nrow(df_gen)
avg_minbucket = reg / num.leaves.teorico
lower.bound <- avg_minbucket*0.25

modelo  <- rpart(formula=    "clase01 ~ . -clase",  
                 data=     df_gen,  
                 method = "class",
                 xval=      5,
                 cp=       -0.3,     
                 minbucket= lower.bound,     
                 maxdepth=  max.depth )    

rules <- as.data.table(rpart.rules(modelo, roundint = F))

nrules <- nrow(rules)



#Función para transformar una regla en un string
get_string = function(x){
  s = str_squish(paste(rules[x, 3:length(rules)], collapse = " "))
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
    new_sub = paste(variable,">=", value1, "&", variable, "<", value2, collapse = " ")
    s = str_replace(s, sub_to_reeplace, new_sub)
    # cat(s,"(2)\n")
  }
  s = str_replace_all(s, " is "," == ")
  # cat(s,"(3)\n")
  return(paste0("as.integer(",s,")"))
}
  
#Para cada regla creadas armo una variable booleana. 
#hay veces que devuelv NA porque la fila evaluada tiene NA en alguna de las variables de la regla.
j <- seq_len( nrules )
df_gen[ , paste0("campo", j) := lapply( j, function(x) eval(parse(text = get_string(x)))) ]
df_apl[ , paste0("campo", j) := lapply( j, function(x) eval(parse(text = get_string(x)))) ]



# Guardo nuevos dataframes en carpeta
setwd( "~/buckets/b1/")
output_folder <- "./exp/FE_RPART_RULES/"
dir.create( output_folder, showWarnings = FALSE )
fwrite(df_gen[, c("numero_de_cliente", paste0("campo", j)), with=FALSE], paste0(output_folder,"fe_rpart_rules_GENERACION.csv.gz"), row.names = F)
fwrite(df_apl[, c("numero_de_cliente", paste0("campo", j)), with=FALSE], paste0(output_folder,"fe_rpart_rules_APLICACION.csv.gz"), row.names = F)

#==========================================================


