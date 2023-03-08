#Creo variables manualmente

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("stringr")


#======================================================
setwd( "~/buckets/b1/datasets/")

# Cargo data
df_gen <- fread("./data/ibarra_generacion.txt.gz")

# Cargo data
df_apl <- fread("./data/ibarra_aplicacion.txt.gz")

#======================================================
# ratios de montos sobre cantidades

find_ratios_MC = function(cols){
  sorted_cols = sort(cols)
  M = sorted_cols[which(grepl('^m', sorted_cols))]
  C = sorted_cols[which(grepl('^c', sorted_cols))]
  matchM = M[is.na( match(str_sub(M, start = 2), str_sub(C, start = 2) ) ) == F ]
  matchC = C[is.na( match(str_sub(C, start = 2), str_sub(M, start = 2) ) ) == F ]
  return(paste(matchM," / ",matchC))
}

cols <- setdiff(names(df_gen), c("numero_de_cliente","clase")) #todas
string.ratios <- find_ratios_MC(cols)

#Para cada regla creadas armo una variable booleana. 
for (j in 1:length(string.ratios)){
df_gen[ , paste0("ratio", j) := eval(parse(text = string.ratios[j])) ]
df_apl[ , paste0("ratio", j) := eval(parse(text = string.ratios[j])) ]
}

i <- 1:length(string.ratios)
df_gen[, c("numero_de_cliente", paste0("ratio", i)):= lapply(.SD, function(x) ifelse(is.nan(x), NA, x)), .SDcols=c("numero_de_cliente", paste0("ratio", i))]
df_apl[, c("numero_de_cliente", paste0("ratio", i)):= lapply(.SD, function(x) ifelse(is.nan(x), NA, x)), .SDcols=c("numero_de_cliente", paste0("ratio", i))]


#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos      <- lapply(names(df_gen),function(.name) df_gen[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  df_gen[mapply(is.infinite, df_gen)] <- NA
  df_apl[mapply(is.infinite, df_apl)] <- NA
}


# Guardo nuevos dataframes en carpeta
setwd( "~/buckets/b1/")
output_folder <- "./exp/FE_RATIOS/"
dir.create( output_folder, showWarnings = FALSE )
j <- 1:length(string.ratios)
fwrite(df_gen[, c("numero_de_cliente", paste0("ratio", j)), with=FALSE], paste0(output_folder,"fe_ratios_GENERACION.csv.gz"), row.names = F)
fwrite(df_apl[, c("numero_de_cliente", paste0("ratio", j)), with=FALSE], paste0(output_folder,"fe_ratios_APLICACION.csv.gz"), row.names = F)





