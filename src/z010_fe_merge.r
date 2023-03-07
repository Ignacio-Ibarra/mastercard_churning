#Unifico los fe que quiera

#limpio la memoria
rm(list=ls())
gc()

require("data.table")

# Creo función para usar distintos fe

fe.merge <- function(fe = list(ratio=TRUE, xgb=TRUE, rpart.rules=TRUE)){
  gen <- fread("./data/ibarra_generacion.txt.gz")
  # cat(length(gen))
  apl <- fread("./data/ibarra_aplicacion.txt.gz")
  if(fe$ratio == TRUE){
    s = "fe_ratios"
    paths = sprintf(c("./exp/%s/%s_GENERACION.csv.gz","./exp/%s/%s_APLICACION.csv.gz"), toupper(s), s)
    gen <- merge(gen, fread(paths[1]), by ="numero_de_cliente")
    # cat(length(gen))
    apl <- merge(apl, fread(paths[2]), by ="numero_de_cliente")
    
  }
  if(fe$xgb == TRUE){
    s = "fe_xgb"
    paths = sprintf(c("./exp/%s/%s_GENERACION.csv.gz","./exp/%s/%s_APLICACION.csv.gz"), toupper(s), s)
    gen <- merge(gen, fread(paths[1]), by.x="numero_de_cliente", by.y="id_cli_gen")
    # cat(length(gen))
    apl <- merge(apl, fread(paths[2]), by.x="numero_de_cliente", by.y="id_cli_apl")
  }
  if(fe$rpart.rules == TRUE){
    s = "fe_rpart_rules"
    paths = sprintf(c("./exp/%s/%s_GENERACION.csv.gz","./exp/%s/%s_APLICACION.csv.gz"), toupper(s), s)
    gen <- merge(gen, fread(paths[1]), by ="numero_de_cliente")
    # cat(length(gen))
    apl <- merge(apl, fread(paths[2]), by ="numero_de_cliente")
  }
  return(list("generacion" = gen, "aplicacion" = apl))
}

to.merge <- fe.merge() #si no le pongo nada es porque uso todas los fe
gen.df <- to.merge$generacion
apl.df <- to.merge$aplicacion

# Guardo nuevos dataframes en carpeta
output_folder <- paste0("./exp/FE_MERGE_", format(Sys.Date(),"%d%m"),"/")
dir.create( output_folder, showWarnings = FALSE )
fwrite(gen.df, paste0(output_folder,"fe_merge_GENERACION.csv.gz"), row.names = F)
fwrite(apl.df, paste0(output_folder,"fe_merge_APLICACION.csv.gz"), row.names = F)


