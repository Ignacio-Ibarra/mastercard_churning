#Leo todas las importancias de variables y las promedio. 
# Luego con esas variables veo las distribuciones a ver si hay mucha diferencia
# entre los datasets generación y aplicación. 

require("data.table")
require("tidyverse")

exp <- "BO2802"


read_from_path = function(path, parent_folder = paste0("./exp/",exp,"/")){
  return(fread(paste0(parent_folder,path)))
}


importance <- 
  list.files(path = "./exp/BO2802/", pattern = "^impo.*\\.txt$") %>% 
  map_df(~read_from_path(.)) %>% 
  group_by(Feature) %>% 
  summarise(Importance = mean(Frequency)) %>% 
  arrange(desc(Importance))

campos <- setdiff(importance$Feature, c("numero_de_cliente"))

datagen <- fread("./data/ibarra_generacion.txt.gz")
datagen$dataset <- "generacion"
datapl <- fread("./data/ibarra_aplicacion.txt.gz")
datapl$dataset <- "aplicacion"

dataplot <- rbind(datagen[, c(campos,"dataset"), with=FALSE], datapl[, c(campos,"dataset"), with=FALSE])

graficar_campo  <- function( campo)
{
  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataplot[ dataset == "generacion", get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataplot[ dataset == "aplicacion", get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  print(dataplot %>%
    ggplot(aes(x = .data[[campo]],
               y = dataset,
               fill = dataset)) +
    xlim(xxmin, xxmax)+
    ggridges::geom_density_ridges(bandwidth = 4)+ylab("")+xlab("")+
    theme(legend.position="none")+
    ggtitle(campo))
  
}

output_folder <- paste0("./exp/DRIFT", format(Sys.Date(),"%d%m"),"/")
dir.create( output_folder, showWarnings = FALSE )
pdf(paste0(output_folder,"densidades_drifting.pdf"))

for( campo in  campos )
{
  cat( campo, "  " )
  
  
  graficar_campo( campo )
  
}

dev.off()



#No se observan grandes diferencias entre las distribuciones. 