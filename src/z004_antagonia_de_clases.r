#Comparo clases en las variables importantes. 

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

campos <- setdiff(importance$Feature, c("numero_de_cliente","clase"))

dataplot <- fread("./data/ibarra_generacion.txt.gz") #solo generacion

antagonia.de.clases  <- function( campo)
{
  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataplot[ clase == "SI", get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataplot[ clase == "NO", get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )
  
  print(dataplot %>%
          ggplot(aes(x = .data[[campo]],
                     y = clase,
                     fill = clase)) +
          xlim(xxmin, xxmax)+
          ggridges::geom_density_ridges(bandwidth = 4)+ylab("")+xlab("")+
          theme(legend.position="none")+
          ggtitle(campo))
  
}

output_folder <- paste0("./exp/CLASES", format(Sys.Date(),"%d%m"),"/")
dir.create( output_folder, showWarnings = FALSE )
pdf(paste0(output_folder,"antagonia_de_clases.pdf"))

for( campo in  campos )
{
  cat( campo, "  " )
  
  
  antagonia.de.clases( campo )
  
}

dev.off()