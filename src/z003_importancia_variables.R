rm(list = ls())
gc(verbose = FALSE)

#Importancia de variables promedio. 

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