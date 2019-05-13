#-----------------------
#title: "Construct MaxEnt model_Moth"
#author: "Anyu Chang" "Chun"
#-----------------------

# species matrix of data

# library(here)
library(readxl)
library(sf)
library(rgdal)
library(magrittr)
library(data.table)
library(tidyr)
library(dplyr)
library(purrr)
library(writexl)
library(here)
#------ import data needed

# load checklist of terrestrail vertebrate
species.checklist <- 
  read_xlsx("Results/Moth checklist.xlsx", sheet = 1) %>% 
  setDT %>% 
  .[is_accepted_name == 1]

# species record 2010
Stage <- c("all-data", "adult", "larva")

species.count_list <- 
  lapply(1:length(Stage), 
         function(x){
           read.csv(here("data/Species", paste0(Stage[x], "_2010s_121.csv"))) %>% 
             data.table %>%  
             .[, .(nPoint = .N, 
                   nGrid = uniqueN(GID)), 
               by = list(Accepted_name_code)] %>%
             setnames(c("accepted_name_code", paste(c("nPoint", "nGrid"), Stage[x], sep = "_")))
         }) %>%
  reduce(left_join, by = "accepted_name_code") %>%
  setDT


# import species SDM data
species.SDM_list <- 
  lapply(1:length(Stage), 
         function(x){
           list.files(file.path("Results", Stage[x]),  
                      pattern = "\\d",
                      full.names = FALSE) %>% 
             data.table %>% 
             .[, do.MaxEnt := as.numeric(1)] %>% 
             setnames(c("accepted_name_code", paste("do.MaxEnt", Stage[x], sep = "_")))
         }) %>%
  reduce(left_join, by = "accepted_name_code") %>%
  setDT

#---- merge summary with Species list
final <- 
  reduce(list(species.checklist, 
              species.count_list,
              species.SDM_list),
         left_join, 
         by = "accepted_name_code") %>% 
  setDT

#---- export as xlsx
write_xlsx(final, here("Results","Results/species_record_table.xlsx"))
