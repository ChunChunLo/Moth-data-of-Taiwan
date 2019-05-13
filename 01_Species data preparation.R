#-----------------------
#title: "Construct MaxEnt model_Moth"
#author: "Anyu Chang" "Chun"
#-----------------------

#------ load packages
library(here)
library(data.table)
library(magrittr)
library(tidyr)
library(rgdal)
#------ import data
data.all <- 
  list.files("data/Species",
             pattern = "txt",
             full.names = TRUE) %>%
  lapply(function(x)
    fread(x, encoding = "UTF-8") %>% 
      separate("Date", c("Year", "Month", "Day"), "-|/")) %>% 
  do.call(rbind, .)

#------ extract data
#-- extract data after 2010
#-- Taiwan or island
#-- different stage

i <- j <- z <-  1
Years <- rbind(c("2010s", 2010, 2019), c("1990s", 1990, 1999)) %>%
  data.table %>%
  setnames(c("Years", "min", "max"))

# for TWD97_121 (120~122) / for TWD97_119 (118~120)
Range <- rbind(c("121", 120, 122, "+init=epsg:3826"), c("119", 118, 120, "+init=epsg:3825")) %>%
  data.table %>%
  setnames(c("Range", "min", "max","epsg"))

Stage <- c("all-data", "adult", "larva")


for(i in 1 : dim(Years)[1]){
  Years.name <- Years[i, Years]
  data_year <- data.all %>% 
    .[Year >= Years[i, min] & Year <= Years[i, max],
      list(Accepted_name_code, Longitude, Latitude, GID, Stage)]
  
  for(j in 1 : dim(Range)[1]){
    Range.name <- Range[j, Range]
    Epsg <- Range[j, epsg]
    data_year.range <- 
      data_year[Longitude >= Range[j, min] & Longitude <= Range[j, max],
                list(Accepted_name_code, Longitude, Latitude, GID, Stage)]
    
    for(z in 1 : length(Stage)){
      stage <- Stage[z]
      
      if( z == 2 ){
        data_year.range.stage <- 
          data_year.range[Stage == "1-成蟲",
                          list(Accepted_name_code, Longitude, Latitude, GID, Stage)]
      } else if (z == 3){
        data_year.range.stage <- 
          data_year.range[Stage %like% "2-幼蟲|3-卵|4-蛹",
                          list(Accepted_name_code, Longitude, Latitude, GID, Stage)]
      } else if (z == 1){
        data_year.range.stage <- 
          data_year.range[,
                          list(Accepted_name_code, Longitude, Latitude, GID, Stage)]
      }
      if(dim(data_year.range.stage)[1] != 0){
        
        # turn WGS84 to TWD97
        coordinates(data_year.range.stage) <- ~ Longitude + Latitude
        proj4string(data_year.range.stage) <- CRS("+init=epsg:4326")
        data_year.range.stage %<>% spTransform(CRS(Epsg)) 
        data <- 
          cbind(data_year.range.stage@data, data_year.range.stage@coords)
        
        #------ export as RDS
        write.csv(data,
                  paste0("data/Species/", stage, "_", Years.name, "_", Range.name, ".csv"))
      }
    }    
  }
}

