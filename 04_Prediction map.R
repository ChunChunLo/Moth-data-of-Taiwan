#-----------------------
#title: "Construct MaxEnt model_Moth"
#author: "Anyu Chang" "Chun"
#-----------------------

# Prediction map for MaxEnt Results

#------ load packages
library(here)
library(raster)
library(rgdal)
library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)
library(parallel)
library(tidyr)
library(dplyr)


#### ####
#------ function "MAP" for automatic plotting and export
MAP <- 
  function(a, b, c){
    plot <- ggplot() +
      geom_raster(data = a, 
                  aes(x = x, y = y, fill = model_predict_stack)) +
      scale_fill_gradient(low = "white", 
                          high = "black", 
                          limits= c(0,1)) +
      coord_fixed() +
      geom_point(data = b, 
                 aes(x = Longitude, y = Latitude), 
                 colour = "red", 
                 size = 0.75) +
      ggtitle(paste0(c$accepted_name_code, "\n", c$name, "\n", c$common_name_c)) +
      theme_dark()+
      theme(plot.title = element_text(size = 12, 
                                      face = "bold"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=10, margin = margin(l = 5, unit = "pt")),
            plot.margin = unit(c(0.5, 0.15, 0.3, 0.3),"cm"),
            legend.key.size = unit(0.8, "cm"), 
            legend.background=element_rect(fill='transparent'),
            legend.box.margin = unit(c(0, 0, 0.05, -0.55), "cm"))
    
    panel_height = unit(1,"npc") - 
      sum(ggplotGrob(plot)[["heights"]][-3]) -
      unit(3, "line")
    
    plot <- plot + guides(fill= guide_colorbar(barheight=panel_height))
    
    
    ggsave(sprintf("%s_%s.png", 
                   c$accepted_name_code, c$name),
           plot = plot,
           path = here(sprintf("Results/predictionMAP/%s", Class)),
           height = 8, width = 5, dpi = 150)
  }


#---- Species data (Final)
# import species data
Class.list <- c("all-data", "adult", "larva")
Name.list <- c("all-data_2010s_121.csv", "adult_2010s_121.csv", "larva_2010s_121.csv")
i=3

for(i in 2:length(Class.list)){
  
  Name <- Name.list[i]
  Class <- Class.list[i]

#------ MaxEnt results
species.list <- 
  list.files(file.path("Results", Class),  
             pattern = "\\d",
             full.names = FALSE)

cl <- makeCluster(7)
clusterEvalQ(cl, c(library(raster),
                   library(magrittr))) %>% 
  invisible
max.results <- 
  list.files(here("Results", Class),  
             pattern = "\\d",
             full.names = TRUE) %>% 
  parLapply(cl, ., function(x)
    raster(sprintf("%s/model_predict_stack.tif", x)) %>% 
      rasterToPoints %>% 
      as.data.frame) 
stopCluster(cl)  

names(max.results) <- species.list

#------ Occurrence data
data.2010.121 <- 
  read.csv(here("data/Species",Name)) %>% 
  setDT %>% 
  unique

#------ Species table
species.table <- 
  read_xlsx("Results/Moth checklist.xlsx", sheet = 1) %>% 
  setDT %>% 
  .[is_accepted_name == 1]

#-- creat folder of species
ifelse(!dir.exists(here(sprintf("Results/predictionMAP/%s", Class))), 
       dir.create(here(sprintf("Results/predictionMAP/%s", Class))), FALSE)

# #-- check to do list
# Done.list <- list.files(here(sprintf("Results/predictionMAP/%s", Class)), pattern = "png") %>% 
#   data.table %>%
#   setnames("fname") %>%
#   separate(fname, c("accepted_name", "name"), sep = "_") %>% 
#   .[, name := gsub(pattern = ".png", replacement = "", x = name)]
# 
# done <- Done.list$accepted_name %>% .[done != "0"]
# 
# species.list %<>% .[!species.list %in% done]


#------ creat prediction plot
plot <- 
  lapply(1:length(max.results), 
         function(x)
           MAP(max.results[[x]], 
               data.2010.121[Accepted_name_code == species.list[x]],
               species.table[accepted_name_code == species.list[x]]))

}






#### Compare different stage #####
#------ load packages
library(here)
library(raster)
library(rgdal)
library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)
library(parallel)
library(tidyr)
library(dplyr)
library(purrr)
library(ggpubr)
library(grid)

#------ function "MAP" for automatic plotting and export
MAP <- 
  function(a, b, Class){
    plot <- ggplot() +
      geom_raster(data = a, 
                  aes(x = x, y = y, fill = model_predict_stack)) +
      scale_fill_gradient(low = "white", 
                          high = "black", 
                          limits= c(0,1),
                          na.value = "#bcc0c4") +
      coord_fixed() +
      geom_point(data = b, 
                 aes(x = Longitude, y = Latitude), 
                 colour = "red", 
                 size = 0.75) +
      ggtitle(Class) +
      theme_dark()+
      theme(plot.title = element_text(size = 14, 
                                      face = "plain"),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=10, margin = margin(l = 5, unit = "pt")),
            plot.margin = unit(c(0.5, 0.15, 0.3, 0.3),"cm"),
            legend.key.size = unit(0.8, "cm"), 
            legend.background=element_rect(fill='transparent'),
            legend.box.margin = unit(c(0, 0, 0.05, -0.55), "cm"))
    
    panel_height = unit(1,"npc") - 
      sum(ggplotGrob(plot)[["heights"]][-3]) -
      unit(3.1, "line")
    
    plot <- plot + guides(fill= guide_colorbar(barheight=panel_height))
    
    return(plot)
    
  }



#---- Species data (Final)
# import species data
Stage <- c("all-data", "adult", "larva")
i=1


species.list 

species.SDM_list <- 
  lapply(1:length(Stage), 
         function(x){
           list.files(file.path("Results", Stage[x]),  
                      pattern = "\\d",
                      full.names = FALSE) %>% 
             data.table %>% 
             setnames("Accepted_name_code") %>% 
             .[, do.MaxEnt := as.numeric(1)] %>% 
             setnames(c("Accepted_name_code", paste("do.MaxEnt", Stage[x], sep = "_")))
         }) %>%
  reduce(left_join, by = "Accepted_name_code") %>%
  setDT
  
#------ MaxEnt results

SDM.Result <- 
  function(file.path = file.path ){
    
list <- 
  list.files(file.path(file.path ),  
             pattern = "\\d",
             full.names = FALSE)

cl <- makeCluster(7)
clusterEvalQ(cl, c(library(raster),
                   library(magrittr))) %>% 
  invisible
max.results <- 
  list.files(file.path(file.path ),  
             pattern = "\\d",
             full.names = TRUE) %>% 
  parLapply(cl, ., function(x)
    raster(sprintf("%s/model_predict_stack.tif", x)) %>% 
      rasterToPoints %>% 
      as.data.frame) 
stopCluster(cl)  

names(max.results) <- list

return(max.results)
}
  
max.results_all <- SDM.Result(file.path = here("Results", Class.list[1]))
max.results_adult <- SDM.Result(file.path = here("Results", Class.list[2]))
max.results_larva <- SDM.Result(file.path = here("Results", Class.list[3]))
  
  #------ Occurrence data
  data.2010.121_all <- 
    read.csv(here("data/Species",Name.list[1])) %>% 
    setDT %>% 
    unique
  data.2010.121_adult <- 
    read.csv(here("data/Species",Name.list[2])) %>% 
    setDT %>% 
    unique
  data.2010.121_larva <- 
    read.csv(here("data/Species",Name.list[3])) %>% 
    setDT %>% 
    unique
  
  #------ Species table
  species.table <- 
    read_xlsx("Results/Moth checklist.xlsx", sheet = 1) %>% 
    setDT %>% 
    .[is_accepted_name == 1]
  
  #-- creat folder of species
  ifelse(!dir.exists(here(sprintf("Results/predictionMAP/%s", "Compare"))), 
         dir.create(here(sprintf("Results/predictionMAP/%s", "Compare"))), FALSE)
  

  

  #------ creat prediction plot
  # plot <- 
  #   lapply(1:length(max.results), 
  #          function(x)
  #            MAP(max.results[[x]], 
  #                data.2010.121[Accepted_name_code == species.list[x]],
  #                species.table[accepted_name_code == species.list[x]]))
  
  #--plot
  max.results_NA <- max.results_all[[1]]
  max.results_NA$model_predict_stack <- NA_integer_
    
  # species.list %<>% .[ !is.na(adult)|!is.na(larva)]
  # species.list %<>% .[ is.na(adult) & is.na(larva)]
  
p <- lapply(1:dim(species.list)[1], function(z){
  
  
  Species <- species.list[z, accepted_name_code]
  Sp.table <- species.table[accepted_name_code == Species]
  
  
  file.name <- paste(Sp.table$accepted_name_code, 
                      Sp.table$name, sep = "_")
  plot.name <- paste0("\n  ", 
                      Sp.table$accepted_name_code, "\n  ", 
                      Sp.table$name, "\n  ", 
                      Sp.table$common_name_c)
  # all-data
  map_all <- MAP(max.results_all[[Species]], 
                 data.2010.121_all[Accepted_name_code == Species],
                 Class.list[1])
  
  # adult
  if(!is.na(species.list[accepted_name_code == Species, adult])){  
    map_adult <- MAP(max.results_adult[[Species]], 
                     data.2010.121_adult[Accepted_name_code == Species],
                     Class.list[2])  
  } else {
    map_adult <- MAP(max.results_NA, 
                     data.2010.121_adult[Accepted_name_code == Species],
                     Class.list[2])  
  }
  
  # larva
  
  if(!is.na(species.list[accepted_name_code == Species, larva])){
    map_larva <- MAP(max.results_larva[[Species]], 
                     data.2010.121_larva[Accepted_name_code == Species],
                     Class.list[3])  
  } else {
    map_larva <- MAP(max.results_NA, 
                     data.2010.121_larva[Accepted_name_code == Species],
                     Class.list[3])  
  }
  
  p <- list(map_all, map_adult, map_larva)
  # align multiple ggplot2 graphs
  main <- ggarrange(plotlist = p,
                    ncol = length(p), align = "h") #align = c("none", "h", "v", "hv")
  
  annotate_figure(main,
                  top = textGrob(
                    plot.name,
                    gp = gpar(fontface = 2, fontsize = 18),
                    hjust = 0, x = 0.012)
  )
  
 
  
  ggsave(sprintf("%s.png", 
                 file.name),
         plot = last_plot(),
         path = here(sprintf("Results/predictionMAP/%s", "Compare")),
         height = 8, width = 13, dpi = 150)
  
  return(last_plot())
  })
  