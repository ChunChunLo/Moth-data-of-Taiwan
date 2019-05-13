#------ Contribution of each environment variable
library(here)
library(data.table)
library(magrittr)
library(ggplot2)
library(readxl)
library(purrr)
library(dplyr)

Stage <- c("all-data", "adult", "larva")
x <- i <- z <- 1

species.SDM_list <- 
  lapply(1:length(Stage), 
         function(x){
           list.files(file.path("Results", Stage[x]),  
                      pattern = "\\d",
                      full.names = FALSE) %>% 
             data.table %>%  
             setnames(c("accepted_name_code"))
         }) 
names(species.SDM_list) <- Stage

species.table <- 
  read_xlsx("Results/species_record_table.xlsx") %>% 
  setDT


contr.table <-
  lapply(1:length(Stage),
         function(i){
           lapply(1:dim(species.SDM_list[[i]])[1],
                  function(x){
                    fread(sprintf("Results/%s/%s/maxentResults.csv",
                                  Stage[i],
                                  species.SDM_list[[Stage[i]]][x])) %>% 
                      .[, accepted_name_code := species.SDM_list[[Stage[i]]][x]] %>% 
                      #.[Species == "species (average)",] %>%
                      .[, accepted_name_code := species.SDM_list[[Stage[i]]][x]] %>%
                      .[, Stage := Stage[i]]
                  }) %>%
             do.call(rbind, .)}) %>%
  do.call(rbind, .) %>%
  melt(id = c("accepted_name_code", "Stage"),
       measure = patterns("contribution"),
       variable.name = "Env",
       value.name = "Contribution") 
# species.table[., on = "accepted_name_code"]


#### 除錯版: 只取maxentResults.csv最後的100筆預測值取平均
contr.table.all <-
  lapply(1:length(Stage),
         function(i){
           lapply(1:dim(species.SDM_list[[i]])[1],
                  function(x){
                    fread(sprintf("Results/%s/%s/maxentResults.csv",
                                  Stage[i],
                                  species.SDM_list[[Stage[i]]][x])) %>% 
                      .[, accepted_name_code := species.SDM_list[[Stage[i]]][x]] %>% 
                      #.[Species == "species (average)",] %>%
                      .[, accepted_name_code := species.SDM_list[[Stage[i]]][x]] %>%
                      .[, Stage := Stage[i]]
                  })}) 

Table <- lapply(1:length(Stage),
                function(i){
                  lapply(1:dim(species.SDM_list[[i]])[1],
                         function(x){
                           if (dim(contr.table.all[[i]][[x]])[1] == 101){
                             contr.table.all[[i]][[x]] %>% 
                               .[Species == "species (average)",] %>%
                               .[, Species:= NULL] %>% 
                               return(.)
                           }else{
                             lapply(1:100 , function(z){
                               contr.table.all[[i]][[x]] %>% 
                                 .[max(which(Species == paste("species", z-1, sep = "_"))), ]}) %>%
                               do.call(rbind, .) %>%
                               .[, 2:121] %>% 
                               .[, lapply(.SD, mean, na.rm = TRUE)] %>% 
                             .[, accepted_name_code := species.SDM_list[[Stage[i]]][x]] %>%
                             .[, Stage := Stage[i]] %>% 
                               return(.)
                           }
                           }) %>% 
                    do.call(rbind, .)}) %>% 
  do.call(rbind, .) 

fwrite(Table, here("Results","MaxEnt.Result_all.csv"))
# Table <- fread(here("Results","MaxEnt.Result_all.csv"))

contr.table <- Table %>%
  melt(id = c("accepted_name_code", "Stage"),
       measure = patterns("contribution"),
       variable.name = "Env",
       value.name = "Contribution") %>% 
  .[, accepted_name_code := as.character(accepted_name_code)] %>%
  .[, Env := gsub(" contribution", "", Env)] %>%
  species.table[., on = "accepted_name_code"] %>%
  .[, Family:= paste(family, family_c, sep = " ")]

contr.table$Stage %<>% 
  gsub("all-data", "All stage", .) %>% 
  gsub("adult", "Adult", .) %>%
  gsub("larva", "Larva", .) %>% 
  factor(., levels= c("All stage", "Adult", "Larva"))

contr.table$Env %<>%
  gsub("aspect", "Aspect", .) %>%
  gsub("bio", "Bio", .) %>% 
  gsub("bl", "BL", .) %>% 
  gsub("bu", "BU", .) %>% 
  gsub("dtriver", "DFW", .) %>% 
  gsub("elesd", "ELESD", .) %>%  
  gsub("ele", "ELE", .) %>% 
  gsub("ff", "FF", .) %>% 
  gsub("fo", "FO", .) %>% 
  gsub("fw", "FW", .) %>% 
  gsub("md", "MD", .) %>% 
  gsub("slope", "Slope", .) %>% 
  gsub("solar", "ASR", .) %>% 
  gsub("ub", "UB", .) %>% 
  gsub("wb", "WB", .) %>% 
  gsub("wl", "WL", .) %>%
  factor(., levels=c("Bio01", "Bio02", "Bio04", 
                     "Bio12", "Bio15", "Bio16", 
                     "ELE", "ELESD", "Aspect", 
                     "Slope", "ASR", "FF", 
                     "MD", "FW", "FO", 
                     "BU", "WL", "UB", 
                     "WB", "BL", "DFW"))

nFamily <- contr.table %>% 
  .[, .(N = .N), by= list(Family, Stage)] %>%
  dcast(Family ~ Stage, value.var = c("N"))

fwrite(contr.table, here("Results","MaxEnt.Result_all.m.csv"))
# contr.table <- fread(here("Results","MaxEnt.Result_all.m.csv"))



# scatter plot
ggplot() +
  geom_jitter(data = contr.table,
             aes(x = reorder(Env, Contribution), y = Contribution),
             width = 0.25,
             size = 0.5) +
  facet_grid(.~ Stage) +
  labs(x = "Variables" , y = "Contribution (%)") +
  coord_flip() +
  theme_bw() + 
  theme(axis.ticks.y = element_blank(), 
        axis.title.x = element_text(margin =  margin(t = 15)),
        axis.title.y = element_text(margin =  margin(r = 15)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(plot = last_plot(), 
       filename = "Plot_contribution_scatter_all.png",
       path = "Results",
       width = 10, height = 6, dpi = 300)

# boxplot
ggplot() +
  geom_boxplot(data = contr.table,
               aes(x = reorder(Env, Contribution), y = Contribution),
               fill = "gray75", outlier.size = .6)  +
  facet_grid(.~ Stage) +
  labs(x = "Variables" , y = "Contribution (%)") +
  coord_flip() +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(), 
        axis.title.x = element_text(margin =  margin(t = 15)),
        axis.title.y = element_text(margin =  margin(r = 15)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), 
       filename = "Plot_contribution_boxplot_all.png",
       path = "Results",
       width = 10, height = 6, dpi = 300)



ggplot() +
  geom_boxplot(data = contr.table,
               aes(x = reorder(Env, -Contribution), y = Contribution, fill = Stage),
               outlier.size = .6)  +
  scale_fill_grey(start = 0.4, end = .8) + 
  scale_color_grey(start = 0.25, end = .75) +
  # coord_cartesian(expand = FALSE) + 
  # facet_grid(.~ Stage) +
  labs(x = "Variables" , y = "Contribution (%)") +
  # coord_flip() +
  theme_bw() + 
  theme(axis.ticks.x = element_blank(), 
        axis.title.x = element_text(margin =  margin(t = 15)),
        axis.title.y = element_text(margin =  margin(r = 15)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

ggsave(plot = last_plot(), 
       filename = "Plot_contribution.boxplot_all.png",
       path = "Results",
       width = 12, height = 6, dpi = 300)


#### group by family ####
# boxplot

ggplot() +
  geom_boxplot(data = contr.table,
               aes(x = reorder(Env, -Contribution), y = Contribution), 
               width = 0.75, size = 0.3, fill = "gray75", outlier.size = .25)  +
  # coord_cartesian(expand = TRUE) + 
  facet_wrap(.~ Family, ncol = 4) +
  labs(x = "Variables" , y = "Contribution (%)") +
  # coord_flip() +
  theme_bw() + 
  theme(text = element_text(size = 15), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = .4), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_text(size = 18, margin =  margin(t = 15)),
        axis.title.y = element_text(size = 18, margin =  margin(r = 15)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), 
       filename = "Plot_contribution_boxplot_family.png",
       path = "Results",
       width = 20, height = 12, dpi = 300)

ggplot() +
  geom_boxplot(data = contr.table,
               aes(x = reorder(Env, -Contribution), y = Contribution, fill = Stage), 
               width = 0.8, size = 0.3, outlier.size = .25)  +
  scale_fill_grey(start = 0.4, end = .8) + 
  # coord_cartesian(expand = TRUE) + 
  facet_wrap(.~ Family, ncol = 4) +
  labs(x = "Variables" , y = "Contribution (%)") +
  # coord_flip() +
  theme_bw() + 
  theme(text = element_text(size = 18), 
        axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = .4), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_text(size = 18, margin =  margin(t = 15)),
        axis.title.y = element_text(size = 18, margin =  margin(r = 15)),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave(plot = last_plot(), 
       filename = "Plot_contribution_boxplot_family_stage.png",
       path = "Results",
       width = 25, height = 15, dpi = 300)


#### each family #### 
# boxplot
for (i in 1: dim(nFamily)[1]) {
  Fn <- nFamily[i, 1]
  Count <- paste("#All data:", nFamily[i, 2], "/ #Adult:", nFamily[i, 3], "/ #Larva:", nFamily[i, 4], sep = " " )
  contr.table_f <- contr.table[Family == Fn]
  
  # boxplot
  Plot_f <- ggplot() +
    geom_boxplot(data = contr.table_f,
                 aes(x = Env, y = Contribution, fill = Stage),
                 width = 0.8, size = 0.3, outlier.size = .2)  +
    scale_fill_grey(start = 0.4, end = .8) +  
    # coord_cartesian(expand = TRUE) +
    theme_bw() + 
    labs(title = paste(Fn), subtitle = Count, x = "Variables" , y = "Contribution (%)") +
    # coord_flip() + 
    theme(text = element_text(size = 8), 
          axis.text.x = element_text(size = 8, angle = 90, 
                                     hjust = 1, vjust = .4), 
          axis.text.y = element_text(size = 8),
          axis.ticks.x = element_blank(), 
          axis.title.x = element_text(margin =  margin(t = 10)),
          axis.title.y = element_text(margin =  margin(r = 10)),
          plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, margin =  margin(t = 3, b = 5)))
  Plot_f
  ggsave(plot = Plot_f, 
         filename = paste0("Plot_contribution.boxplot_family_", Fn, ".png"),
         path = "Results",
         width = 8, height = 5, dpi = 300)
  rm(Plot_f)
}

