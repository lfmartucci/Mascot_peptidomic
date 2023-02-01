#!/usr/bin/Rscript


library(tidyverse)
library(MSnbase)



cachedpeaklist<- commandArgs(trailingOnly=TRUE)


mgf<-cachedpeaklist



#Contaminant database path
MaConDa <- read_delim("E:\\Mascot files\\mascot_daemon_setup\\Filtering_script\\MaConDa__v1_0__extensive.txt", 
                      delim = "\t", escape_double = FALSE, 
                      trim_ws = TRUE)

MaConDa <- 
  MaConDa %>% 
  
  select(name,
         type_of_contaminant,
         ion_mode,
         mz, 
         exact_mass,
         exact_adduct_mass) %>% 
  mutate(across(where(is.numeric), ~ round(.,2)))


Data <- MSnbase::readMgfData(mgf)

Peaks_info <- Data@featureData@data %>% 
  separate(PEPMASS, into=c("Mass", "Intensity"), sep = " ") %>% 
  mutate(Charge= gsub("\\+", "", CHARGE)) %>% 
  select(Mass, Intensity, Charge) %>% 
  mutate(across(everything(), ~ as.numeric(.) %>% round(2)),
         Peaks_ID=rownames(.))


Id_contaminants <- Peaks_info %>% 
  mutate(Mass2= Mass) %>% 
  left_join(MaConDa,
            by= c("Mass"="mz",
                  "Mass2"="exact_adduct_mass")) %>% 
  mutate(Contaminant= ifelse(!is.na(ion_mode), FALSE, TRUE)) %>% ### FALSE=contaminant, to allow subset
  select(Contaminant) %>% 
  unlist() %>% 
  unname()

Filtered_mgf <- subset(Data, Id_contaminants)

new_name <- Data@processingData@files

MSnbase::writeMgfData(Filtered_mgf, new_name)