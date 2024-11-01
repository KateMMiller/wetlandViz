#----------------------------------------------------------------
# Appends latest year of veg and site data to existing datasets
# Last updated for 2024. Run this before prep_photopoints_for_viz.R
#----------------------------------------------------------------
library(tidyverse)
library(wetlandACAD)
#setwd("./wetlandViz")

importRAM(export_protected = T)
prev_path <- "C:/NETN/R_Dev/data/wetlandViz_backup/data_prev_versions/"
#--- VMMI data update ---
# RAM sites
vmmi<- sumVegMMI(site = "all", years = 2020:2024) |> mutate(Label = gsub("R-", "RAM-", Code))
sitedata <- read.csv(paste0(prev_path, 'Sentinel_and_USA-RAM_Sites_2023.csv'))

vmmi_site <- left_join(vmmi, sitedata, by = c("Label", "Panel")) |> 
  select(Code, Label, Year, Visit_Type, Mean_C = meanC, Invasive_Cover, 
         Pct_Cov_TolN = Cover_Tolerant, Sphagnum_Cover= Bryophyte_Cover,
         VMMI = vmmi, VMMI_Rating = vmmi_rating)

# NWCA sites  
vmmi_21 <- NWCA21_vegMMI(path = "../data/NWCA21", export = T)
vmmi21 <- vmmi_21 |> 
  mutate(Visit_Type = "VS", Year = 2021) |> 
  select(Code = site_name, Label = LOCAL_ID, Year, Visit_Type, Mean_C = mean_c_adj, Invasive_Cover = cov_inv_adj,
         Sphagnum_Cover = cov_bryo_adj, Pct_Cov_TolN = cov_tol_adj, VMMI = vmmi, VMMI_Rating = vmmi_rank)

# combine RAM and NWCA sites
vmmi_comb1 <- rbind(vmmi_site, vmmi21) %>% arrange(Code, Year)# |> mutate(across(c(Mean_C:VMMI), round))
vmmi_comb1$Pct_Cov_TolN <- as.numeric(vmmi_comb1$Pct_Cov_TolN)
vmmi_comb <- vmmi_comb1 |> mutate(across(c(Mean_C, Invasive_Cover, Sphagnum_Cover, Pct_Cov_TolN, VMMI),
                                         \(x) round(x, 2))) 
head(vmmi_comb)
table(vmmi_comb$Year)
write.csv(vmmi_comb, "./data/vmmi_2020-2024.csv", row.names = F)

#--- Species data update ---
sppdata_prev <- read.csv(paste0(prev_path, "Sentinel_and_RAM_species_data_2019-2023.csv")) |> filter(Year > 2019)

sppdata_new <- sumSpeciesList(years = 2024) |> 
  mutate(Label = gsub("R-", "RAM-", Code),
         Site_Type = "RAM",
         Ave_Cov = NA_real_) |> 
  select(Latin_Name, Common, Label, Longitude, Latitude, Site_Type, Year, 
         PctFreq = quad_freq, Ave_Cov, Invasive, Protected_species)

spplist_comb <- rbind(sppdata_prev, sppdata_new)

# Drop protected species
spplist_prot <- spplist_comb |> filter(Protected_species == FALSE)

head(spplist_prot)

write.csv(spplist_prot, "./data/Sentinel_and_RAM_species_data_2020-2024.csv", row.names = F)
