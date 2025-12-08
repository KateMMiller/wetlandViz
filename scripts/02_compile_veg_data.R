#----------------------------------------------------------------
# Run 01_prep_photopoints_for_viz.R before running this script
# Appends latest year of veg and site data to existing datasets
#----------------------------------------------------------------
library(tidyverse)
library(wetlandACAD)
#setwd("./wetlandViz")

importRAM(export_protected = T)
path <- "C:/Users/KMMiller/OneDrive - DOI/NETN/R_Dev/wetlandViz/data/"
#--- VMMI data update ---
# RAM sites
vmmi<- sumVegMMI(site = "all", years = 2021:2025) |> mutate(Label = gsub("R-", "RAM-", Code))
sitedata <- read.csv(paste0(path, 'Sentinel_and_USA-RAM_Sites_2025.csv'))

vmmi_site <- left_join(vmmi, sitedata, by = c("Label", "Panel")) |> 
  select(Code, Label, Year, Visit_Type, Mean_C = meanC, Invasive_Cover, 
         Pct_Cov_TolN = Cover_Tolerant, Sphagnum_Cover = Bryophyte_Cover,
         VMMI = vmmi, VMMI_Rating = vmmi_rating)

# NWCA sites  
vmmi_21 <- NWCA21_vegMMI(path = "C:/Users/KMMiller/OneDrive - DOI/NETN/R_Dev/data/NWCA21", export = T)
vmmi21 <- vmmi_21 |> 
  mutate(Visit_Type = "VS", Year = 2021) |> 
  select(Code = site_name, Label = LOCAL_ID, Year, Visit_Type, Mean_C = mean_c_adj, Invasive_Cover = cov_inv_adj,
         Sphagnum_Cover = cov_bryo_adj, Pct_Cov_TolN = cov_tol_adj, VMMI = vmmi, VMMI_Rating = vmmi_rank)

# combine RAM and NWCA sites
vmmi_comb1 <- rbind(vmmi_site, vmmi21) |> arrange(Code, Year)
vmmi_comb1$Pct_Cov_TolN <- as.numeric(vmmi_comb1$Pct_Cov_TolN)
vmmi_comb <- vmmi_comb1 |> mutate(across(c(Mean_C, Invasive_Cover, Sphagnum_Cover, Pct_Cov_TolN, VMMI),
                                         \(x) round(x, 2))) 
head(vmmi_comb)
table(vmmi_comb$Year)
write.csv(vmmi_comb, paste0(path, "vmmi_2021-2025.csv"), row.names = F)

#--- Species data update ---
sppdata_sen <- read.csv(paste0(path, "Sentinel_species_data_2021.csv")) 

sppdata_new <- sumSpeciesList(years = 2022:2025) |> 
  mutate(Label = gsub("R-", "RAM-", Code),
         Site_Type = "RAM",
         Ave_Cov = NA_real_) |> 
  select(Latin_Name, Common, Label, Longitude, Latitude, Site_Type, Year, 
         PctFreq = quad_freq, Ave_Cov, Invasive, Protected_species)

spplist_comb <- rbind(sppdata_new, sppdata_sen) |> 
  filter(Protected_species == FALSE) |> 
  arrange(Label, Latin_Name)

head(spplist_comb)
table(spplist_comb$Protected_species)

write.csv(spplist_comb, paste0(path, "Sentinel_and_RAM_species_data_2021-2025.csv"), row.names = F)
