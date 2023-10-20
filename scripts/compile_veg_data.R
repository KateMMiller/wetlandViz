library(tidyverse)
library(wetlandACAD)

importQueries(type = 'file', path = "C:/NETN/Monitoring_Projects/Freshwater_Wetland/Database/2023_Wetland_Database/NETN_RAM_frontend_20231025.accdb")

# Note that the tbl_VMMI is the table that gets imported. You have to rerun that make table query in the front end to
# update the data. I will move all of this over into R eventually, so will deal with it as is for now.

#--- VMMI data update ---
sitedata <- read.csv('./data/Sentinel_and_USA-RAM_Sites_2023.csv')
vmmi_ram <- vmmi |> filter(Year > 2018) #only includes RAMs
table(vmmi_ram$Year)

vmmi_21 <- NWCA21_vegMMI(path = "../data/NWCA21", export = T)
vmmi21 <- vmmi_21 |> 
  mutate(Visit_Type = "VS", Year = 2021) |> 
  select(Code = site_name, Label = LOCAL_ID, Year, Visit_Type, Mean_C = mean_c_adj, Invasive_Cover = cov_inv_adj,
         Sphagnum_Cover = cov_bryo_adj, Pct_Cov_TolN = cov_tol_adj, VMMI = vmmi, VMMI_Rating = vmmi_rank)

vmmi_comb1 <- rbind(vmmi_ram, vmmi21) %>% arrange(Code, Year)# |> mutate(across(c(Mean_C:VMMI), round))
vmmi_comb1$Pct_Cov_TolN <- as.numeric(vmmi_comb1$Pct_Cov_TolN)
vmmi_comb <- vmmi_comb1 |> mutate(across(c(Mean_C, Invasive_Cover, Sphagnum_Cover, Pct_Cov_TolN, VMMI),
                                         round, 2)) 
head(vmmi_comb)
write.csv(vmmi_comb, "./data/vmmi_2019-2023.csv", row.names = F)

#--- Species data update ---
sppdata_prev <- read.csv("./data/Sentinel_and_RAM_species_data_2011_2018-2022.csv") |> filter(Year > 2018)
head(sppdata_prev)

head(sitedata)
head(veg_spp_cov21) # comes from NWCA21_vegMMI
spp21 <- veg_spp_cov21 |> mutate(Site = paste0("EPA-R", substr(site_name, 12, 14)), Year = 2021)

spp21b <- left_join(spp21, sitedata |> select(Label, Site, Longitude, Latitude, Site_Type), by = "Site") |> 
  select(species, PLANTS, Label, Longitude, Latitude, Site_Type, Year, PctFreq = pct_freq, Ave_Cov = avg_cov)

spp21c <- left_join(spp21b, plants |> select(Latin_Name, Common, PLANTS_Code, Invasive), 
                    by = c("PLANTS" = "PLANTS_Code")) |> 
  select(Latin_Name, Common, Label, Longitude, Latitude, Site_Type, Year, PctFreq, Ave_Cov, Invasive)

veg_check <- spp21c |> filter(is.na(Latin_Name))
if(nrow(veg_check) > 0) warning("Some species names were not linked between EPA and NETN lookups.")

spplist23 <- spplist %>% 
  filter(Year == 2023) %>% 
  mutate(PctFreq = Num_Quarters/4 * 100,
         Ave_Cov = NA_real_,
         Site_Type = "RAM") %>% 
  left_join(., 
            plants %>% select(TSN, Protected_species), by = c("Plant_ID" = "TSN")) %>%
  left_join(., sitedata %>% select(Label, Longitude, Latitude), by = "Label") %>%
  filter(Protected_species == FALSE) %>% 
  select(Latin_Name, Common, Label, Longitude, Latitude, Site_Type, Year, PctFreq, Ave_Cov, Invasive = Exotic) 

spplist_comb <- rbind(sppdata_prev, spp21c, spplist23)

table(spplist_comb$Year)
# Drop protected species
spplist_prot <- left_join(spplist_comb, plants |> select(Latin_Name, Protected_species), by = "Latin_Name") |> 
  filter(Protected_species == FALSE)

head(spplist_prot)

write.csv(spplist_prot, "./data/Sentinel_and_RAM_species_data_2019-2023.csv", row.names = F)
