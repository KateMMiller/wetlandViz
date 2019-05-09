#----------------------------------------------
# Global file for wetlandViz
#----------------------------------------------
library(shiny)
library(shinyjs)
library(dplyr)

# Pull in water level data
welld<-read.csv('./data/well_prec_data_2013-2018.csv')
sitedata<-read.csv('./data/Sentinel_and_USA-RAM_Sites.csv')
sentsites<-data.frame(sitename=c("Big Heath", "Duck Pond", "Gilmore Meadow", "Heath Brook", 
                                 "Hodgdon Swamp","Little Hunter's Brook","New Mills Meadow","Western Mtn. Swamp"), 
                      well=c('BIGH_WL', 'DUCK_WL','GILM_WL','HEBR_WL','HODG_WL','LIHU_WL','NEMI_WL',
                             'WMTN_WL'))

# Pull in veg data
#sitedata<-read.csv('./data/Sentinel_and_USA-RAM_Sites.csv') # Site-level data for plotting
vmmi<-read.csv('./data/vmmi.csv')
sppdata<-read.csv("./data/Sentinel_and_RAM_species_data.csv")

sampleyears<-c(2011,2014,2015,2017,2018) # most recent survey of available data

DataTypes<-list(vmmi='Veg. MMI', spplist='Spp. List')

# Prep veg data for Map panel
vmmimap<-merge(sitedata,vmmi,by=c('Label'),all.x=T, all.y=T) %>% 
  mutate(VMMI_Rating=factor(VMMI_Rating, levels=c('Good','Fair','Poor'))) %>% 
  filter(Year %in% sampleyears)

sppmap<-sppdata %>% filter(Year %in% sampleyears) %>% arrange(Label, Latin_Name) %>% droplevels()


