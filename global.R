#----------------------------------------------
# Global file for wetlandViz
#----------------------------------------------
library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)

# Pull in water level data
welld <- read.csv('./data/well_prec_data_2013-2019.csv')
well_stats <- read.csv('./data/well_growing_season_stats_2013-2019_long.csv')
well_stats <- well_stats %>% 
  mutate(Label = case_when(site == "BIGH_WL" ~ "Big Heath",
                           site == "DUCK_WL" ~ "Duck Pond",
                           site == "GILM_WL" ~ "Gilmore Meadow",
                           site == "HEBR_WL" ~ "Heath Brook",
                           site == "HODG_WL" ~ "Hodgdon Swamp",
                           site == "LIHU_WL" ~ "Little Hunter's Brook",
                           site == "NEMI_WL" ~ "New Mills Meadow",
                           site == "WMTN_WL" ~ "Western Mtn. Swamp"))

sitedata<-read.csv('./data/Sentinel_and_USA-RAM_Sites.csv')

sentsites<-data.frame(sitename=c("Big Heath", "Duck Pond", "Gilmore Meadow", "Heath Brook", 
                                 "Hodgdon Swamp","Little Hunter's Brook","New Mills Meadow","Western Mtn. Swamp"), 
                      well=c('BIGH_WL', 'DUCK_WL','GILM_WL','HEBR_WL','HODG_WL','LIHU_WL','NEMI_WL',
                             'WMTN_WL'))

plotList<- noquote(as.character(unique(sitedata$Label)))

# Pull in veg data
vmmi<-read.csv('./data/vmmi_2019.csv') #includes 2019 data
#sppdata<-read.csv("./data/Sentinel_and_RAM_species_data_2011-2019.csv")
sppdata<-read.csv("./data/Sentinel_and_RAM_species_data_2011-2019_pub.csv") #sensitive spp. removed
sampleyears<-c(2011,2015,2017,2018,2019) # most recent survey of available data
DataTypes<-list(vmmi='Veg. MMI', spplist='Spp. List')
sitemap<-merge(sitedata,vmmi[,c("Label","Year")], by='Label', all.x=T) %>% 
  filter(Year %in% sampleyears) %>% droplevels()

# Prep veg data for Map panel
vmmimap<-merge(sitedata,vmmi,by=c('Label'),all.x=T, all.y=T) %>% mutate_at(vars(Mean_C:VMMI),list(~round(.,1))) %>% 
  mutate(VMMI_Rating=factor(VMMI_Rating, levels=c('Good','Fair','Poor'))) %>% arrange(Site_Type,Label) %>% 
  filter(Year %in% sampleyears) %>% 
  select(Label,Site_Type,Longitude,Latitude,Year,Mean_C:VMMI_Rating, North_View:West_View)

sppmap1<-merge(sppdata,sitedata[,c('Label','HGM_Class','HGM_Subclass','Cowardin_Class')],by='Label',all.x=T,all.y=T)
sppmap<-sppmap1 %>% filter(Year %in% sampleyears) %>% arrange(Label, Latin_Name, PctFreq, Ave_Cov) %>% 
        droplevels()

spplistall<-noquote(as.character(unique(sppmap$Latin_Name)))

spplist<- sppmap %>% select(-Common, -Invasive) %>% mutate(present=ifelse(PctFreq>0,'Present','Absent')) %>% 
  select(Label:Site_Type,Latin_Name, Year, present, HGM_Class:Cowardin_Class) %>% 
                             spread(Latin_Name,present,fill='Absent') %>% 
  gather("Latin_Name","Present", -Label,-Longitude,-Latitude,-Site_Type, -Year,
         -HGM_Class, -HGM_Subclass, -Cowardin_Class)

sppinv <- sppmap %>%  mutate(inv_num=ifelse(PctFreq>0 & Invasive==TRUE, 1, 0)) %>% 
  select(Label:Site_Type, inv_num) %>% group_by(Label, Longitude, Latitude, Site_Type) %>% 
  summarise(num_inv=sum(inv_num), inv_present=ifelse(num_inv>0,'Present','Absent'), .groups = 'drop')

plotlist1<-vmmimap %>% arrange(desc(Site_Type), Label) 
plotlist<-noquote(as.character(unique(plotlist1$Label)))
