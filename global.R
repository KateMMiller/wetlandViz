#----------------------------------------------
# Global file for wetlandViz
#----------------------------------------------
library(shiny)
library(shinyjs)

welld<-read.csv('./data/well_prec_data_2013-2018.csv')
names(welld)

sentsites<-data.frame(sitename=c("Big Heath", "Duck Pond", "Gilmore Meadow", "Heath Brook", 
             "Hodgdon Swamp","Little Hunter's Brook","New Mills Meadow","Western Mtn. Swamp"), 
             well=c('BIGH_WL', 'DUCK_WL','GILM_WL','HEBR_WL','HODG_WL','LIHU_WL','NEMI_WL',
                    'WMTN_WL'))

sitedata<-read.csv('./data/Sentinel_and_USA-RAM_Sites.csv')



