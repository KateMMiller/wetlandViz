#----------------------------------
# Script pulls in wetland photopoints, resizes, adds border and title, and saves as JPG
#----------------------------------
library(tidyverse)
library(magick)
library(stringi)
library(stringr)

setwd("D:/NETN/R_Dev/")
full_names<- list.files('./photopoints', pattern='JPG$', full.names=T)
full_names
photo_name<- list.files('./photopoints', pattern='JPG$', full.names=F)
photo_name[1]

#photo_name2<-str_replace(photo_name, ".JPG", ".gif")
photo_name2<-str_replace(photo_name, "_07.", "_07_")
photo_name2<-str_replace(photo_name2, "_08.", "_08_")

photo_name2

# Function to name each photo based on its name
view_namer<-function(pname){
  if (grepl("RAM", pname)){
    ifelse(grepl("360L", pname), paste("North View"),
           ifelse(grepl("090L", pname), paste("East View"),
                  ifelse(grepl("180L", pname), paste("South View"),
                         ifelse(grepl("270L", pname), paste("West View"),
                                "none"
                         ))))
  } else {
    ifelse(grepl("P-0", pname), paste("North View"),
           ifelse(grepl("P-90", pname), paste("East View"),
                  ifelse(grepl("P-180", pname), paste("South View"),
                         ifelse(grepl("P-270", pname), paste("West View"),
                                "none"
                         ))))
  }
}

process_image<-function(import_name, export_name){
  title<-view_namer(pname=export_name)
  
  img<-image_read(import_name)
  img2<-image_border(img, 'black','5x5')
  img3<-image_scale(img2, 'X500')
  img4<-image_annotate(img3, paste(title), size=16, color='black',
                       boxcolor='white', location='+10+10')
  image_write(img4, format='jpeg', paste0("./wetlandViz/www/", export_name))
  print(export_name)
}

#process_image(import_name=full_names[40], export_name=photo_name2[40]) #test
length(full_names)

map2(full_names[1:length(full_names)], photo_name2[1:length(full_names)], ~process_image(.x,.y))

# Dealing with RAM-17
full_names17<- list.files('./photopoints/RAM-17', pattern='JPG$', full.names=T)
photo_name17<- list.files('./photopoints/RAM-17', pattern='JPG$', full.names=F)
photo_name17[1]

photo_name17_2<-str_replace(photo_name17, ".JPG", ".gif")

img<-image_read(full_names17[4])
img2<-image_border(img, 'black','5x5')
img3<-image_scale(img2, 'X500')
img4<-image_annotate(img3, paste("Scene 4"), size=16, color='black',
                     boxcolor='white', location='+10+10')
image_write(img4, format='JPG', paste0("./wetlandViz/www/", photo_name17_2[4]))

#---------------------
# Update file names in sitedata for viz
sitedata <- read.csv('./wetlandViz/data/Sentinel_and_USA-RAM_Sites.csv')
sitedata_old <- sitedata %>% filter(Panel != 4)
all_photos <- data.frame('file' = list.files("./wetlandViz/www/", pattern='JPG$'))

sitedata_2020 <- sitedata %>% filter(Panel == 4) %>% select(Site:Cowardin_Class)
panel4 <- paste(sitedata_2020$Label, sep = "", collapse = "|")

photos_2020 <- all_photos %>% filter(str_detect(file, panel4))
photos_2020

photos_2020 <- photos_2020 %>% mutate(file2 = str_remove(file, ".JPG"),
                                      scene = case_when(grepl("360L", .$file) ~ paste0("North_View"),
                                                        grepl("090L", .$file) ~ paste0("East_View"),
                                                        grepl("180L", .$file) ~ paste0("South_View"),
                                                        grepl("270L", .$file) ~ paste0("West_View")),
                                      Label = substr(file, 1, 6)) %>% 
                               select(file2, scene, Label) %>% 
                               pivot_wider(names_from = scene, values_from = file2)

photos_2020

sitedata_update <- merge(sitedata_2020, photos_2020, by = "Label", all.x = TRUE, all.y = TRUE)
sitedata_final <- rbind(sitedata_old, sitedata_update)
write.csv(sitedata_final, "./wetlandViz/data/Sentinel_and_USA-RAM_Sites_2020.csv", row.names = FALSE)

#photos_2020 <- all_photos %>% 

