#----------------------------------
# Script pulls in wetland photopoints, resizes, adds border and title, and saves as JPG
#----------------------------------
library(tidyverse)
library(magick)
library(stringi)
library(stringr)

setwd("E:/NETN/R_Dev/")
full_names<- list.files('./photopoints', pattern='JPG$', full.names=T)
full_names
photo_name<- list.files('./photopoints', pattern='JPG$', full.names=F)
photo_name[1]

#photo_name2<-str_replace(photo_name, ".JPG", ".gif")
photo_name2 <- str_replace(photo_name, "_07.", "_07_")
photo_name2 <- str_replace(photo_name2, "_08.", "_08_")

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
                         ifelse(grepl("P-270|P1-270", pname), paste("West View"),
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
sitedata <- read.csv('./wetlandViz/data/Sentinel_and_USA-RAM_Sites_2022.csv')
sitedata_old <- sitedata %>% filter(!Panel %in% c(0, 2))

all_photos <- data.frame('file' = list.files("C:/NETN/R_Dev/wetlandViz/www/", pattern='JPG$'))
colnames(all_photos) <- "filename"

sitedata_2023 <- sitedata %>% filter(Panel == 2) %>% select(Site:Cowardin_Class)
panel2 <- paste(sitedata_2023$Label, sep = "", collapse = "|")

photos_2023 <- all_photos %>% filter(str_detect(filename, panel2))
photos_2023
photos_2023 <- photos_2023 %>% mutate(file2 = str_remove(filename, ".JPG"),
                                      scene = case_when(grepl("360L", .$filename) ~ paste0("North_View"),
                                                        grepl("090L", .$filename) ~ paste0("East_View"),
                                                        grepl("180L", .$filename) ~ paste0("South_View"),
                                                        grepl("270L", .$filename) ~ paste0("West_View")),
                                      Label = substr(filename, 1, 6)) %>% 
                               select(file2, scene, Label) %>% 
                               pivot_wider(names_from = scene, values_from = file2)

sitedata_RAM <- merge(sitedata_2023, photos_2023, by = "Label", all.x = TRUE, all.y = TRUE)

sitedata_2021 <- sitedata %>% filter(Panel == 0) %>% select(Site:Cowardin_Class)
panel0 <- paste(sitedata_2021$Label, sep = "", collapse = "|")
head(sitedata)

photos_2021 <- all_photos %>% filter(grepl("NWC21", filename)) %>% 
  mutate(file2 = str_remove(filename, ".JPG"),
         scene = case_when(grepl("P-0", .$filename) ~ paste0("North_View"),
                           grepl("P-90", .$filename) ~ paste0("East_View"),
                           grepl("P-180", .$filename) ~ paste0("South_View"),
                           grepl("P-270|P1-270", .$filename) ~ paste0("West_View")),
         Site = paste0("EPA-R", substr(filename, 12, 14))) %>%
  select(Site, file2, scene) %>% pivot_wider(names_from = scene, values_from = file2)

photos_2021b <- left_join(photos_2021, sitedata_2021, by = "Site")
photos_2021b <- photos_2021b[, names(sitedata)]

sitedata_final <- rbind(sitedata_old, sitedata_RAM, photos_2021b)

write.csv(sitedata_final, "C:/NETN/R_Dev/wetlandViz/data/Sentinel_and_USA-RAM_Sites_2023.csv", row.names = FALSE)

#photos_2020 <- all_photos %>% 

