library("here")
library("readxl")
library("sp")
library("raster")
library("rgdal")
library("tidyverse")
library("ggplot2")
library("graphics")


#load data:

#read csv file Version 1! version2 would be better, where is it?

#bdata <- read.csv(here("bird_classification_v1","bird_classification_v1.csv"), check.names=TRUE,sep=",")
bdata <- read.csv(here("bird_classification_v1","bird_classification_v1.csv"), check.names=TRUE,sep=",")
#farmland: EUFarmlandBirdsVicky.csv
#forest EUForestbBirdsVicky.csv

eubirds <- read.csv(here("EUForestbBirdsVicky.csv"), check.names=TRUE,sep=";")

names(bdata) #read header csv
names(eubirds)

data_species <- (unique(bdata$Common.Name))
data_species
eubirds$Englisch

length(data_species)
unique(bdata$Common.Name)
       

#compare species in data frame with EU birds#
rel_species <- list()
for(i in seq_along(data_species)){
  if(data_species[i] %in% eubirds$Englisch){
    rel_species <- c(rel_species,data_species[i])}}
    #print(data_species[i])}}

rel_species #species in both datasets
rel_species
####################################################################################
#check datasets
#if("Common Kestrel" %in% eubirds$Englisch){print('yep')}else{print('nope')}
#if("Black-headed bunting" %in% bdata$Common.Name){print('yep')}else{print('nope')}
#"Common Raven" %in% bdata$Common.Name

#check if data is there
length(unique(bdata$Site))
unique(bdata$Site)
unique(bdata[bdata$Site == 32,]$date)

num_rec <- list()
sites <- list()
for(si in c(unique(bdata$Site))){
  sites <- append(sites, si)
  
  num_rec <- append(num_rec,length(unique(bdata[bdata$Site == si,]$date)))
}

test <-do.call(cbind.data.frame, sites)
test <- t(test)
colnames(test) <- 'Sites'
test <- as.data.frame(test)
test$num_rec_days <- unlist(num_rec)
test
write.csv(test,"C:/Users/wittekii/Documents/data analysis Audiomoth\\num_rec_days.csv", row.names = FALSE)
#C:/Users/wittekii/Documents/data analysis Audiomoth
#most common birds in general
tail(names(sort(table(bdata$Common.Name))), 20)


###########################################################################################
site_num <-40  # which site

unique(bdata[bdata$Site == site_num,  ]$Common.Name)#


#for site print species which are detected

unique(bdata[bdata$Site == site_num,  ]$Common.Name)#

length(unique(bdata[bdata$Site == site_num,  ]$Common.Name))# how many differnce species


#for site count detections

det_species <- bdata[bdata$Site == site_num,  ]$Common.Name 

table(unlist(det_species, recursive = TRUE, use.names = FALSE)) # lists count species

count(bdata[bdata$Site == site_num,  ],vars = "Common.Name") #sum of detections

length(unique(bdata[bdata$Site == site_num,  ]$Common.Name)) #how many species in site


#most common bird(s) in site, make that sense? confidence? time? day?
tail(names(sort(table(det_species))), 2)
###########################################################################################



############################################################################################

species <- "Eurasian Jay"

############################################################################################
#data cut Rank
data_rank <- subset(bdata, bdata$Rank == 1)

#data cut eu birds
dat_eu <- subset(data_rank, data_rank$Common.Name %in% rel_species )
sort(table(dat_eu$Common.Name))

#data cut common
#commonbirds <- tail(names(sort(table(data_rank$Common.Name))), 15)# 15 most common
#data_cb <- subset(data_rank,data_rank$Common.Name%in%commonbirds)
#data_cb_day <- subset(data_rank,data_rank$Common.Name%in%commonbirds & data_rank$date == 20210524)


################################################################################
#find out 
#permanent_grassland_proportion_class
#edge_length_class
#of sites
unique(dat_eu$edge_length_class)
unique(dat_eu[dat_eu$Site == 58,]$permanent_grassland_proportion_class)
unique(dat_eu[dat_eu$Site == 58,]$edge_length_class)

#summary(dat_eu[dat_eu$Common.Name == species,])
length(unique(dat_eu$Site))
##################################################################################
#Correlation`?`

#Point-Biserial Correlation #p values of rel species

for(s in rel_species){
  species_vec <- c()
  cat_vec <- c()
  for(i in unique(dat_eu$Site)){if (s %in% dat_eu[dat_eu$Site == i,]$Common.Name) { species_vec <- append(species_vec, 1)
  } else {species_vec <- append(species_vec, 0)
}}
for(i in unique(dat_eu$Site)){cat_vec <- append(cat_vec,unique(dat_eu[dat_eu$Site == i,]$edge_length_class ))
} 
  outpu <- c(s,cor.test(cat_vec,species_vec, method="pearson")$p.value)
print(outpu)
}

#count days of occurrence corr. p-value
for(s in rel_species){
  species_vec <- c()
  for(i in unique(dat_eu$Site)){
    num <- 0
  for(d in unique(dat_eu$date)){
    if(s %in% dat_eu[dat_eu$date == d & dat_eu$Site == i,]$Common.Name){num <- num +1}
    else{num <- num} }
    species_vec <- append(species_vec, num)
  }
  cat_vec <- c()
  for(i in unique(dat_eu$Site)){cat_vec <- append(cat_vec,unique(dat_eu[dat_eu$Site == i,]$edge_length_class ))#permanent_grassland_proportion_class ))
  } 
  outpu <- c(s,cor.test(cat_vec,species_vec, method="pearson")$p.value)
  outpu_c <- c(s,chisq.test(table(cat_vec,species_vec))$p.value)
  
  print(outpu)
}





#number of sites relevant species occure:
for(i in rel_species){output <- c(i,length(unique(dat_eu[dat_eu$Common.Name == i,]$Site)))
print(output)}
#################################################################################
#common/site
ggplot(dat_eu, aes(x =Common.Name , y = Site )) + geom_point() +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#common/site/day
ggplot(data_cb_day, aes(x =Common.Name , y = Site )) + geom_point() +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#count occurence of birds of one site
as.data.frame(table(unlist(data_cb[data_cb$Site == site_num,]$Common.Name)))
pt <- ggplot(as.data.frame(table(unlist(data_cb[data_cb$Site == site_num,]$Common.Name))), aes(x =Var1 , y = Freq ))+ geom_point() + xlab("Common Name") +ylab('n')
pt +  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#number of difference species/site
for (site in c(unique(bdata$Site))){print(paste('Site',site,':',length(unique(bdata[bdata$Site == site,  ]$Common.Name))))}

#test test


