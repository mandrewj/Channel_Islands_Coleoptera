#California Channel Islands Beetles - M. Andrew Johnston 2022
#the following code was used to analyze the channel islands data generated as part of producing an annotated checklist of beetles for them.

#Install dependencies if needed - uncomment line below
#install.packages(c("ggpubr","vegan","iNEXT","cowplot"))

#load dependencies
library("ggplot2")
library("ggpubr")
library("vegan")
library("iNEXT")
library("cowplot")
library("svglite")

#Read in island data
data<-read.csv("~/Desktop/CCI/island_data.csv", header=TRUE)
labels(data)

#Add Log-transformed data
data$logarea<- log(data$Area,10)
data$logspecies<- log(data$Species,10)

#Plot various island data against each other and include a linear model
#the calulated line and r2 value are printed onto the plot graphics


#species by area
#png(filename="~/Desktop/CCI/IslandArea.png", width=600, height=600)

areaplot<-ggplot(data, aes(x=Area, y=Species, label=Islands)) +
  geom_point()+
  xlim(0,300) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 475, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 425, aes(label = ..rr.label..)) +
  labs(x = "Island Area (km2)", y = "Number of Species") +
  #ggtitle("A. Beetle Species by Island Area") +
  theme(plot.title = element_text(hjust = 0.5))
#dev.off()

#species by distance
#png(filename="~/Desktop/CCI/IslandDistance.png", width=600, height=600)
distanceplot<-ggplot(data, aes(x=distance, y=Species, label=Islands)) +
  geom_point()+
  xlim(0,120) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 475, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 425, aes(label = ..rr.label..)) +
  labs(x = "Distance to Mainland (km)", y = "Number of Species") +
  #ggtitle("B. Beetle Species by Island Distance to Mainland") +
  theme(plot.title = element_text(hjust = 0.5))
#dev.off()
p<-plot_grid(areaplot,distanceplot, labels="AUTO", ncol=1, rel_heights = c(2,2), align="hv")
#save_plot("~/Desktop/CCI/Figure1.svg",p,base_width=8.09, base_height=10)
save_plot("~/Desktop/CCI/Figure1.pdf",p,base_width=8.09, base_height=10)


#Species by log area
#Note: not used in manuscript
png(filename="~/Desktop/CCI/IslandArea-OneLog.png", width=600, height=600)
ggplot(data, aes(x=logarea, y=Species, label=Islands)) +
  geom_point()+
  xlim(0,3) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 475, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 425, aes(label = ..rr.label..)) +
  labs(x = "log Island Area (km2)", y = "Number of Species") +
  ggtitle("Beetle Species by Island Area") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


#logSpecies by log area
#Note: not used in manuscript
png(filename="~/Desktop/CCI/IslandArea-LogLog.png", width=600, height=600)
ggplot(data, aes(x=logarea, y=logspecies, label=Islands)) +
  geom_point()+
  xlim(0,3) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 2.6, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 2.4, aes(label = ..rr.label..)) +
  labs(x = "log Island Area (km2)", y = "log Number of Species") +
  ggtitle("Beetle Species by Island Area") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


#Species by rainfall
#Rainfall had no signal and we questioned the validity of the measurements from Miller 1985, so this was left out of manuscript
png(filename="~/Desktop/CCI/IslandRainfall.png", width=600, height=600)
spprain<-ggplot(data, aes(x=Rainfall, y=Species, label=Islands)) +
  geom_point()+
  xlim(0,60) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 475, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 425, aes(label = ..rr.label..)) +
  labs(x = "Annual Rainfall (cm)", y = "Number of Species") +
  ggtitle("Beetle Species by Rainfall") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()




#Species by DigRecords
#png(filename="~/Desktop/CCI/SPecDigRecords.png", width=600, height=600)

sppDigit<-ggplot(data, aes(x=DigRecords, y=Species, label=Islands)) +
  geom_point()+
  xlim(0,8000) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 475, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 425, aes(label = ..rr.label..)) +
  labs(x = "Digitized Records", y = "Number of Species") +
  #ggtitle("Beetle Species by Digitized Records") +
  theme(plot.title = element_text(hjust = 0.5))
#dev.off()

#Digrecords by Island Area
#png(filename="~/Desktop/CCI/DigIsland.png", width=600, height=600)
areaDigit<-ggplot(data, aes(x=Area, y=DigRecords, label=Islands)) +
  geom_point()+
  xlim(0,300) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 6500, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 5500, aes(label = ..rr.label..)) +
  labs(x = "Island Area", y = "Number of Digitized Records") +
  #ggtitle("Digitized Records by Island Area") +
  theme(plot.title = element_text(hjust = 0.5))
#dev.off()

p2<-plot_grid(sppDigit,areaDigit, labels="AUTO", ncol=1, rel_heights = c(2,2), align="hv")
save_plot("~/Desktop/CCI/Figure2.pdf",p2,base_width=8.09, base_height=10)
save_plot("~/Desktop/CCI/Figure2.png",p2,base_width=8.09, base_height=10)


#This list is the number of islands each species is known from.
#The counts were generated from the master table published with the checklist and includes digitized and literature records
numIslands<-c(1,2,4,2,4,4,2,6,1,3,8,2,1,3,6,3,2,1,2,3,2,2,1,1,1,1,1,2,1,3,3,1,1,6,5,5,1,6,2,1,5,2,2,1,3,5,5,2,1,1,3,1,1,2,1,1,2,2,2,1,2,3,1,5,4,2,1,1,2,1,1,3,4,1,1,2,3,3,1,3,1,1,2,1,1,1,7,2,2,6,1,2,1,3,1,1,2,1,1,4,4,2,2,1,1,2,2,1,5,4,1,5,3,3,4,3,2,3,1,3,2,2,1,1,2,4,1,3,1,2,1,1,1,2,1,1,1,1,1,1,2,1,3,1,1,1,1,1,5,2,1,2,1,1,2,1,2,2,1,1,2,2,1,3,1,1,1,1,2,1,1,1,1,1,4,1,2,2,1,5,5,5,1,1,2,6,1,7,1,1,4,1,1,1,3,1,1,1,3,6,2,4,2,2,2,1,1,1,5,1,2,1,1,4,1,2,1,2,2,1,3,2,3,3,2,1,1,1,1,1,1,4,3,4,1,1,1,1,2,1,1,1,2,1,4,3,3,1,1,3,2,3,1,7,1,1,1,1,2,3,3,2,3,1,1,5,2,5,1,1,1,1,1,1,1,1,1,1,2,1,1,3,2,1,3,1,2,3,1,1,1,1,1,2,1,1,1,1,2,6,4,1,1,2,1,2,2,2,5,6,2,3,3,3,3,1,5,6,6,1,5,3,5,1,4,2,6,4,3,2,2,2,1,7,1,4,3,1,1,1,1,2,1,1,2,5,1,2,4,4,1,1,3,1,6,2,1,3,1,2,1,3,2,4,2,1,1,1,1,1,1,1,1,3,1,2,2,2,2,1,3,1,1,1,5,4,6,4,1,3,5,4,1,2,1,1,1,1,2,1,1,2,3,1,2,1,1,1,4,5,4,2,2,2,4,1,1,2,1,1,1,1,1,2,1,1,3,1,2,1,1,2,1,1,1,1,3,4,2,1,4,1,3,3,1,1,3,7,2,2,1,1,1,2,3,4,1,1,1,1,6,2,4,5,1,1,1,1,1,3,4,1,2,4,1,4,1,3,1,1,1,2,2,1,1,2,3,1,2,3,3,1,1,1,1,1,2,1,1,1,1,6,1,1,1,7,7,2,2,1,2,6,1,1,1,1,1,1,3,2,2,2,6,4,2,1,1,3,4,5,1,4,7,8,3,2,1,7,1,1,3,2,1,1,2,8,3,1,5,2,6,6,3,4,2,1,7,1,3,5,8,1,1,1,1,2,1,2,1,1,1,1,1,5,6,1,1,1,1,1,2,1,1,2,1,1,3,1,1,5,1,1,1,1,2,2,8,3,1,8,3,3,3,2,8,7,1,2,1,1,1,1,4,4,1,2,1,1,5,1,4,2,3,1,2,4,1,3,2,5,4,3,2,3,4,1,3,3,1,2,1,2,6,2,1,2,4,4,2,1,3,4,4,2,1,2,1,1,4,3,1,3,1,2,1,1,3,3,1,1,1,7,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,2,1,1,2,1,1,8,2,2,1,1,1,1,2,2,2,2,1,1,1,1,2,1,1,1,1,2,4,4,2,7,2,2,1,2,3,1,4,7,1,2,1,1,1,3,2,1,1,2,3,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,2,1,2,2,1,2,1,1,1,3,1,1,1,1,1,1,2,5,1,5,4,3,1,1,4,3,1,4,1,1,1,2,3,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1,3,5,1,1,1,2,1,1,1,3,1,1,1,2,1,1,1,2,2,1,1,1,2,2)
length(numIslands) #824
sum(numIslands==1) #416

#Plot histogram of islands inhabited by each species
pdf(file="~/Desktop/CCI/Figure3-IslandsInhabited.pdf", width=8.09, height=5)
spIslands<-hist(numIslands, col='blue', breaks=c(1,2,3,4,5,6,7,8), labels = TRUE, ylim=c(0,600), main=" ", xlab="Number of Islands", ylab="Species")
dev.off()


#The list is the number of digitized taxon records (pooled across all islands) for each species.
#Generated same as individual island counts below, but using a single occurrence file for all islands together
#from a tab-delimited occurrence file, run the following bash command to get number of records per species
#tail -n +2 FILENAME | cut -f15 -d$'\t' |sed '/^$/d' | sort | uniq -c | awk '{print $1}'
#print FILENAME without first line, cut out scientificName (field 15 in my files), remove empty lines, sort alphabetically, count for each unique value, print just the 

taxonRecords<-c(24,177,163,8,96,3,5,5,4,1,1,5,20,2,16,5,1,31,1,28,1,16,1,14,14,1,78,129,2,4,73,22,5,63,5,87,4,9,446,1,15,3,3,7,4,3,14,6,56,14,8,1,7,11,6,20,61,20,27,11,4,2,31,17,4,11,9,13,14,5,11,1,5,1,8,1,24,9,58,1,35,8,5,11,397,33,2,1,1,3,1,24,3,6,1,4,3,5,1,1,5,239,30,9,1,2,1,24,8,2,25,1,14,13,1,34,84,22,14,14,1,2,194,32,49,40,8,1,22,58,67,22,14,1,23,35,8,16,12,1,189,69,48,135,8,8,18,367,1,33,2,27,7,40,197,1,6,2,1,27,1,1,13,2,3,25,3,2,9,1,35,276,74,4,11,2,2,2,38,19,9,2,12,1,5,90,3,2,21,95,4,1,9,2,201,2,46,6,1,63,26,106,63,133,21,3,21,1,4,22,48,139,21,1,45,25,3,10,4,5,142,1124,19,4,58,3,16,7,3,44,83,3,1,1,4,5,45,1,230,5,10,80,74,2,123,1,1,2,5,1,1,2,1,17,19,4,4,1,10,3,18,1,11,13,2,1,4,216,18,1,5,15,30,1,15,3,5,88,10,27,20,2,67,12,6,5,27,7,59,63,2,25,1,1,1,74,4,31,89,3,4,3,65,85,5,7,1,4,7,38,4,12,7,4,9,2,2,10,10,70,3,26,198,175,13,3,1,56,303,89,3,12,12,9,18,29,6,9,49,1,6,1,2,1,5,5,30,8,339,23,6,2,13,10,47,28,1,32,82,3,26,11,1,13,20,107,2,4,18,35,2,1,1,1,4,1,6,4,41,1,1,72,3,4,4,2,2,1,28,15,6,85,2,2,8,14,1,46,3,1,74,21,46,80,211,14,554,1,3,2,11,39,2,29,41,2,12,18,20,106,1,20,3,8,2,4,30,21,76,1,13,1,85,8,6,2,17,77,1,25,59,5,1,7,294,4,22,11,7,51,30,4,12,1,4,15,4,2,85,3,3,106,10,2,5,2,21,1,23,2,27,3,224,10,35,3,3,4,7,2,12,7,1,1,1,15,1,156,2,16,6,21,2,4,22,2,4,20,3,1,25,6,8,2,2,2,8,13,106,22,36,1,1,21,12,6,2,4,1,6,1,38,22,13,11,7,6,14,1,1,2,92,8,189,1,17,15,1,3,16,24,3,1,8,13,2,5,1,29,1,2,1,142,1,1,14,20,1,19,66,2,271,3,2,1,1,21,2,2,127,2,9,9,1,1,1,29,4,1,16,3,1,1,78,2,20,1,1,2,1,1,29,14,5,6,1,1,9,37,30,6,1,2,10,14,3,41,21,28,4,5,6,11,5,16,2,1,5,41,47,11,38,86,4,129,3,2,1,13,1,2,9,2,64,12,43,8,2,43,1,2,11,1,2,15,4,45,2,46,33,132,28,3,28,1,1,10,88,4,51,3,77,1,1,16,7,2,3,1,11,11,6,1,100,1,4,4,6,1,4,2,1,8,50,16,15,3,1,2,2,43,7,43,5,1,71,19,2,4,3,1,6,1,11,12,3,10,3,24,47,6,2,39,2,8,7,1,1,1,8,6,35,1,34,12,14,53,15,1,1,2,138,1,4,3,11,14,2,11,21,5,11,1,1,11,2,2,16,4,56,5,1,1,2,9,23,5,9,162,74,84,9,1,2,21,1,13,56,4,9,5,17,16,8,59,62,1,5,2,1,116,12,5,1,4,5,223,18,17,153,12,1,1,19,20,118,18,24,2033,31,35,266,138,46,36,18,1,16,2,84,6,192,6,2,6,11,1,8,12,8,22,20,2,27,2,1,10,1,5,14,3,1)
#count number of taxa with 1 digitized record
sum(taxonRecords==1) #154

#Plot historgram of total digitized records per species
png(filename="~/Desktop/CCI/SpecRecords.png", width=600, height=600)
hist(taxonRecords, col='blue',xlim=c(0,600), labels = FALSE, breaks=100, main="Number of digitized records per species", xlab="Number of records", ylab="Species")
dev.off()

#This list contains the number of collecting events -  similar to the total records above but pooling by specimens collected on the same day by the same person within species
#tail -n +2 All_Islands.tab | cut -f15,31,34 -d$'\t' | sort | uniq | cut -f1 -d$'\t' | sed '/^$/d' | sort | uniq -c | awk -vORS=',' '{print $1} END {printf "\n"}'
#field 15 is scientificName, 31 is recordedBy [collector] and 34 is eventDate

taxonCollEvents<-c(9,25,21,3,23,3,4,3,4,1,1,2,6,1,7,1,1,7,1,7,1,7,1,4,3,1,18,28,2,1,17,4,2,21,3,16,4,4,85,1,3,2,2,3,4,2,8,1,9,2,2,1,7,6,1,4,15,2,10,5,2,1,13,3,2,3,3,2,3,2,4,1,2,1,4,1,14,6,12,1,6,2,3,2,108,22,2,1,1,3,1,9,2,4,1,2,3,5,1,1,2,45,14,1,1,1,1,11,2,2,7,1,6,7,1,5,17,4,5,4,1,2,28,15,11,8,7,1,12,12,20,4,1,1,10,8,1,11,4,1,29,12,15,33,3,5,3,70,1,5,1,15,7,16,53,1,4,2,1,10,1,1,5,1,3,2,2,2,2,1,14,2,17,1,4,2,2,2,10,4,3,1,3,1,5,11,2,1,6,31,3,1,7,2,21,1,24,1,1,4,14,14,3,4,6,1,4,1,3,4,25,92,13,1,39,10,1,6,3,3,25,105,4,4,22,2,8,5,2,11,24,1,1,1,1,4,23,1,47,2,1,8,21,1,23,1,1,2,4,1,1,2,1,9,4,3,2,1,4,1,7,1,4,2,2,1,3,55,10,1,3,9,20,1,11,3,4,18,5,12,5,1,11,2,1,4,11,2,13,15,1,10,1,1,1,15,3,15,30,2,2,1,15,23,1,7,1,2,7,2,2,1,3,2,4,1,1,4,3,13,3,10,56,78,6,3,1,20,58,31,3,7,10,8,4,8,6,2,11,1,2,1,2,1,2,1,12,2,48,7,4,2,3,2,15,9,1,12,36,3,11,4,1,3,5,15,2,1,7,15,2,1,1,1,1,1,2,2,16,1,1,21,2,1,1,2,2,1,9,2,3,39,2,2,6,6,1,10,3,1,20,6,8,39,76,12,78,1,3,2,4,9,1,8,13,2,5,3,1,12,1,7,3,7,2,2,9,10,21,1,5,1,22,5,4,2,4,51,1,8,17,2,1,3,49,2,7,3,6,4,4,3,4,1,2,2,4,2,24,2,1,11,1,2,1,2,6,1,6,1,8,1,30,4,4,2,2,4,3,1,3,1,1,1,1,3,1,31,2,1,3,4,2,4,4,1,2,13,3,1,13,4,7,1,1,1,1,7,18,12,17,1,1,9,3,2,1,4,1,5,1,13,9,3,6,4,3,6,1,1,2,8,6,30,1,9,6,1,1,5,13,3,1,4,6,2,4,1,15,1,2,1,14,1,1,8,6,1,11,29,1,22,2,2,1,1,6,2,1,16,2,3,2,1,1,1,7,4,1,5,1,1,1,23,2,7,1,1,1,1,1,13,5,1,1,1,1,4,13,4,4,1,2,3,8,1,15,9,12,3,3,2,4,4,5,2,1,5,18,12,2,3,7,2,43,1,1,1,3,1,2,8,1,36,10,4,4,1,15,1,1,1,1,2,6,2,5,1,22,4,59,9,3,7,1,1,4,12,4,7,2,11,1,1,6,3,1,3,1,4,4,1,1,20,1,1,1,1,1,3,2,1,2,10,6,3,2,1,1,1,18,6,17,4,1,25,6,1,3,3,1,4,1,4,7,2,4,3,11,11,5,1,11,1,3,5,1,1,1,3,1,10,1,8,5,4,5,2,1,1,2,25,1,1,1,3,4,1,4,8,3,1,1,1,1,2,1,3,2,14,4,1,1,1,1,7,1,1,60,12,17,4,1,2,5,1,6,27,1,3,3,4,2,3,6,19,1,4,1,1,82,7,2,1,2,2,27,3,3,22,3,1,1,11,15,20,8,4,28,8,3,42,16,9,3,8,1,5,1,14,3,35,5,1,2,2,1,3,4,4,7,9,1,8,2,1,4,1,5,5,2,1)
#count number of taxa with 1 digitized collecting event
sum(taxonCollEvents==1) #248

#Plot histogram of collecting events per species
png(filename="~/Desktop/CCI/CollEvents.png", width=600, height=600)
hist(taxonCollEvents, col='blue',xlim=c(0,110),breaks=50, labels = FALSE, main="Number of collecting events per species", xlab="Number of collecting events", ylab="Species")
dev.off()

#Create figure 3 - bar chart for 8 islands on top row for part 3A
p3A<-ggplot() +aes(numIslands) + geom_bar(stat="count", color="black", fill="blue") +
  geom_text(aes(label=..count..),stat="count", vjust=-0.7) +
  labs(x = "Islands inhabited", y = "Number of species") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#save_plot("~/Desktop/CCI/Figure3.pdf",p3,base_width=8.09, base_height=5)


#Create 3B-C in two parts for bottom row

trReduced<-taxonRecords[taxonRecords < 600]
p3B<-ggplot() +aes(trReduced) + geom_histogram(binwidth=5, color="black", fill="blue",) +
  #geom_text(aes(label=..count..),stat="count", vjust=-0.7) +
  #xlim(0.5,600.5) +
  labs(x = "Digitized specimens", y = "Number of species") +
  #scale_x_continuous() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))


p3C<-ggplot() +aes(taxonCollEvents) + geom_histogram(stat="bin", binwidth=5, color="black", fill="blue",) +
  #geom_text(aes(label=..count..),stat="count", vjust=-0.7) +
  #xlim(0,6000) +
  labs(x = "Unique Collecting Events", y = "Number of species") +
  #scale_x_continuous() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Align top and bottom rows into grids, combine into single plot and save
topRow<-plot_grid(NULL, p3A, NULL, labels=c("","A",""), label_size=22 ,rel_widths = c(1,4,1), ncol=3)
bRow<-plot_grid(p3B,p3C, labels=c("B","C"), label_size=22 , rel_widths=c(1,1))
Fig3<-plot_grid(topRow,bRow, labels=NULL, ncol=1)
save_plot("~/Desktop/CCI/Figure3.pdf",Fig3,base_width=10, base_height=10)
save_plot("~/Desktop/CCI/Figure3.png",Fig3,base_width=10, base_height=10)



#The following lists are specimen record counts per species separated by islands
#from a tab-delimited occurrence file (one file per island), run the following bash command to get number of records per species
#tail -n +2 FILENAME | cut -f15 -d$'\t' |sed '/^$/d' | sort | uniq -c | awk '{print $1}'
#print FILENAME without first line, cut out scientificName (field 15 in my files), remove empty lines, sort alphabetically, count for each unique value, print just the 

#Data below generated from bash command explained above
specdataN<-c()
specdataS<-c()
specdataN$Anacapa<-c(77,7,4,6,40,4,2,4,5,5,8,1,7,5,1,83,1,10,1,7,1,52,5,1,1,15,1,3,2,9,19,5,3,3,1,1,6,3,12,15,41,1,1,1,1,1,8,5,56,16,1,25,1,1,2,6,1,9,1,4,1,6,2,1,1,6,2,12,1,1,16,2,5,1,9,12,2,18,35)
specdataS$SanClemente<-c(16,11,29,9,19,2,5,5,7,3,5,42,6,3,20,7,1,3,3,104,1,1,1,11,1,1,1,1,7,1,2,4,16,6,14,32,9,7,6,2,24,31,1,1,1,5,3,63,4,28,8,15,2,313,19,8,1,5,30,2,5,2,5,5,3,3,3,23,8,1,29,21,36,19,2,6,2,20,29,1,5,7,6,6,1,65,15,8,11,1,15,6,10,11,1,7,11,72,6,1,14,2,1,1,11,1,16,7,16,1,1,2,97,1,2,1,18,2,3,3,26,5,3,10,1,6,23,11,1,1,18,1,52,6,4,36,1,35,9,28,2,2,1,27,7,7,5,1,36,1,14,9,18,4,2,2,42,12,42,5,9,2031,2,6)
specdataN$SanMiguel<-c(12,6,3,5,1,1,56,17,12,21,11,1,1,3,1,3,1,1,2,22,1,7,1,16,38,3,1,4,80,1,6,31,199,4,1,223,19,1,14,12,3,1,2,5,447,1,2,41,2,18,25,2,22,1,19,2,2,1,26,41,29,47,213,3,2,1,14,5,49,1,3,26,1,9,9,5,2,1,71,64,1,1,10,4,19,1,16,22,2,3,14,27,3,2,1,6,3,7,5,1,3,3,8,50,10,2,10,30,1,6,6,28,84,4,7,4,2,10,1,2,11,2,1,22,24,17,2,7,12,19,1,37,14,31,1,2)
specdataS$SanNicolas<-c(9,1,8,4,6,5,4,17,221,1,2,6,8,9,119,1,32,9,1,3,3,3,17,26,12,78,5,5,73,4,43,3,1,53,2,54,1,26,19,29,22,10,4,1,105,146,1,78,2,9,4,7,25,1,8,8,9,5,27,60,9,18,19,14,1,1,102,14,8,1,4,6,13,2,22,6,5,3,19,175,2,7,1,1,2,7,2,12,2,1,2,32,4,9,11,2,1,2,1,1,9,1,6,35,1,9,3,22,1,2,1,1,1,6,4,11,42,6,4,6,1,1,1,8,1,12,5,3,1,51,4,5,15,5,5,1,11,1,11,10,14,1,27,118,2,56,264)
specdataS$SantaBarbara<-c(2,21,128,2,1,149,3,19,3,1,5,71,15,1,3,35,3,2,3,1,16,1,3,1,3,1,1,1,3,3,2,1,1,9,1,1,1,1,6,1,18,1,2,2,46,1)
specdataS$SantaCatalina<-c(3,1,1,5,9,2,1,14,2,1,14,4,5,6,4,4,6,1,1,1,3,3,9,5,5,1,1,11,2,13,14,3,3,3,1,3,24,2,10,13,1,1,7,1,6,4,1,1,23,8,12,1,3,5,1,31,3,3,5,76,1,2,9,1,7,1,17,1,1,16,133,3,1,16,7,10,1,17,4,57,2,3,2,17,5,1,1,2,1,2,1,3,1,1,50,18,1,8,2,8,5,1,12,2,5,9,23,1,15,15,32,1,4,49,5,9,3,52,3,8,7,6,5,8,4,2,3,2,47,1,1,1,1,1,6,12,4,18,2,1,7,1,1,4,7,5,1,3,10,73,1,2,1,1,58,25,4,9,1,4,2,35,30,3,1,56,3,3,16,1,1,3,25,2,10,4,2,4,22,2,4,9,13,6,2,3,42,3,33,1,1,12,1,7,1,4,1,1,1,3,2,8,2,1,1,1,2,2,8,1,4,8,8,5,15,14,1,72,2,4,1,3,36,1,1,7,3,1,1,2,5,21,2,2,1,30,7,9,1,9,16,1,1,11,4,13,30,3,1,4,38,7,63,1,7,1,4,3,1,5,1,34,1,4,7,2,6,5,2,4,1,1,3,1,11,1,3,2,34,10,1,8,14,13,1,1,12,1,3,9,24,4,74,1,3,13,3,5,2,1)
specdataN$SantaCruz<-c(2,20,2,8,93,3,4,4,3,1,1,2,3,1,14,1,8,1,14,15,60,40,1,15,2,3,4,4,11,5,10,50,14,8,7,6,61,19,3,3,2,2,4,17,4,11,10,2,2,8,8,3,51,15,3,1,11,17,1,1,2,17,3,1,1,2,5,123,19,11,4,10,1,3,19,6,2,2,1,2,188,17,33,40,4,12,8,15,3,2,4,12,7,4,14,4,18,57,30,2,2,40,31,1,2,19,4,2,25,3,2,34,33,8,1,2,2,37,2,4,2,12,1,5,7,3,2,21,89,4,1,6,1,114,2,22,6,1,63,22,48,2,21,1,4,16,2,19,11,25,3,1,1,16,101,3,6,7,2,32,2,1,1,4,2,7,1,27,5,10,3,36,51,1,1,4,1,8,15,4,5,3,18,8,13,1,4,62,5,14,21,1,6,5,13,10,6,18,2,24,6,3,7,7,50,5,5,1,14,3,23,16,2,4,3,2,22,5,3,1,3,27,3,12,2,2,2,10,16,3,26,29,12,1,33,5,3,1,2,14,1,1,1,5,21,131,6,3,10,38,1,5,9,1,11,20,29,1,8,11,4,4,4,27,1,1,13,4,1,1,1,16,5,1,9,7,3,38,1,43,50,32,13,56,2,1,23,2,29,38,1,12,9,20,12,16,3,1,4,8,3,21,9,5,2,2,10,1,20,21,4,5,203,4,14,5,4,1,3,15,2,1,15,79,10,2,5,1,3,11,2,66,4,35,3,1,7,7,1,1,15,1,80,1,10,5,3,10,1,2,2,8,7,14,12,6,6,1,6,1,4,3,7,4,5,9,1,2,89,7,110,6,1,3,14,3,2,19,1,69,1,11,1,5,25,2,30,3,2,4,40,2,9,1,1,10,2,1,16,1,1,41,1,20,1,2,1,1,19,6,5,1,3,14,1,3,10,9,3,28,2,3,6,1,4,14,1,1,2,11,4,10,38,4,67,2,1,13,1,2,48,11,7,2,43,1,1,15,45,2,21,38,28,10,4,27,3,3,2,1,1,10,8,1,5,1,4,6,1,2,8,9,3,3,1,2,7,3,30,12,19,2,4,1,3,1,8,1,11,38,4,2,25,2,8,3,1,8,6,5,1,7,4,11,1,73,14,2,7,6,2,11,1,16,1,5,3,1,1,2,9,17,25,29,30,2,21,1,13,6,4,9,8,10,1,2,1,14,5,4,5,19,9,16,68,19,7,5,36,18,1,10,2,76,6,58,6,4,6,1,8,7,2,11,2,1,9,12,1)
specdataN$SantaRosa<-c(7,141,58,2,1,4,5,30,7,4,2,5,10,1,23,9,2,3,3,1,5,8,2,23,3,4,3,20,4,4,1,1,1,7,1,27,10,3,1,1,11,9,2,11,10,16,11,10,5,15,10,4,1,12,21,1,1,24,47,11,21,6,2,73,1,4,5,2,1,9,1,8,4,1,5,6,2,33,8,2,17,6,22,4,3,9,95,11,1,14,75,9,2,14,7,4,2,1,54,1,9,37,13,1,2,10,1,11,1,7,4,1,10,9,2,2,11,7,4,6,21,31,18,3,9,42,9,1,2,1,8,1,1,5,1,3,6,23,9,5,24,6,1,1,4,2,2,16,4,16,1,2,8,13,1,12,21,1,4,16,1,35,19,2,6,66,1,123,10,9,1,4,4,10,13,11,16,3,4,17,8,11,2,5,4,7,1,2,11,27,1,8,1,8,6,3,51,16,6,11,1,2,1,24,2,1,13,5,2,3,1,3,1,3,1,1,16,2,1,2,2,1,1,36,6,1,16,103,1,16,15,7,15,1,1,9,8,27,3,26,1,3,36,1,44,7,3,7,2,1,2,33,23,1,24,30,1,1,13,2,1,3,3,1,1,34,12,2,26,1,10,1,3,1,3,3,3,2,2,14,1,1,1,47,1,1,8,4,11,2,14,1,2,15,1,9,52,16,13,6,1,4,2,8,41,13,1,14,24,1,36,1,25,138,6,3,60,7,2,11,12,4,2,13,1,1)

#The genus Trigonoscute (Curculionidae) represented a huge proportion of San Clemente Beetles.
#The following lists were with and without Trigonoscuta included to make sure this abundant taxon was not skewing diversity analyses
SanClemente<-c()
SanClemente$all_taxa<-c(16,11,29,9,19,2,5,5,7,3,5,42,6,3,20,7,1,3,3,104,1,1,1,11,1,1,1,1,7,1,4,16,6,14,32,9,7,6,2,24,31,1,1,1,5,3,63,4,28,8,15,2,313,19,8,1,5,30,2,5,2,5,5,3,3,3,23,8,1,29,21,36,19,2,6,2,20,29,1,5,7,6,6,1,65,15,8,11,1,15,6,10,11,1,7,11,72,6,1,14,2,1,1,11,1,16,7,16,1,1,2,97,1,2,1,18,2,3,3,26,5,3,10,1,6,23,11,1,1,18,1,52,6,4,36,1,35,9,28,2,2,1,27,7,7,5,1,1,14,9,18,4,2,2,42,12,42,5,9,2031,2,6)
SanClemente$no_Trigonoscuta<-c(16,11,29,9,19,2,5,5,7,3,5,42,6,3,20,7,1,3,3,104,1,1,1,11,1,1,1,1,7,1,4,16,6,14,32,9,7,6,2,24,31,1,1,1,5,3,63,4,28,8,15,2,313,19,8,1,5,30,2,5,2,5,5,3,3,3,23,8,1,29,21,36,19,2,6,2,20,29,1,5,7,6,6,1,65,15,8,11,1,15,6,10,11,1,7,11,72,6,1,14,2,1,1,11,1,16,7,16,1,1,2,97,1,2,1,18,2,3,3,26,5,3,10,1,6,23,11,1,1,18,1,52,6,4,36,1,35,9,28,2,2,1,27,7,7,5,1,1,14,9,18,4,2,2,42,12,42,5,9,2,6)

#Call iNEXT to create rarefaction analyses and species diversity estimates for the north islands
outN<-iNEXT(specdataN, q=0, datatype="abundance")

#Use iNEXT ggplot2 wrapper to plot north island rarefaction curves
#png(filename="~/Desktop/CCI/NorthIslands.png", width=1200, height=800)
p4A<-ggiNEXT(outN, type=1) #+
  #ggtitle("A. North Islands Species Rarefaction")
#dev.off()

#Call iNEXT to create rarefaction analyses and species diversity estimates for the south islands
outS<-iNEXT(specdataS, q=0, datatype="abundance")

#Use iNEXT ggplot2 wrapper to plot south island rarefaction curves
#png(filename="~/Desktop/CCI/SouthIslands.png", width=1200, height=800)
p4B<-ggiNEXT(outS, type=1) #+
  #ggtitle("B. South Islands Species Rarefaction")
#dev.off()
outS

#Check San Clemente diversity using iNEXT with and without Trigonoscuta
png(filename="Desktop/SanClemente.png", width=1200, height=800)
outSC<-iNEXT(SanClemente, q=0, datatype="abundance")
ggiNEXT(outSC, type=1)
dev.off()


#Run iNEXT on combined (all islands pooled) dataset - create rarefaction plot
#png(filename="~/Desktop/CCI/allIslands.png", width=1200, height=800)
outAll<-iNEXT(taxonRecords, q=0, datatype="abundance")
p4C<-ggiNEXT(outAll, type=1) +
  theme(legend.position="none") #+
  #ggtitle("Species Richness estimate for all islands combined")
#dev.off()

#Call the iNEXT outputs to find species richness and diversity stats for each island and for the combined dataset
outS
outN
outAll

#Combine and plot all 3 parts in 1 figure.

p4C

p4<-plot_grid(p4A,p4B,p4C, labels="AUTO", label_size=32, ncol=1, rel_heights = c(3,3,2.5), align="v")
save_plot("~/Desktop/CCI/Figure4.pdf",p4,base_width=12, base_height=18)
save_plot("~/Desktop/CCI/Figure4.png",p4,base_width=12, base_height=18)


#Select statistics from the iNEXT outputs above were put into a csv file for reporting in the manuscript and for easier plotting below
#read in formatted data
richnessData<-read.csv("/Users/andrew/Desktop/CCI/SpeciesRichness.csv", header=TRUE)

#Predicted by observed species
png(filename="~/Desktop/CCI/Predicted_by_Observed.png", width=600, height=600)
ggplot(richnessData, aes(x=ObservedTaxa, y=EstimatedTaxa, label=Islands)) +
  geom_point()+
  xlim(0,600) +
  geom_text(aes(label=Islands, hjust=0, vjust=0)) +
  geom_smooth(method=lm, se=FALSE) +
  stat_regline_equation(label.y = 475, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 425, aes(label = ..rr.label..)) +
  labs(x = "Observed species richness", y = "Estimated species richness") +
  ggtitle("Predicted species richness by observed") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

