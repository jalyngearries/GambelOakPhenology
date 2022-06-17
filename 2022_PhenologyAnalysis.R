# Grand Canyon National Park
# Science and Resource Management Program
# R script for plant phenology data
# Starter script for Jalyn Gearries, Vegetation Program  
# Set up by Skye Salganek, Data Management and Analytics Program 
# 6/16/2022

# There are cleaner ways to have less lines of code in this script 
# but because you're learning R - I'm going to break it down, especially so you can understand which packages are 
# responsible for each function 

########## ########## ########## ##########
# PART 1 : Extract 1st date/year for each plant for 3 phenology stages 

# Import your data as a dataframe
pheno <- read.csv("Z:/Team/Citizen_Science/Phenology/Analysis/RScriptsandData/Interp 2013-2022 All Site Analysis.csv")
head(pheno)

# Some initial data cleaning
# Make a column for year
if(!require(lubridate)){
  install.packages("lubridate")
  require(lubridate)}

pheno$Date <- as.Date(pheno$Date, format = '%m/%d/%Y')
pheno$year <- year(pheno$Date) 

# Now to find the first instance for each tree in different phenological states each year 

{if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)}
if(!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)}}
unique(pheno$Breaking.Leaf.Buds)


########## ########## ########## ########## 
# 1A:  Breaking Leaf Buds
pheno.BLB <- pheno[pheno$Breaking.Leaf.Buds == 'Yes',]; dim(pheno.BLB); dim(pheno)
pheno.BLB = pheno.BLB %>%
  group_by(Plant.ID,year) %>%
  slice_min(Date)
pheno.BLB <- as.data.frame(pheno.BLB); dim(pheno.BLB)
pheno.BLB
write.csv(pheno.BLB,"Z:/Team/Citizen_Science/Phenology/Analysis/RScriptsandData/20220616_BreakingLeafBuds.csv")

########## ########## ########## ########## 
# 1B:  Canopy Max Leafed Out 
# This one is a bit more complicated 
# I think that R function dplyr::rank could be used also but for the sake of time right now I'm going to 
# make a new column to rank Leafed-outedness 

pheno$What...of.canopy.is.full. #  we want to remove %, <, >, - and make this numeric
unique(pheno$What...of.canopy.is.full.)
pheno.ML <- pheno
for (i in 1:length(pheno.ML)) {
  if (pheno.ML$What...of.canopy.is.full.[i] == "<5%" ) {
    pheno.ML$LeafRank[i] = 1}
  else if (pheno.ML$What...of.canopy.is.full.[i] == "5-24%" ) {
    pheno.ML$LeafRank[i] = 2}
  else if (pheno.ML$What...of.canopy.is.full.[i] == "25-49%" ) {
    pheno.ML$LeafRank[i] = 3}
  else if (pheno.ML$What...of.canopy.is.full.[i] == "50-74%" ) {
    pheno.ML$LeafRank[i] = 4}
  else if (pheno.ML$What...of.canopy.is.full.[i] == "75-94%" ) {
    pheno.ML$LeafRank[i] = 5}
  else{ pheno.ML$LeafRank[i] = 6
  }} ; print(pheno.ML$LeafRank)

pheno.ML = pheno.ML %>%
  group_by(Plant.ID,year) %>%
  slice_max(LeafRank) %>%
  slice_min(Date)
pheno.ML <- as.data.frame(pheno.ML)
dim(pheno.ML); head(pheno.ML)
write.csv(pheno.ML,"Z:/Team/Citizen_Science/Phenology/Analysis/RScriptsandData/20220616_MaxLeaf.csv")

# 1C:  Fruit == Yes
pheno.F <- pheno[pheno$Ripe.Fruits == 'Yes',]; dim(pheno.F); dim(pheno)
pheno.F = pheno.F %>%
  group_by(Plant.ID,year) %>%
  slice_min(Date)
pheno.F <- as.data.frame(pheno.F); dim(pheno.F)
pheno.F
write.csv(pheno.F,"Z:/Team/Citizen_Science/Phenology/Analysis/RScriptsandData/20220616_FirstFruits.csv")

########## ########## ########## ########## 
# Note : I assume not all data was collected for each tree each year based on the dimensions of the output tables
# 26*9 = 234
# but the output table lengths are 158, 218, and 163 respectively 

########## ########## ########## ##########
# Linear Regression 

unique(pheno$Site) # hmmm... we have values: "SHRINE", "Shrine", "shrine" which is problematic but we can fix that:
pheno$Site <- toupper(pheno$Site); unique(pheno$Site)
# You'll have to populate degree day 

model.BLB <- lm(degreeday ~ year + 1|Site, data = pheno.BB,  na.action = 'na.pass')

########## ########## ########## ##########
# PART 2 : Phenology graphs etc.

