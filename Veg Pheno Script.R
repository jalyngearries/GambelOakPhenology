# Grand Canyon National Park
# Science & Resource Management
# R script for phenology data
# Jalyn Gearries

install.packages("lubridate")
install.packages("tidyverse")
library("dplyr")
library("lubridate")
require("lubridate")
library("tibble")
library("ggplot2")

########## Phase A: Parsing First Dates of Phenophases: Veg (ERD) Trees (1-10)

VegPheno <- as.data.frame(Veg.2013.2022.Analysis)
sapply(VegPheno, class)

## Format Dates/Cleaning

VegPheno$Date <- as_date(VegPheno$Date, format = '%m/%d/%Y')

##### A-1: Breaking Leaf Buds (BLB)

##      Condense Data 

VegPheno.BLB <- data.frame(VegPheno$Plant.., VegPheno$Date, VegPheno$Year, VegPheno$Breaking.Leaf.Buds)

colnames(VegPheno.BLB) <- c('Plant.ID', 'Date', 'Year', 'B.L.B')

unique(VegPheno.BLB$B.L.B)

##    Find first instance for each tree in BLB Phenophase

VegPheno.BLB <- VegPheno.BLB[VegPheno.BLB$B.L.B == '1',]
VegPheno.BLB <- VegPheno.BLB %>%
      group_by(Plant.ID, Year) %>%
      slice_min(Date)

##    Add DOY variable as metric of date

VegPheno.BLB.DOY <- VegPheno.BLB %>%
    add_column(DOY = "")

colnames(VegPheno.BLB.DOY) <- c('Plant.ID', 'Date', 'Year', 'BLB', 'DOY')

VegPheno.BLB.DOY$DOY <- yday(VegPheno.BLB.DOY$Date)

##    Export as csv.

file.exists('C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS')

write.csv(VegPheno.BLB.DOY, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno_DOE_Veg_BLB.csv')

##    Visualize Data

ggplot(VegPheno.BLB.DOY, aes(x = Year, y = DOY)) +
    geom_point()+
    facet_wrap(~Plant.ID)

##### A-2: Max Leaf-Out (ML)

##    Condense Data

VegPheno.ML <- data.frame(VegPheno$Plant.., VegPheno$Date, VegPheno$Year, VegPheno$Leaves., VegPheno$What...of.canopy.is.full.)

colnames(VegPheno.ML) <- c('Plant.ID', 'Date', 'Year', 'Leaves?', 'Canopy.Density')

unique(VegPheno.ML$Canopy.Density)

##    Add DOY as a metric of date

VegPheno.ML <- VegPheno.ML %>%
  add_column(DOY = "", .before = "Year")

VegPheno.ML$DOY <- yday(VegPheno.ML$Date)

head(VegPheno.ML)

##    Establish "Ranks" For Max Leaf-out

sapply(VegPheno.ML, class)

VegPheno.ML <- VegPheno.ML %>%
      add_column(Leaf.Rank = "")

for (i in 1:length(VegPheno.ML$Plant.ID)) {
  if (VegPheno.ML$Canopy.Density[i] == "<25%" ) {
    VegPheno.ML$Leaf.Rank[i] = 1}
  else if (VegPheno.ML$Canopy.Density[i] == "25-49%" ) {
    VegPheno.ML$Leaf.Rank[i] = 2}
  else if (VegPheno.ML$Canopy.Density[i] == "50-74%" ) {
    VegPheno.ML$Leaf.Rank[i] = 3}
  else if (VegPheno.ML$Canopy.Density[i] == "75-94%" ) {
    VegPheno.ML$Leaf.Rank[i] = 4}
  else{
    VegPheno.ML$Leaf.Rank[i] = 5}}

sapply(VegPheno.ML, class)

##      Get rid of outliers - Limiting dates between May 1 - July 1 (DOY 121, 182, respectively)
##      Potentially accounts for monitoring error

VegPheno.ML <- VegPheno.ML %>%
  filter(DOY<182, DOY>121)

##    Find first instance for each tree in "Max Leaf" phenophase

VegPheno.ML <- VegPheno.ML%>%
    group_by(Plant.ID, Year) %>%
    slice_max(Leaf.Rank) %>%
    slice_min(DOY)

##    A bit of clean-up

VegPheno.ML$Leaf.Rank <- as.numeric(VegPheno.ML$Leaf.Rank)

VegPheno.ML <- subset(VegPheno.ML, select = c(1:6))

head(VegPheno.ML)

##    Export as .csv

write.csv(VegPheno.ML, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno_DOE_Veg_ML.csv')

##    Visualize Data

ggplot(VegPheno.ML, aes(x = Year, y = DOY)) +
  geom_point()+
  facet_wrap(~Plant.ID)

##### A-3: Fruits Present (F)

##    Condense Data

VegPheno.F <- data.frame(VegPheno$Plant.., VegPheno$Date, VegPheno$Year, VegPheno$Fruits, DOY)

colnames(VegPheno.F) <- c('Plant.ID', 'Date', 'Year', 'Fruit.Y', 'DOY')

##    Add DOY as metric of date (added DOY column above)

VegPheno.F$DOY <- yday(VegPheno.F$Date)

##    Get rid of outliers - Limiting dates between June 1-Nov 1 (DOY 152 & 305, respectively)
      #Potentially accounts for monitoring error

yday("2022-6-20")

VegPheno.F <- VegPheno.F %>%
  filter(DOY<305, DOY>171)

##    Find first instance for each tree in "Fruiting" phenophase

VegPheno.F <- VegPheno.F[VegPheno.F$Fruit.Y =='1',]

VegPheno.F <- VegPheno.F %>%
    group_by(Plant.ID, Year) %>%
    slice_min(Date)

VegPheno.F <- as.data.frame(VegPheno.F); dim(VegPheno.F)

##    Export as .csv

write.csv(VegPheno.F, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno_DOE_Veg_F.csv')

##    Visualize data

ggplot(VegPheno.F, aes(x = Year, y = DOY)) +
  geom_point()+
  facet_wrap(~Plant.ID)
