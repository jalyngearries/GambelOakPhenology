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

########## Phase A: Parsing First Dates of Phenophases: Interp Trees: GCVC (10), HQ (2), SHRINE (4)

##    Data Cleaning
##    Plant.ID's
InterpPheno <- as.data.frame(Interp.2013.2022.All.Site.Analysis)

InterpPheno$Site <- toupper(InterpPheno$Site)

InterpPheno <- subset(InterpPheno, Site!="GCS" & Site!="IG" & Site!="PR")

unique(InterpPheno$Site)

dim(InterpPheno)

unique(InterpPheno$Year)

###    Cleaning phenology data
##     BLB    
unique(InterpPheno$Breaking.Leaf.Buds)
InterpPheno$Breaking.Leaf.Buds <- toupper(InterpPheno$Breaking.Leaf.Buds)
InterpPheno$Breaking.Leaf.Buds <- trimws(InterpPheno$Breaking.Leaf.Buds)

##    ML

unique(InterpPheno$What...of.canopy.is.full.)

##    F

InterpPheno$Fruits <- toupper(InterpPheno$Fruits)
unique(InterpPheno$Fruits)

##    Cleaning dates
##    Add column for year

InterpPheno$Date <- as_date(InterpPheno$Date, format = '%m/%d/%Y')
InterpPheno$Year <- year(InterpPheno$Date)

InterpPheno$Year <- replace(InterpPheno$Year, InterpPheno$Year == "2105", "2015")
InterpPheno$Year <- replace(InterpPheno$Year, InterpPheno$Year == "2107", "2017")
unique(InterpPheno$Year)
####################################

##### A-1: Breaking Leaf Buds (BLB)

##    Condense Data

InterpPheno.BLB <- data.frame(InterpPheno$Plant.ID, InterpPheno$Plant.., InterpPheno$Site, InterpPheno$Date, DOY, InterpPheno$Year, InterpPheno$Breaking.Leaf.Buds)

colnames(InterpPheno.BLB) <- c('Tree.ID', 'Plant.ID', 'Site','Date', 'DOY', 'Year', 'BLB')

dim(InterpPheno.BLB)

InterpPheno.BLB$DOY <- yday(InterpPheno.BLB$Date)

##    Find first instance for each tree in BLB phenophase

InterpPheno.BLB <- InterpPheno.BLB[InterpPheno.BLB$BLB =='YES',]
InterpPheno.BLB <- InterpPheno.BLB %>%
    group_by(Tree.ID, Year) %>%
    slice_min(Date)

dim(InterpPheno.BLB)

##    Export as .csv

write.csv(InterpPheno.BLB, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno_DOE_Interp_BLB.csv')

##    Visualize Data

ggplot(InterpPheno.BLB, aes(x = Year, y = DOY)) +
    geom_point()+
    facet_wrap(~Plant.ID)


##### A-2: Max Leaf-Out (ML)

##    Condense Data

InterpPheno.ML <- data.frame(InterpPheno$Plant.ID, InterpPheno$Plant.., InterpPheno$Site, InterpPheno$Date, DOY, InterpPheno$Year, InterpPheno$What...of.canopy.is.full.)

colnames(InterpPheno.ML) <- c('Tree.ID', 'Plant.ID', 'Site','Date', 'DOY', 'Year', 'ML')

InterpPheno.ML$DOY <- yday(InterpPheno.ML$Date)

##    Establish "ranks" for max leaf-out

InterpPheno.ML <- InterpPheno.ML %>%
    add_column(Leaf.Rank = "")

for (i in 1:length(InterpPheno.ML$Plant.ID)) {
  if (InterpPheno.ML$ML[i] == "<25%" ) {
    InterpPheno.ML$Leaf.Rank[i] = 1}
  else if (InterpPheno.ML$ML[i] == "25-49%" ) {
    InterpPheno.ML$Leaf.Rank[i] = 2}
  else if (InterpPheno.ML$ML[i] == "50-74%" ) {
    InterpPheno.ML$Leaf.Rank[i] = 3}
  else if (InterpPheno.ML$ML[i] == "75-94%" ) {
    InterpPheno.ML$Leaf.Rank[i] = 4}
  else {InterpPheno.ML$Leaf.Rank[i] = 5}}

##      Get rid of outliers - Limiting dates between May 1 - July 1 (DOY 121, 182, respectively)
##      Potentially accounts for monitoring error

InterpPheno.ML <- InterpPheno.ML %>%
  filter(DOY<182, DOY>121)

##    Find first instance for each tree in "Max Leaf" phenophase

InterpPheno.ML <- InterpPheno.ML %>%
    group_by(Tree.ID, Year) %>%
    slice_max(Leaf.Rank) %>%
    slice_min(DOY)

InterpPheno.ML$Leaf.Rank <- as.numeric(InterpPheno.ML$Leaf.Rank)
sapply(InterpPheno.ML, class)

head(InterpPheno.ML)

##    Export as .csv

write.csv(InterpPheno.ML, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno_DOE_Interp_ML.csv')

##    Visualize Data

ggplot(InterpPheno.ML, aes(x = Year, y = DOY)) +
  geom_point() +
  facet_wrap(~Tree.ID)

##### A-3: Fruits Present (F)

##    Condense Data

InterpPheno.F <- data.frame(InterpPheno$Plant.ID, InterpPheno$Plant.., InterpPheno$Site, InterpPheno$Date, DOY, InterpPheno$Year, InterpPheno$Fruits)

colnames(InterpPheno.F) <- c('Tree.ID', 'Plant.ID', 'Site','Date', 'DOY', 'Year', 'F')

InterpPheno.F$DOY <- yday(InterpPheno.F$Date)

InterpPheno$F <- toupper(InterpPheno.F$F)

unique(InterpPheno.F$F)

##    Get rid of outliers - Limiting dates between June 1-Nov 1 (DOY 152 & 305, respectively)
#Potentially accounts for monitoring error

InterpPheno.F <- InterpPheno.F %>%
  filter(DOY<305, DOY>171)

##    Find first instance for each tree in "fruiting" phenophase

InterpPheno.F <- InterpPheno.F[InterpPheno.F$F == 'YES',]

InterpPheno.F <- InterpPheno.F %>%
  group_by(Tree.ID, Year) %>%
  slice_min(Date)

InterpPheno.F <-  as.data.frame(InterpPheno.F)

dim(InterpPheno.F)

##    Export as .csv

write.csv(InterpPheno.F, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno_DOE_Interp_F.csv')

##    Visualize data

ggplot(InterpPheno.F, aes(x = Year, y = DOY)) +
  geom_point()+
  facet_wrap(~Tree.ID)
