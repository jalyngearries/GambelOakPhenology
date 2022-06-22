library("ggplot2")
library("dplyr")
library("lubridate")
library("tidyverse")
library("rlang")

# Grand Canyon National Park
# Science & Resource Management
# R script for phenology data
# Jalyn Gearries

########  Initial cleaning

VegPheno.BLB <- VegPheno.BLB.DOY

######## Grouping Important Dates by Cluster

# Breaking Leaf Buds:  ERD.A.BLB ; ERD.B.BLB ; SHRINE.BLB ; HQ.BLB ; GCVC.A.BLB ; GCVC.B.BLB

ERD.A.BLB <- as.data.frame(filter(VegPheno.BLB, Plant.ID < 7))
ERD.B.BLB <- as.data.frame(filter(VegPheno.BLB, Plant.ID > 6))
GCVC.A.BLB <- as.data.frame(filter(InterpPheno.BLB, Plant.ID < 7, Site == "GCVC"))
GCVC.B.BLB <- as.data.frame(filter(InterpPheno.BLB, Plant.ID > 6, Site == "GCVC"))
SHRINE.BLB <- as.data.frame(filter(InterpPheno.BLB, Site == "SHRINE"))
HQ.BLB <- as.data.frame(filter(InterpPheno.BLB, Site == "HQ"))



# Max Leaf-Out: ERD.A.ML ; ERD.B.ML ; SHRINE.ML ; HQ.ML ; GCVC.A.ML ; GCVC.B.ML

ERD.A.ML <- as.data.frame(filter(VegPheno.ML, Plant.ID < 7))
ERD.B.ML <- as.data.frame(filter(VegPheno.ML, Plant.ID > 6))
GCVC.A.ML <- as.data.frame(filter(InterpPheno.ML, Plant.ID <7, Site == "GCVC"))
GCVC.B.ML <- as.data.frame(filter(InterpPheno.ML, Plant.ID > 6, Site == "GCVC"))
SHRINE.ML <- as.data.frame(filter(InterpPheno.ML, Site == "SHRINE"))
HQ.ML <- as.data.frame(filter(InterpPheno.ML, Site == "HQ"))

                      

# Fruiting: ERD.A.F ; ERD.B.F ; SHRINE.F ; HQ.F ; GCVC.A.F ; GCVC.B.F

ERD.A.F <- as.data.frame(filter(VegPheno.F, Plant.ID < 7))
ERD.B.F <- as.data.frame(filter(VegPheno.F, Plant.ID > 6))
GCVC.A.F <- as.data.frame(filter(InterpPheno.F, Plant.ID <7, Site == "GCVC"))
GCVC.B.F <- as.data.frame(filter(InterpPheno.F, Plant.ID > 6, Site == "GCVC"))
SHRINE.F <- as.data.frame(filter(InterpPheno.F, Site == "SHRINE"))
HQ.F <- as.data.frame(filter(InterpPheno.F, Site == "HQ"))

######## Calculating Average DOY for each year for each of the 18 cluster-phases (^)

library(tibble)

##### BLB

ERD.A.BLB <- ERD.A.BLB %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

ERD.A.BLB <- ERD.A.BLB %>%
  add_column(ID = "ERD.A", .before = "Year")
##

ERD.B.BLB <- ERD.B.BLB %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

ERD.B.BLB <- ERD.B.BLB %>%
  add_column(ID = "ERD.B", .before = "Year")

##
GCVC.A.BLB <- GCVC.A.BLB %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

GCVC.A.BLB <- GCVC.A.BLB %>%
  add_column(ID = "GCVC.A", .before = "Year")


##
GCVC.B.BLB <- GCVC.B.BLB %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

GCVC.B.BLB <- GCVC.B.BLB %>%
  add_column(ID = "GCVC.B", .before = "Year")

GCVC.B.BLB

##
SHRINE.BLB <- SHRINE.BLB %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

SHRINE.BLB <- SHRINE.BLB %>%
  add_column(ID = "SHRINE", .before = "Year")

SHRINE.BLB
##
HQ.BLB <- HQ.BLB %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

HQ.BLB <- HQ.BLB %>%
  add_column(ID = "HQ", .before = "Year")


##### ML

ERD.A.ML <- ERD.A.ML %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

ERD.A.ML <- ERD.A.ML %>%
  add_column(ID = "ERD.A", .before = "Year")
##

ERD.B.ML <- ERD.B.ML %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

ERD.B.ML <- ERD.B.ML %>%
  add_column(ID = "ERD.B", .before = "Year")

##
GCVC.A.ML <- GCVC.A.ML %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

GCVC.A.ML <- GCVC.A.ML %>%
  add_column(ID = "GCVC.A", .before = "Year")


##
GCVC.B.ML <- GCVC.B.ML %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

GCVC.B.ML <- GCVC.B.ML %>%
  add_column(ID = "GCVC.B", .before = "Year")

GCVC.B.ML

##
SHRINE.ML <- SHRINE.ML %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

SHRINE.ML <- SHRINE.ML %>%
  add_column(ID = "SHRINE", .before = "Year")

SHRINE.ML
##
HQ.ML <- HQ.ML %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

HQ.ML <- HQ.ML %>%
  add_column(ID = "HQ", .before = "Year")

##### F

ERD.A.F <- ERD.A.F %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

ERD.A.F <- ERD.A.F %>%
  add_column(ID = "ERD.A", .before = "Year")
##

ERD.B.F <- ERD.B.F %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

ERD.B.F <- ERD.B.F %>%
  add_column(ID = "ERD.B", .before = "Year")

##
GCVC.A.F <- GCVC.A.F %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

GCVC.A.F <- GCVC.A.F %>%
  add_column(ID = "GCVC.A", .before = "Year")


##
GCVC.B.F <- GCVC.B.F %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

GCVC.B.F <- GCVC.B.F %>%
  add_column(ID = "GCVC.B", .before = "Year")

GCVC.B.F

##
SHRINE.F <- SHRINE.F %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

SHRINE.F <- SHRINE.F %>%
  add_column(ID = "SHRINE", .before = "Year")

SHRINE.F
##
HQ.F <- HQ.F %>%
  group_by(Year) %>%
  summarise(meanDOE = mean(DOY))

HQ.F <- HQ.F %>%
  add_column(ID = "HQ", .before = "Year")

########## Organizing/Condensing/Exporting

#### Combine all clusters (HQ, SHRINE, ERD.A, ERD.B, GCVC.A, GCVC.B) into phases (BLB, ML, F)

BLB.Mean.DOE.AllSites <- as.data.frame(rbind(SHRINE.BLB , HQ.BLB , ERD.A.BLB , ERD.B.BLB , GCVC.A.BLB , GCVC.B.BLB))

dim(BLB.Mean.DOE.AllSites)

ML.Mean.DOE.AllSites <- as.data.frame(rbind(SHRINE.ML, HQ.ML, ERD.A.ML, ERD.B.ML, GCVC.A.ML, GCVC.B.ML))

dim(ML.Mean.DOE.AllSites)

F.Mean.DOE.AllSites <- as.data.frame(rbind(SHRINE.F, HQ.F, ERD.A.F, ERD.B.F, GCVC.A.F, GCVC.B.F))

dim(F.Mean.DOE.AllSites)

#### Exporting

write.csv(BLB.Mean.DOE.AllSites, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno.DOE.AllSites.BLB.csv')
write.csv(ML.Mean.DOE.AllSites, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno.DOE.AllSites.ML.csv')
write.csv(F.Mean.DOE.AllSites, 'C:\\Users\\jgearries\\Desktop\\Gearries Pheno Project\\G ANALYSIS\\Pheno.DOE.AllSites.F.csv') 

########## Visualizing
library("ggplot2")
#### BLB

sapply(BLB.Mean.DOE.AllSites, class)

BLB.Mean.DOE.AllSites <- BLB.Mean.DOE.AllSites %>%
    group_by(ID)

# Every cluster = own plot

ggplot(BLB.Mean.DOE.AllSites, aes(x = Year, y = meanDOE, group = ID)) +
  geom_line()+
  facet_wrap(~ID)

# All on one plot

ggplot(BLB.Mean.DOE.AllSites, aes(x = Year , y = meanDOE, color = ID, group = ID)) +
  geom_point(size = 2)+
  geom_line()

#### ML

ML.Mean.DOE.AllSites <- ML.Mean.DOE.AllSites %>%
  group_by(ID)

# Every cluster = own plot

ggplot(ML.Mean.DOE.AllSites, aes(x = Year, y = meanDOE, group = ID)) +
  geom_line ()+
  facet_wrap(~ID)

# All on one plot

ggplot(ML.Mean.DOE.AllSites, aes(x = Year, y = meanDOE, color = ID, group = ID)) +
  geom_point(size=2) +
  geom_line()

#### F

F.Mean.DOE.AllSites <- F.Mean.DOE.AllSites %>%
  group_by(ID)

# Every cluster = own plot

ggplot(F.Mean.DOE.AllSites, aes(x = Year, y = meanDOE, group = 6)) + 
  geom_line()+
  facet_wrap(~ID)

# All on one plot

ggplot(F.Mean.DOE.AllSites, aes(x = Year, y = meanDOE, color = ID, group = ID)) +
  geom_point(size = 2) +
  geom_line()
  