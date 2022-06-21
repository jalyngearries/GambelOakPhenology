
VegPheno <- Veg.2013.2022.Analysis

dim(VegPheno)

install.packages("lubridate")
library(dplyr)
library("lubridate")

# Format date variable in VegPheno df

VegPheno$Date <- as.Date(VegPheno$Date, format = '%m-%d-%Y')

####### First instance for each tree in different phenological states each year

## A - First date of breaking leaf buds - ERD

unique(VegPheno$Breaking.Leaf.Buds)

VegPheno.BLB <- VegPheno[VegPheno$Breaking.Leaf.Buds == '1',]

VegPheno.BLB <- VegPheno.BLB %>% 
    group_by(Plant.., Year) %>%
    slice_min(Date)

VegPheno.BLB <- as.data.frame(VegPheno.BLB)

VegPheno.BLB.F <- data.frame(VegPheno.BLB$Plant.., VegPheno.BLB$Date, VegPheno.BLB$Year)

VegPheno.BLB.F$VegPheno.BLB.Date <- yday(VegPheno.BLB.F$VegPheno.BLB.Date)

colnames(VegPheno.BLB.F) <- c('Plant.ID', 'DOY', 'Year')

write.csv(VegPheno.BLB.F, 'C:\\Users\\jalyn\\OneDrive\\Desktop\\RStudio\\Gearries Pheno\\VegPheno.BLB.F.csv')

## A-2: Visualizing ERD Dates



    
