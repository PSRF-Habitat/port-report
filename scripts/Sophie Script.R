##Port of Seattle Report##
#install packages#


#####Instruments#####

#####Temperature of all selected sites 2024-2025 (SURFACE)#####


#####Temperature of all selected sites  2024-2025 (BOTTOM)#####



#####Dissolved Oxygen of all selected sites 2025 (SURFACE)#####


#####Dissolved Oxygen of all selected sited 2025 (BOTTOM)#####


#####Kelp#####
install.packages("ggplot2")
library(ggplot2)
library(readr)
Kelp <- read_csv("data/Kelp.csv")

#remove everything not needed#
Kelp_clean<-subset(Kelp, select = -c(survey_id, site_country, latitude, longitude, region, stipes, observer, date_start, date_end))
View(Kelp_clean)

#add density column#
Kelp_clean$density<-Kelp_clean$amount/Kelp_clean$distance

#####Kelp densities for all species deep vs. shallow years 2024 and 2025 stacked#####
##(how do you stack densities/error bars)##




#####Grouped kelp density for all selected sites 2024 and 2025#####




