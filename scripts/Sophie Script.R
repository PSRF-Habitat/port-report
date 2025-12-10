##Port of Seattle Report##
#install packages#


#####Instruments#####

#####Temperature of all selected sites 2024-2025 (SURFACE)#####


#####Temperature of all selected sites  2024-2025 (BOTTOM)#####



#####Dissolved Oxygen of all selected sites 2025 (SURFACE)#####


#####Dissolved Oxygen of all selected sited 2025 (BOTTOM)#####


#####Kelp#####
install.packages("dplyr")
library(dplyr)
library(readr)
Kelp <- read_csv("data/Kelp.csv")

##remove everything not needed##
#remove variables#
Kelp_clean<-subset(Kelp, select = -c(survey_id, site_country, latitude, longitude, region, stipes, observer, date_start, date_end))

#select date range#
Kelp_clean$start_at<- as.Date(Kelp_clean$start_at, format = "%m/%d/%Y" )
Kelp_clean<- Kelp_clean %>%
  filter(start_at>=as.Date("2024-01-01"),
         start_at<=as.Date("2025-12-30"))
#select sites#
Kelp_clean<-Kelp_clean %>%
  filter(site_name %in% c("Magnolia","Grain Terminal","Freshwater Bay","Jefferson Head","Blake Island South","Seattle Waterfront","Wing Point"))

View(Kelp_clean)

#add density column#
Kelp_clean$density<-Kelp_clean$amount/Kelp_clean$distance

#####Kelp densities for all species deep vs. shallow years 2024 and 2025 stacked#####
##(how do you stack densities/error bars)##




#####Grouped kelp density for all selected sites 2024 and 2025#####




