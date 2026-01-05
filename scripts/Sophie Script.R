##Port of Seattle Report##
#install packages#
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
install.packages("scales")
install.packages("lubridate")
install.packages("stringr")
install.packages("ggpattern")
library("tidyverse")
library("ggplot2")
library("readr")
library("dplyr")
library("scales")
library("lubridate")
library("stringr")
library("ggpattern")

#####Instruments#####
kelp_index_combined_sensor_data_2025_12_17 <- read_csv("~/Desktop/PSRF-R Work/kelp_index_combined_sensor_data_2025-12-17.csv")
View(kelp_index_combined_sensor_data_2025_12_17)
#####Temperature of all selected sites 2024-2025 (SURFACE)#####
all_site_temp<-subset(kelp_index_combined_sensor_data_2025_12_17, select = c(site, position, datetime, temp_logger_id, tidbit_temp_c, ph_temp_c, wl_temp_c))
View(all_site_temp)

all_site_temp_surface<-all_site_temp %>%
  filter(position %in% c("surface"))%>%
  filter(site %in% c("centennialpark", "wingpoint", "edmonds", "jeffersonhead"))

View(all_site_temp_surface)

##daily averages#
daily_temp_surface <- all_site_temp_surface %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(site, date) %>%
  summarise(
    daily_avg_temp_sur = mean(tidbit_temp_c, na.rm = TRUE),
    .groups = "drop")

View(daily_temp_surface)

daily_temp_surface<- daily_temp_surface %>%
  filter(date>=as.Date("2024-09-18"),
         date<=as.Date("2025-12-30"))

####Plot surface temps##

ggplot() +
  geom_line(
    data = subset(daily_temp_surface, site != "centennialpark"),
    aes(x = date, y = daily_avg_temp_sur, color = site),
    size = 1
  ) +
  geom_line(
    data = subset(daily_temp_surface, site == "centennialpark"),
    aes(x = date, y = daily_avg_temp_sur, color = site),
    size = 1.3   # optional: slightly thicker
  ) +
  labs(
    title = "Daily Average Surface Temperature by Site",
    x = "Date",
    y = "Temperature (°C)",
    color = "Site"
  ) +
  scale_color_manual(
    values = c(
      "edmonds" = "#6BAED6",
      "wingpoint" = "#74C476",
      "centennialpark" = "#FEC44F",
      "jeffersonhead" = "#C994C7"
    ),
    labels = c(
      "centennialpark" = "Centennial Park",
      "jeffersonhead" = "Jefferson Head",
      "edmonds" = "Edmonds",
      "wingpoint" = "Wing Point"
    )
  )


#####Temperature of all selected sites  2024-2025 (BOTTOM)#####
all_site_temp<-subset(kelp_index_combined_sensor_data_2025_12_17, select = c(site, position, datetime, temp_logger_id, tidbit_temp_c, ph_temp_c, wl_temp_c))
#View(all_site_temp)

all_site_temp_bottom<-all_site_temp %>%
  filter(position %in% c("bottom"))%>%
  filter(site %in% c("centennialpark", "wingpoint", "edmonds", "jeffersonhead"))

#View(all_site_temp_bottom)

daily_temp_bottom <- all_site_temp_bottom %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(site, date) %>%
  summarise(
    ph_mean = mean(ph_temp_c, na.rm = TRUE),
    do_mean = mean(wl_temp_c, na.rm = TRUE),
    daily_avg_temp = mean(c(ph_mean, do_mean), na.rm = TRUE),
    .groups = "drop"
  )
View(daily_temp_bottom)

daily_temp_bottom<- daily_temp_bottom %>%
  filter(date>=as.Date("2024-09-18"),
         date<=as.Date("2025-12-30"))




# Plot with custom colors#
ggplot() +
  geom_line(
    data = subset(daily_temp_bottom, site != "centennialpark"),
    aes(x = date, y = daily_avg_temp, color = site),
    size = 1
  ) +
  geom_line(
    data = subset(daily_temp_bottom, site == "centennialpark"),
    aes(x = date, y = daily_avg_temp, color = site),
    size = 1.3   # optional: slightly thicker
  ) +
  labs(
    title = "Daily Average Bottom Temperature by Site",
    x = "Date",
    y = "Temperature (°C)",
    color = "Site"
  ) +
  scale_color_manual(
    values = c(
      "edmonds" = "#08519C",
      "wingpoint" = "#006D2C",
      "centennialpark" = "#EC7014",
      "jeffersonhead" = "#810F7C"
    ),
    labels = c(
      "centennialpark" = "Centennial Park",
      "jeffersonhead" = "Jefferson Head",
      "edmonds" = "Edmonds",
      "wingpoint" = "Wing Point"))

 


#####Dissolved Oxygen of all selected sites 2025 (SURFACE)#####
all_site_DO<-subset(kelp_index_combined_sensor_data_2025_12_17, select = c(site, position, datetime, do_logger_id, do_conc_mg_per_L))

all_site_do_surface<-all_site_DO %>%
  filter(position %in% c("surface"))%>%
  filter(site %in% c("centennialpark", "wingpoint", "edmonds", "jeffersonhead"))

daily_do_surface <- all_site_do_surface %>%
  filter(do_conc_mg_per_L>=0 | is.na(do_conc_mg_per_L))%>%
  mutate(date = as.Date(datetime)) %>%
  group_by(site, date) %>%
  summarise(
    daily_avg_do_sur = mean(do_conc_mg_per_L, na.rm = TRUE),
    .groups = "drop")

View(daily_do_surface)

daily_do_surface<- daily_do_surface %>%
  filter(date>=as.Date("2024-12-13"),
         date<=as.Date("2025-12-30"))
#plot surface do#
ggplot() +
  geom_line(
    data = subset(daily_do_surface, site != "centennialpark"),
    aes(x = date, y = daily_avg_do_sur, color = site),
    size = 1
  ) +
  geom_line(
    data = subset(daily_do_surface, site == "centennialpark"),
    aes(x = date, y = daily_avg_do_sur, color = site),
    size = 1.3   # optional: slightly thicker
  ) +
  labs(
    title = "Daily Average Surface Dissolved Oxygen by Site",
    x = "Date",
    y = "Temperature (°C)",
    color = "Site"
  ) +
  scale_color_manual(
    values = c(
      "edmonds" = "#6BAED6",
      "wingpoint" = "#74C476",
      "centennialpark" = "#FEC44F",
      "jeffersonhead" = "#C994C7"
    ),
    labels = c(
      "centennialpark" = "Centennial Park",
      "jeffersonhead" = "Jefferson Head",
      "edmonds" = "Edmonds",
      "wingpoint" = "Wing Point"
    )
  )

##plot bottom do##
ggplot() +
  geom_line(
    data = subset(daily_do_bottom, site != "centennialpark"),
    aes(x = date, y = daily_avg_do, color = site),
    size = 1
  ) +
  geom_line(
    data = subset(daily_do_bottom, site == "centennialpark"),
    aes(x = date, y = daily_avg_do, color = site),
    size = 1.3   # optional: slightly thicker
  ) +
  labs(
    title = "Daily Average Bottom Dissolved Oxygen by Site",
    x = "Date",
    y = "Temperature (°C)",
    color = "Site"
  ) +
  scale_color_manual(
    values = c(
      "edmonds" = "#08519C",
      "wingpoint" = "#006D2C",
      "centennialpark" = "#EC7014",
      "jeffersonhead" = "#810F7C"
    ),
    labels = c(
      "centennialpark" = "Centennial Park",
      "jeffersonhead" = "Jefferson Head",
      "edmonds" = "Edmonds",
      "wingpoint" = "Wing Point"
    )
  )
#####Dissolved Oxygen of all selected sited 2025 (BOTTOM)#####
all_site_do_bottom<-all_site_DO %>%
  filter(position %in% c("bottom"))%>%
  filter(site %in% c("centennialpark", "wingpoint", "edmonds", "jeffersonhead"))

daily_do_bottom <- all_site_do_bottom %>%
  filter(do_conc_mg_per_L>=0 | is.na(do_conc_mg_per_L))%>%
  mutate(date = as.Date(datetime)) %>%
  group_by(site, date) %>%
  summarise(
    daily_avg_do = mean(do_conc_mg_per_L, na.rm = TRUE),
    .groups = "drop")

daily_do_bottom<- daily_do_bottom %>%
  filter(date>=as.Date("2024-12-13"),
         date<=as.Date("2025-12-30"))

#####Kelp#####
Kelp <- read_csv("data/Kelp.csv")

##remove everything not needed##
#remove variables#
Kelp_clean<-subset(Kelp, select = -c(survey_id, site_country, latitude, longitude, region, stipes, observer, date_start, date_end))

#select date range#
Kelp_clean$start_at<- as.Date(Kelp_clean$start_at, format = "%m/%d/%Y" )
Kelp_clean<- Kelp_clean %>%
  filter(start_at>=as.Date("2024-01-01"),
         start_at<=as.Date("2025-12-30"))
Kelp_clean <- Kelp_clean %>% 
  mutate(Year = year(start_at))

#select sites#
Kelp_clean<-Kelp_clean %>%
  filter(site_name %in% c("Magnolia","Grain Terminal","Freshwater Bay", "Blake Island South","Seattle Waterfront","Wing Point"))

View(Kelp_clean)

#mutating transect numbers to deep or shallow
Kelp_clean <- Kelp_clean %>% 
  mutate(depth_strata = ifelse(transect %in% c(1, 2, 3), "Deep",
                               ifelse(transect %in% c(4,5,6), "Shallow", NA)))

#changing Grain Terminal site name to Centennial Park in Site column
Kelp_clean <- Kelp_clean %>% 
  mutate(site_name = ifelse(site_name == "Grain Terminal", "Centennial Park", site_name))




#####Kelp densities for all species deep vs. shallow years 2024 and 2025 stacked#####
##(how do you stack densities/error bars)##

#filtering out the years for comparison
yearly_comparison_kelp_CP <- Kelp_clean %>% 
  filter(site_name == "Centennial Park",
         Year %in% c(2024, 2025))

yearly_comparison_kelp_CP<- yearly_comparison_kelp_CP %>%
  mutate(total_amount_distance = amount*(30/distance))%>%
  mutate(density = (total_amount_distance/60))

#creating the full table with all the missing pieces 
all_species_kelp <- unique(yearly_comparison_kelp_CP$species)
all_years_kelp <- unique(yearly_comparison_kelp_CP$Year)

transect_grid <- expand.grid(
  species = all_species_kelp,
  Year = all_years_kelp,
  depth_strata = c("Deep", "Shallow"),
  transect = 1:6)

#filtering transect numbers by depth
transect_grid <- transect_grid %>% 
  mutate(
    transect = case_when(
      depth_strata == "Deep" & transect %in% 1:3 ~ transect,
      depth_strata == "Shallow" & transect %in% 4:6 ~ transect,
      TRUE ~ NA_real_)) %>% 
  filter(!is.na(transect))




#merging the data with the "missing data" <- fill in the blanks so all transects have all species, even if ZERO were observed for a transect
yearly_comparison_kelp_CP <- transect_grid %>% 
  left_join(yearly_comparison_kelp_CP, by = c("species", "Year", "depth_strata", "transect")) %>% 
  mutate(amount = ifelse(is.na(amount), 0, amount))%>%
         mutate(distance = ifelse(is.na(distance), 0, distance))%>%
                mutate(total_amount_distance = ifelse(is.na(total_amount_distance), 0, total_amount_distance))%>%
  mutate(density = ifelse(is.na(density), 0, density))

#calculating SD and Density (per depth strata, hence the 180)
yearly_comparison_kelp_CP <- yearly_comparison_kelp_CP %>%    
  group_by(species, depth_strata, Year) %>% 
  summarise(
    total_amount = sum(total_amount_distance),
    total_density = sum(density) / 3,
    SD = sd(density))
##Plot##
ggplot(yearly_comparison_kelp_CP, 
       aes(x=species, y=total_density, 
           fill=factor(Year), 
           pattern = depth_strata)) + 
  geom_bar_pattern(
    stat= "identity",
    position = position_dodge(width = 0.9),  # MK trying a different dodge function
    color = "black",
    width = .9,
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.02,
    pattern_angle = 45) +
 geom_errorbar(
    aes(ymin = pmax(total_density-SD,0), ymax =total_density+SD, 
     #   group = interaction(Year, depth_strata)    # MK kind of thinking this is optional and possibly throwing things off
        ),
    position = position_dodge(width = 0.9), # MK trying a different dodge function
    width = 0.2) +
  scale_fill_manual(values=c("2024" = "chartreuse4", "2025" = "chartreuse2")) +
  scale_pattern_manual(values = c("Deep" = "stripe", "Shallow" = "none")) +
  ylab(bquote(Density (m^-2))) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color="black",
                                    fill=NA,
                                    size = 1),
        legend.position = c(0.98,0.03),
        legend.justification = c(1,0),
        legend.background = element_rect(fill = "white", color = NA),
        legend.margin = margin(4,4,4,4),
        # Increase legend text and key size
        legend.text = element_text(size = 10),          # Larger legend text
        legend.title = element_text(size = 11),         # Larger legend title
        legend.key.size = unit(1, "lines"),           # Larger legend keys
        legend.key.height = unit(0.8, "lines"),         # Adjust the height of legend keys (if necessary)
        legend.key.width = unit(1, "lines"),
        axis.title.x = element_text(size = 12),         # Larger x-axis label
        axis.title.y = element_text(size = 12),         # Larger y-axis label
        axis.text.x = element_text(size = 9),  # Larger x-axis tick labels
        axis.text.y = element_text(lineheight = 0.8, size = 10)) +       # Larger y-axis tick labels)
  labs(x="Species", y = expression(Density~(m^2))) +
  guides(fill = guide_legend(title = "Year", order = 1,
                             override.aes = (list(
                               pattern = "none",
                               pattern_Density = 0,
                               pattern_fill = NA,
                               pattern_Spacing = 0,
                               pattern_angle = 0,
                               color = "black"))),
         pattern = guide_legend(title = "Depth Strata", order = 2,
                                override.aes = list(
                                  fill = "white",
                                  color = "black")))+
  coord_flip()

  
  
  
  
  
  
#geom_errorbarh()#####Grouped kelp density for all selected sites 2024 and 2025#####
#creating the full table with all the missing pieces 
yearly_comparison <- Kelp_clean %>% 
  filter(Year %in% c(2024, 2025))

yearly_comparison<- yearly_comparison %>%
  mutate(total_amount_distance = amount*(30/distance))%>%
  mutate(density = (total_amount_distance/60))

all_sites_kelp_all<- unique(yearly_comparison$site_name)
all_species_kelp_all <- unique(yearly_comparison$species)
all_years_kelp_all <- unique(yearly_comparison$Year)

transect_grid_all <- expand.grid(
  site_name = all_sites_kelp_all,
  species = all_species_kelp_all,
  Year = all_years_kelp_all,
  depth_strata = c("Deep", "Shallow"),
  transect = 1:6)

#filtering transect numbers by depth
transect_grid_all <- transect_grid_all %>% 
  mutate(
    transect = case_when(
      depth_strata == "Deep" & transect %in% 1:3 ~ transect,
      depth_strata == "Shallow" & transect %in% 4:6 ~ transect,
      TRUE ~ NA_real_)) %>% 
  filter(!is.na(transect))

View(transect_grid_all)
View(yearly_comparison)

#merging the data with the "mising data" <- fill in the blanks so all transects have all species, even if ZERO were observed for a transect
yearly_comparison_all <- transect_grid_all %>% 
  left_join(yearly_comparison, by = c("site_name","species", "Year", "depth_strata", "transect")) %>% 
  mutate(amount = ifelse(is.na(amount), 0, amount))%>%
  mutate(distance = ifelse(is.na(distance), 0, distance))%>%
  mutate(total_amount_distance = ifelse(is.na(total_amount_distance), 0, total_amount_distance))%>%
  mutate(density = ifelse(is.na(density), 0, density))

View(yearly_comparison_all)

#filtering grouped species
yearly_comparison_all <- yearly_comparison_all %>% 
 mutate(species_group = case_when(
   species %in% c ("Bull Kelp") ~ "Canopy",
   species %in% c ("Woody Kelp", "Torn Kelp", "Broad-Ribbed Kelp") ~ "Stipitate",
   species %in% c ("Winged Kelp", "Three-Ribbed Kelp", "Sugar Kelp", "Sieve Kelp", "Five-Ribbed Kelp") ~ "Prostrate",
   TRUE ~ "Other"))

#calculating SD and Density (per depth strata, hence the 180)
yearly_comparison_all <- yearly_comparison_all %>%    
  group_by(site_name, species_group, depth_strata, Year) %>% 
  summarise(
    total_amount = sum(total_amount_distance),
    total_density = sum(density) / 3,
    SD = sd(density))

##plots##
AllSite_Kelp<- ggplot(yearly_comparison_all, 
       aes(x=species_group, y=total_density, 
           fill=factor(Year), 
           pattern = depth_strata)) + 
  geom_bar_pattern(
    stat= "identity",
    position = position_dodge(width = .9),
    color = "black",
    width = .7,
    pattern_fill = "black",
    pattern_density = 0.2,
    pattern_spacing = 0.02,
    pattern_angle = 45) +
  geom_errorbar(
    aes(ymin = pmax(total_density-SD,0), ymax =total_density+SD, 
       #group = interaction(depth_strata, Year)
       ),
    position = position_dodge(width=.9),
   width = 0.2) +  
  scale_fill_manual(values=c("2024" = "chartreuse4", "2025" = "chartreuse2")) +
  scale_pattern_manual(values = c("Deep" = "stripe", "Shallow" = "none")) +
  ylab(bquote(Density (m^-2))) +
  facet_wrap(~ site_name, ncol = 3)+
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color="gray"),
        panel.grid.minor.x = element_blank(),
        panel.border = element_rect(color="black",
                                    fill=NA,
                                    size = 1),
       #legend.position = c(0.98,0.03),
        #legend.justification = c(1,0),
        #legend.background = element_rect(fill = "white", color = NA),
        #legend.margin = margin(4,4,4,4),
        # Increase legend text and key size
        #legend.text = element_text(size = 10),          # Larger legend text
        #legend.title = element_text(size = 11),         # Larger legend title
        #legend.key.size = unit(1, "lines"),           # Larger legend keys
        #legend.key.height = unit(0.8, "lines"),         # Adjust the height of legend keys (if necessary)
        #legend.key.width = unit(1, "lines"),
        axis.title.x = element_text(size = 12),         # Larger x-axis label
        axis.title.y = element_text(size = 12),         # Larger y-axis label
        axis.text.x = element_text(size = 9),  # Larger x-axis tick labels
        axis.text.y = element_text(lineheight = 0.8, size = 10)) +       # Larger y-axis tick labels)
  labs(x="Species", y = expression(Density~(m^2))) +
  guides(fill = guide_legend(title = "Year", order = 1,
                             override.aes = (list(
                               pattern = "none",
                               pattern_Density = 0,
                               pattern_fill = NA,
                               pattern_Spacing = 0,
                               pattern_angle = 0,
                               color = "black"))),
         pattern = guide_legend(title = "Depth Strata", order = 2,
                                override.aes = list(
                                  fill = "white",
                                  color = "black")))+
  coord_flip()
 

##
ggsave('C:/2025/port-report/figures/ KelpAllSites.jpg', 
       width = 6, height = 4, dpi = 500, scale = 1.75)
