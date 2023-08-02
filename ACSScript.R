library(tidyverse)
library(tidycensus)
library(ggmap)
library(sf)
library(raster)
library(plotly)
library(ggplotify)
library(usmap)
library(arcgisbinding)
library(rmapshaper)
library(writexl)
library(openxlsx)
library(ggthemes)

# Read in the Preferred Variable spreadsheet (See the ACSVariables Excel file for alternate sheet Names/Variable Types. Other sheets include 'RaceVariables', 'IncomeVariables', 'EducationVariables', etc.)
Variables <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/ACSProject/ACSVariables.xlsx", sheet = "HouseholdVariables")
# Select 'Code' and 'AmendedLabel'
Variables <- Variables[,c(1,2)]

# Create Codes, containing all of the preferred variable codes
Codes <- Variables$Code
# Create Labels, containing all of the amended labels
Labels <- Variables$AmendedLabel

Years <- 2021

for (i in Years){
  assign(paste0('Data_', i),
         value = get_acs(
                         # Define the geography  for ACS data, change this to 'zcta' for zip code data
                         geography = 'county',
                         variables = Codes,
                         year = i,
                         # Read data in as non-spatial. Change geometry to TRUE for spatial data (this will take a lot longer to read in)
                         geometry = FALSE,
                         # Provide the Census API Keu, if others are running this you will need to get a Census API key here: https://api.census.gov/data/key_signup.html
                         key = "6dd2c4143fc5f308c1120021fb663c15409f3757",
                         # Define the survey to pull data from, 'acs5' for 5-year, 'acs1' for 1 year
                         survey = 'acs5',
                         # Show the call made to the Census API in the console, this will help if an error is thrown
                         show_call = TRUE))
  
}

get_decennial()
Data_2021 <- Data_2021 %>%
  # Rename 'variable' to 'Code'
  rename(Code = variable) %>%
  # Join the variable spreadsheet to the ACS data by 'Code'
  left_join(Variables, by = 'Code') %>%
  # Rename the listed 'Variable' with the 'AmendedLabel' from the variable spreadsheet
  rename(Variable = AmendedLabel, Estimate = estimate) %>%
  # Drop the 'Code' column
  select(-Code)

# Reorder columns
Data_2021 <- Data_2021[, c(1,2,5,3,4)]

# Pivot the ACS data to a wide format, with columns named by variable. Each county/zip code (geography unit) will have one row with one column per variable
Data_2021 <- Data_2021 %>%
  pivot_wider(names_from = 'Variable', values_from = 'Estimate', id_cols = c('GEOID', 'NAME'))


write.xlsx(Data_2021, "C:/Users/ikennedy/Downloads/GISData/AHSCountyData.xlsx")




Data_2021 <- Data_2021 %>%
  mutate(OOSFPrime = OOAge_90+OOAge_80,
         OOSFShare = OOSFPrime/sum(OOSFPrime)) %>%
  mutate(OOSFVal_Pr = OOAge_80*MedVal_80 + OOAge_90*MedVal_90,
         OOSFVal_PrSh = OOSFVal_Pr/sum(OOSFVal_Pr, na.rm = TRUE))

Data_2021 <- Data_2021 %>%
  mutate(Agg_LOMort = (MedHHInc_M*Mort) - (MedCost_M*12*Mort),
         Agg_LONoMort = (MedHHInc_NM*NoMort) - (MedCost_NM*12*NoMort),
         LOMort = Agg_LOMort/Mort,
         LONoMort = Agg_LONoMort/NoMort,
         LO = LOMort * (Mort/OOTotal) + LONoMort * (NoMort/OOTotal),
         LOPrime = LO*OOSFPrime)

# If reading in county data, use the lines 86-110. If reading in Zip Code data, comment out lines 80-105, and use lines 115-134 (will need to uncomment)

# Read in the USA counties shapefile
Counties <- st_read("C:/Users/ikennedy/Downloads/GISData/USAShapefiles/County/cb_2018_us_county_500k.shp")

# Convert 'Counties' to a data.frame
Counties <- as.data.frame(Counties)

# Filter 'Counties' to not include territories and/or Alaska/Hawaii
Counties <- Counties %>%
  filter(STATEFP <= 56 & !STATEFP %in% c('02', '15'))
# Select GEOID, NAME, and geometry from 'Counties'
Counties <- Counties[,c(5,6,10)]

# Join the ACS Data to 'Counties' by GEOID
Counties <- Counties %>%
  left_join(Data_2021, by = 'GEOID')
# Rename the 'NAME' column and drop the duplicate listing (a duplicate was created when the data was joined together)
Counties <- Counties %>%
  rename(NAME = NAME.x) %>%
  select(-NAME.y)
# Convert 'Counties' back to a spatial data frame
Counties <- st_as_sf(Counties)

# Check to make sure there is an Active ArcGIS Installation
arc.check_product()
# Output the ACS county data to the path specified
arc.write(path = "C:/Users/ikennedy/Downloads/GISData/AHSCountyData.shp", data = Counties, overwrite = TRUE)


# If reading in county data, use the lines 86-110. If reading in Zip Code data, comment out lines 80-105, and use lines 115-134 (will need to uncomment)
# # Read in the USA zip codes shapefile
# ZipCodes <- st_read("C:/Users/ikennedy/Downloads/GISData/USAShapefiles/ZipCodes/cb_2020_us_zcta520_500k.shp")
# 
# # Convert 'ZipCodes' to a data.frame
# ZipCodes <- as.data.frame(ZipCodes)
# 
# # Select 'GEOID20' and 'geometry' from 'ZipCodes' and rename 'GEOID20' to 'GEOID'
# ZipCodes <- ZipCodes %>%
#   select(GEOID20, geometry) %>%
#   rename(GEOID = GEOID20)
# 
# # Join the ACS Data to 'ZipCodes' by GEOID
# ZipCodes <- ZipCodes %>%
#   left_join(Data_2021, by = 'GEOID')
# # Convert 'ZipCodes' back into a spatial data frame
# ZipCodes <- st_as_sf(ZipCodes)
# 
# # Check to make sure there is an Active ArcGIS Installation
# arc.check_product()
# # Output the ACS zip code data to the path specified
# arc.write(path = "C:/Users/ikennedy/Downloads/GISData/AHSZipCodeData.shp", data = ZipCodes, overwrite = TRUE)




# # Set albers_crs to hold the Albers Contiguous USA Coordinate Reference System
# albers_crs <- st_crs(5070)

# States <- st_read("C:/Users/ikennedy/Downloads/GISData/USAShapefiles/States/cb_2018_us_state_500k.shp")
# States <- States %>%
#   filter(GEOID <= 56 & !GEOID %in% c('02', '15'))
# 
# Counties <- st_transform(Counties, albers_crs)
# States <- st_transform(States, albers_crs)


# Old Mutate Code

# Data_2021 <- Data_2021 %>%
#   mutate(PropKitchenLack = KitchenLack/Kitchen_All,
#          PropPlumbingLack = PlumbingLack/Plumbing_All) %>%
#   
#   mutate(PopKitchenLack = PropKitchenLack*Kitchen_All,
#          PopPlumbingLack = PropPlumbingLack*Plumbing_All,
#          PropKitchenLack = PropKitchenLack*100,
#          PropPlumbingLack = PropPlumbingLack*100) %>%
#   
#   mutate(InctoPov_.5 = (InctoPov_.5/InctoPov)*100,
#          InctoPov_.99 = (InctoPov_.99/InctoPov)*100,
#          InctoPov_1.24 = (InctoPov_1.24/InctoPov)*100,
#          InctoPov_1.49 = (InctoPov_1.49/InctoPov)*100,
#          InctoPov_1.84 = (InctoPov_1.84/InctoPov)*100,
#          InctoPov_1.99 = (InctoPov_1.99/InctoPov)*100,
#          InctoPov_2 = (InctoPov_2/InctoPov)*100) %>%
#   
#   mutate(Prop_Dis18 = (Dis_18_1+Dis_18_2)/Pop_18 * 100,
#          Prop_Dis18_64 = (Dis_18_64_1+Dis_18_64_2)/Pop_18_64 *100,
#          Prop_Dis_65 = (Dis_65_1+Dis_65_2)/Pop_65 *100,
#          Prop_Dis_All = (Dis_18_1 + Dis_18_2 + Dis_18_64_1 + Dis_18_64_2 + Dis_65_1 + Dis_65_2)/Dis_Total * 100) %>%
#   
#   mutate(`1BR_Under1000` = (`1Br_Cash_300` + `1Br_Cash_499` + `1Br_Cash_749` + `1Br_Cash_999`)/`1Br_Cash` * 100,
#          `2BR_Under1000` = (`2Br_Cash_300` + `2Br_Cash_499` + `2Br_Cash_749` + `2Br_Cash_999`)/`2Br_Cash` * 100,
#          `3BR_Under1000` = (`3Br_Cash_300` + `3Br_Cash_499` + `3Br_Cash_749` + `3Br_Cash_999`)/`3Br_Cash` * 100)



# Exploratory Plots

# font_add("FreightBlack", regular = "C:/Users/ikennedy/OneDrive - JBREC/Freight Display Pro/FreightDispProBlack-Regular.otf",
#          italic = "C:/Users/ikennedy/OneDrive - JBREC/Freight Display Pro/FreightDispProBlack-Regular.otf")

# ggplot() +
#   geom_sf(data = Counties, aes(fill = LOPrime/1000000000), color = NA) +
#   geom_sf(data = States, color = 'white', fill = NA) +
#   #geom_sf(data = Metros, color = 'green', fill = 'transparent') +
#   scale_fill_viridis_c("% of Population", option = 'magma') +
#   ggtitle("Proportion of Population (25+) with a Graduate Degree") +
#   theme_map(base_family = 'FreightBlack') +
#   theme(title = element_text(size = 20),
#         panel.background = element_rect(fill = "transparent", color = NA),
#         legend.text = element_text(size = 12, family = 'FreightBlack')) +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, label.position = "right",
#                                label.hjust = 0.5, barwidth = 5, barheight = 15))
# 
# ggplot() +
#   geom_sf(data = Counties, aes(fill = `1BR_Under1000`), color = NA) +
#   geom_sf(data = States, color = 'white', fill = NA) +
#   scale_fill_viridis_c("% of Renters", option = 'rocket') +
#   ggtitle("Proportion of 1 BR Rents under $1,000/Month") +
#   theme_map(base_family = 'FreightBlack') +
#   theme(title = element_text(size = 20),
#         panel.background = element_rect(fill = "transparent", color = NA),
#         legend.text = element_text(size = 12, family = 'FreightBlack')) +
#   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, label.position = "right", 
#                                label.hjust = 0.5, barwidth = 5, barheight = 15))


# v21 <- load_variables(2021, "acs1", cache = TRUE)
# v21 <- v21 %>%
#   mutate(label = str_remove(label, "^Estimate!!"))
# write.xlsx(v21, "C:/Users/ikennedy/Downloads/ACS/ACSVariables.xlsx")