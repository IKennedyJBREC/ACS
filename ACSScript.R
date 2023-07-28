library(tidyverse)
library(tidycensus)
library(ggmap)
library(sf)
library(raster)
library(plotly)
library(ggplotify)
library(usmap)
library(arcgisbinding)
library(writexl)
library(openxlsx)
library(extrafontdb)
library(extrafont)

# v21 <- load_variables(2021, "acs5", cache = TRUE)
# v21 <- v21 %>%
#   mutate(label = str_remove(label, "^Estimate!!"))
# 
# write.xlsx(v21, "C:/Users/ikennedy/Downloads/ACS/ACSVariables.xlsx")


Variables <- read.xlsx("C:/Users/ikennedy/JBREC/BP research public use microdata coding and data - General/ACSProject/ACSVariables.xlsx", sheet = 2)
Variables <- Variables[,c(1,2)]
Codes <- Variables$Code
Labels <- Variables$AmendedLabel


Years <- 2021



# All Variables
# Variables <- read.xlsx("C:/Users/ikennedy/Downloads/ACS/ACSVariables.xlsx", sheet = 1)
# Variables <- Variables$name
# Income and Race
for (i in Years){
  assign(paste0('Data_', i),
         value = get_acs(geography = 'county',
                         variables = Codes,
                         year = 2021,
                         geometry = FALSE,
                         key = "6dd2c4143fc5f308c1120021fb663c15409f3757",
                         survey = 'acs5',
                         show_call = TRUE))
  
}


Data_2021 <- Data_2021 %>%
  rename(Code = variable) %>%
  left_join(Variables, by = 'Code') %>%
  rename(Variable = AmendedLabel, Estimate = estimate) %>%
  select(-Code)
Data_2021 <- Data_2021[, c(1,2,5,3,4)]

Data_2021 <- Data_2021 %>%
  pivot_wider(names_from = 'Variable', values_from = 'Estimate', id_cols = c('GEOID', 'NAME'))

Data_2021 <- Data_2021 %>%
  mutate(PropKitchenLack = KitchenLack/Kitchen_All,
         PropPlumbingLack = PlumbingLack/Plumbing_All) %>%
  
  mutate(PopKitchenLack = PropKitchenLack*Kitchen_All,
         PopPlumbingLack = PropPlumbingLack*Plumbing_All,
         PropKitchenLack = PropKitchenLack*100,
         PropPlumbingLack = PropPlumbingLack*100) %>%
  
  mutate(InctoPov_.5 = (InctoPov_.5/InctoPov)*100,
         InctoPov_.99 = (InctoPov_.99/InctoPov)*100,
         InctoPov_1.24 = (InctoPov_1.24/InctoPov)*100,
         InctoPov_1.49 = (InctoPov_1.49/InctoPov)*100,
         InctoPov_1.84 = (InctoPov_1.84/InctoPov)*100,
         InctoPov_1.99 = (InctoPov_1.99/InctoPov)*100,
         InctoPov_2 = (InctoPov_2/InctoPov)*100) %>%
  
  mutate(Prop_Dis18 = (Dis_18_1+Dis_18_2)/Pop_18 * 100,
         Prop_Dis18_64 = (Dis_18_64_1+Dis_18_64_2)/Pop_18_64 *100,
         Prop_Dis_65 = (Dis_65_1+Dis_65_2)/Pop_65 *100,
         Prop_Dis_All = (Dis_18_1 + Dis_18_2 + Dis_18_64_1 + Dis_18_64_2 + Dis_65_1 + Dis_65_2)/Dis_Total * 100) %>%
  
  mutate(`1BR_Under1000` = (`1Br_Cash_300` + `1Br_Cash_499` + `1Br_Cash_749` + `1Br_Cash_999`)/`1Br_Cash` * 100,
         `2BR_Under1000` = (`2Br_Cash_300` + `2Br_Cash_499` + `2Br_Cash_749` + `2Br_Cash_999`)/`2Br_Cash` * 100,
         `3BR_Under1000` = (`3Br_Cash_300` + `3Br_Cash_499` + `3Br_Cash_749` + `3Br_Cash_999`)/`3Br_Cash` * 100)
  

Counties <- st_read("C:/Users/ikennedy/Downloads/GISData/USAShapefiles/County/cb_2018_us_county_500k.shp")

Counties <- as.data.frame(Counties)
Counties <- Counties %>%
  filter(STATEFP <= 56 & !STATEFP %in% c('02', '15'))
Counties <- Counties[,c(5,6,10)]


Counties <- Counties %>%
  left_join(Data_2021, by = 'GEOID')
Counties <- Counties %>%
  rename(NAME = NAME.x) %>%
  select(-NAME.y)
Counties <- st_as_sf(Counties)


arc.check_product()
arc.write(path = "C:/Users/ikennedy/Downloads/GISData/AHSCountyData.shp", data = Counties, overwrite = TRUE)


States <- st_read("C:/Users/ikennedy/Downloads/GISData/USAShapefiles/States/cb_2018_us_state_500k.shp")

States <- States %>%
  filter(GEOID <= 56 & !GEOID %in% c('02', '15'))

Metros <- st_read("C:/Users/ikennedy/Downloads/GISData/USAShapefiles/Metros/cb_2018_us_cbsa_500k.shp")
MetrosPop <- read.csv("C:/Users/ikennedy/Downloads/GISData/MetroPop.csv")

MetrosPop <- MetrosPop %>%
  select(CBSA, POPESTIMATE2022) %>%
  mutate(GEOID = as.character(CBSA)) %>%
  select(-CBSA)

Metros <- Metros %>%
  select(GEOID, NAME) %>%
  left_join(MetrosPop, by = 'GEOID') %>%
  rename(Pop22 = POPESTIMATE2022) %>%
  filter(Pop22 >= 2000000)
rm(MetrosPop)

font_add("FreightBlack", regular = "C:/Users/ikennedy/OneDrive - JBREC/Freight Display Pro/FreightDispProBlack-Regular.otf", 
         italic = "C:/Users/ikennedy/OneDrive - JBREC/Freight Display Pro/FreightDispProBlack-Regular.otf")

albers_crs <- st_crs(5070)
Counties <- st_transform(Counties, albers_crs)
States <- st_transform(States, albers_crs)
Metros <- st_transform(Metros, albers_crs)
ggplot() +
  geom_sf(data = Counties, aes(fill = (Bachelors+Graduate)/Pop_Education * 100), color = NA) +
  geom_sf(data = States, color = 'white', fill = NA) +
  geom_sf(data = Metros, color = 'green', fill = 'transparent') +
  scale_fill_viridis_c("% of Population", option = 'magma') +
  ggtitle("Proportion of Population (25+) with a Graduate Degree") +
  theme_map(base_family = 'FreightBlack') +
  theme(title = element_text(size = 20),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 12, family = 'FreightBlack')) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, label.position = "right", 
                               label.hjust = 0.5, barwidth = 5, barheight = 15))

ggplot() +
  geom_sf(data = Counties, aes(fill = `1BR_Under1000`), color = NA) +
  geom_sf(data = States, color = 'white', fill = NA) +
  scale_fill_viridis_c("% of Renters", option = 'rocket') +
  ggtitle("Proportion of 1 BR Rents under $1,000/Month") +
  theme_map(base_family = 'FreightBlack') +
  theme(title = element_text(size = 20),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 12, family = 'FreightBlack')) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5, label.position = "right", 
                               label.hjust = 0.5, barwidth = 5, barheight = 15))




arc.check_product()
arc.write(path = "C:/Users/ikennedy/Downloads/GISData/AHSCountyData.shp", data = Counties, overwrite = TRUE)