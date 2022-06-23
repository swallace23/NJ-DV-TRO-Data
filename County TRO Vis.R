library(dplyr)
library(ggplot2)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

library(readxl)
#This is the county data on TROs from the AOG DV report
#County_Level_Data <- read_excel(County_Level_Data)
county <- data.frame(County_Level_Data)

#applying row names from excel column
library(tibble)
county <- tibble::column_to_rownames(county, "County")
#fixing name formatting
names(county) <- gsub("\\."," ",names(county))

# min max normalizing data 0-1
county_norm <- as.data.frame(lapply(county, min_max_norm))

#fixing name formatting
row.names(county_norm) <- row.names(county)
names(county_norm) <- gsub("\\."," ",names(county_norm))

#create heat map
library(superheat)
superheat(county_norm, pretty.order.rows = TRUE,
          pretty.order.cols = TRUE, scale = FALSE, title = "Temporary Restraining Orders by County", 
          title.size = 8, row.title = "Counties", row.title.size = 6)

# easy to compare TROs across counties with heatmap + does the grouping for you visually!


#creating maps
counties <- c(row.names(county))
TRO_percentage <- c(county$`New TRO Percentage`)
incidents <- c(county$`Total Number  of Incident`)

#tro percentage df
trop <- data.frame(county = c("atlantic", "bergen", "burlington",
                              "camden", "cape may", "cumberland", "essex", "gloucester", "hudson",
                              "hunterdon", "mercer", "middlesex", "monmouth", "morris",
                              "ocean", "passaic", "salem", "somerset", "sussex", "union",
                              "warren"), value = TRO_percentage)
#total number of incidents df
tni <- data.frame(county = c("atlantic", "bergen", "burlington",
                             "camden", "cape may", "cumberland", "essex", "gloucester", "hudson",
                             "hunterdon", "mercer", "middlesex", "monmouth", "morris",
                             "ocean", "passaic", "salem", "somerset", "sussex", "union",
                             "warren"), value = incidents)

#obtain region codes
library(choroplethr)
library(choroplethrMaps)
library(ggmap)
data(county.regions, 
     package = "choroplethrMaps")
region <- county.regions %>%
  filter(state.name == "new jersey")

#join values and county regions
trop_data <- inner_join(trop, region, by=c("county" = "county.name"))
tni_data <- inner_join(tni, region, by=c("county" = "county.name"))

#create maps

#TRO percentage map
#registering google maps API key
#register_google(key = 'xxx')


#creating new layer for borders 
library(tidyverse)

data(county.regions)
nj_regions = filter(county.regions, state.name == "new jersey") %>% pull(region)


data(county.map)
nj.county.map = filter(county.map, region %in% nj_regions)

#creating new layer for labels - aggregating location data basically
nj.locs <- aggregate(by = list(nj.county.map$NAME), cbind(nj.county.map$long, nj.county.map$lat), 
                     data=nj.county.map, FUN=mean)
nj.locs <- nj.locs %>%
  rename(county = Group.1) %>%
  rename(long = V1) %>%
  rename(lat = V2)

#nudging labels around (would do it in photoshop but unsure how lol)
nj.locs[7,3] = 40.79 #essex
nj.locs[16,3] = 41.09 #passaic
nj.locs[16,2] = -74.35
nj.locs[18,2] = -74.6 #somerset
nj.locs[13,3] = 40.25 #monmouth
nj.locs[9,2] = -73.95 #hudson
nj.locs[2,2] = -73.95 #bergen
nj.locs[15,3] = 39.93 #ocean
nj.locs[12,2] = -74.3 #middlesex
nj.locs[3,2] = -74.7 #burlington
nj.locs[17,2] = -75.3 #salem
nj.locs[17,3] = 39.61
nj.locs[14,3] = 40.87 #morris
nj.locs[11,3] = 40.27 #mercer

#total number of incidents map
#three layers - data, then borders, then labels.

county_choropleth(tni_data,
                  state_zoom = "new jersey",
                  reference_map = TRUE,
                  num_colors = 8) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "DV Incidents by New Jersey County",
       fill = "DV Incidents (total)") +
  geom_polygon(data = nj.county.map,
               aes(long, lat, group=group),
               color="black",
               fill=NA,
               size=0.1) +
  geom_label(data = nj.locs, aes(long,lat, label=county, fontface = 2), size=2, label.padding = unit(0.1, "lines"))


#TRO utilization map
county_choropleth(trop_data, 
                  state_zoom = "new jersey",
                  reference_map = TRUE,
                  num_colors = 8) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "TRO Utilization by County",
       fill = "New TRO Percentage") +
  geom_polygon(data = nj.county.map,
               aes(long, lat, group=group),
               color="black",
               fill=NA,
               size=0.1)


#per capita maps! - based on 2019 census bureau estimates (not using these)

# getting per capita data
library(covidcast)
data(county_census)

NJ_census <- data.frame(county_census)
NJ_census <- county_census %>%
  filter(STNAME == "New Jersey")

rm(county_census)
NJ_census <-  select(NJ_census, "REGION", "STNAME", "CTYNAME", "POPESTIMATE2019")
NJ_census <- NJ_census[-1,]

#reformatting county names, manually is easiest
tni_for_census <- data.frame(county = c("Atlantic County", "Bergen County", "Burlington County",
                                        "Camden County", "Cape May County", "Cumberland County", 
                                        "Essex County", "Gloucester County", "Hudson County",
                                        "Hunterdon County", "Mercer County", "Middlesex County", 
                                        "Monmouth County", "Morris County",
                                        "Ocean County", "Passaic County", "Salem County", 
                                        "Somerset County", 
                                        "Sussex County", "Union County",
                                        "Warren County"), TNI.INCIDENTS = incidents)

#creating per capita data frame for TNI
tni_census_data <- inner_join(NJ_census, tni_for_census, by=c("CTYNAME" = "county"))
tni_census_data <- mutate(tni_census_data, TNI.PER.CAPITA=(TNI.INCIDENTS/POPESTIMATE2019))

#adding per capita data to original mapping data frame
tni_data <- mutate(tni_data, tni.per.capita=tni_census_data$TNI.PER.CAPITA)
head(tni_data)

#renaming columns for choropleth
capita_tni_data <- tni_data %>%
  rename(total.incidents = value) %>%
  rename(value=tni.per.capita)

#Incidents per capita map
county_choropleth(capita_tni_data,
                  state_zoom = "new jersey",
                  reference_map = TRUE,
                  num_colors = 8) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "DV Incidents by New Jersey County",
       fill = "DV Incidents (per capita)") +
  geom_polygon(data = nj.county.map,
               aes(long, lat, group=group),
               color="black",
               fill=NA,
               size=0.1)


#creating per capita data for everything
county_with_pop <- county %>%
  mutate(pop2019 = NJ_census$POPESTIMATE2019)

#note - TRO percentage is not adjusted for population as it's based on incidents
county_with_pop <- county_with_pop %>%
  mutate(TNI.per.capita=`Total Number  of Incident`/pop2019) %>%
  mutate(prior.TRO.per.capita=`Prior TRO   Existed`/pop2019) %>%
  mutate(new.TRO.per.capita = `New TRO   Issued`/pop2019) %>%
  mutate(new.TROP = `New TRO Percentage`)

county_per_capita <- county_with_pop %>%
  select(TNI.per.capita, prior.TRO.per.capita, new.TRO.per.capita, new.TROP) %>%
  rename(new.TROP.per.capita = new.TROP)

#normalizing per capita data
county_per_capita_normalized <- as.data.frame(lapply(county_per_capita, min_max_norm))
#fixing row names
row.names(county_per_capita_normalized) <- row.names(county_per_capita)

#renaming for clarity
county_per_capita_normalized <- county_per_capita_normalized %>%
  rename('New TRO percentage' = `new.TROP`) %>%
  rename('Incidents per capita' = `TNI.per.capita`) %>%
  rename('Prior TRO' = `prior.TRO.per.capita`) %>%
  rename('New TRO' = `new.TRO.per.capita`)

#per capita heat map
superheat(county_per_capita_normalized, pretty.order.rows = TRUE,
          pretty.order.cols = TRUE, scale = FALSE, title = "Temporary Restraining Orders Per Capita, by County", 
          title.size = 8, row.title = "Counties", row.title.size = 6)

#per capita compact heat map (only total incidents and new TRO%)

CPCN_compact <- county_per_capita_normalized %>%
  select(`Incidents per capita`, `New TRO percentage`)

#sorting by incidents per capita
CPCN_compact <- CPCN_compact[order(CPCN_compact$`Incidents per capita`, decreasing=FALSE), ]

#creating map
superheat(CPCN_compact, pretty.order.rows=FALSE, pretty.order.cols = FALSE, scale = FALSE, title = 
            "Temporary Restraining Orders Per Capita, by County", title.size = 8, row.title = "Counties",
          row.title.size = 6)
