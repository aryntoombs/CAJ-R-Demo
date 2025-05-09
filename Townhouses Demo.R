#------------------------------------------------------------------------------#
# CAJ DEMO IN R
# Data based on Building Permits in Calgary
# https://data.calgary.ca/Business-and-Economic-Activity/Building-Permits/c2es-76ed/data_preview
#------------------------------------------------------------------------------#

library(tidyverse)
library(sf)

Residental_Building_Permits <- read_csv("data/Residental Building Permits.csv")

Townhouses <- Residental_Building_Permits %>%
  filter(str_detect(PermitClass, "1606"))

Townhouses_by_year <- Townhouses %>%
  mutate(t_year = year(AppliedDate)) %>%
  group_by(t_year) %>%
  summarise(count = n())

Townhouses_by_community <- Townhouses %>%
  #filter(AppliedDate >= as.Date("2025-01-01")) %>%
  group_by(CommunityCode, CommunityName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

calgary_comm_sf <- read_sf(dsn = "geojson/calgary_communities_2025.geojson")

Townhouses_with_spatial <- calgary_comm_sf %>%
  left_join(Townhouses_by_community, by=c("COMM_CODE" = "CommunityCode")) %>%
  filter(!is.na(count))

ggplot(Townhouses_with_spatial) +
  geom_sf(aes(fill=count))