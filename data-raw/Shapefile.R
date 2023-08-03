## code to prepare `Shapefile` dataset goes here
library(tidyverse)
library(conflicted)
library(tigris)
library(tidycensus)

zcta_data <- get_acs(
  geography = "zcta",
  variables = c(totPop18 = "B01001_001")
)

zcta_data <- zcta_data |>
  select(NAME,
         estimate) |>
  rename(ZCTA5CE20 = NAME,
         Population = estimate) |>
  mutate(ZCTA5CE20 = trimws(ZCTA5CE20,
                            which = "left",
                            whitespace = "ZCTA5 ")) |>
  mutate(ZCTA5CE20 = as.character(ZCTA5CE20))

AllZips <- zctas()

AllZips <- AllZips |>
  dplyr::filter(ZCTA5CE20 %in% c("94501",
                                 "94502",
                                 "94601",
                                 "94602",
                                 "94603",
                                 "94604",
                                 "94605",
                                 "94606",
                                 "94607",
                                 "94608",
                                 "94609",
                                 "94610",
                                 "94611",
                                 "94612",
                                 "94613",
                                 "94614",
                                 "94615",
                                 "94617",
                                 "94618",
                                 "94619",
                                 "94620",
                                 "94621",
                                 "94622",
                                 "94623",
                                 "94624",
                                 "94649",
                                 "94659",
                                 "94660",
                                 "94661",
                                 "94662",
                                 "94666",
                                 "94704",
                                 "94705")) |>
  select(-c(GEOID20:INTPTLON20)) |>
  mutate(ZCTA5CE20 = as.character(ZCTA5CE20)) |>
  left_join(zcta_data)

Shapefile <- AllZips

usethis::use_data(Shapefile, overwrite = TRUE)
