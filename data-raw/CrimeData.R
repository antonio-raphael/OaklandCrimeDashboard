## code to prepare `CrimeData` dataset goes here
library(tidyverse)
library(conflicted)
library(tidygeocoder)
library(lubridate)


data <- read_csv("C:/Project-Storage/OCD/CrimeWatch_Maps_Past_90-Days.csv")

data <- data |>
  select(-c(CRIMETYPE,
            CASENUMBER,
            POLICEBEAT:STATE)) |>
  mutate(Location = gsub("POINT ",
                         "",
                         Location),
         Location = gsub(")",
                         "",
                         Location,
                         fixed = TRUE),
         Location = gsub("(",
                         "",
                         Location,
                         fixed = TRUE)) |>
  separate(Location,
           into = c("Lon", "Lat"),
           sep = " ",
           remove = FALSE) |>
  select(-c(Location))

data(ClassifiedDescriptions)

data <- data |>
  left_join(ClassifiedDescriptions) |>
  select(offense_type_desc,
         offense_category_desc,
         everything()) |>
  rename(Offense_Category   = offense_type_desc,
         Offense_Generalize = offense_category_desc) |>
  mutate(DATETIME = parse_date_time(DATETIME,
                                    "%m/%d/%Y %I:%M:%S %p"))

data <- data |>
  mutate(DATEFILTER = as.character(DATETIME)) |>
  mutate(DATEFILTER = str_sub(DATEFILTER,
                              start = 1,
                              end = 10)) |>
  select(Offense_Category:DATETIME,
         DATEFILTER,
         everything()) |>
  mutate(DATEFILTER = as.Date(DATEFILTER,
                              format = "%Y-%m-%d"))

data <- data |>
  mutate(FIREARM = str_detect(DESCRIPTION, "FIREARM"),
         SHOOTING = str_detect(DESCRIPTION, "SHOOT")) |>
  mutate_at(vars(FIREARM,
                 SHOOTING),
            function(x){

              x = as.character(x)

              x = na_if(x, "FALSE")

            }) |>
  mutate(FIREARM = coalesce(FIREARM,
                            SHOOTING)) |>
  select(-c(SHOOTING)) |>
  mutate(FIREARM = dplyr::recode(FIREARM,
                                 "TRUE" = "Firearm Involved")) |>
  select(Offense_Category,
         FIREARM,
         everything()) |>
  mutate(Offense_Category = replace_na(Offense_Category, "Other")) |>
  mutate(Offense_Category = dplyr::recode(Offense_Category,
                                          "Criminal traffic" = "Criminal Traffic"))

CrimeData <- data

usethis::use_data(CrimeData, overwrite = TRUE)

