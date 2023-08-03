## code to prepare `ClassifiedDescriptions` dataset goes here
library(tidyverse)
library(conflicted)
library(readxl)

data <- read_csv("C:/Project-Storage/OCD/CrimeWatch_Maps_Past_90-Days.csv")

Single_Items <- data |>
  select(DESCRIPTION) |>
  distinct()

Single_Items <- data.frame(Single_Items)

# write.csv(Single_Items,
#                  "C:/Project-Storage/OCD/Crime_Descriptions.csv")

Classified_Descriptions <- read_excel("C:/Project-Storage/OCD/Classified_Descriptions.xlsx",
                                      sheet = "TOC_Result")

UCCS_Schema <- read_excel("C:/Project-Storage/OCD/Classified_Descriptions.xlsx",
              sheet = "UCCS_Schema")

Classified_Descriptions <- Classified_Descriptions |>
  select(DESCRIPTION,
         uccs_code) |>
  left_join(UCCS_Schema |>
              select(uccs_code,
                     offense_type_desc,
                     offense_category_desc),
            by = "uccs_code") |>
  select(-c(uccs_code))

ClassifiedDescriptions <- Classified_Descriptions

usethis::use_data(ClassifiedDescriptions, overwrite = TRUE)
