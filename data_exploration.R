library(tidyverse)
housing <- read_csv("Melbourne_housing_FULL.csv")
glimpse(housing)

housing_clean <- housing |>
  filter(!is.na(Price), !is.na(Regionname)) |>
  mutate(
    Type = factor(Type,
                  levels = c("h", "t", "u"),
                  labels = c("House", "Townhouse", "Unit")),
    Method = case_when(
      Method %in% c("S", "SP", "SA", "SS", "PN", "SN") ~ "Sold",
      Method == "PI" ~ "Passed In",
      Method == "VB" ~ "Vendor Bid",
      Method == "W"  ~ "Withdrawn"
    ),
    Method = factor(Method),
    Regionname = factor(Regionname),
    Date = dmy(Date),
    SaleYear = year(Date)
  ) |>
  select(Suburb, Type, Method, Regionname, CouncilArea, Date, SaleYear,
         Price, Rooms, Bathroom, Car, Distance, Landsize, BuildingArea,
         YearBuilt, Lattitude, Longtitude)
