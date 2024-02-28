# Getting the data ready! ----

DIR_MODULE = "AgLabor"


ListV2024 %>% names()


REG_1 <- c("World", "AFRICA", "CHINA+", "INDIA+", "NORTH_AM")
COMMSector <- c("Energy crops", "Staple crops", "Oil crops", #"Fodder crops",
                "Other crops", "Livestock", "Forest")

c("para_ls_SSP2", "para_gamma_hi", "para_gamma_lo",
  "para_ls_SSP5", "para_ls_SSP3",  "para_eta6", "para_eta1",  "para_static") -> ScenAll
c("Evolving", "High Elas.", "Low Elas.",
  "High transition", "Low transition", "High productivity", "Low productivity",  "Static") -> ScenAllLabel

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    rename(region0 = region) %>%
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6),
                             by = "region0") %>%
    filter(scenario %in% ScenAll) %>%
    mutate(scenario = factor(scenario, levels = ScenAll,
                             labels = ScenAllLabel))
}



# Get data ready ----

## Ag capital by sector ----
"CapitalDemandSec" %>% PluckBind() %>%
  Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(
    PluckBind("CapitalDemandSec") %>%
      Agg_reg(sector, region)
  ) %>%
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM3), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM3, region) %>%
  mutate(sector = factor(sector, levels = COMMSector)) %>%
  Agg_reg(sector, region) %>%
  filter(year >= 2015) %>%
  mutate(value = value * gdp_deflator(2015, 1975))-> Pcapital

## Ag labor by sector ----
PluckBind("LaborDemandSec") %>%
  Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(
    PluckBind("LaborDemandSec") %>%
      Agg_reg(sector, region)
  ) %>%
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM3), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM3, region) %>%
  mutate(sector = factor(sector, levels = COMMSector)) %>%
  Agg_reg(sector, region)%>%
  filter(year >= 2015) -> Plabor

## Ag land by sector ----
PluckBind("Aggland") %>% filter(year >= 2015) %>%
  left_join_error_no_match(LandMapping %>% select(LandLeaf, land = LandCover3), by = "LandLeaf") %>%
  group_by(scenario, region, land, year, branch) %>%
  # to Mha
  summarise(value = sum(value)/10, .groups = "drop") %>%
  Agg_reg(land, region) %>%
  mutate(land = factor(land, levels = c(
    "Cropland - Energy", "Cropland - Staples", "Cropland - Others",
    "Forest - Managed", "Forest - Unmanaged",
    "Pasture - Managed", "Pasture - Unmanaged",
    "Other Natural", "Other Fixed" ) )) %>% filter(year >= 2015) %>%
  filter(!grepl("Rock|Urban", land)) %>%
  #mutate(land = gsub("- Staples|- Others", "- NonEnergy", land)) %>%
  mutate(land = gsub(" - Unmanaged| - Managed", "", land)) %>%
  mutate(land = gsub("Fixed", "Land", land)) %>%
  mutate(land = factor(land, levels = c(
    "Cropland - Energy", "Cropland - Staples", "Cropland - Others",
    "Forest", "Pasture", "Other Natural", "Other Land" ),
    labels = c(
      "Cropland: Energy", "Cropland: Staples", "Cropland: Others",
      "Forest", "Pasture", "Other natural", "Other land" ))) %>%
  group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") -> Pland

Pland %>% Agg_reg(land) %>% mutate(region = "World") %>%
  bind_rows(Pland) -> Pland

## Ag water by sector ----
"Water_Wsector" %>% PluckBind %>% filter(year >= 2015) %>% filter(value >0) %>%
  mutate(sector1 = if_else(sector %in% MapAgCOMM$AgCOMM, "Ag", sector)) %>%
  mutate(sector1 = if_else(grepl("elec", sector1), "Elec.", sector1)) %>%
  mutate(sector1 = if_else(grepl("Ag|Elec", sector1), sector1, "Others")) %>%
  filter(sector1 == "Ag") %>%
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, sector2 = AgCOMM3), by = "sector"
                           ) %>%
  mutate(sector2 = factor(sector2, levels = COMMSector)) %>%
  Agg_reg(region, sector = sector2) -> Pwater

Pwater %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(Pwater) -> Pwater

## Ag Fertilizer ----
"FertizerUse" %>% PluckBind %>% filter(year >= 2015) %>% filter(sector != "Exports_fertilizer") %>%
  left_join(MapAgCOMM %>% select(sector = AgCOMM, sector2 = AgCOMM3)) %>%
  mutate(sector2 = factor(sector2, levels = COMMSector)) %>%
  Agg_reg(region, sector = sector2) -> Pfertilizer
Pfertilizer %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(Pfertilizer) -> Pfertilizer


## Emissions ----

source("R/paper/Emissions.R")

## CO2 ----
pCEM %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(pCEM) -> pCEM
## nonCO2 GHG ----
pNCEM %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(pNCEM) -> pNCEM

## Ag SUA & prices----
### source AgBalElement here ----

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    # rename(region0 = region) %>%
    # left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6),
    #                          by = "region0") %>%
    filter(scenario %in% ScenAll) %>%
    mutate(scenario = factor(scenario, levels = ScenAll,
                             labels = ScenAllLabel))
}


source("R/paper/AgBalElement_Storage.R")

c(brewer.pal(n = length(ELEMAll), name = "BrBG")[1:length(ElEMSupply)],
  brewer.pal(n = length(ELEMAll), name = "BrBG")[length(ElEMSupply)+1:length(ELEMDemand)]
) -> ColUpdate


PSUA %>% Agg_reg(sector, element) %>% mutate(region = "World") %>%
  bind_rows(PSUA)  -> PSUA

PAgPrice %>%
  group_by_at(vars(scenario, year, sector)) %>%
  summarise(value = weighted.mean(value, w = prod), prod = sum(prod), .groups = "drop") %>%
  drop_na() %>% mutate(region = "World") %>%
  bind_rows(PAgPrice) %>%
  mutate(sector = factor(sector, levels = COMMSector)) ->
  PAgPrice








