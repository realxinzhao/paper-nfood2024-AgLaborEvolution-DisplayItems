

## Read in GWP mapping ----
readr::read_csv("data/maps/GWP.csv") -> GWP


# calculate the imbal between CEM and NCEM_sector, which is resource prod emissions
# for both CO2 and nonCO2
"NCEM" %>% PluckBind() %>%
  Agg_reg(region, GHG, Units) %>%
  rename(CEM_reg = value) %>%
  left_join("NCEM_sector" %>% PluckBind() %>% Agg_reg(region, GHG, Units),
            by = c("scenario", "region", "GHG", "Units", "year") ) %>%
  mutate(value = CEM_reg - value)  %>% select(-CEM_reg) %>%
  mutate(sector = "Resource production") %>%
  ## bind NCEM_sector
  bind_rows("NCEM_sector" %>% PluckBind() ) %>%
  ## bind CLUC
  bind_rows(
    "CLUC" %>% PluckBind() %>% Agg_reg(region, Units) %>% mutate(sector = "LULUCF", GHG = "CO2")
  ) %>%
  Agg_reg(region, GHG, Units, sector) -> NCEM_All



# carbon and map sectors ----
NCEM_All %>% filter(GHG == "CO2") %>%
  mutate(sector = case_when(
    grepl("trn_|LDV", sector) ~ "Transport",
    grepl("elec_|backup", sector) ~ "Electricity",
    grepl("resid |comm ", sector) ~ "Building & Heating",
    grepl("refining|unconventional", sector) ~ "Refining",
    grepl("H2|gas", sector) ~ "Gas & Hydrogen",
    grepl("district heat", sector) ~ "Building & Heating",
    grepl("biomassOil|corn|sugar", sector) ~ "1G bio-feedstock",
    grepl("regional biomass", sector) ~ "2G bio-feedstock",
    grepl("cement|industrial|fertilizer|water|alumina|chemical|construction|mining|iron|agricultural", sector) ~ "Industry",
    grepl("N2O|CH4|Other GHGs", sector) ~ "Non-CO2 GHG",
    TRUE ~ sector)
  ) %>% filter(sector != "Non-CO2 GHG") %>%
  #mutate(sector = replace(sector, sector %in% c("Building", "Industry", "Transport"), "Final energy")) %>%
  Agg_reg(sector, region) %>%
  mutate(value = value / 1000 * 44/12) %>% filter(year >= 2015) %>%
  mutate(year = as.character(year)) -> NCEM_sector_agg_reg


NCEM_sector_agg_reg %>% mutate(year = as.integer(year)) %>%
  mutate(sector = replace(sector, sector %in% c("Refining", "Gas & Hydrogen", "Resource production"), "Other energy trans." ) ) %>%
  mutate(sector = replace(sector, sector %in% c("Building & Heating", "Industry", "Transport"), "Final energy" ) ) %>%
  Agg_reg(sector, region) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Other energy trans.",
                                    "Final energy",
                                    "1G bio-feedstock",
                                    "2G bio-feedstock", "LULUCF") ) ) -> pCEM



# NonCO2 details ----

NCEM_All %>%
  filter(!GHG %in% c("CO2"))%>%
  mutate(GHG = replace(GHG, grepl("SO2_", GHG), "SO2_3")) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)), by = "GHG") %>%
  mutate(GHG1 = if_else(GHG1 == "Other GHGs", GHG1, GHG)) %>%
  mutate(GHG1 = if_else(sector == "UnmanagedLand", paste0(GHG1, "_UnMGMTLand"), GHG1)) %>%
  group_by(scenario, region, sector = GHG1, year) %>%
  summarise(GHGnc_AR6 = sum(value * AR6all)/1000,
            GHGnc_AR5 = sum(value * AR5all)/1000,
            GHGnc_AR4 = sum(value * AR4all)/1000, .groups = "drop") %>%
  select(-GHGnc_AR5) %>% rename(value = GHGnc_AR6) %>%
  mutate(value = value * 1000 * 12 / 44) %>% # convert to MTC
  mutate(sector = case_when(
    sector %in% c("CH4_AGR", "CH4_AWB") ~ "CH4_Ag",
    sector %in% c("CH4") ~ "CH4_En",
    sector %in% c("N2O_AGR", "N2O_AWB") ~ "N2O_Ag",
    sector %in% c("N2O") ~ "N2O_En",
    TRUE ~ sector)
  ) %>%
  Agg_reg(sector, region) %>%
  mutate(value = value / 1000 * 44/12) %>%
  filter(year >= 2015, sector != "Other GHGs_UnMGMTLand") %>%
  mutate(year = as.integer(year)) ->
  pNCEM


