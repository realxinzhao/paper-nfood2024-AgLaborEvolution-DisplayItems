

## Read in GWP mapping ----
readr::read_csv("data/maps/GWP.csv") -> GWP

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
    rename(region0 = region) %>%
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
    mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                             labels = c("Evolving", "Static")))
}



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
  ) %>% filter(sector != "Non-CO2 GHG") %>% distinct(sector)
  #mutate(sector = replace(sector, sector %in% c("Building", "Industry", "Transport"), "Final energy")) %>%
  Agg_reg(sector, region) %>%
  mutate(value = value / 1000 * 44/12) %>% filter(year >= 2015) %>%
  mutate(year = as.character(year)) -> NCEM_sector_agg_reg

NCEM_sector_agg_reg %>% Agg_reg(sector) -> NCEM_sector_agg

NCEM_sector_agg %>% Agg_reg() -> NCEM_sector_agg_Total


NCEM_sector_agg_reg %>% mutate(year = as.integer(year)) %>%
  mutate(sector = replace(sector, sector %in% c("Refining", "Gas & Hydrogen", "Resource production"), "Other energy trans." ) ) %>%
  mutate(sector = replace(sector, sector %in% c("Building & Heating", "Industry", "Transport"), "Final energy" ) ) %>%
  Agg_reg(sector, region) %>%
  mutate(sector = factor(sector,
                         levels = c("Electricity", "Other energy trans.",
                                    "Final energy",
                                    "1G bio-feedstock",
                                    "2G bio-feedstock", "LULUCF") ) ) %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value / 1000) %>% ungroup() -> df

df %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(df) -> pCEM

pCEM %>%
  filter(scenario == "Evolving") %>%
  ggplot + facet_wrap(~region, scales = "free") +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_hline(yintercept = 0) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black", size = 0.4) +
  geom_line(data = pCEM %>% Agg_reg(region) %>%  mutate(ss = "Net Total"),
           aes(x = year, y = value, color = ss ), size = 1.2, linetype = 5) +
  labs(x = "Year", y = expression(paste(GtCO[2], " per year")), fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", name = "Source", direction = -1) +
  scale_color_manual(values = "red") +
  theme_bw() + theme0 + theme1  -> A1; A1



Pland %>% Agg_reg(land, region) %>%
  group_by_at(vars(-scenario, -value)) %>% mutate(value = value - value[scenario == "Static"]) %>%
  filter(scenario == "Evolving") %>% mutate(value = value / 1000) %>%
  ggplot +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Billion hectare", fill = "Land") +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_bw() + theme0 + theme1  -> A1; A1

# NonCO2 details ----

"NCEM_sector" %>% PluckBind() %>%
  filter(!GHG %in% c("CO2"))%>%
  mutate(GHG = replace(GHG, grepl("SO2_", GHG), "SO2_3")) %>%
  left_join(GWP %>% replace_na(list(AR6all = 0)),
                           by = "GHG") %>%
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
  Agg_reg(sector) %>%
  mutate(value = value / 1000 * 44/12) %>% filter(year >= 2015) ->
  NonCO2Check


NonCO2Check %>% filter(sector != "Other GHGs_UnMGMTLand") %>%
  mutate(year = as.integer(year)) %>%
  ggplot + facet_wrap(~scenario) +
  geom_hline(yintercept = 0) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black") +
  labs(x = "Year", fill = "Source",
       y = expression(paste(GtCO[2]-equivalent, " per year"))) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  scale_fill_brewer(palette = "Set1", direction = -1,
                    labels = c(expression(paste(CH[4], " Agriculture")),
                               expression(paste(CH[4], " Energy")),
                               expression(paste(CH[4], " Unmanaged Land")),
                               expression(paste(N[2], "O Agriculture")),
                               expression(paste(N[2], "O Energy")),
                               expression(paste(CH[4], " Unmanaged Land")),
                               "Other GHGs")) +
    theme_bw() + theme0 + theme1

