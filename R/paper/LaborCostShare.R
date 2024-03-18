# calculate labor cost share across sectors and regions ----

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario == "para_ls_SSP2") %>%
    mutate(scenario = "Evolving")
}

# wage ----

ListV2024 %>% purrr::pluck("LaborPrice") %>%
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  mutate(branch = scenario, scenario = ss) %>%
  filter(scenario == "para_ls_SSP2") %>%
  mutate(scenario = "Evolving") %>%
  select(scenario, region, year, wage = value) ->
  AgWage_32


# labor ----

PluckBind("LaborDemandSec") %>%
  Agg_reg(sector, region) %>%
  group_by(scenario, region, sector, year) %>%
  summarise(Labor = sum(value)) ->
  AgLabor_32

# labor expenditure ----

AgLabor_32 %>%
  left_join(AgWage_32, by = c("scenario", "region", "year")) %>%
  mutate(EXP_L = wage * Labor) %>%  # billion 1975$
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM3), by = "sector") %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6),
                           by = "region0") %>%
  group_by(scenario, region, year, AgCOMM3) %>%
  summarise(Labor = sum(Labor),
            EXP_L = sum(EXP_L)) %>%
  filter(scenario == "Evolving") -> EXP_L

head(EXP_L)
dim(EXP_L)

# production ----
"Agprod" %>% PluckBind() %>%
  Agg_reg(branch, region, sector = tolower(sector), element = "Production") %>%
  bind_rows(
    "MeatProd" %>% PluckBind() %>% filter(Units == "Mt") %>%
      Agg_reg(branch, region, sector = tolower(sector), element = "Production")) %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>%
  filter(sector != "Pasture") %>%
  filter(scenario == "Evolving") %>%
  select(scenario, region, sector, sector0, year, Production = value) ->
  Q

# price ----
"Agprices" %>% PluckBind() %>%
  bind_rows("Meatprices" %>% PluckBind()) %>%
  mutate(sector = tolower(sector)) %>%
  rename(Price = value) %>% select(-Units) %>%
  filter(scenario == "Evolving") %>%
  select(scenario, region, sector, year, Price) ->
  P

Q %>% left_join_error_no_match(P %>% rename(sector0 = sector), by = c("scenario", "region", "sector0", "year")) %>%
  mutate(Revenue = Price * Production) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6),
                           by = "region0") %>%
  group_by(scenario, region, year, sector) %>%
  summarise(Revenue = sum(Revenue)) ->
  Rev

Rev %>% left_join_error_no_match(EXP_L %>% rename(sector = AgCOMM3)) %>%
  mutate(share = EXP_L / Revenue) %>%
  select(-Labor) ->
  LS_reg

LS_reg %>%
  group_by(scenario, year, sector) %>%
  summarise(Revenue = sum(Revenue),
            EXP_L = sum(EXP_L)) %>%
  mutate(share = EXP_L / Revenue,
         region = "World") %>%
  bind_rows(LS_reg) ->
  LS

# LS %>% filter(year %in% c(2015, 2025, 2100)) %>%
#   select(scenario, year, region, sector, share) %>% ungroup() %>%
#   spread(region, share) %>%
#   write.csv("AgLaborCostShare.csv")



