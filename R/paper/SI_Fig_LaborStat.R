
ListV2024 %>% names()

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
    rename(region0 = region) %>%
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
    mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                             labels = c("Evolving", "Static")))
}


ListV2024 %>% purrr::pluck("LaborPrice") %>%
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  mutate(branch = scenario, scenario = ss) ->
  AgWage_32

AgLabor_32 %>%
  left_join(ListV2024 %>% purrr::pluck("LaborDemandSec") %>%
              mutate(branch = scenario, scenario = ss) %>%
              Agg_reg(region) %>% rename(labor = value), by = c("scenario", "region", "year") ) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
  group_by(branch, scenario, region, year) %>%
  summarize(value = weighted.mean(value, weight = labor),
            labor = sum(labor), .groups = "drop") ->
  AgWage

# plot AR10 ag wage rate ----
AgWage %>% filter(year >= 2015) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  filter(scenario == "Evolving") %>%
  Proc_Diff(type = "R", -year, -labor) %>%
  ggplot() + facet_wrap(~scenario) +
  geom_line(aes(x = year, y = value, color = region))


ListV2024 %>% purrr::pluck("LaborDemandSec") %>%
  mutate(branch = scenario, scenario = ss) %>%
  left_join_error_no_match(Regmapping %>% select(region, REG10_AR6), by = "region") %>%
  group_by(branch, scenario, region, year, REG10_AR6) %>%
  summarise(value = sum(value), .groups = "drop") ->
  AgLabor_32

# plot AR10 labor stat ----

driver <- readRDS("data/input/DRIVER.RDS")

eta <- driver %>% filter(var == "eta") %>% unique()

AgLabor_32 %>%
  rename(phy.labor = value) %>%
  left_join_error_no_match(eta %>% select(region, year, eta = value),
                           by = c("region", "year")) %>%
  mutate(g = eta - 1,
         eta = ifelse(grepl("eta1", scenario), 1 + (1-0.3)*g, eta),
         eta = ifelse(grepl("eta2", scenario), 1 + (1-0.2)*g, eta),
         eta = ifelse(grepl("eta3", scenario), 1 + (1-0.1)*g, eta),
         eta = ifelse(grepl("eta4", scenario), 1 + (1+0.1)*g, eta),
         eta = ifelse(grepl("eta5", scenario), 1 + (1+0.2)*g, eta),
         eta = ifelse(grepl("eta6", scenario), 1 + (1+0.3)*g, eta),
         eta = ifelse(grepl("static", scenario), 1, eta)) %>%
  mutate(eff.labor = phy.labor * eta) %>%
  left_join_error_no_match(AgWage_32 %>% select(scenario, region, year, wage = value),
                           by = c("scenario", "region", "year")) %>%
  select(-g) ->
  df.key.32

df.key.32 %>%
  group_by(scenario, REG10_AR6, year) %>%
  summarise(phy.labor = sum(phy.labor),
            eff.labor = sum(eff.labor),
            exp = sum(wage*phy.labor)) %>%
  mutate(eta = eff.labor / phy.labor,
         wage = exp / phy.labor) %>%
  select(-exp, region = REG10_AR6) ->
  df.key.AR10

df.key.32 %>%
  group_by(scenario, year) %>%
  summarise(phy.labor = sum(phy.labor),
            eff.labor = sum(eff.labor),
            exp = sum(wage*phy.labor)) %>%
  mutate(eta = eff.labor / phy.labor,
         wage = exp / phy.labor) %>%
  mutate(region = "World") %>%
  select(names(df.key.AR10)) ->
  df.key.glb


df.key.AR10 %>%
  bind_rows(df.key.glb) %>%
  filter(year >= 2015) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  filter(scenario == "Evolving") %>%
  gather(var, value, phy.labor: wage) %>%
  Proc_Diff(type = "R", -year) %>%
  ggplot() + facet_wrap(~region, ncol = 11) +
  geom_line(aes(x = year, y = value, color = var))



