
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

"LaborPrice" %>% PluckBind() %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static"))

ListV2024 %>% purrr::pluck("LaborPrice") %>%
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  mutate(branch = scenario, scenario = ss) %>%
  left_join(ListV2024 %>% purrr::pluck("LaborDemandSec") %>%
              mutate(branch = scenario, scenario = ss) %>%
              Agg_reg(region) %>% rename(labor = value), by = c("scenario", "region", "year") ) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
  group_by(branch, scenario, region, year) %>%
  summarize(value = weighted.mean(value, weight = labor),
            labor = sum(labor), .groups = "drop") ->
  AgWage

AgWage %>% filter(year >= 2015) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  filter(scenario == "Evolving") %>%
  Proc_Diff(type = "R", -year, -labor) %>%
  ggplot() + facet_wrap(~scenario) +
  geom_line(aes(x = year, y = value, color = region))



