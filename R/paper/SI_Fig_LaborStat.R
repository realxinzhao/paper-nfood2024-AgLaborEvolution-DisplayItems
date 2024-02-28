
ListV2024 %>% names()

## theme1 ----

theme1 <- theme(axis.text.x = element_text(angle = 30, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_line(linetype = 2, color = "grey80", size = 0.3),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"),
                legend.justification = "left")



# Getting data ready ----

### start with wage ----
ListV2024 %>% purrr::pluck("LaborPrice") %>%
  mutate(region = gsub("Labor_Ag", "", market)) %>%
  mutate(branch = scenario, scenario = ss) ->
  AgWage_32

AgWage_32 %>%
  left_join(ListV2024 %>% purrr::pluck("LaborDemandSec") %>%
              mutate(branch = scenario, scenario = ss) %>%
              Agg_reg(region) %>% rename(labor = value), by = c("scenario", "region", "year") ) %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
  group_by(branch, scenario, region, year) %>%
  summarize(value = weighted.mean(value, weight = labor),
            labor = sum(labor), .groups = "drop") ->
  AgWage

# c("para_ls_SSP2", "para_gamma_hi", "para_gamma_lo",
#   "para_ls_SSP5", "para_ls_SSP3",  "para_eta6", "para_eta1",  "para_static") -> ScenAll
# c("Evolving", "High Elas.", "Low Elas.",
#   "High transition", "Low transition", "High productivity", "Low productivity",  "Static") -> ScenAllLabel
#
# AgWage %>% filter(year >= 2015) %>%
#   group_by(scenario, year) %>%
#   summarize(value = weighted.mean(value, weight = labor),
#             labor = sum(labor), .groups = "drop") %>%
#   filter(scenario %in% ScenAll) %>%
#     mutate(scenario = factor(scenario, levels = ScenAll,
#                              labels = ScenAllLabel)) %>%
#   filter(grepl("trans|Evol", scenario)) %>%
#     Proc_Diff(type = "R", -year, -labor) %>%
#     ggplot() + #facet_wrap(~scenario) +
#     geom_line(aes(x = year, y = value, color = scenario))

### plot AR10 ag wage rate ----
# AgWage %>% filter(year >= 2015) %>%
#   filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
#   mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
#                            labels = c("Evolving", "Static"))) %>%
#   filter(scenario == "Evolving") %>%
#   Proc_Diff(type = "R", -year, -labor) %>%
#   ggplot() + facet_wrap(~scenario) +
#   geom_line(aes(x = year, y = value, color = region))


ListV2024 %>% purrr::pluck("LaborDemandSec") %>%
  mutate(branch = scenario, scenario = ss) %>%
  left_join_error_no_match(Regmapping %>% select(region, REG10_AR6), by = "region") %>%
  group_by(branch, scenario, region, year, REG10_AR6) %>%
  summarise(value = sum(value), .groups = "drop") ->
  AgLabor_32

# use any scenario's base year data to calculate the base year regional labor weight
AgLabor_32 %>%
  filter(year == 2015) %>%
  filter(scenario == "para_static") %>%
  group_by(scenario, year) %>%
  mutate(TL = sum(value),
         weight = value / TL) %>%
  ungroup() ->
  L.weight.base

### driver data ----

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
  left_join_error_no_match(L.weight.base %>% select(region, weight), by = c("region")) %>%
  group_by(scenario, REG10_AR6, year) %>%
  summarise(phy.labor = sum(phy.labor),
            eff.labor = sum(eff.labor),
            exp = sum(wage*phy.labor),
            eta = sum(eta*weight)) %>% # use base-year labor weights to aggregate eta (driver)
  mutate(wage = exp / phy.labor) %>%
  # mutate(eta.realized = eff.labor / phy.labor) %>% # realized aggregated labor productivity(eta)
  select(-exp, region = REG10_AR6) %>%
  ungroup() ->
  df.key.AR10

df.key.32 %>%
  left_join_error_no_match(L.weight.base %>% select(region, weight), by = c("region")) %>%
  group_by(scenario, year) %>%
  summarise(phy.labor = sum(phy.labor),
            eff.labor = sum(eff.labor),
            exp = sum(wage*phy.labor),
            eta = sum(eta*weight)) %>%
  mutate(wage = exp / phy.labor) %>%
  # mutate(eta.realized = eff.labor / phy.labor) %>%
  mutate(region = "World") %>%
  select(names(df.key.AR10)) %>%
  ungroup() ->
  df.key.glb


# plots ----

df.key.AR10 %>%
  bind_rows(df.key.glb) %>%
  filter(year >= 2015) %>%
  gather(var, value, phy.labor: wage) %>%
  mutate(var = factor(var, levels = c("phy.labor", "eta", "eff.labor", "wage"),
                      labels = c("Agricultural labor", "Labor productivity",
                                 "Effective labor", "Agricultural wage"))) ->
  Plaborstat_AllScens

Plaborstat_AllScens %>%
  saveRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("Plaborstat_AllScens", ".RDS")))


df.key.AR10 %>%
  bind_rows(df.key.glb) %>%
  filter(year >= 2015) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  #filter(scenario != "Evolving") %>%
  gather(var, value, phy.labor: wage) %>%
  mutate(var = factor(var, levels = c("phy.labor", "eta", "eff.labor", "wage"),
                      labels = c("Agricultural labor", "Labor productivity", "Effective labor", "Agricultural wage"))) %>%
  Proc_Diff(type = "R", -year) ->
  Plaborstat

Plaborstat %>% saveRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("Plaborstat", ".RDS")))

Plaborstat %>%
  ggplot() + facet_wrap(~region, nrow = 3, ncol = 5) +
  geom_hline(yintercept = 1, size = 0.4) +
  geom_line(aes(x = year, y = value, color = var, linetype = scenario, size = var, alpha = scenario)) +
  labs(x = "Year", y = "Index (2015 = 1)", color = "Variable", alpha = "Scenario",
       linetype = "Scenario", size = "Variable") +
  scale_color_brewer(palette = "Set1", direction = -1) +
  scale_size_manual(values = c(1.3, 1.3, 1, 1)) +
  scale_alpha_manual(values = c(1, 0.9)) +
  scale_linetype_manual(values = c(1, 5)) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1 + theme_leg -> pp

pp %>% Write_png(.name = "LaborStat", .DIR_MODULE = DIR_MODULE, h = 10, w = 13)

# FigS_Productivity ----

c("para_ls_SSP2", "para_gamma_hi", "para_gamma_lo",
  "para_ls_SSP5", "para_ls_SSP3",  "para_eta6", "para_eta1",  "para_static") -> ScenAll
c("Evolving", "High Elas.", "Low Elas.",
  "High transition", "Low transition", "High productivity", "Low productivity",  "Static") -> ScenAllLabel


Plaborstat_AllScens %>%
  filter(var == "Labor productivity") %>%
  filter(scenario %in% c("para_eta6", "para_ls_SSP2", "para_eta1", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_eta6", "para_ls_SSP2", "para_eta1", "para_static"),
                           labels = c("High productivity", "Evolving", "Low productivity",  "Static"))) %>%
  filter(year >= 2015) -> df

df %>% filter(region == "World", year %in% c(2015, 2050, 2100)) %>%
  mutate(year = as.character(year)) %>%
  ggplot +   facet_wrap(~scenario, nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = scenario), stat = "identity", position = "stack",
           color = "black") +
  geom_hline(yintercept = 1, linetype = 2, color = "gray") +
  geom_hline(yintercept = 3, linetype = 2, color = "gray") +
  geom_hline(yintercept = 5, linetype = 2, color = "gray") +
  labs(x = "Year", y = "Index (2015 = 1)", Color = "Scenario", fill = "Scenario") +
  scale_fill_brewer(palette = "PuOr", direction = -1) +
  scale_y_continuous(breaks = 1:6) +
  theme_bw() + theme0 +  theme1 -> A1; A1

df %>% filter(region != "World") %>%
  ggplot +   facet_wrap(~region, ncol = 5, scales = "fixed") +
  geom_hline(yintercept = 1) +
  geom_line(aes(x = year, y = value, group = interaction(region, scenario),
                color = scenario), size = 1) +
  labs(x = "Year", y = "Index (2015 = 1)", color = "Scenario") +
  scale_color_brewer(palette = "PuOr", direction = -1) +
  scale_y_continuous(breaks = 1:6) +
  theme_bw() + theme0 + theme1 -> A2


(A1 + ggtitle("(A) Global mean agricultural labor productivity growth") ) /
  (A2 + ggtitle("(B) Regional (R10) agricultural labor productivity growth")) +
  plot_layout(guides = "collect", heights = c(1,1.8)) -> pp

pp %>% Write_png(.name = "FigS_Productivity", .DIR_MODULE = DIR_MODULE, h = 12, w = 12)



