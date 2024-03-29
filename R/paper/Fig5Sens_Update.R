# Sensitivity

# The variable SensYear is an input



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

# Get data ready ----



c("para_ls_SSP2", "para_gamma_hi", "para_gamma_lo",
  "para_ls_SSP5", "para_ls_SSP3",  "para_eta6", "para_eta1",  "para_static") -> ScenAll
c("Evolving", "High Elas.", "Low Elas.",
  "High transition", "Low transition", "High productivity", "Low productivity",  "Static") -> ScenAllLabel

driver <- readRDS("data/input/DRIVER.RDS")
driver %>%
  filter(var %in% c("LF")) %>%
  rename(region0 = region) %>% as_tibble() %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6),
                           by = "region0") %>% mutate(scenario = "ss") %>%
  Agg_reg(region) -> LF
LF %>% Agg_reg() %>% mutate(region = "World") %>%
  bind_rows(LF) %>% select(-scenario) %>% rename(LF = value) -> LF

readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("Plaborstat_AllScens", ".RDS"))) %>%
  filter(var == "Agricultural labor") %>%
  left_join_error_no_match(LF, by = c("region", "year")) %>%
  mutate(value = value / LF, var = "Labor share") %>%
  select(-LF) -> AgLaborShare

Plaborstat_AllScens <-
  readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("Plaborstat_AllScens", ".RDS"))) %>%
  Proc_Diff(type = "R", -year) %>%
  bind_rows(
    AgLaborShare
  ) %>%
  filter(scenario %in% ScenAll) %>%
  mutate(scenario = factor(scenario, levels = ScenAll, labels = ScenAllLabel)) %>%
  rename(sector = var)


Plot_Sens <- function(.df,
                      .ScenRM = c("Static0"),
                      .Year = SensYear,
                      .Type = "R",
                      .Unit = "Index (Evolving = 1)",
                      .ReturnData = F){

  .df %>%
    filter(year == .Year) %>%
    group_by_at(vars(-value, -scenario)) -> .df1

  if (.Type == "R") {
    .df1 %>% mutate(value = value / value[scenario == "Evolving"]) -> .df2
    .yintercept = 1
  } else  if (.Type == "A") {
    .df1 %>%
      mutate(value = value - value[scenario == "Evolving"]) -> .df2
    .yintercept = 0
  }

  .df2 %>%
    filter(!scenario %in% c("Evolving")) %>% ungroup() %>%
    mutate(region0 = if_else(region %in% c("AFRICA", "CHINA+", "NORTH_AM", "World"), region, "Others")) %>%
    mutate(region0 = factor(region0, levels = c("AFRICA", "CHINA+", "NORTH_AM", "Others", "World")))  %>%
    mutate(scen0 = factor(scenario, levels = ScenAllLabel,
                          labels = c("Evolving", "Elasticity", "Elasticity", "Transition", "Transition", "Productivity", "Productivity", "Static") )) %>%
    mutate(HighLow = factor(scenario, levels = ScenAllLabel,
                            labels = c("Evolving", "High", "Low",  "High", "Low", "High", "Low", "Static") )) %>%
    mutate(scen0 = factor(scen0, levels = c("Productivity", "Transition", "Elasticity", "Static"))) ->
    .df3

  if (.ReturnData == T) {
    return(.df3)
  }

  .df3 %>% filter(region0 != "World") %>%
    filter(!scen0 %in% .ScenRM) %>%
    ggplot() +
    facet_wrap(~sector, scales = "fixed", nrow = 1) +
    geom_hline(yintercept = .yintercept, size = 0.6) +

    geom_boxplot(aes(x = scen0, y = value, group = interaction(HighLow, scen0), alpha = HighLow),
                 varwidth = TRUE,
                 fill = "orange", outlier.shape = NA,
                 position = position_dodge2(padding = 0.2#,#preserve = "single", #width = 0.5
                 ) ) +

    geom_point(data = .df3 %>%
                 filter(!scen0 %in% .ScenRM),
               aes(x = scen0, y = value, group = interaction(HighLow, scen0),
                   fill = region0, shape = region0, size = region0), varwidth = TRUE,
               color = "black", alpha = 0.8,
               position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.75)) +
    labs(x = "Sensitivity scenario", y = .Unit,
         color = "Region", shape = "Region",
         fill = "Region", size = "Region",
         alpha = "Scenario") +
    scale_alpha_manual(values = c(0.6, 0.2, 0) ) +
    scale_fill_manual(values = c(brewer.pal(4, "Set1")[2:4], "grey60", "red")) +
    scale_shape_manual(values = c(22:25, 21)) +
    scale_size_manual(values = c(2.5, 2.5, 2.5, 1.5, 3)) +
    theme_bw() + theme0 + theme1

}

# At 32 region impacts on effective labor vs. physical labor should be the
# same in transition scenario, but not after regional aggregation
Plaborstat_AllScens %>%
  filter(!(sector == "Agricultural labor" & scenario == "Static")) %>%
  filter(grepl("labor", sector)) %>%
  filter(scenario %in% c("Evolving", "Low transition")) %>%
  group_by_at(vars(-value, -scenario)) %>%
  mutate(value = value / value[scenario == "Evolving"]) %>%
  spread(sector, value)

# labor metrics ----



# Main Labor and markets----

Plaborstat_AllScens %>%
  filter(!(sector == "Agricultural labor" & scenario == "Static")) %>%
  filter(grepl("labor", sector)) %>%
  Plot_Sens(.Type = "R") +
  geom_rect(data = Plaborstat_AllScens %>% Plot_Sens(.Type = "R", .ReturnData = T) %>%
              filter(scen0 == "Static", sector == "Agricultural labor"),
            aes(xmin = which(levels(as.factor(scen0))=="Static") +0.3,
                xmax= which(levels(as.factor(scen0))=="Static") -0.3 ,
                ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  scale_y_continuous(breaks = seq(0.8, 1.2, 0.1), limits = c(0.73, 1.28), expand = c(0,0)) +
  ggtitle("(A) Physical and effective agricultural labor") ->
  p1;p1



Plabor %>% filter(!grepl("Oil|Other|Forest|Energy", sector)) %>%
  Plot_Sens(.ScenRM = "Static") +
  scale_y_continuous(breaks = seq(0.7, 1.3, 0.1), limits = c(0.65, 1.36), expand = c(0,0)) +
  ggtitle("(B) Agricultural labor input by key sectors") ->
  p2;p2

PSUA %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>%
  filter(element == "Supply: Production") %>% Agg_reg(element, region, sector) %>%
  filter(grepl("Staple|Livestock", sector)) %>%
  mutate(sector = factor(sector, levels = c("Staple crops", "Livestock"))) %>%
  Plot_Sens(.Type = "R") +
  scale_y_continuous(breaks = seq(0.7, 1.3, 0.1), limits = c(0.65, 1.36), expand = c(0,0)) +
  ggtitle("(C) Agricultural production by key sectors") -> p5;p5

cowplot::get_legend(p1) -> pleg


((p1 + theme(legend.position = "none", axis.title.x = element_blank())) +
    pleg + plot_layout(widths = c(1,0.17))) /
(  (p2 + theme(legend.position = "none") + theme(axis.title.x = element_blank()))|
  (p5 + theme(legend.position = "none") + theme(axis.title.x = element_blank())) ) +
  patchwork::plot_layout(guides = "collect") -> pp1

Plaborstat_AllScens %>%
  filter(grepl("wage", sector)) %>%
  Plot_Sens(.Type = "R") +
  scale_y_continuous(breaks = seq(0.5, 1.8, 0.1), limits = c(0.45, 1.85), expand = c(0,0)) +
  ggtitle("(D) Wage") ->
  p3;p3

PAgPrice %>% select(-prod) %>%
  filter(grepl("Staple|Livestock", sector)) %>%
  Plot_Sens() +
  scale_y_continuous(breaks = seq(0.5, 1.8, 0.1), limits = c(0.85, 1.55), expand = c(0,0)) +
  ggtitle("(E) Agricultural prices") ->
  p4;p4
(p3 + theme(legend.position = "none")) +
  (p4 + theme(legend.position = "none")) +
  patchwork::plot_layout(widths = c(1, 2)) -> pp2


pp1/pp2 +
  patchwork::plot_layout(guides = "collect") -> pp3

#pp3 %>% Write_png(.name = paste0("Sens_LaborMarket_", SensYear), .DIR_MODULE = DIR_MODULE, h = 14, w = 13)


# Main envir ----
Pland %>% rename(sector = land) %>% filter(!grepl("Other land", sector)) %>%
  Plot_Sens(.Type = "A", .Unit = "Million hectare") +
  facet_wrap(~sector, nrow = 2, scales = "free_y") +
  ggtitle("(A) Land use and land cover") -> p_sens_land


Pfertilizer %>% Agg_reg(region) %>% mutate(sector = "Nitorgen fertilizer") %>%
  Plot_Sens(.Type = "A", .Unit = "Million tonne N") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(B) Nitrogen fertilizer") ->
  p_sens_fert

Pwater %>% Agg_reg(region) %>% #filter(region != "World") %>%
  mutate(sector = "Water withdrawal") %>%
  Plot_Sens(.Type = "A", .Unit = "Billion cubic meter") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(C) Agricultural water withdrawal") ->
  p_sens_water


pCEM %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() ->
  pCEM1

pNCEM %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() ->
  pNCEM1

pNCEM1 %>% filter(grepl("Ag", sector)) %>%
  bind_rows(
    pCEM1 %>% filter(grepl("LULUCF", sector))
  ) %>%
  mutate(sector = factor(sector, levels = c("CH4_Ag", "N2O_Ag", "LULUCF"),
                         labels =
                           c(expression(paste(CH[4], " Agriculture")),
                             expression(paste(N[2], "O Agriculture")),
                             expression(paste(CO[2], " LULUCF")))) ) %>%
  Plot_Sens(.Type = "A", .Unit = expression(paste(GtCO[2]-eq))) +
  facet_wrap(~sector, scales = "free_y", nrow = 1,
             labeller = label_parsed) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(D) Cumulative agricultural and land use change emissions") -> p_sens_ems


cowplot::get_legend(p_sens_land) -> pleg

p_sens_fert + p_sens_water + patchwork::plot_spacer() + pleg + plot_layout(widths = c(1, 1,0.1, 0.35)) -> pp2

(p_sens_land + theme(legend.position = "none", axis.title.x = element_blank())) /
  pp2 /
  p_sens_ems +
  patchwork::plot_layout(heights = c(2.5, 1, 1)) -> pp

#pp %>% Write_png(.name = paste0("Sens_LandEnvir_", SensYear), .DIR_MODULE = DIR_MODULE, h = 14, w = 13)




# updates ----


Plaborstat_AllScens %>%
  filter(!(sector == "Agricultural labor" & scenario == "Static")) %>%
  filter(grepl("labor", sector)) %>%
  Plot_Sens(.Type = "R") +
  geom_rect(data = Plaborstat_AllScens %>% Plot_Sens(.Type = "R", .ReturnData = T) %>%
              filter(scen0 == "Static", sector == "Agricultural labor"),
            aes(xmin = which(levels(as.factor(scen0))=="Static") +0.3,
                xmax= which(levels(as.factor(scen0))=="Static") -0.3 ,
                ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  scale_y_continuous(breaks = seq(0.8, 1.2, 0.1), limits = c(0.73, 1.28), expand = c(0,0)) +
  ggtitle("(A) Physical and effective agricultural labor") ->
  p1;p1


cowplot::get_legend(p1) -> pleg



Plaborstat_AllScens %>%
  filter(grepl("wage", sector)) %>%
  Plot_Sens(.Type = "R") +
  scale_y_continuous(breaks = seq(0.5, 1.8, 0.1), limits = c(0.45, 1.85), expand = c(0,0)) +
  ggtitle("(B) Wage") ->
  p3;p3

PAgPrice %>% select(-prod) %>%
  filter(grepl("Staple|Livestock", sector)) %>%
  Plot_Sens() +
  scale_y_continuous(breaks = seq(0.5, 1.8, 0.1), limits = c(0.85, 1.55), expand = c(0,0)) +
  ggtitle("(C) Agricultural prices") ->
  p4;p4

(p3 + theme(legend.position = "none")) +
  (p4 + theme(legend.position = "none")) +
  patchwork::plot_layout(widths = c(1, 2)) -> pp2


pCEM %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() ->
  pCEM1

pNCEM %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() ->
  pNCEM1

pNCEM1 %>% filter(grepl("Ag", sector)) %>%
  bind_rows(
    pCEM1 %>% filter(grepl("LULUCF", sector))
  ) %>%
  mutate(sector = factor(sector, levels = c("CH4_Ag", "N2O_Ag", "LULUCF"),
                         labels =
                           c(expression(paste(CH[4], " Agriculture")),
                             expression(paste(N[2], "O Agriculture")),
                             expression(paste(CO[2], " LULUCF")))) ) %>%
  Plot_Sens(.Type = "A", .Unit = expression(paste(GtCO[2]-eq))) +
  facet_wrap(~sector, scales = "free_y", nrow = 1,
             labeller = label_parsed) +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(D) Cumulative agricultural and land use change emissions") -> p_sens_ems



((p1 + theme(legend.position = "none", axis.title.x = element_blank())) +
    pleg + plot_layout(widths = c(1,0.17))) /
  pp2 /
  p_sens_ems +
  patchwork::plot_layout(guides = "collect") -> ppp

ppp %>% Write_png(.name = paste0("Sens_Fig5_", SensYear), .DIR_MODULE = DIR_MODULE, h = 14, w = 13)



# update 2 ----

Plabor %>% filter(!grepl("Oil|Other|Forest|Energy", sector)) %>%
  Plot_Sens(.ScenRM = "Static") +
  scale_y_continuous(breaks = seq(0.7, 1.3, 0.1), limits = c(0.65, 1.36), expand = c(0,0)) +
  ggtitle("(A) Agricultural labor input by key sectors") ->
  p2;p2

PSUA %>%
  rename(sector0 = sector) %>%
  left_join_error_no_match(
    MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
    by = "sector0") %>%
  filter(element == "Supply: Production") %>% Agg_reg(element, region, sector) %>%
  filter(grepl("Staple|Livestock", sector)) %>%
  mutate(sector = factor(sector, levels = c("Staple crops", "Livestock"))) %>%
  Plot_Sens(.Type = "R") +
  scale_y_continuous(breaks = seq(0.7, 1.3, 0.1), limits = c(0.65, 1.36), expand = c(0,0)) +
  ggtitle("(B) Agricultural production by key sectors") -> p5;p5

Pland %>% rename(sector = land) %>% filter(!grepl("Other land", sector)) %>%
  Plot_Sens(.Type = "A", .Unit = "Million hectare") +
  facet_wrap(~sector, nrow = 2, scales = "free_y") +
  ggtitle("(C) Land use and land cover") -> p_sens_land


Pfertilizer %>% Agg_reg(region) %>% mutate(sector = "Nitorgen fertilizer") %>%
  Plot_Sens(.Type = "A", .Unit = "Million tonne N") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(D) Nitrogen fertilizer") ->
  p_sens_fert

Pwater %>% Agg_reg(region) %>% #filter(region != "World") %>%
  mutate(sector = "Water withdrawal") %>%
  Plot_Sens(.Type = "A", .Unit = "Billion cubic meter") +
  theme(legend.position = "none", axis.title.x = element_blank()) +
  ggtitle("(E) Agricultural water withdrawal") ->
  p_sens_water



  (  (p2 + theme(legend.position = "none") + theme(axis.title.x = element_blank()))|
       (p5 + theme(legend.position = "none") + theme(axis.title.x = element_blank())) ) +
  patchwork::plot_layout(guides = "collect") -> pp1



cowplot::get_legend(p_sens_land) -> pleg

p_sens_fert + p_sens_water + patchwork::plot_spacer() + pleg + plot_layout(widths = c(1, 1,0.1, 0.35)) -> pp3

pp1/
(p_sens_land + theme(legend.position = "none", axis.title.x = element_blank())) /
  pp3  +
  patchwork::plot_layout(heights = c(1, 2.5, 1)) -> pp

pp %>% Write_png(.name = paste0("Sens_Fig5_SI", SensYear), .DIR_MODULE = DIR_MODULE, h = 14, w = 13)



