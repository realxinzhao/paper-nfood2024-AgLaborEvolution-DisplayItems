# Main Fig.3 & 4
# Focus on 2050 and 2100

# Reference plot ----

ProcScen <- function(.df){
  .df %>% filter(scenario %in% c("Evolving", "Static"),
                 region %in% c("World", "AFRICA", "CHINA+", "NORTH_AM"), year %in% c(2015, 2050, 2100)) %>%
    mutate(region = factor(region, levels = c("World", "AFRICA", "CHINA+", "NORTH_AM") )) %>%
    mutate(scenarioYear = paste(scenario, year)) %>%
    filter(scenarioYear != "Static 2015") %>%
    mutate(scenarioYear = if_else(scenarioYear== "Evolving 2015", "2015", scenarioYear)) %>%
    mutate(scenarioYear = factor(scenarioYear, levels = c("2015", "Evolving 2050", "Static 2050",
                                                          "Evolving 2100", "Static 2100"),
                                   labels = c("2015", "Evo. 2050", "Sta. 2050",
                                                          "Evo. 2100", "Sta. 2100")))
}


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


# plots ----
# Fig.3 agroeconomic outcomes ----
# Laborstat

Plaborstat <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("Plaborstat", ".RDS")))

Plaborstat %>%
  filter(region %in% c("World", "AFRICA", "CHINA+", "NORTH_AM")) %>%
  mutate(region = factor(region, levels = c("World", "AFRICA", "CHINA+", "NORTH_AM") )) %>%
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
  theme_bw() + theme0 + theme1 + theme(legend.key.width = unit(.8, "cm")) -> A1;A1


## Labor ----
Plabor %>% #filter(region != "World") %>%
  ProcScen() %>%
  ggplot +
  guides(alpha = guide_legend(order = 1),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 3)) +
  facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 10, linetype = 2, color = "grey80", size = 0.3) +
  geom_bar(aes(x = scenarioYear, y = value, fill = sector, linetype = scenario, alpha = scenario),
           stat = "identity", position = "stack", size = 0.5,
           color = "black") +
  scale_alpha_manual(values = c(1, 0.8)) +
  scale_linetype_manual(values = c(1, 5)) +
  labs(x = "Scenario & Year", y = "Million people", fill = "Sector", alpha = "Scenario", linetype = "Scenario") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1  -> A2; A2

## SUA staples ----
PSUA %>%
  filter(sector %in% tolower(c("Corn", "Wheat", "Rice", "OtherGrain", "RootTuber"))) %>%
  Agg_reg(element, region) %>%
  ProcScen() %>%
  #mutate(value = value /1000) %>%
  ggplot +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1500, linetype = 2, color = "grey80", size = 0.3) +
  geom_hline(yintercept = -1500, linetype = 2, color = "grey80", size = 0.3) +
  geom_bar(aes(x = scenarioYear, y = value, fill = element, linetype = scenario, alpha = scenario),
           stat = "identity", position = "stack", size = 0.5,
           color = "black") +
  scale_color_manual(values = "black") +
  scale_alpha_manual(values = c(1, 0.8), guide = "none") +
  scale_linetype_manual(values = c(1, 5), guide = "none") +
  labs(x = "Scenario & Year", y = "Million tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Production" = ColUpdate[1],
                               "Supply: Import" = ColUpdate[2], "Demand: Export" = ColUpdate[3],
                               "Demand: Food" = ColUpdate[4],"Demand: Feed" = ColUpdate[5], "Demand: Bioenergy" = ColUpdate[6],
                               "Demand: Other use" = ColUpdate[7]) ) +
  theme_bw() + theme0 + theme1 -> A3; A3

#PSUA %>%
  #filter(sector %in% tolower(c("pork", "poultry"))) %>%
  #filter(sector %in% tolower(c("Soybean", "OilCrop", "OilPalm", "FiberCrop", "SugarCrop", "Legumes", "NutsSeeds", "MiscCrop", "Vegetables", "Fruits"))) %>%
  #filter(sector %in% tolower(c("Beef")))




## Ag prices ----

PAgPrice %>% filter(scenario %in% c("Evolving", "Static")) %>%
  Proc_Diff(type = "R", -year, -prod) %>%
  filter(region %in% c("World", "AFRICA", "CHINA+", "NORTH_AM")) %>%
  mutate(region = factor(region, levels = c("World", "AFRICA", "CHINA+", "NORTH_AM") )) %>%
  ggplot() +  facet_wrap(~region, nrow = 1, scales = "fixed") +
  geom_hline(yintercept = 1, size = 0.4) +
  geom_line(aes(x = year, y = value, color = sector, linetype = scenario, size = sector, alpha = scenario)) +
  labs(x = "Year", y = "Index (2015 = 1)", color = "Sector", alpha = "Scenario",
       linetype = "Scenario", size = "Sector") +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  scale_size_manual(values = c(1, 1, 1, 1)) +
  scale_alpha_manual(values = c(1, 0.9), guide = "none") +
  scale_linetype_manual(values = c(1, 5), guide = "none") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  scale_y_continuous(n.breaks = 6) +
  theme_bw() + theme0 + theme1 + theme(legend.key.width = unit(.8, "cm")) -> A4;A4



(A1 + ggtitle("(A) Agricultural labor variables") +
    theme(axis.title.x = element_blank(), legend.position = "right")  +
    labs(color = "Variable (Panel A)", size = "Variable (Panel A)", linetype = "Scenario (Panels A & D)", alpha = "Scenario (Panels A & D)"))/
(A2 + ggtitle("(B) Agricultural labor input by sector") +
   theme(axis.title.x = element_blank(), legend.position = "right")  +
   labs(fill = "Sector (Panel B)", linetype = "Scenario (Panels B-C)", alpha = "Scenario (Panels B-C)"))/
(A3 + ggtitle("(C) Supply utilization accounts for staple crops")+ labs(fill = "SUA element (Panels C)")+
   theme(axis.title.x = element_blank(), legend.position = "right") ) /
  (A4 + ggtitle("(D) Agricultural prices by sector") +
     theme(axis.title.x = element_blank(),legend.position = "right", legend.justification = "left")+
     labs(color = "Sector (Panel D)", size = "Sector (Panel D)", linetype = "Scenario (Panels A & D)", alpha = "Scenario (Panels A & D)")) +
  patchwork::plot_layout(guides = "keep") -> pp


pp %>% Write_png(.name = "LaborMarketEvo_AgLU", .DIR_MODULE = DIR_MODULE, h = 14, w = 14)



# Fig. 4 environmental ----


## Land ----
Pland %>%
  Proc_Diff(type = "A", -year) %>%
  ProcScen() %>%
  filter(land != "Other land") %>% filter(year != 2015) %>%
  #mutate(value = value/1000) %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  guides(alpha = guide_legend(order = 1),
         linetype = guide_legend(order = 1),
         fill = guide_legend(order = 3)) +
  geom_hline(yintercept = 100, linetype = 2, color = "grey80", size = 0.3) +
  geom_hline(yintercept = -100, linetype = 2, color = "grey80", size = 0.3) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = scenarioYear, y = value, fill = land, linetype = scenario, alpha = scenario),
           stat = "identity", position = "stack", size = 0.5,
           color = "black") +
  scale_alpha_manual(values = c(1, 0.8)) +
  scale_linetype_manual(values = c(1, 5)) +
  labs(x = "Scenario & Year", y = "Million hectare", fill = "Land", alpha = "Scenario", linetype = "Scenario") +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_bw() + theme0 + theme1  -> A5; A5


## Water ----
Pwater %>% ProcScen() %>%  mutate(value = value) %>%
  ggplot +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 400, linetype = 2, color = "grey80", size = 0.3) +
  geom_bar(aes(x = scenarioYear, y = value, fill = sector, linetype = scenario, alpha = scenario),
           stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Scenario & Year", y = "Billion cubic meter", fill = "Sector", alpha = "Scenario", linetype = "Scenario") +
  scale_alpha_manual(values = c(1, 0.8), guide = "none") +
  scale_linetype_manual(values = c(1, 5), guide = "none") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 -> A6; A6

## GHG emissions ----


pCEM %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() %>% ProcScen()  ->
  pCEM1

pCEM1 %>% filter(sector != "LULUCF") %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free") +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = scenarioYear, y = value, fill = sector, linetype = scenario, alpha = scenario),
            stat = "identity", position = "stack",
            color = "black", size = 0.4) +
  # geom_line(data = pCEM1 %>% Agg_reg(region) %>%  mutate(ss = "Net Total"),
  #           aes(x = year, y = value, color = ss ), size = 1.2, linetype = 2) +
  labs(x = "Year", y = expression(paste(GtCO[2])), fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  scale_alpha_manual(values = c(1, 0.8), guide = "none") +
  scale_linetype_manual(values = c(1, 5), guide = "none") +
  scale_color_manual(values = "red") +
  theme_bw() + theme0 + theme1  -> A9; A9



## NonCO2 GHG emissions ----

pNCEM  %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() %>% ProcScen() ->
  pNCEM1

pNCEM1 %>% filter(grepl("Ag", sector)) %>%
  bind_rows(
    pCEM1 %>% filter(grepl("LULUCF", sector))
  ) %>%
  mutate(sector = factor(sector, levels = c("CH4_Ag", "N2O_Ag", "LULUCF"))) %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free") +
  geom_hline(yintercept = 300, linetype = 2, color = "grey80", size = 0.3) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = scenarioYear, y = value, fill = sector, linetype = scenario, alpha = scenario),
           stat = "identity", position = "stack",
            color = "black") +
  labs(x = "Scenario & Year", fill = "Source", y = expression(paste(GtCO[2]-eq))) +
  scale_fill_brewer(palette = "Accent", direction = -1,
                    labels = c(expression(paste(CH[4], " Agriculture")),
                               expression(paste(N[2], "O Agriculture")),
                               expression(paste(CO[2], " LULUCF")) ) ) +
  scale_alpha_manual(values = c(1, 0.8), guide = "none") +
  scale_linetype_manual(values = c(1, 5), guide = "none") +
  theme_bw() + theme0 + theme1 -> A7; A7



pNCEM_sector %>% Agg_reg(sector, sector0) %>% mutate(region = "World") %>%
  bind_rows(pNCEM_sector) %>%
  group_by(scenario, region, sector, sector0) %>%
  Fill_annual(CUMULATIVE = T) %>%
  ungroup() %>% ProcScen() ->
  pNCEM_sector1

pNCEM_sector1 %>% filter(region != "World") %>%
  filter(grepl("Ag", sector)) %>%
  mutate(sector = factor(sector, levels = c("CH4_Ag", "N2O_Ag"))) %>%
  ggplot + facet_grid(sector0~region, scales = "free") +
  #geom_hline(yintercept = 100, linetype = 2, color = "grey80", size = 0.3) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = scenarioYear, y = value, fill = sector, linetype = scenario, alpha = scenario),
           stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Scenario & Year", fill = "Source", y = expression(paste(GtCO[2]-eq))) +
  scale_fill_brewer(palette = "Dark2", direction = -1,
                    labels = c(expression(paste(CH[4], " Agriculture")),
                               expression(paste(N[2], "O Agriculture"))
                               ) ) +
  scale_alpha_manual(values = c(1, 0.8), guide = "none") +
  scale_linetype_manual(values = c(1, 5), guide = "none") +
  theme_bw() + theme0 + theme1 -> pp; pp
pp %>% Write_png(.name = "AgLU_Compare_GHGSector", .DIR_MODULE = DIR_MODULE, h = 8, w = 10)

## Land use emissions maps ----

ListV2024 %>% purrr::pluck("CLUC") %>%
  mutate(branch = scenario, scenario = ss) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  mutate(value = value / 1000 * 44/12) %>%
  select(scenario, region, year, value) %>%
  group_by(scenario, region) %>%
  Fill_annual(CUMULATIVE = T) %>%
  ungroup() %>% filter(year == 2100) -> CLUC_cum


basin <- read.csv(file = "data/maps/basin_to_country_mapping.csv", header = T, comment.char = "#") %>%
  dplyr::select(GCAM_basin_ID, GLU_name)
basin_noise <- read.csv(file = "data/maps/basin_noise.csv", header = T, comment.char = "#")
library(sf)

map_424_sf <- st_read("data/maps/map_424.shp") %>%
  # Separate Taiwan
  mutate(cntry_n = if_else(basn_nm == "Taiwan", "Taiwan", cntry_n)) %>%
  left_join(basin %>% select(basn_ID = GCAM_basin_ID, GLU_name), by = "basn_ID") %>%
  rename(basin = GLU_name, region = cntry_n) %>%
  # remove Greenland
  filter(basin != "ArcticIsl") %>%
  # remove small noise regions
  anti_join(basin_noise) # 381 combinations ~10 small areas not included
summary(map_424_sf)

map_424_sf %>% select(region, basin) %>% st_set_geometry(NULL) %>%
  full_join(CLUC_cum) ->
  CLUC_cum1


CLUC_cum %>% Agg_reg() %>%
  mutate(value = as.character(round(value, 0))) ->
  NetCLUCValue

CLUC_cum1 %>%
  ggcamMapBasin(Facet_Var = "scenario") +
  facet_wrap(~scenario) +
  geom_label(data = NetCLUCValue, aes(label = paste0(value, "Gt"), x = -135, y = -40),
             size = 10, label.size = NA) +
  theme(axis.title =  element_blank()) +
  scale_fill_fermenter(n.breaks = 6,
                       palette = "YlGnBu", na.value="white", direction = 1,
                       expression(paste(GtCO[2], " (Panel D)")) ) +
  theme(legend.key.width = unit(1.5,"line"), legend.key.height=unit(1.8,"line"),
        plot.title = element_text(hjust = 0, face = "bold"),
        legend.justification = "left") -> A8

((A5 + ggtitle("(A) Land use change relative to 2015")+ labs(fill = "Land (Panel A)", linetype = "Scenario (Panels A-C)", alpha = "Scenario (Panels A-C)")+
    theme(axis.title.x = element_blank(), legend.position = "right") ) /
(A6 + ggtitle("(B) Agricultural water withdrawal") +
     theme(axis.title.x = element_blank(),legend.position = "right") + labs(fill = "Sector (Panel B)"))/
(A7 + ggtitle("(C) Cumulative agricultural and land use change emissions since 2020") +
    theme(axis.title.x = element_blank(),legend.position = "right")+
    labs(fill = "Source (Panel C)", size = "Sector (Panel C)")) +
  patchwork::plot_layout(guides = "collect") ) -> AA

AA/(A8 + ggtitle("(D) Cumulative land use change emissions (2020 - 2100) by GCAM region") +
      theme(axis.title.x = element_blank(),legend.position = "right")) +
  patchwork::plot_layout(guides = "keep", widths = 1) ->
  pp

pp %>% Write_png(.name = "LaborMarketEvo_Envir", .DIR_MODULE = DIR_MODULE, h = 14, w = 14)





# Done ----




