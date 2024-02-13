# Fig 2

DIR_MODULE = "Fig2"


ListV2024 %>% names()


REG_1 <- c("World", "AFRICA", "CHINA+", "INDIA+", "NORTH_AM")
COMMSector <- c("Energy crops", "Staple crops", "Oil crops", "Fodder crops", "Other crops", "Livestock", "Forest")

PluckBind <- function(.query ){
  ListV2024 %>% purrr::pluck(.query) %>%
    mutate(branch = scenario, scenario = ss) %>%
    filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
    rename(region0 = region) %>%
    left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6)) %>%
    mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                             labels = c("Evolving", "Static")))
}

# Get data ready ----

"CapitalDemandSec" %>% PluckBind() %>%
  Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(
    PluckBind("CapitalDemandSec") %>%
      Agg_reg(sector, region)
  ) %>%
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM2), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM2, region) %>%
  mutate(sector = factor(sector, levels = COMMSector)) %>%
  Agg_reg(sector, region) %>%
  filter(year >= 2015) %>%
  mutate(value = value * gdp_deflator(2015, 1975)/1000)-> Pcapital

## Ag labor by sector ----
PluckBind("LaborDemandSec") %>%
  Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(
    PluckBind("LaborDemandSec") %>%
      Agg_reg(sector, region)
  ) %>%
  left_join_error_no_match(
    MapAgCOMM %>% select(sector = AgCOMM, AgCOMM2), by = "sector"
  ) %>%
  Agg_reg(sector = AgCOMM2, region) %>%
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
  mutate(land = replace(land, land %in% c("Grassland", "Shrubland"), "Other natural")) %>%
  mutate(land = factor(land, levels = c(
    "Cropland - Energy", "Cropland - Staples", "Cropland - Others",
    "Forest", "Pasture", "Other Natural", "Other Land" ) )) %>%
  group_by_at(vars(-value)) %>% summarise(value = sum(value), .groups = "drop") -> Pland

Pland %>% Agg_reg(land) %>% mutate(region = "World") %>%
  bind_rows(Pland) -> Pland

## Ag water by sector ----
"Water_Wsector" %>% PluckBind %>% filter(year >= 2015) %>% filter(value >0) %>%
  mutate(sector1 = if_else(sector %in% MapAgCOMM$AgCOMM, "Ag", sector)) %>%
  mutate(sector1 = if_else(grepl("elec", sector1), "Elec.", sector1)) %>%
  mutate(sector1 = if_else(grepl("Ag|Elec", sector1), sector1, "Others")) %>%
  filter(sector1 == "Ag") %>%
  left_join_error_no_match(MapAgCOMM %>% select(sector = AgCOMM, sector2 = AgCOMM2), by = "sector"
                           ) %>%
  mutate(sector2 = factor(sector2, levels = COMMSector)) %>%
  Agg_reg(region, sector = sector2) -> Pwater

Pwater %>% Agg_reg(sector) %>% mutate(region = "World") %>%
  bind_rows(Pwater) -> Pwater

## Ag Fertilizer ----
"FertizerUse" %>% PluckBind %>% filter(year >= 2015) %>% filter(sector != "Exports_fertilizer") %>%
  left_join(MapAgCOMM %>% select(sector = AgCOMM, sector2 = AgCOMM2)) %>%
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

source("R/paper/AgBalElement_Storage.R")

c(brewer.pal(n = length(ELEMAll), name = "BrBG")[1:length(ElEMSupply)],
  brewer.pal(n = length(ELEMAll), name = "BrBG")[length(ElEMSupply)+1:length(ELEMDemand)]
) -> ColUpdate


PSUA %>% Agg_reg(sector, element) %>% mutate(region = "World") %>%
  bind_rows(PSUA) -> PSUA

PAgPrice %>%
  group_by_at(vars(scenario, year, branch, sector)) %>%
  summarise(value = weighted.mean(value, w = prod), prod = sum(prod), .groups = "drop") %>%
  drop_na() %>% mutate(region = "World") %>%
  bind_rows(PAgPrice) %>%
  mutate(sector = factor(sector, levels = COMMSector)) ->
  PAgPrice



# Reference plot ----

## theme1 ----
theme1 <- theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.grid = element_blank(),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

## Land ----
Pland %>% Agg_reg(land, region) %>%
  filter(scenario == "Evolving") %>% mutate(value = value / 1000) %>%
  ggplot +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = land), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Billion hectare", fill = "Land") +
  scale_fill_brewer(palette = "Spectral", direction = -1) +
  theme_bw() + theme0 + theme1  -> A1; A1

## Labor ----
Plabor %>% #filter(region != "World") %>%
  filter(scenario == "Evolving") %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million people", fill = "Sector") +
  scale_fill_brewer(palette = "Set2", direction = 1) +
  theme_bw() + theme0 + theme1 -> A2; A2

## Capital ----
Pcapital %>% #filter(region != "World") %>%
  filter(scenario == "Evolving") %>%
  ggplot + facet_wrap(~region, scale = "free_y", nrow = 1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Trillion 2015$") +
  scale_fill_brewer(palette = "Set2", name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A3; A3

## Water ----
Pwater %>% filter(scenario == "Evolving") %>%  mutate(value = value / 1000) %>%
  ggplot +   facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Trillion cubic meter") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A4; A4

## Fertilzer ----
Pfertilizer %>% filter(scenario == "Evolving") %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
           color = "black") +
  labs(x = "Year", y = "Million tonne N") +
  scale_fill_brewer(palette = "Set2",
                    name = "Sector", direction = 1) +
  theme_bw() + theme0 + theme1 -> A5; A5


## Ag SUA ----

PSUA %>%
  filter(sector %in% tolower(c("Corn", "Wheat", "Rice", "OtherGrain", "RootTuber"))) %>%
  Agg_reg(element, region) %>%
  filter(scenario == "Evolving") %>% mutate(value = value /1000) %>%
  ggplot +

  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  #facet_grid(~scenario, scales = "free_y") +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  scale_color_manual(values = "black") +
  labs(x = "Year", y = "Billion tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Production" = ColUpdate[1],
                               "Supply: Import" = ColUpdate[2], "Demand: Export" = ColUpdate[3],
                               "Demand: Food" = ColUpdate[4],"Demand: Feed" = ColUpdate[5], "Demand: Bioenergy" = ColUpdate[6],
                               "Demand: Other use" = ColUpdate[7]) ) +
  theme_bw() + theme0 + theme1 -> A6; A6

PSUA %>%
  filter(sector %in% tolower(c("Beef", "SheepGoat"))) %>%
  Agg_reg(element, region) %>%
  filter(scenario == "Evolving") %>% mutate(value = value) %>%
  ggplot +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  #facet_grid(~scenario, scales = "free_y") +
  facet_wrap(~region, nrow = 1, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = year, y = value, fill = element), stat = "identity", position = "stack",
           color = "black") +
  scale_color_manual(values = "black") +
  labs(x = "Year", y = "Million tonne", fill = "Element") +
  scale_fill_manual(values = c("Supply: Production" = ColUpdate[1],
                               "Supply: Import" = ColUpdate[2], "Demand: Export" = ColUpdate[3],
                               "Demand: Food" = ColUpdate[4],"Demand: Feed" = ColUpdate[5], "Demand: Bioenergy" = ColUpdate[6],
                               "Demand: Other use" = ColUpdate[7]) ) +
  theme_bw() + theme0 + theme1 -> A7; A7


## Ag prices ----

PAgPrice %>%
  filter(scenario == "Evolving") %>%
  Proc_Diff(type = "R", -year, -prod) %>%
  ggplot +
  facet_wrap(~region, nrow = 1, scales = "fixed") +
  geom_hline(yintercept = 1) +
  geom_line(aes(x=year, y=value, color=sector), size = 1.4) +
  #scale_color_brewer(palette = "Set1", direction = 1) +
  scale_color_npg() +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  labs(x = "Year", y = "Index (2015 = 1)", color = "Sector") +
  theme_bw() + theme0 + theme1  -> A8; A8


## Carbon emissions ----


pCEM %>% filter(scenario == "Evolving") %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value / 1000) %>%
  ungroup() ->
  pCEM1

pCEM1 %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free") +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  geom_hline(yintercept = 0) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black", size = 0.4) +
  geom_line(data = pCEM1 %>% Agg_reg(region) %>%  mutate(ss = "Net Total"),
            aes(x = year, y = value, color = ss ), size = 1.2, linetype = 2) +
  labs(x = "Year", y = expression(paste("Trillion ", tCO[2])), fill = "Sector", color = "") +
  scale_fill_brewer(palette = "RdBu", direction = -1) +
  scale_color_manual(values = "red") +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  theme_bw() + theme0 + theme1  -> A9; A9

## NonCO2 GHG emissions ----

pNCEM %>% filter(scenario == "Evolving") %>%
  group_by(scenario, region, sector) %>%
  Fill_annual(CUMULATIVE = T) %>% mutate(value = value) %>%
  ungroup() ->
  pNCEM1

pNCEM1 %>%
  ggplot + facet_wrap(~region, nrow = 1, scales = "free") +
  geom_hline(yintercept = 0) +
  geom_area(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
            color = "black") +
  labs(x = "Year", fill = "Source",
       y = expression(paste(GtCO[2]-eq.))) +
  scale_x_continuous(breaks = c(2025, 2050 ,2075, 2100)) +
  scale_fill_brewer(palette = "Set1", direction = -1,
                    labels = c(expression(paste(CH[4], " Agriculture")),
                               expression(paste(CH[4], " Energy")),
                               expression(paste(CH[4], " Unmanaged Land")),
                               expression(paste(N[2], "O Agriculture")),
                               expression(paste(N[2], "O Energy")),
                               expression(paste(CH[4], " Unmanaged Land")),
                               "Other GHGs")) +
  theme_bw() + theme0 + theme1 -> A10; A10



(A1 + ggtitle("(A) Land cover and use by sector and region")+ labs(fill = "Land (Panel A)")+theme(axis.title.x = element_blank(), legend.position = "right") )/
(A2 + ggtitle("(B) Agricultural labor input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "right")  + labs(fill = "Sector (Panels B-E)"))/
  (A3 + ggtitle("(C) Agricultural capital input by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
  (A4 + ggtitle("(D) Agricultural water withdrawal by sector and region") + theme(axis.title.x = element_blank(),legend.position = "none")) /
  (A5 + ggtitle("(E) Fertilizer use by sector and region") + theme(axis.title.x = element_blank(), legend.position = "none")) /
  (A6 + ggtitle("(F) Supply utilization accounts for staple crops by region") + theme(axis.title.x = element_blank(), legend.position = "right") + labs(fill = "SUA element (Panel F-G)"))/
  (A7 + ggtitle("(G) Supply utilization accounts for ruminants by region") + theme(axis.title.x = element_blank(), legend.position = "none") )/
  (A8 + ggtitle("(H) Agricultural prices by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(color = "Sector (Panel H)")) +
  (A9 + ggtitle("(I) Cumulative carbon dioxide emissions by sector and region") + theme(axis.title.x = element_blank(),legend.position = "right")+ labs(fill = "Sector (Panel I)")) +
  (A10 + ggtitle("(J) Cumulative non-carbon dioxide GHG emissions by sector and region") + theme(legend.position = "right")+ labs(fill = "Sector (Panel J)")) +
  patchwork::plot_layout(guides = "collect", heights = rep(1, 10)) -> pp

pp %>% Write_png(.name = "LaborLandWaterEvo_reg", .DIR_MODULE = DIR_MODULE, h = 24, w = 22)





# df %>%   filter(region %in% REG_1,
#                 year %in% c(2020, 2050, 2100)) %>%
#   mutate(year = as.character(year)) %>%
#   ggplot +   #facet_wrap(~region+scenario, scale = "free_y", nrow = 2) +
#   facet_grid(region~scenario, scale = "free_y") +
#   geom_hline(yintercept = 0) +
#   geom_bar(aes(x = year, y = value, fill = sector), stat = "identity", position = "stack",
#            color = "black") +
#   labs(x = "Year", y = "Million People") +
#   scale_fill_brewer(palette = "Set2",
#                     name = "Sector", direction = -1) +
#   theme_bw() + theme0 +
#   theme(axis.text.x = element_text(angle = 40, hjust = 0.9, vjust = 1), legend.text.align = 0,
#         strip.background = element_rect(fill="grey99"),
#         strip.text = element_text(size = 16),
#         panel.grid = element_blank(),
#         panel.spacing.y = unit(0.5, "lines"),
#         panel.spacing.x = unit(0.5, "lines")) -> A1; A1










