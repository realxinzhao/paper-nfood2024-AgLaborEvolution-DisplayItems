# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)
library(purrr)
library(patchwork)

source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")
DIR_DATA <- "data"
DIR_OUTPUT <- "output"
Project <- "AgLabor"
DIR_MODULE = "AgLabor"

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


readr::read_csv("data/input/RuralMax_SSP.csv") -> RuralPOP


RuralPOP %>%
  rename(region0 = region) %>%
  left_join_error_no_match(Regmapping %>% select(region0 = region, region = REG10_AR6), by = "region0") %>%
  select(value = maxSubResource, year, scenario, region) %>%
  Agg_reg(region) -> RuralPOP

RuralPOP %>% Agg_reg() %>% filter(year %in% c(2015, 2050, 2100)) %>%
  mutate(year = as.character(year)) %>%
  ggplot +   facet_wrap(~scenario, nrow = 1) +
  geom_hline(yintercept = 0) +

  geom_bar(aes(x = year, y = value, fill = scenario), stat = "identity", position = "stack",
           color = "black") +
  geom_hline(yintercept = 1000, linetype = 2, color = "gray") +
  geom_hline(yintercept = 2000, linetype = 2, color = "gray") +
  geom_hline(yintercept = 3000, linetype = 2, color = "gray") +
  labs(x = "Year", y = "Million People", fill = "Scenario") +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  theme_bw() + theme0 + theme1 -> A1; A1

RuralPOP %>% #filter(year %in% c(2015, 2050, 2100)) %>%
  filter(year >= 2015) %>%
  ggplot +   facet_wrap(~region, nrow = 2, scales = "free_y") +
  #geom_hline(yintercept = 0) +
  geom_line(aes(x = year, y = value, group = interaction(region, scenario), color = scenario), size = 1) +
  labs(x = "Year", y = "Million People", color = "Scenario") +
  scale_color_brewer(palette = "Set2", direction = -1) +
  theme_bw() + theme0 + theme1 -> A2; A2


(A1 + ggtitle("(A) Global rural population projections by SSP") ) /
  (A2 + ggtitle("(B) Regional (R10) rural population projections by SSP")) +
  plot_layout(guides = "collect", heights = c(1,1.8)) -> pp

pp %>% Write_png(.name = "FigS_RuralPop", .DIR_MODULE = DIR_MODULE, h = 12, w = 12)



RuralPOP %>% filter(year %in% c(2015, 2100)) %>% filter(scenario == "SSP2") %>%
  spread(year, value) %>%
  mutate(diff = `2100` - `2015`)


# Fig.S labor data compilation

labor_all <- readRDS("data/input/labor_all.rds")

# share to global total ----
labor_all %>%
  filter(group != "Ag_GCAM") %>%
  left_join_error_no_match(Regmapping %>% select(region, REG = REG10_AR6)) %>%
  group_by_at(vars(-region, -value)) %>%
  summarize(value = sum(value)) %>%
  ungroup() %>%
  mutate(Total = sum(value),
         share = 100 * value / Total) ->
  share_sec

share_sec %>%
  mutate(group = gsub("Staples", "Staple crops", group),
         group = gsub("Fish", "Fishery", group)) ->
  df.Fig.S2

df.Fig.S2$group <- factor(df.Fig.S2$group,
                          levels = c("Staple crops", "Oil crops",
                                     "Other crops", "Livestock",
                                     "Fishery", "Forest"))

df.Fig.S2 %>%
  ggplot(aes(x = group, y = REG)) +
  geom_tile(aes(fill = share)) +
  geom_text(aes(label = round(share, 1)), size = 6) +
  scale_fill_fermenter(palette = "BuPu", direction = 1) +
  #scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "", y = paste0(""), fill = "%") +
  ggtitle("") +
  theme_bw() + theme0 + theme_leg ->
  labor.share.10REG; labor.share.10REG


labor.share.10REG %>% Write_png(.name = "FigS_LaborShareData", .DIR_MODULE = DIR_MODULE, h = 9, w = 12)
