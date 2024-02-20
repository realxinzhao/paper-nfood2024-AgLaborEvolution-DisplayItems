
# Fig.1 Main ----

# load data ----
DIR_MODULE = "AgLabor"
driver <- readRDS("data/input/DRIVER.RDS")
metric <- readRDS("data/input/metrics.RDS")
output <- readRDS("data/input/output.RDS")

cluster <- Regmapping %>% select(region, REG = REG10_AR6)

theme0 <- theme0 +
  theme(  panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

REG_1 <- c("World", "AFRICA", "CHINA+", "NORTH_AM")

## remove fishery labor in historical periods ----
SHARE <- read.csv("data/input/FOR_FISH_share_L.csv") %>%
  select(-X, -GCAM_region_ID)

driver %>%
  filter(var == "ag labor") %>%
  left_join(SHARE, by = "region") %>%
  mutate(value = ifelse(year < 2020, value * (1-FISH), value)) %>%
  select(names(driver)) %>%
  bind_rows(driver %>% filter(var != "ag labor")) ->
  driver

output %>%
  spread(source, value) %>%
  mutate(value = ifelse(year < 2020, USDA, GCAM_no_For)) %>%
  select(region, year, value) %>%
  mutate(var = "Output", var1 = "GDP") ->
  out

out %>%
  bind_rows(driver) %>%
  left_join(cluster, by = "region") %>%
  filter(var %in% c("LF" , "rural", "pop", "ag labor", "Output")) %>%
  group_by(REG, var, year) %>%
  summarise(value = sum(value)) %>%
  group_by(REG, var) %>%
  mutate(index = value / value[year == 2015]) %>%
  filter(year %in% c(1975, 2050, 2100)) ->
  df.1

out %>%
  bind_rows(driver) %>%
  left_join(cluster, by = "region") %>%
  filter(var %in% c("LF" , "rural", "pop", "ag labor", "Output")) %>%
  group_by( var, year) %>%
  summarise(value = sum(value)) %>%
  group_by(var) %>%
  mutate(index = value / value[year == 2015]) %>%
  mutate(REG = "World") %>%
  select(names(df.1)) %>%
  bind_rows(df.1) %>%
  filter(year %in% c(1975, 2050, 2100)) ->
  df.1.all

df.1 %>%
  group_by(var, year) %>%
  summarise(MIN = min(index),
            MAX = max(index)) ->
  df.1.range

eta <- driver %>% filter(var == "eta") %>% unique()

driver %>% filter(var == "ag labor") %>%
  select(region, year, LA = value) %>%
  left_join(eta %>% select(region, year, eta = value), by = c("region", "year")) %>%
  left_join(cluster, by = "region") %>%
  mutate(value = LA * eta) %>%
  group_by(REG, year) %>%
  summarise(value = sum(value,na.rm = T),
            LA = sum(LA,na.rm = T)) %>%
  mutate(eta = value / LA) ->
  df_eta

df_eta %>%
  group_by(year) %>%
  summarise(value = sum(value),
            LA = sum(LA)) %>%
  mutate(REG = "World",
         eta = value / LA) %>%
  select(names(df_eta)) %>%
  bind_rows(df_eta) %>%
  mutate(var = "eta") %>%
  filter(year %in% c(1975,  2050, 2100)) %>%
  select(REG, var, year, value, index = eta) ->
  plot.eta

plot.eta %>%
  filter(REG != "World") %>%
  group_by(year) %>%
  summarise(MIN = min(index),
            MAX = max(index)) %>%
  filter(year %in% c(1975, 2050,  2100)) %>%
  mutate(var = "eta") %>%
  select(names(df.1.range)) ->
  eta.range

VARIABLE <- c("pop","Output","eta","ag labor","rural","LF" )

df.1.range %>% bind_rows(eta.range) %>%
  mutate(MIN = if_else(year == 1975, 1/MIN, MIN),
         MAX = if_else(year == 1975, 1/MAX, MAX)) %>%
  mutate(MAX = 100*(MAX - 1), MIN = 100 * (MIN - 1)) %>%
  mutate(year = factor(year, levels = c(1975, 2050, 2100),
                       labels = c("1975 - 2015", "2015 - 2050", "2015 - 2100"))) %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "eta", "Output"),
                      labels = c("Total population", "Labor force", "Rural population", "Agiculture labor",
                                 "Labor productivity", "Agricultural output"))) ->
  df_pointrange

df.1.all %>% bind_rows(plot.eta) %>%
  mutate(index = if_else(year == 1975, 1/index, index)) %>%
  mutate(index = (index - 1)*100) %>%
  mutate(year = factor(year, levels = c(1975, 2050, 2100),
                       labels = c("1975 - 2015", "2015 - 2050", "2015 - 2100"))) %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "eta", "Output"),
                      labels = c("Total population", "Labor force", "Rural population", "Agiculture labor",
                                 "Labor productivity", "Agricultural output"))) %>%
  filter(REG %in% REG_1) ->
  df_points


# Plots ----


driver %>%
  filter(var %in% c("LF", "rural", "pop", "ag labor")) %>%
  left_join(eta %>% select(region, year, mult = value),
            by = c("region", "year")) %>%
  select(-var1) %>%
  spread(var, value) %>%
  mutate(eff = mult * `ag labor`) ->
  df.driver.lev


df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>% filter(var == "ag labor") %>%
  left_join(cluster) %>%
  group_by(REG, year) %>% summarize(value = sum(value)) %>% ungroup() %>%
  mutate(value = value / 1000) %>% filter(year >= 1975) %>%
  ggplot() +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_hline(yintercept = 0, color = "black") +
  geom_area(aes(x = year, y = value, fill = REG), stat="identity", color="black", size = 0.2) +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  geom_vline(xintercept = 2003, linetype = 2, color = "red") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.1)) +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "right") +
  labs(x = "Year", y = "Billion people", fill = "Region (Panels A & B)") +
  ggtitle("(A) Agricultural labor input") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p1; p1

df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>% filter(var == "ag labor") %>%
  left_join(cluster) %>%
  group_by(REG, year) %>% summarize(value = sum(value)) %>% ungroup() %>%
  group_by(REG) %>%
  mutate(value = value - value [year == 2015]) %>% filter(year >= 1975) %>%
  ggplot() +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  geom_hline(yintercept = 0, color = "black") +
  #geom_area(aes(x = year, y = value, fill = REG), stat="identity", color="black", size = 0.2) +
  geom_line(aes(x = year, y = value, color = REG), size = 1) +
  geom_vline(xintercept = 2003, linetype = 2, color = "red") +
  scale_fill_brewer(palette = "Paired") +
  scale_color_brewer(palette = "Paired") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-180, 180), breaks = seq(-150, 150, 50) ) +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "none") +
  labs(x = "Year", y = "Million people (2015 = 0)", fill = "Region", color = "Region") +
  ggtitle("(B) Agricultural labor input change (2015 = 0)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p2


p1 + p2 + patchwork::plot_layout(guides = "collect") -> pp1; pp1


df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>%
  group_by(year, var) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  filter(year >= 1975) %>%
  filter(var != "eff") %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "Effective labor"),
                      labels = c("Total population", "Labor force", "Rural population", "Agiculture labor",
                                 "Effective labor"))) %>%
  ggplot() +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  geom_line(aes(x = year, y = value/1000, color = var), size = 1.3) +
  labs(x = "Year", y = paste0("Billion people"), color = "Variable") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "right") +
  ggtitle("(C) World population and labor changes") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p3;p3

ggplot() +
  geom_rect(data = df_pointrange,
            aes(#xmin= which(levels(as.factor(year))=="1975 - 2015") -0.5,
              xmin = -Inf,
              xmax= which(levels(as.factor(year))=="1975 - 2015") +0.5,
              ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(data = df_pointrange,
                 aes(x = year, ymin = MIN, ymax = MAX, group = interaction(year, var), color = var),
                 position = position_dodge(width = 0.75), size = 0.5) +
  geom_point(data = df_points,
             aes(x = year, y = index, group = interaction(year, var), shape = REG, fill = var),
             color = "black", size = 3, alpha = 0.8,
             position = position_dodge(width = 0.75)) +
  labs(x = "Period", y = "Growth rate (%)", color = "Variable", shape = "Region (Panel D)", fill = "Variable") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(22:24, 21)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  theme_bw() + theme0 + theme_leg + #theme_add +
  theme(legend.position = "right") +
  ggtitle("(D) Regional growth rate of key variables") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p4; p4


df_points %>%
  ggplot() +
  geom_line(aes(x = year, y  = value, color = var), size = 1) +
  geom_point(aes(x = year, y  = value, shape = REG), size = 3) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(22:24, 21)) + theme_bw() + theme0 +
  labs(color = "Variable (Panels C & D)",
       shape = "Region (Panel D)")+ theme_leg + theme(legend.position = "right")+
  guides(color = guide_legend(order = 1))-> A;A

library(cowplot)
get_legend(A)-> p_leg2
get_legend(p1) -> p_leg1

(p1 + theme(legend.position = "none")) +
  p2 + p_leg1 + patchwork::plot_layout(guides = 'collect', widths = c(1, 1, 0.5)) -> pp1

p3 + theme(legend.position = "none") +
  p4 +
  theme(legend.position = "none") +
  p_leg2 +
  plot_layout(guides = 'collect', widths = c(1, 1, 0.5)) -> pp2

# global Fig1 ----
pp1/pp2 -> pp3; pp3

pp3 %>% Write_png(.name = "Fig1", .DIR_MODULE = DIR_MODULE, h = 12, w = 15)

# regional drivers ----
df.driver.lev %>%
  gather(var, value, mult:eff) %>%
  filter(var != "mult") %>%
  na.omit() %>%
  left_join(cluster) %>%
  group_by(year, REG, var) %>%
  summarise(value = sum(value)) %>% ungroup() %>%
  filter(year >= 1975) %>%
  filter(var != "eff") %>%
  mutate(var = factor(var, levels = c("pop", "LF", "rural", "ag labor", "Effective labor"),
                      labels = c("Total population", "Labor force", "Rural population", "Agiculture labor", "Effective labor"))) %>%
  ggplot() +
  facet_wrap(~REG, nrow = 2, scale = "free_y") +
  geom_rect(aes(xmin=-Inf, xmax=2015, ymin=-Inf,ymax=Inf), alpha=0.1, fill="grey90") +
  geom_vline(xintercept = 2015, linetype = 2, size = 0.6, color = "grey50") +
  #geom_hline(yintercept = 0, color = "black") +

  geom_line(aes(x = year, y = value/1000, color = var), size = 1.3) +
  labs(x = "Year", y = paste0("Billion people"), color = "Variable") +
  scale_x_continuous(expand = c(0, 0), breaks = c(1975, 2000, 2015, 2050, 2075, 2100)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 10)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() + theme0 + theme_leg +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1),
        plot.margin = margin(t = 10, r = 18, b = 10, l = 10)) -> p3_reg;p3_reg


p3_reg %>% Write_png(.name = "Fig1_Sreg", .DIR_MODULE = DIR_MODULE, h = 8, w = 14)
