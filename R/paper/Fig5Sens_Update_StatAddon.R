


# effective labor shocks ----
Plaborstat_AllScens %>%
  #filter(!(sector == "Agricultural labor")) %>%
  filter(grepl("Eff", sector)) %>%
  mutate(sector = "Eff_Labor") %>%
  group_by(region, year, sector) %>%
  mutate(value = value / value[scenario == "Evolving"]) %>%
  ungroup() %>%  filter(scenario != "Evolving") %>%
  mutate(value = log(value)*100) %>%
  spread(sector, value) ->
  Imp_Eff_Labor

# df_sens_agec ----
PAgPrice %>% select(-prod) %>%
  filter(grepl("Staple|Livestock", sector)) %>%
  mutate(sector = gsub(" crops", "", sector),
         sector = paste0("P_", sector) ) %>%
  bind_rows(
    Plaborstat_AllScens %>%
      filter(grepl("wage", sector)) %>%
      mutate(sector = gsub("Agricultural ", "", sector))
  ) %>%
  bind_rows(
    Plaborstat_AllScens %>%
      #filter(!(sector == "Agricultural labor")) %>%
      filter(grepl("Eff", sector)) %>%
      mutate(sector = "Eff_Labor")
  ) %>%
  bind_rows(
    PSUA %>%
      rename(sector0 = sector) %>%
      left_join_error_no_match(
        MapAgCOMM %>% transmute(sector0 = tolower(AgCOMM), sector = AgCOMM3),
        by = "sector0") %>%
      filter(element == "Supply: Production") %>% Agg_reg(element, region, sector) %>%
      filter(grepl("Staple|Livestock", sector)) %>%
      select(-element) %>%
      mutate(sector = paste0("Prod_", sector)) %>%
      mutate(sector = gsub(" crops", "",sector))
  ) %>%
  group_by(region, year, sector) %>%
  mutate(value = value / value[scenario == "Evolving"]) %>%
  ungroup() %>%  filter(scenario != "Evolving") %>%
  mutate(value = log(value)*100) %>%
  left_join(Imp_Eff_Labor) -> df_sens_agec

#df_sens_envir ----
Pland %>% rename(sector = land) %>%
  filter(!grepl("Other land", sector)) %>%
  mutate(sector = gsub(": ", "_", sector)) %>%
  bind_rows(
    Pfertilizer %>% Agg_reg(region) %>%
      mutate(sector = "Nfertilizer")
  ) %>%
  bind_rows(
    Pwater %>% Agg_reg(region) %>%
      mutate(sector = "Water withdrawal")
  ) %>%
  bind_rows(
    pNCEM1 %>% filter(grepl("Ag", sector)) %>%
      bind_rows(
        pCEM1 %>% filter(grepl("LULUCF", sector))
      )
  ) %>%
  group_by(region, year, sector) %>%
  mutate(value = value - value[scenario == "Evolving"]) %>%
  ungroup() %>%  filter(scenario != "Evolving") %>%
  left_join(Imp_Eff_Labor) -> df_sens_envir

# SD ----
df_sens_agec %>%
  filter(year == 2100) %>%
  group_by(region, sector) %>%
  summarise(sd = sd(value), n = n()) %>% spread(region, sd) %>%
  bind_rows(
    df_sens_envir %>%
      filter(year == 2100) %>%
      group_by(region, sector) %>%
      summarise(sd = sd(value), n = n()) %>% spread(region, sd)
  ) %>%
  left_join(
    df_sens_agec %>% filter(region != "World") %>%
      filter(year == 2100) %>%
      group_by(sector) %>%
      summarise(sd = sd(value), n = n()) %>%
      mutate(unit = "Percentage change") %>%
      bind_rows(
        df_sens_envir %>% filter(region != "World") %>%
          filter(year == 2100) %>%
          group_by(sector) %>%
          summarise(sd = sd(value), n = n()) %>%
          mutate(unit = "Real abslolute unit")
      ) %>% rename(world_n70 = sd) %>%
      select(-n)
    ) %>% write.csv("output/AgLabor/AgLabor/Sens_sd.csv")


# regression ----
library(broom)

# fixed region ----

df_sens_envir %>% filter(region != "World") %>% #filter(sector == "Forest") %>%
  select(sector, value, Eff_Labor, region) %>%
  nest(data = -sector) %>%
  mutate(
    fit = map(data, ~ lm(value~Eff_Labor + region, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)#, augmented = map(fit, augment)
  ) -> regressions


regressions %>% unnest(tidied) %>%
  select(sector, term, estimate) %>%
    spread(term, estimate) %>% mutate(term = "est") %>%
  bind_rows(
    regressions %>% unnest(tidied) %>%
      select(sector, term, pvalue = `p.value`) %>%
      spread(term, pvalue) %>% mutate(term = "pvalue")
  ) %>%
  left_join(
    regressions %>% unnest(glanced) %>%
      select(sector, r2 = `r.squared`)
  ) -> regre_sens_envir


regre_sens_envir %>% write.csv("output/AgLabor/AgLabor/regre_sens_envir.csv")

df_sens_agec %>% filter(region != "World") %>% #filter(sector == "Forest") %>%
  select(sector, value, Eff_Labor, region) %>%
  nest(data = -sector) %>%
  mutate(
    fit = map(data, ~ lm(value~Eff_Labor + region, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance)#, augmented = map(fit, augment)
  ) -> regressions


regressions %>% unnest(tidied) %>%
  select(sector, term, estimate) %>%
  spread(term, estimate) %>% mutate(term = "est") %>%
  bind_rows(
    regressions %>% unnest(tidied) %>%
      select(sector, term, pvalue = `p.value`) %>%
      spread(term, pvalue) %>% mutate(term = "pvalue")
  ) %>%
  left_join(
    regressions %>% unnest(glanced) %>%
      select(sector, r2 = `r.squared`)
  ) -> regre_sens_agec
regre_sens_agec %>% write.csv("output/AgLabor/AgLabor/regre_sens_agec.csv")



# pooled ----
df_sens_envir %>% filter(region != "World") %>%
  select(sector, value, Eff_Labor, region) %>%
  nest(data = -sector) %>%
  mutate(
    fit = map(data, ~ lm(value~Eff_Labor, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance) #, augmented = map(fit, augment)
  ) -> regressions

regressions %>% unnest(tidied) %>%
  select(sector, term, estimate, pvalue = `p.value`) %>%
  left_join(
    regressions %>% unnest(glanced) %>%
      select(sector, r2 = `r.squared`)
  ) -> A


df_sens_agec %>% filter(region != "World") %>%
  select(sector, value, Eff_Labor, region) %>%
  nest(data = -sector) %>%
  mutate(
    fit = map(data, ~ lm(value~Eff_Labor, data = .x)),
    tidied = map(fit, tidy),
    glanced = map(fit, glance) #, augmented = map(fit, augment)
  ) -> regressions

regressions %>% unnest(tidied) %>%
  select(sector, term, estimate, pvalue = `p.value`) %>%
  left_join(
    regressions %>% unnest(glanced) %>%
      select(sector, r2 = `r.squared`)
  )

