
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
library(RColorBrewer)

source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_DATA <- "data"
DIR_OUTPUT <- "output"
DIR_MODULE = "AgLabor"

Project <- "AgLabor"
Version <- "V2024"
Scenario <- Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T)

# Check availability
Load_GCAM(projnm = Project, return_availversion = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availscen = T)
Load_GCAM(projnm = Project, versionnm = Version, return_availquery = T)


# Load the saved list ----
ListV2024 <- readRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListV2024", ".RDS")))


# Fig.1 ----
source("R/paper/Fig1.R")

# Figs.3 - 5 and SIs ----
# source data
source("R/paper/AgLUDashBoard_Data.R")

ScenName = "Evolving"
source("R/paper/AgLUDashBoard_SingleScen.R")
ScenName = "Static"
source("R/paper/AgLUDashBoard_SingleScen.R")

# Diff between Evolving and Static
source("R/paper/AgLUDashBoard_PlotDiff.R")


source("R/paper/Figs3and4.R")

# Fig.5 and SIs ----

SensYear <- 2050
source("R/paper/Fig5Sens.R")
source("R/paper/Fig5Sens_Update.R")
SensYear <- 2100
source("R/paper/Fig5Sens.R")
source("R/paper/Fig5Sens_Update.R")



# check others ----
ListV2024 %>% purrr::pluck("MeanTemp") %>%
  mutate(branch = scenario, scenario = ss) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  select(-ss, -branch) %>%
  spread(scenario, value) %>%
  mutate(diff = Evolving - Static) %>%
  filter(year == 2100) -> A


ListV2024 %>% purrr::pluck("ForcingTotal") %>%
  mutate(branch = scenario, scenario = ss) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  select(-ss, -branch) %>%
  spread(scenario, value) %>%
  mutate(diff = Evolving - Static) %>%
  filter(year == 2100) -> A


ListV2024 %>% purrr::pluck("Fooddemandca") %>%
  mutate(branch = scenario, scenario = ss) %>%
  filter(scenario %in% c("para_ls_SSP2", "para_static")) %>%
  mutate(scenario = factor(scenario, levels = c("para_ls_SSP2", "para_static"),
                           labels = c("Evolving", "Static"))) %>%
  select(-ss, -branch) %>%
  #Agg_reg() %>%
  spread(scenario, value) %>%
  mutate(diff = Evolving / Static) %>%
  filter(year == 2100) -> A






# *******************-----------
# Functions and code to save data ----

# Modify/customize read csv function ----
read_csv_bind <- function(.multiCSVpath){

  library(doParallel)
  myCluster <-
    makeCluster(4, # number of cores to use
                type = "PSOCK") # type of cluster
  #detectCores()
  registerDoParallel(myCluster)

  foreach(csvDir = .multiCSVpath,
          .combine=rbind,
          .packages = "dplyr" ,.errorhandling = "remove"
  ) %dopar% {
    readr::read_csv(csvDir, skip = 1)%>%
      select(-matches("^X|\\...")) %>%
      na.omit() %>%
      filter(scenario != "scenario") %>%
      mutate(scenario = gsub(",date.*$", "", scenario)) %>%
      gcamdata::gather_years() %>%
      mutate(ss = sub(".*/([^/]+)/.*", "\\1", csvDir))
  } -> df

  stopCluster(myCluster)
  return(df)
}

rm(ListV2024)
# Load everything into lists ----
Load_GCAM(projnm = Project, versionnm = "V2024", outputlistnm = "ListV2024")


# create a project data output folder and save data
dir.create(file.path(DIR_OUTPUT, Project, "ProjectRDS"), showWarnings = F)
ListV2024 %>% saveRDS(file.path(DIR_OUTPUT, Project, "ProjectRDS", paste0("ListV2024", ".RDS")))




