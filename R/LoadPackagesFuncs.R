library(dplyr)
library(tidyr)
library(ggplot2)

# Load mappings ----
readr::read_csv("data/maps/LandMappingV7p1.csv") -> LandMapping
readr::read_csv("data/maps/Regmapping.csv") -> Regmapping
readr::read_csv("data/maps/AgCommMapping.csv") -> MapAgCOMM

# Load ggplot themes ----
fontfamily = "Arial"
windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey75"),
  #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  #axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  #plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.title = element_text(hjust = 0, face = "bold"),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())

# Load other utility funcs ----

Write_png <- function(.plot, .name, .DIR_MODULE, w = 10, h = 10, r = 300){

  # create a fig data output folder
  dir.create(file.path(DIR_OUTPUT, Project, .DIR_MODULE), showWarnings = F)

  ggsave(file.path(DIR_OUTPUT, Project, .DIR_MODULE, paste0(.name,".png")),
         plot = .plot,
         width = w, height = h, dpi = r)

  # png(file.path(DIR_OUTPUT, Project, paste0(name,".png")), width = w, height = h, res = r)
  # print(.plot)
  # dev.off()
}


Agg_reg <- function(.data, ...){
  .data %>%
    group_by(scenario, ... , year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    ungroup() %>%
    return()
}


Proc_Diff <- function(.df, type = "R", ...){
  if (type == "R"){
    .df %>%
      group_by_at(vars(-value, ...)) %>%
      mutate(value = value / first(value)) %>%
      ungroup() %>%
      return()
  } else
    if (type == "A") {
      .df %>%
        group_by_at(vars(-value, ...)) %>%
        mutate(value = value - first(value)) %>%
        ungroup() %>%
        return()
    }
}

Fill_annual <- function(.df, CUMULATIVE = FALSE,
                        CUM_YEAR_START = 2020,
                        CUM_OUT_STEP = 5){
  YEAR_START <- min(unique(.df$year))
  YEAR_END <- max(unique(.df$year))
  .df %>% mutate(year = as.integer(year)) -> .df


  .df %>% filter(year >= YEAR_START) %>%
    bind_rows(
      .df %>%
        #assuming YEAR_END has values for all
        filter(year == YEAR_END) %>% select(-year) %>%
        mutate(value = NA) %>%
        gcamdata::repeat_add_columns(tibble(year = setdiff(seq(YEAR_START,YEAR_END), unique(.df$year))))
    ) %>% arrange(year) %>%
    mutate(value = gcamdata::approx_fun(year, value, rule = 2)) -> .df1

  if (CUMULATIVE == TRUE ) {
    assertthat::assert_that(CUM_YEAR_START >= YEAR_START)
    .df1 %>% filter(year >= CUM_YEAR_START) %>%
      mutate(value = cumsum(value)) %>% filter(year >= CUM_YEAR_START) %>%
      filter(year %in% seq(YEAR_START,YEAR_END, CUM_OUT_STEP))-> .df1
  }
  return(.df1)
}

