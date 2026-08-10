library(dplyr)
library(ggplot2)
library(ggrepel)
library(janitor)
library(readr)
library(tidyr)
library(units)
library(systemfonts)
library(ragg)
library(patchwork)
library(sf)
library(readxl)

##*****************##
##### Figure 1 ######
##*****************##

## data is from TWDB
## use is in acre-feet

# 2026-08-06
# I am unsure of the provenance of the datain the commented out code block.
# county level use data has been downloaded directly from TWDB as
# Data/TWDB_Summary_county_pre1999.xlsx
# Data/TWDB_Summary_county_post1999xlsx.xlsx
# the updated code includes the data cleaning steps.
#

# total_use_df_twdb <- read_csv("Data/Total_Irrigation_Use_TWDB_Figure_1.csv") |> 
#   clean_names() |> 
#   pivot_longer(cols = -year) |> 
#   mutate(value = as_units(value, "acre_feet/year")) |> 
#   mutate(value = set_units(value, "1000000 acre_feet/year")) |> 
#   mutate(name = stringr::str_replace(name, "_", " ")) |> 
#   mutate(name = stringr::str_to_title(name))
# 
# total_use_df_twdb_labels <- total_use_df_twdb |> 
#   group_by(name) |> 
#   filter(year == max(year))


pre1999 <- read_excel("Data/TWDB_Summary_county_pre1999.xlsx") |> 
  janitor::clean_names()
post1999 <- read_excel("Data/TWDB_Summary_county_post1999xlsx.xlsx") |> 
  janitor::clean_names()
pre1999 <- pre1999 |> 
  select(year, county, source_type, irrigation) |> 
  pivot_wider(names_from = source_type, values_from = irrigation) |> 
  group_by(year) |> 
  summarize(Groundwater = sum(Groundwater, na.rm = TRUE),
         `Surface Water` = sum(`Surface water`, na.rm = TRUE)) |> 
  mutate(Total = Groundwater + `Surface Water`)

post1999 <- post1999 |> 
  select(year, county, irrigation, irrigation_groundwater, irrigation_reuse, irrigation_surface_water, irrigation_reuse) |> 
  group_by(year) |> 
  summarize(Groundwater = sum(irrigation_groundwater, na.rm = TRUE),
            `Surface Water` = sum(irrigation_surface_water, na.rm = TRUE),
            Reuse = sum(irrigation_reuse, na.rm = TRUE),
            Total = sum(irrigation, na.rm = TRUE))

total_use_df_twdb <- pre1999 |> 
  bind_rows(post1999) |> 
  pivot_longer(c(Groundwater,`Surface Water`, Total, Reuse)) |> 
  mutate(value = as_units(value, "acre_feet/year")) |> 
  mutate(value = set_units(value, "1000000 acre_feet/year")) 

total_use_df_twdb
total_use_df_twdb_labels <- total_use_df_twdb |>
  group_by(name) |>
  filter(year == max(year, na.rm = TRUE))
total_use_df_twdb_labels

total_use_df_usgs <- arrow::read_csv_arrow("Data/usgs/cleaned_annual_usgs_withdrawals.csv")
total_use_df_usgs <- total_use_df_usgs |> 
  rename(Total = total_irrigative_withdrawals,
         Groundwater = gw_irrigative_withdrawals,
         `Surface Water` = sw_irrigative_withdrawals) |> 
  pivot_longer(c("Total", "Groundwater", "Surface Water")) |> 
  mutate(value = as_units(value, "acre_feet/year")) |> 
  mutate(value = set_units(value, "1000000 acre_feet/year")) |> 
  bind_rows(tibble(year = 2023,
                   name = "Reuse",
                   value = as_units(NA, "acre_feet/year"))) ## add dummy value for Reuse

total_use_df_usgs_labels <- total_use_df_usgs |> 
  group_by(name) |> 
  filter(year == max(year))



p2 <- ggplot(total_use_df_usgs) +
  geom_line(aes(year, value, color = name)) +
  geom_point(aes(year, value, color = name)) +
  geom_text_repel(data = total_use_df_usgs_labels,
                  aes(year, value, label = name, color = name),
                  size = 2.5,
                  direction = "y",
                  hjust = 0,
                  nudge_x = 1,
                  min.segment.length = 3,
                  family = "Open Sans",
                  fontface = "bold") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(NULL,
                     limits = c(1974, 2024),
                     expand = expansion(mult = c(0.015, 0.1)),
                     breaks = c(seq(from = 1960,
                                    to = 2020, 
                                    by = 10))) +
  scale_y_units("Volume [Million Acre-Feet/Year]",
                limits = c(0, 13),
                breaks = c(0,2.5,5,7.5,10,12.5),
                expand = expansion(mult = c(0,0.02))) +
  labs(subtitle = "Volume withdrawn (ac-ft)\nestimated by USGS",
       caption = "source: USGS National Water\nAvailability Assessment") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain", size = 8),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40", size = 8))

p1 <- ggplot(total_use_df_twdb) +
  geom_line(aes(year, value, color = name)) +
  geom_point(aes(year, value, color = name)) +
  geom_text_repel(data = total_use_df_twdb_labels,
                  aes(year, value, label = name, color = name),
                  size = 2.5,
                  direction = "y",
                  hjust = 0,
                  nudge_x = 1,
                  min.segment.length = 3,
                  family = "Open Sans",
                  fontface = "bold") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(NULL,
                     limits = c(1974, 2024),
                     expand = expansion(mult = c(0.015, 0.1)),
                     breaks = c(seq(from = 1960,
                                    to = 2020, 
                                    by = 10))) +
  scale_y_units("Volume [Million Acre-Feet/Year]",
                limits = c(0, 13),
                breaks = c(0,2.5,5,7.5,10,12.5),
                expand = expansion(mult = c(0,0.02))) +
  labs(
       subtitle = "Volume applied (ac-ft)\nestimated by TWDB",
       caption = "source: Texas Water Development Board") +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain", size = 8),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40", size = 8))

p1

layout <- "
1111
2222"
p1 + p2 + plot_layout(axis_titles = "collect_y", axes = "collect_y", design = layout) +
  plot_annotation(title = "Estimated Irrigative Water Use in Texas",
                  theme = theme(plot.title = element_text(family = "Oswald", face = "bold"),
                                plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
                                plot.caption = element_text(color = "grey40")))

ggsave(fs::path("Figures", "Figure_1", ext = "png"), dpi = 300, width = 6.5, height = 4.5, units = "in")




##*****************##
##### Figure 3 ######
##*****************##

## data is from TWDB
## use is in acre-feet

# again, data provenance is uncertain. using county level data directly from TWDB
# Data/TWDB_Summary_county_pre1999.xlsx
# Data/TWDB_Summary_county_post1999xlsx.xlsx


# region_use_df <- read_csv("Data/Total_Irrigation_Use_By_REgion_TWDB_Figure_3.csv") |> 
#   clean_names() |> 
#   rename(region = x5) |> 
#   pivot_longer(cols = -c(year, region)) |> 
#   mutate(value = as_units(value, "acre_feet/year")) |> 
#   mutate(value = set_units(value, "1000000 acre_feet/year")) |> 
#   mutate(name = stringr::str_replace(name, "_", " ")) |> 
#   mutate(name = stringr::str_to_title(name))

# provides us with comptroller regions, thanks to Ed Rhodes
counties <- st_read(fs::path("Data", "spatial", "Counties", "txdot-2015-county-generalized_tx", ext = "shp"))
counties

pre1999 <- read_excel("Data/TWDB_Summary_county_pre1999.xlsx") |> 
  janitor::clean_names()
post1999 <- read_excel("Data/TWDB_Summary_county_post1999xlsx.xlsx") |> 
  janitor::clean_names()

pre1999 <- pre1999 |> 
  select(year, county, source_type, irrigation) |> 
  pivot_wider(names_from = source_type, values_from = irrigation) |> 
  group_by(county, year) |> 
  mutate(Total = Groundwater + `Surface water`) |> 
  select(year, county, Total)

region_use_df <- post1999 |> 
  select(year, county, Total = irrigation) |> 
  bind_rows(pre1999) |> 
  arrange(year) |> 
  mutate(county = case_when(
    county == "DEWITT" ~ "DE WITT",
    .default = county
  )) |>
  mutate(county = stringr::str_to_title(county)) |> 
  left_join(as_tibble(counties), by = c("county" = "CNTY_NM")) |> 
  filter(REGION %in% c("High Plains", "South Texas", "Gulf Coast", "Upper Rio Grande")) |> 
  mutate(Total = as_units(Total, "acre_feet/year")) |>
  mutate(Total = set_units(Total, "1000000 acre_feet/year")) |> 
  group_by(year, REGION) |> 
  summarise(value = sum(Total, na.rm = TRUE))

total_region_use_df <- region_use_df |> 
  mutate(facets = case_when(
    REGION == "High Plains" ~ "High Plains",
    .default = "Other Regions"
  ))

total_region_use_label <- total_region_use_df |> 
  group_by(REGION) |> 
  filter(year == max(year))

total_region_use_df

ggplot(total_region_use_df) +
  geom_line(aes(year, value, color = REGION, group = REGION)) +
  geom_point(aes(year, value, color = REGION)) +
  geom_text_repel(data = total_region_use_label,
                  aes(year, value, label = REGION, color = REGION),
                  direction = "y",
                  hjust = 0,
                  nudge_x = 1,
                  min.segment.length = 3,
                  family = "Open Sans",
                  fontface = "bold",
                  size = 2.25) +
  facet_wrap(vars(facets), scales = "free_y", ncol = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous("Year", expand = expansion(mult = c(0.015, 0.25)),
                     breaks = c(seq(from = 1960,
                                    to = 2020, 
                                    by = 10))) +
  scale_y_units("Volume [Million Acre-Feet/Year]") +
  labs(title = "Regional Irrigative Water Use in Texas",
       subtitle = "Total water applied, 1974-2023",
       caption = "source: Texas Water Development Board") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain"),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

ggsave(fs::path("Figures", "Figure_3", ext = "png"), dpi = 300, width = 6.5, height = 5, units = "in")




##*****************##
##### Figure 4 ######
##*****************##

## data is from TWDB
## use is in acre-feet

pre1999 <- read_excel("Data/TWDB_Summary_county_pre1999.xlsx") |> 
  janitor::clean_names()
post1999 <- read_excel("Data/TWDB_Summary_county_post1999xlsx.xlsx") |> 
  janitor::clean_names()


region_use_df <- post1999 |> 
  select(year, county, Total = irrigation, `Surface Water` = irrigation_surface_water, `Groundwater` = irrigation_groundwater, `Reuse` = irrigation_reuse) |> 
  arrange(year) |> 
  mutate(county = case_when(
    county == "DEWITT" ~ "DE WITT",
    .default = county
  )) |>
  mutate(county = stringr::str_to_title(county)) |> 
  left_join(as_tibble(counties), by = c("county" = "CNTY_NM")) |> 
  filter(REGION %in% c("High Plains", "South Texas", "Gulf Coast", "Upper Rio Grande")) |> 
  pivot_longer(c(Total, `Surface Water`, Groundwater, Reuse)) |> 
  select(year, county, REGION, name, value) |> 
  mutate(value = as_units(value, "acre_feet/year")) |>
  mutate(value = set_units(value, "1000000 acre_feet/year")) |> 
  mutate(value = as.numeric(value)) |> 
  group_by(year, REGION, name) |> 
  summarise(value = sum(value, na.rm = TRUE)) |> 
  filter(name != "Total")

region_use_df

ggplot(region_use_df) +
  geom_col(aes(x = year, y = value, fill = name), position = "fill") +
  facet_wrap(vars(REGION)) +
  scale_y_continuous("Percent of toal irrigation", 
                     labels = scales::percent, expand = expansion(mult = 0)) +
  scale_x_continuous(NULL) +
  scale_fill_brewer(NULL, palette = "Dark2") +
  labs(title = "Regional Irrigative Water Sources in Texas",
       subtitle = "Relative proportion of water sources used for irrigation (percent)",
       caption = "source: Texas Water Development Board") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain", size = 7),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain", size = 9),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "bottom",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

# ggplot(region_use_df) +
#   geom_line(aes(year, value, color = name), size = 0.2) +
#   geom_point(aes(year, value, color = name), size = 0.5) +
#   geom_text_repel(data = region_use_labels,
#                   aes(year, value, label = name, color = name),
#                   direction = "y",
#                   hjust = 0,
#                   nudge_x = 1,
#                   min.segment.length = 3,
#                   family = "Open Sans",
#                   fontface = "bold",
#                   size = 2) +
#   facet_wrap(vars(region), scales = "free_y", ncol = 2) +
#   scale_color_brewer(palette = "Dark2") +
#   scale_x_continuous("Year", expand = expansion(mult = c(0.015, 0.35)),
#                      breaks = c(seq(from = 1960,
#                                     to = 2020, 
#                                     by = 20))) +
#   scale_y_units("Volume [Million Acre-Feet/Year]") +
#   labs(title = "Regional Irrigative Water Use in Texas",
#        subtitle = "Total regional irrigative withdrawls, 1958-2023",
#        caption = "source: Texas Water Development Board") +
#   theme_classic() +
#   theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain", size = 7),
#         axis.title.y.left = element_text(family = "Open Sans",  face = "plain", size = 7),
#         axis.text = element_text(family = "Open Sans"),
#         legend.position = "none",
#         panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
#         panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
#         plot.title = element_text(family = "Oswald", face = "bold"),
#         plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
#         plot.caption = element_text(color = "grey40"),
#         strip.background = element_rect(linewidth = 0),
#         strip.text = element_text(family = "Open Sans", face = "bold"))

ggsave(fs::path("Figures", "Figure_4", ext = "png"), dpi = 300, width = 6.5, height = 5, units = "in")



##*****************##
##### Figure 5 ######
##*****************##

## data is from TWDB and USGS
## units are 1,000 acres


## updated 2026-08-07

irrigated_acres_twdb_df <- read_csv("Data/Irrigation_Crop_Water_Use.csv") |> 
  janitor::clean_names() |> 
  mutate(acres = units::as_units(acres, "acres"),
         volume = units::as_units(volume, "acre_feet")) |> 
  group_by(yr) |> 
  summarise(acres = sum(acres, na.rm = TRUE)) |> 
  mutate(acres = units::set_units(acres, "1000000 acres"))

## this might be the best USGS source:
## https://water.usgs.gov/watuse/data/
## includes 1985 - 2015 by county



irrigated_acres_usgs_df <- read_csv("Data/usgs/county_level_water_use/usco2015v2.0.csv", 
                                    skip = 1) |> 
   select(STATE, COUNTY, YEAR, `IR-IrTot`) |> 
  janitor::clean_names() |> 
  filter(state == "TX") |> 
  group_by(state, year) |> 
  summarise(acres = sum(ir_ir_tot, na.rm  = TRUE)) |> 
  bind_rows(
    read_delim("Data/usgs/county_level_water_use/usco2010.txt", 
                       delim = "\t", escape_double = FALSE, 
                       trim_ws = TRUE) |> 
  select(STATE, COUNTY, YEAR, `IR-IrTot`) |> 
  janitor::clean_names() |> 
  filter(state == "TX") |> 
  group_by(state, year) |> 
  summarise(acres = sum(ir_ir_tot, na.rm  = TRUE))
  ) |> 
  bind_rows(
    read_delim("Data/usgs/county_level_water_use/usco2005.txt", 
               delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE) |> 
      select(STATE, `IR-IrTot`) |> 
      janitor::clean_names() |> 
      filter(state == "TX") |> 
      group_by(state) |> 
      summarise(acres = sum(ir_ir_tot, na.rm  = TRUE)) |> 
      mutate(year = 2005)
    ) |> 
  bind_rows(
    read_delim("Data/usgs/county_level_water_use/usco2000.txt", 
               delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE) |>
      select(STATE, `IT-IrTot`) |> 
      janitor::clean_names() |> 
      filter(state == "TX") |> 
      group_by(state) |> 
      summarise(acres = sum(it_ir_tot, na.rm  = TRUE)) |> 
      mutate(year = 2000)
  ) |> 
  bind_rows(
    read_delim("Data/usgs/county_level_water_use/us95co.txt", 
               delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE) |>
      select(State, `IR-IrTot`, Year) |> 
      janitor::clean_names() |> 
      filter(state == "TX") |> 
      group_by(state, year) |> 
      summarise(acres = sum(ir_ir_tot, na.rm  = TRUE)) 
  ) |> 
  bind_rows(
    read_delim("Data/usgs/county_level_water_use/us85co.txt", 
               delim = "\t", escape_double = FALSE, 
               trim_ws = TRUE) |>
      select(state, `ir-irrig`, year) |> 
      janitor::clean_names() |> 
      filter(state == "TX") |> 
      group_by(state, year) |> 
      summarise(acres = sum(ir_irrig, na.rm  = TRUE)) 
  )

irrigated_acres_usgs_df <- irrigated_acres_usgs_df |> 
  ungroup() |> 
  mutate(acres = units::as_units(acres, "1000 acres")) |> 
  mutate(acres = units::set_units(acres, "1000000 acres")) |> 
  rename(yr = year) |> 
  select(-c(state))

irrigated_acres_df <- irrigated_acres_twdb_df |> 
  mutate(name = "TWDB Estimate") |> 
  bind_rows(irrigated_acres_usgs_df |> 
              mutate(name = "USGS Estimate"))
  

irrigated_acres_labels <- irrigated_acres_df |> 
  filter(!is.na(acres)) |> 
  group_by(name) |> 
  filter(yr == min(yr))

ggplot(irrigated_acres_df) +
  geom_col(aes(yr, acres, fill = name), position = position_dodge2(padding = 0, 
                                                                   preserve = "single"),
           width = 1) +
  geom_text_repel(data = irrigated_acres_labels,
                  aes(yr, acres, color = name, label = name),
                  force_pull   = 0, # do not pull toward data points
                  hjust = 0,
                  direction = "x",
                  angle = 45,
                  family = "Open Sans",
                  fontface = "bold",
                  size = 2,
                  min.segment.length = 2,
                  max.iter = 1e4, max.time = 0.1,
                  position = position_dodge2(width = 1,
                                             preserve = "single")) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous("Year", 
                     breaks = c(seq(from = 1985,
                                    to = 2025, 
                                    by = 5))) +
  scale_y_units("Million Acres", expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Irrigated Agricultural Acreage in Texas",
       subtitle = "Estimated total irrigated acres",
       caption = "source: Texas Water Development Board and\nUnited States Geological Survey") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain"),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40", size = 6),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

ggsave(fs::path("Figures", "Figure_5", ext = "png"), dpi = 300, width = 6.5, height = 4, units = "in")


##*****************##
##### Figure 6 ######
##*****************##

## data is from TWDB
## units are acres and acre-feet




crop_use_df <- read_csv("Data/crop_use.csv") |> 
  janitor::clean_names() |> 
  select(-c(column1, column2)) |> 
  mutate(crop_acreage = units::as_units(crop_acreage, "acres"),
         water_use_in_acft = units::as_units(water_use_in_acft, "acre_feet"))
crop_use_df <- crop_use_df |> 
  group_by(crop_name, year) |> 
  summarise(acres = sum(crop_acreage, na.rm = TRUE),
            water_use = sum(water_use_in_acft, na.rm = TRUE)) |> 
  mutate(acres = set_units(acres, "1000000 acres"),
         water_use = set_units(water_use, "1000000 acre_feet"),
         crop_name = stringr::str_to_sentence(crop_name))

crop_use_df_labels <- crop_use_df |> 
  group_by(crop_name) |> 
  filter(year == max(year))

ggplot(crop_use_df |> 
         filter(year >= 2000)) +
  geom_ribbon(data = irrigated_acres_df |> 
                filter(name == "TWDB", Year >= 2000) |>  
                mutate(ymin = as_units(0, "1000000 acres")),
            aes(x = Year, ymax = acres, ymin = as_units(0, "acres")), fill = "grey", alpha = 0.5) +
  geom_line(aes(year, acres, color = crop_name)) +
  geom_point(aes(year, acres, color = crop_name)) +
  geom_text_repel(data = crop_use_df_labels,
                  aes(year, acres, color = crop_name, label = crop_name),
                  direction = "y",
                  hjust = 0,
                  nudge_x = .5,
                  min.segment.length = 3,
                  family = "Open Sans",
                  fontface = "bold",
                  size = 3) +
  annotate(x = 2022.5, y = as_units(6, "1000000 acres"), label = "Grey area = Irrigated acreage for all crops",
           geom = "text", vjust = 0, hjust = 1, family = "Open Sans", color = "grey40", fontface = "bold",
           size = 3) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous("Year", 
                     breaks = c(seq(from = 1960,
                                    to = 2020, 
                                    by = 5)),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_units("Million Acres", expand = expansion(mult = c(0, 0.02)),
                unit = "1000000 acres", breaks = c(1,2,3,4,5,6)) +
  labs(title = "Irrigated Corn and Cotton Acreage in Texas",
       subtitle = "Total irrigated acres, 2000-2023",
       caption = "source: Texas Water Development Board") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain"),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))


ggsave(fs::path("Figures", "Figure_7", ext = "png"), dpi = 300, width = 6.5, height = 4, units = "in")


crop_use_df <- read_csv("Data/Irrigation_Crop_Water_Use.csv") |> 
  janitor::clean_names() |> 
  mutate(acres = units::as_units(acres, "acres"),
         volume = units::as_units(volume, "acre_feet")) |> 
  filter(yr <= 2023)



crop_use_df_top_three <- crop_use_df |> 
  group_by(crop_name, yr) |> 
  summarise(acres = sum(acres, na.rm = TRUE),
            volume = sum(volume, na.rm = TRUE)) |> 
  mutate(acres = set_units(acres, "1000000 acres"),
         volume = set_units(volume, "1000000 acre_feet"),
         crop_name = stringr::str_to_sentence(crop_name)) |> 
  filter(crop_name %in% c("Cotton", "Corn", "Wheat"))


crop_use_df_labels <- crop_use_df_top_three |> 
  group_by(crop_name) |> 
  filter(yr == max(yr))

crop_use_total <- crop_use_df |> 
  group_by(yr) |> 
  summarise(tota_area = sum(acres, na.rm = TRUE)) |> 
  filter(yr <= 2023)

ggplot(crop_use_df_top_three) +
  geom_ribbon(data = crop_use_total,
              aes(yr, ymax = tota_area, ymin = as_units(0, "1000000 acres")),
              fill = "grey", alpha = 0.5) +
  geom_line(aes(yr, acres, color = crop_name)) +
  geom_point(aes(yr, acres, color = crop_name)) +
  geom_text_repel(data = crop_use_df_labels,
                  aes(yr, acres, color = crop_name, label = crop_name),
                  direction = "y",
                  hjust = 0,
                  nudge_x = .5,
                  min.segment.length = 3,
                  family = "Open Sans",
                  fontface = "bold",
                  size = 3) +
  annotate(x = 2022.5, y = as_units(5, "1000000 acres"), label = "Grey area = TWDB estimated irrigated\nacreage for all crops",
           geom = "text", vjust = 0, hjust = 1, family = "Open Sans", color = "grey40", fontface = "bold",
           size = 3) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous("Year", 
                     breaks = c(seq(from = 1960,
                                    to = 2020, 
                                    by = 5)),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_y_units("Million Acres", expand = expansion(mult = c(0, 0.02)),
                unit = "1000000 acres", breaks = c(1,2,3,4,5,6)) +
  labs(title = "Irrigated Corn, Cotton, and Wheat Acreage in Texas",
       subtitle = "Total irrigated acres, 1985-2023",
       caption = "source: Texas Water Development Board") +
  theme_classic() +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain"),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "none",
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

ggsave(fs::path("Figures", "Figure_7", ext = "png"), dpi = 300, width = 6.5, height = 4, units = "in")


library(waffle)

major_crop <- crop_use_df |> 
  filter(yr == 2023) |> 
  mutate(acres = set_units(acres, "acres")) |> 
  mutate(acres = as.numeric(acres)/100000) |>
  ungroup() |> 
  mutate(crop = case_when(
    acres < 3 ~ "Other crops",
    .default = crop_name
  )) |> 
  group_by(crop) |> 
  summarise(acres = sum(acres))

ggplot(major_crop) +
  geom_waffle(aes(fill = crop, values = acres),
              color = "white",
              size = 1, 
              n_rows = 4) +
  scale_x_discrete(
    expand = c(0,0,0,0)
  ) +
  scale_y_discrete(
    expand = c(0,0,0,0)
  ) +
  theme_minimal() +
  scale_fill_ordinal("", option = "F") +
  labs(title = "Irrigated Crop Acres in Texas",
       subtitle = "Each square ~ 100k acres, Year = 2023",
       caption = "source: Texas Water Development Board") +
  theme(axis.title.x.bottom = element_text(family = "Open Sans", face = "plain"),
        axis.title.y.left = element_text(family = "Open Sans",  face = "plain"),
        axis.text = element_text(family = "Open Sans"),
        legend.position = "bottom",
        legend.text = element_text(family = "Open Sans"),
        panel.grid.major.x = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.2, linetype = "dashed"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))


ggsave(fs::path("Figures", "waffle_crop", ext = "png"), dpi = 300, width = 6.25, height = 3.25, units = "in")
