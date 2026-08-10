library(dplyr)
library(ggplot2)
library(ggrepel)
library(janitor)
library(readr)
library(tidyr)
library(units)
library(systemfonts)
library(ragg)
library(sf)
library(patchwork)
library(shadowtext)
library(broom)
library(purrr)

counties <- st_read(fs::path("Data", "spatial", "Counties", "txdot-2015-county-generalized_tx", ext = "shp"))

counties <- st_transform(counties, "epsg:3083")


labs <- counties |> 
  group_by(REGION) |> 
  summarize()



ggplot(counties) +
  geom_sf(aes(fill = REGION), alpha = 0.7, color = "grey") +
  geom_sf_text(data = labs, aes(label = REGION),
               family = "Oswald", fontface = "bold",
               color = "grey20", size = 3) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Regions",
       subtitle = "based on Texas Comptroller Economic Regions") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40"),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))


ggsave(fs::path("Figures", "regions", ext = "png"), dpi = 300, width = 4, height = 4, units = "in")


# Change in irrigation volume map
library(readxl)
pre1999 <- read_excel("Data/TWDB_Summary_county_pre1999.xlsx") |> 
  janitor::clean_names()
post1999 <- read_excel("Data/TWDB_Summary_county_post1999xlsx.xlsx") |> 
  janitor::clean_names()

change <- pre1999 |> 
  select(year, county, source_type, irrigation) |> 
  group_by(county, year) |> 
  summarize(irrigation = sum(irrigation)) |> 
  bind_rows(post1999 |> 
   select(year, county, irrigation)) |> 
  filter(year %in% c(1974, 2023)) |> 
  pivot_wider(names_from = year, values_from = irrigation) |> 
  mutate(change = `2023` - `1974`) |> 
  mutate(county = case_when(
    county == "DEWITT" ~ "DE WITT",
    .default = county
  )) |>
  mutate(county = stringr::str_to_title(county))

change <- counties |> 
  mutate(join_column = stringr::str_to_title(CNTY_NM)) |> 
  left_join(change, by = c("join_column" = "county"))

county_irrigation_volume_label <- change |> 
  filter(`2023` == max(`2023`))



slopes <- pre1999 |> 
  select(year, county, source_type, irrigation) |> 
  group_by(county, year) |> 
  summarize(irrigation = sum(irrigation)) |> 
  bind_rows(post1999 |> 
              select(year, county, irrigation)) |>
  group_by(county) |> 
  filter(!is.na(county)) |> 
  nest() |> 
  mutate(model = purrr::map(data,
                            ~lm(irrigation~as.numeric(scale(year, scale = FALSE)), data = .x)),
         tidied = map(model, tidy)) |> 
  unnest(tidied) |> 
  mutate(term = case_when(term == "as.numeric(scale(year, scale = FALSE))" ~ "Beta1",
                          .default = term)) |> 
  filter(term == "Beta1") |> 
  mutate(county = case_when(
    county == "DEWITT" ~ "DE WITT",
    .default = county
  )) |>
  mutate(county = stringr::str_to_title(county))

slopes <- counties |> 
  mutate(join_column = stringr::str_to_title(CNTY_NM)) |> 
  left_join(slopes, by = c("join_column" = "county"))

slope_labels <- slopes |> 
  select(CNTY_NM, estimate) |> 
  filter(min_rank(estimate) <= 2 | min_rank(desc(estimate)) <= 2) |> 
  mutate(label = paste0(CNTY_NM, " County:\n", format(estimate, scientific = FALSE, big.mark = ",", digits = 1), " ac-ft/year"))
  #filter(estimate == max(estimate, na.rm = TRUE)| estimate == min(estimate, na.rm = TRUE)) 

p1 <- ggplot(slopes) +
  geom_sf(aes(fill = estimate/1000)) +
  geom_sf(data = labs, fill = NA, size = 0.75, color = "grey10") +
  geom_text_repel(data = slope_labels,
                  aes(label = label,
                      color = estimate/1000,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  box.padding = 1,
                  family = "Open Sans", fontface = "bold", hjust = 0.5, vjust = 0.5,
                  xlim = c(-Inf, Inf), ylim = c(-Inf, NA), direction = "both",
                  segment.color = "black",
                  arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
                  bg.color = alpha("grey95",0.5), 
                  bg.r = 0.25,size = 2.5,
                  seed = 100) +
  scale_fill_distiller(
    "Thousand\nAcre-Feet/year",
    type = "div",
    palette = "BrBG",
    direction = 1,
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-6, 6),
    breaks = seq(from = -6, to = 6, by = 2),
    labels = seq(from = -6, to = 6, by = 2),
    oob = scales::squish
  ) +
  scale_color_distiller(
    "Thousand\nAcre-Feet/year",
    type = "div",
    palette = "BrBG",
    direction = 1,
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-1, 1),
    breaks = seq(from = -6, to = 6, by = 2),
    labels = seq(from = -6, to = 6, by = 2),
    oob = scales::squish,
    guide = NULL
  ) +
  labs(title = "Change in irrigated agriculture",
       subtitle = "Average annual irrigation change (acre-feet, 1974-2023)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Open Sans", size = 8,
                                    margin = margin(t = 0, r = 0, b = 0, l = 16, unit = "pt")),
        legend.title.position = "right",
        legend.text = element_text(family = "Open Sans", size = 7),
        legend.key.width = unit(2, "lines"),
        legend.key.height = unit(0.75, "lines"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40", size = 8),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

p1

p2 <- ggplot(change) +
  geom_sf(aes(fill = `2023`/1000)) +
  geom_sf(data = labs, fill = NA, size = 0.75, color = "grey10") +
  geom_shadowtext(data = labs,
                  aes(label  = REGION,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  family = "Open Sans", fontface = "bold",
                  color = "grey25", bg.color = alpha("grey95",0.5), 
                  bg.r = 0.25,size = 2.5) +
  geom_text_repel(data = county_irrigation_volume_label,
                  aes(label = paste0(CNTY_NM, " County:\n", format(`2023`, scientific = FALSE, big.mark = ","), " acre-feet"),
                      geometry = geometry),
                  stat = "sf_coordinates",
                  family = "Open Sans", size = 2.5, hjust = 0.5, vjust = 0.5,
                  nudge_x = -305000) +
  scale_fill_distiller(
    "Thousand\nAcre-Feet",
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 400),
    breaks = seq(from = 0, to = 400, by = 100),
    labels = seq(from = 0, to = 400, by = 100),
    oob = scales::squish
  ) +
  labs(title = "Irrigated agriculture",
       subtitle = "Volume applied (acre-feet) in 2023") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Open Sans", size = 8,
                                    margin = margin(t = 0, r = 0, b = 0, l = 16, unit = "pt")),
        legend.title.position = "right",
        legend.text = element_text(family = "Open Sans", size = 7),
        legend.key.width = unit(2, "lines"),
        legend.key.height = unit(0.75, "lines"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40", size = 8),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))


p2 + p1
ggsave(fs::path("Figures", "Figure_2", ext = "png"), dpi = 300, width = 6.5, height = 4.5, units = "in")



## acreage maps
crop_use_df <- read_csv("Data/Irrigation_Crop_Water_Use.csv") |> 
  janitor::clean_names() |> 
  mutate(county_name = case_when(
    county_name == "DEWITT" ~ "DE WITT",
    .default = county_name
  )) |>
  mutate(county_name = stringr::str_to_title(county_name))

county_acreage <- crop_use_df |> 
  group_by(yr, county_name) |> 
  summarize(acres = sum(acres, na.rm = TRUE)) |> 
  filter(yr == 2023) 

county_acreage <- counties |> 
  left_join(county_acreage, by = c("CNTY_NM" = "county_name"))

county_acreage_labels <- county_acreage |> 
  filter(min_rank(desc(acres)) <= 2) |> 
  mutate(label = paste0(CNTY_NM, " County:\n", format(acres, scientific = FALSE, big.mark = ",", digits = 1), " acres"))
county_acreage_labels


county_acreage_slopes <- crop_use_df |> 
  select(-c(volume)) |> 
  group_by(yr, county_name) |> 
  summarise(acres = sum(acres, na.rm = TRUE)) |> 
  filter(!is.na(county_name)) |> 
  ungroup() |> 
  group_by(county_name) |> 
  nest() |> 
  mutate(model = purrr::map(data,
                            ~lm(acres~yr, data = .x)),
         tidied = map(model, tidy)) |> 
  unnest(tidied) |> 
  filter(term == "yr") |> 
  select(county_name, term, estimate)

county_acreage_slopes <- counties |> 
  left_join(county_acreage_slopes, by = c("CNTY_NM" = "county_name"))

slope_labels <- county_acreage_slopes |> 
  select(CNTY_NM, estimate) |> 
  filter(min_rank(estimate) <= 2 | min_rank(desc(estimate)) <= 2) |> 
  mutate(label = paste0(CNTY_NM, " County:\n", format(estimate, scientific = FALSE, big.mark = ",", digits = 1), " ac/year"))


p1 <- ggplot(county_acreage) +
  geom_sf(aes(fill = acres/1000, alpha = is.na(acres))) +
  geom_sf(data = labs, fill = NA, size = 1, color = "grey1") +
  geom_text_repel(data = county_acreage_labels,
                  aes(label = label,
                      color = acres/1000,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  box.padding = 1,
                  family = "Open Sans", fontface = "bold", hjust = 0.5, vjust = 0.5,
                  xlim = c(-Inf, Inf), ylim = c(NA, NA), direction = "both",
                  segment.color = "black",
                  arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
                  bg.color = alpha("grey95",0.5), 
                  bg.r = 0.25,size = 2.5,
                  seed = 100) +
  scale_fill_distiller(
    "Thousand\nAcres",
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 400),
    breaks = seq(from = 0, to = 400, by = 100),
    labels = seq(from = 0, to = 400, by = 100),
    oob = scales::squish,
    na.value = alpha("grey", 50)
  ) +
  scale_color_distiller(
    "Thousand\nAcres",
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 400),
    breaks = seq(from = 0, to = 400, by = 100),
    labels = seq(from = 0, to = 400, by = 100),
    oob = scales::squish,
    na.value = alpha("grey", 50),
    guide = NULL
  ) +
  scale_alpha_manual(
    name = NULL,
    values = c("TRUE" = 1, "FALSE" = 1),       # Keep both NA and non-NA fully visible
    breaks = "TRUE",                           # Only show the "TRUE" (is.na) key in legend
    labels = "Missing Data",                    # Your legend label for NAs
    guide = guide_legend(
      override.aes = list(
        fill = alpha("grey", 50),                       # Match your scale's na.value color
        color = "black",                      # Matches your plot tile border
        alpha = 1
      )
    )
  ) +
  labs(title = "Area of irrigated agriculture",
       subtitle = "acres in 2023") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Open Sans", size = 8,
                                    margin = margin(t = 0, r = 0, b = 0, l = 16, unit = "pt")),
        legend.title.position = "right",
        legend.text = element_text(family = "Open Sans"),
        legend.key.width = unit(1, "lines"),
        legend.key.height = unit(0.5, "lines"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40", size = 8),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

p1
p2 <- ggplot(county_acreage_slopes) +
  geom_sf(aes(fill = estimate/1000)) +
  geom_sf(data = labs, fill = NA, size = 1, color = "grey1") +
  geom_text_repel(data = slope_labels,
                  aes(label = label,
                      color = estimate/1000,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  box.padding = 1,
                  family = "Open Sans", fontface = "bold", hjust = 0.5, vjust = 0.5,
                  xlim = c(-Inf, Inf), ylim = c(NA, NA), direction = "both",
                  segment.color = "black",
                  arrow = arrow(length = unit(0.01, "npc"), type = "closed"),
                  bg.color = alpha("grey95",0.5), 
                  bg.r = 0.25,size = 2.5,
                  seed = 100) +
  scale_fill_distiller(
    "Thousand\nAcres",
    type = "div",
    palette = "BrBG",
    direction = 1,
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-8.5, 6),
    breaks = seq(from = -8, to = 6, by = 2),
    labels = seq(from = -8, to = 6, by = 2),
    oob = scales::squish,
    na.value = alpha("grey", 50)
  ) +
  scale_color_distiller(
    "Thousand\nAcres",
    type = "div",
    palette = "BrBG",
    direction = 1,
    rescaler = ~ scales::rescale_mid(.x, mid = 0),
    limits = c(-3, 3),
    breaks = seq(from = -3, to = 3, by = 1),
    labels = seq(from = -3, to = 3, by = 1),
    oob = scales::squish,
    guide = NULL
  ) +
  labs(title = "Change in irrigated agriculture acreage",
       subtitle = "Average annual change (acres, 1985-2023)") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(family = "Open Sans", size = 8,
                                    margin = margin(t = 0, r = 0, b = 0, l = 16, unit = "pt")),
        legend.title.position = "right",
        legend.text = element_text(family = "Open Sans"),
        legend.key.width = unit(1.75, "lines"),
        legend.key.height = unit(0.5, "lines"),
        plot.title = element_text(family = "Oswald", face = "bold"),
        plot.subtitle = element_text(family = "Open Sans", face = "italic", color = "grey40", size = 8),
        plot.caption = element_text(color = "grey40"),
        strip.background = element_rect(linewidth = 0),
        strip.text = element_text(family = "Open Sans", face = "bold"))

p1 + p2
ggsave(fs::path("Figures", "Figure_6", ext = "png"), dpi = 300, width = 6.5, height = 4.5, units = "in")
