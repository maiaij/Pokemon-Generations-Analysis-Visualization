# Analysis on All Generations of Pokemon 
# Maia Johnson
# 100739773
# April 2nd, 2023


# Pokemon is a show that originated in Japan and gained popularity around the world.
# The selected dataset includes the stats for pokemon from pokedex index 1 - 890.
# The dataset was retrieved from: https://www.kaggle.com/datasets/igorcoelho24/pokemon-all-generations
# I have also put all data files in a Google Drive folder in case you require them:
# https://drive.google.com/drive/folders/11M2sLPrME8YQ-k-lAWA41UKnUO6sfNyu?usp=sharing

map_packages <- c("sf", "maps", "mapdata")
install.packages(map_packages)

library(tidyverse)
library(readxl)
library(maps)
library(mapdata)
library(ggthemes)
library(sf)

# The first graphic is of the population under 15 years on in different Prefectures of Japan.
# Since Pokemon is geared towards children in this age range, this map graphic seemed
# fitting. The population statistics were retrieved from Japan's stat.go site:
# https://www.e-stat.go.jp/en/stat-search/files?stat_infid=000040019815

jap_data <- st_as_sf(map('japan', plot = FALSE, fill = TRUE, group = TRUE)) |> rename ('Prefecture' = 'ID')
pref_data <- read_xlsx('Japan_Prefecture_Population_2021.xlsx')
full_japan_data <- left_join(jap_data, pref_data, join_by(Prefecture)) |> as_tibble()

jap_pop_map <- ggplot() +
  geom_sf(data = jap_data, aes(fill = full_japan_data$Pop_u_15), color = 'white') +
  scale_fill_gradient2(low = scales::muted('white'),
                       mid = 'seashell',
                       high = scales::muted('pink'),
                       midpoint = median(full_japan_data$Pop_u_15),
                       name = 'Population') +
  coord_sf(crs = st_crs(4326)) + 
  labs(fill = 'Population Under 15 Yrs Old',
       title = 'Population of Youth Under 15 Across Japan') + 
  theme_map() +
  theme(legend.position = c(1.02, 0.05),
        panel.background = element_rect(fill = 'gray90'),
        plot.title = element_text(size=16, face = "bold.italic", colour = "black", hjust = 0),
        legend.title = element_text(face="bold"))

jap_pop_map

# The rest of the graphics will involve data from the Pokemon datasets. The first
# graphic will be making a comparison between the type, speed, and weight of different
# Pokemon. By comparing these 3 things you can see that: on average more Pokemon weigh
# more than 500, and the speed and weights of Pokemon are not directly related
pokemon_data <- read.csv('Pokemon_full.csv') |> rename ('secondary_type' = 'secundary.type')
tsw <- select(pokemon_data, type, speed, weight)

final_table1 <- mutate(tsw %>% group_by(type) %>% summarise(avg_speed = mean(speed, na.rm=TRUE), 
                                                           avg_weight = mean(weight, na.rm=TRUE)),
                      over_500 = if_else(avg_weight > 500, '>500', '<500'))

poke_speed <- ggplot(final_table1, aes(x = type, y = avg_speed, fill = factor(over_500))) +
  geom_bar(stat = 'identity') +
  labs(x = 'Pokemon Type', y = 'Speed', 
       colour = 'Weight',
       title = 'Pokemon Speeds in Comparison with Type and Weight',
       fill = 'Weight') +
  theme_bw() + 
  theme(
    plot.title = element_text(size=16, face = "bold.italic", colour = "black", hjust = 0),
    legend.title = element_text(face="bold"),
    axis.text.x.bottom = element_text(face = "bold", angle = -60, margin = margin(b=10))
  )

poke_speed

# The next graphic will display the amount of dragon Pokemon that have a 
# secondary type and what those types are. This will be done in the form of a pie chart.
# This graphic shows that the largest percentage of dragon type Pokemon have no
# secondary type and the rarest secondary types are: electric, fire, and, ice
poke_types <- select(pokemon_data %>% filter(type=='dragon'), type, secondary_type)
data1 <- mutate(poke_types %>% group_by(secondary_type) %>% summarise(total = n()))
final_table2 = tibble(Secondary_Types = data1$secondary_type, 
                       Amounts = data1$total ) |> mutate(Amounts_scaled = Amounts/sum(Amounts)*100,
          Secondary_Types = factor(Secondary_Types, levels = (Secondary_Types))) |>
  mutate(pal_alpha = alpha("paleturquoise3", seq(1, 0.2, length = n())))

pal_alpha <- scales::alpha("paleturquoise3", seq(1, 0.2, length = nrow(final_table2)))

dragon_types <- ggplot(final_table2, aes(x = '', y = Amounts_scaled, fill = Secondary_Types)) +
  geom_bar(width = 1, stat = "identity", color = "seashell", linewidth = 0.5) + 
  coord_polar(theta ="y") +
  xlab(NULL) + 
  ylab(NULL) +
  scale_fill_manual(values = final_table2$pal_alpha) +
  labs(title = 'Dragon Type Pokemon\'s Secondary Types',
       fill = 'Secondary Type') +
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold.italic", colour = "black", hjust = 0),
    legend.title = element_text(face="bold"))
  
dragon_types
