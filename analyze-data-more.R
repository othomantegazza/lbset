library(tidyverse)
library(janitor)
library(ggrepel)
library(ggvoronoi)


# get the data ------------------------------------------------------------

park_visit_url <- paste0("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/",
                         "data/2019/2019-09-17/All%20National%20Parks%20Visitation%201904-2016.csv")

# before running this, make a folder named data in your working directory
park_visit_path <- "data/2-38-park-visits.Rdata"


# Check if data have already been downloaded,
# If not, read data from github and saves them locally
# YOU MUST MAKE a new FOLDER NAMED DATA
if(!file.exists(park_visit_path)) {
  visits <- 
    park_visit_url %>% 
    read_csv() %>% 
    janitor::clean_names()
  
  save(visits, file = park_visit_path)
} else {
  load(park_visit_path)
}


# tidy -----------------------------------------------------------------

# year_raw is formatted as character
visits %>% 
  pull(year_raw) %>% 
  unique()



# any NA?
visits %>% 
  map(~is.na(.) %>% sum())
# the column parkname has a lot of NA and can be removed
# redundant with unit_name ?


# tidy year raw and remove year column
# with declarative sintax
visits2 <- 
  visits %>% 
  filter(year_raw != "Total") %>% 
  mutate(year_raw = as.numeric(year_raw)) %>% 
  select(-year, parkname)


# explore -----------------------------------------------------------------

# how many parks
visits2 %>%
  pull(unit_name) %>% 
  unique()

# or
visits2 %>% 
  count(unit_name, sort = TRUE)

p_lines <- 
  visits2 %>% 
  ggplot(aes(x = year_raw,
             y = visitors)) +
  geom_line(aes(group = unit_code),
            alpha = .2)
# scale_y_log10()

p_lines

visits_top <- 
  visits2 %>%
  filter(year_raw == max(year_raw)) %>% 
  filter(visitors > quantile(visitors, .99))


p_lines2 <- 
  p_lines + 
  geom_line(data = . %>% 
              filter(unit_code %in% visits_top$unit_code),
            aes(colour = unit_code,
                group = unit_code),
            size = 1) +
  geom_label_repel(data = visits_top,
                   aes(label = unit_name %>% str_wrap(width = 20),
                       colour = unit_code),
                   xlim = c(2020, NA),
                   size = 3) +
  lims(x = c(NA, 2050)) +
  guides(colour = FALSE) +
  theme_minimal()  


p_lines2



# on a map ----------------------------------------------------------------

library(sf)
library(geojsonsf)
library(grid)

# read_map_data -----------------------------------------------------------

# park_maps <- geojson_sf("https://opendata.arcgis.com/datasets/6042ea0d29894cc4a694d34b5812b4a1_0.geojson")

# map_file <- "data/2-38-park-visits.zip"
# 
# download.file(url = "https://opendata.arcgis.com/datasets/6042ea0d29894cc4a694d34b5812b4a1_0.zip",
#               destfile = map_file)
# map_file <- unzip(map_file, overwrite = T)
# 
# read_sf(temp %>% unzip())

centroid_url <- "https://opendata.arcgis.com/datasets/c54be84491364a04a0caecc837ab492a_0.csv"

centroid_path <- "data/2-38-park-centroids.Rdata"

if(!file.exists(centroid_path)) {
  centroid_parks <-
    read_csv(centroid_url) %>% 
    clean_names()
  
  save(centroid_parks, file = centroid_path)
} else {
  load(centroid_path)
}

visits_centroid <- 
  visits2 %>% 
  group_by(unit_code, unit_name) %>% 
  summarise(visitors = sum(visitors)) %>% 
  left_join(centroid_parks, by = "unit_code") %>% 
  distinct(x, y, .keep_all = T)

usa <- map_data("usa")

bg_color <- "#F1F3F4"

p <- 
  visits_centroid %>%
  filter(y > 25,
         y < 50,
         x < -70,
         x > -140) %>%
  # .[keep, ] %>% 
  ggplot(aes(x = x,
             y = y)) + 
  # geom_point() +
  ggvoronoi::geom_voronoi(aes(fill = visitors),
                          colour = bg_color,
                          size = .2,
                          # alpha = .5,
                          outline = usa) + 
  geom_point(colour = "white",
             size = .2) +
  scale_fill_viridis_c(trans = "log10",
                       breaks = c(1e4, 1e5, 1e6, 1e7, 1e8),
                       guide = guide_legend(nrow = 1,
                                            label.position = "bottom", title.position = "top",
                                            keywidth = unit(6, units = "mm"),
                                            keyheight = unit(1.2, units = "mm"))) +
  lims(x = c(-125, -45)) +
  coord_map() +
  theme_void(base_size = 14, base_family = "courier") +
  theme(legend.position = c(.95, .7),
        plot.margin = margin(10, 30, 10, 0, unit = "mm"))

svglite::svglite("plots/us-voronoi.svg",
                 width = 12,
                 height = 5)
grid.newpage()
grid.rect(gp = gpar(fill = bg_color, col = bg_color))
p %>% print(vp = viewport())
grid.text(label = "Voronoi grid of US Natural Parks",
          x = .8, y = .3, 
          gp = gpar(fontfamily = "courier",
                    fontface = "bold",
                    fontsize = 15))
grid.text(label = "With total visitors from 1900 until now",
          x = .8, y = .25, 
          gp = gpar(fontfamily = "courier",
                    fontsize = 8))
grid.text(label = "Data from data.world | Plot by @othomn",
          x = .99, y = .03, 
          hjust = 1,
          gp = gpar(fontfamily = "courier",
                    # fontface = "bold",
                    fontsize = 8))
dev.off()


