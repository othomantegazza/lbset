
# install.packages("tidyverse")


# load packages -----------------------------------------------------------

library(tidyverse) # Tidyverse: a collection of packages for data science


# download data -----------------------------------------------------------

# and assign them to a variable  called visits

# The  read_csv function is from the package readr

visits <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/All%20National%20Parks%20Visitation%201904-2016.csv")



# clean -------------------------------------------------------------------

# we can use functions from the dplyr package to explopre and clean the data

# year_raw is formatted as character
visits %>% 
  pull(YearRaw) %>% 
  unique()


# The code below:
# - filters (removes) the record in which the YearRaw is "total"
# - converts the YearRaw column to a numeric one
# - removes the year and parkname columns,
#      which are poorly formatted and redundant with other columsn
visits_clean <- 
  visits %>% 
  filter(YearRaw != "Total") %>% 
  mutate(YearRaw = as.numeric(YearRaw)) %>% 
  select(-Year, Parkname)

# This is how you would do the operations above in base R
# filtering in base R
visits_base <- visits[visits$YearRaw != "Total", ]
visits_base$Year <- NULL
visits_base$Parkname <- NULL
visits_base$YearRaw <- as.numeric(visits_base$YearRaw)

# plot --------------------------------------------------------------------

# we can use the package ggplot2 to make exploratory plots

# a boxplot----------------------------------------------------------------
p_box <- 
  visits_clean %>% 
  ggplot(aes(x = YearRaw,
             y = Visitors,
             group = YearRaw)) +
  geom_boxplot(fill = "#68DDFF")

# print the plot on the screen
p_box

# with log y axis
p_box +
  scale_y_log10()

# scatter plot  ----------------------------------------------------------

p_points <-
  visits_clean %>% 
  ggplot(aes(x = YearRaw,
             y = Visitors)) +
  theme_bw()

# the plot is empty if I don't call any geom
p_points

# we can add the points
p_points2 <- 
  p_points +
  geom_point(aes(colour = Region),
             alpha = .7)

# print on the screen
p_points2


# and a smoothed trend
# with a log  y  axis
p_points_smooth <- 
  p_points +
  geom_point(alpha = .3, colour = "grey70") +
  geom_smooth(aes(colour = Region)) +
  scale_y_log10()

p_points_smooth

# and you can also add facets
p_points_smooth + 
  facet_wrap(facets = "Region")


# last --------------------------------------------------------------------

# a bit of manipulation with dplyr
# to make a faceted bar plot of total visitor per region by year

visits_by_region <- 
  visits_clean %>% 
  group_by(YearRaw, Region) %>% 
  summarize(Visitors = sum(Visitors))

p_bars <- 
  visits_by_region %>% 
  ggplot(aes(x = reorder(Region, Visitors),
             y = Visitors,
             fill = Visitors)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c()

p_bars

# The plot looks rather dumb before we add facets
p_bars_facets <- 
  p_bars +
  facet_wrap(facets = "YearRaw")

p_bars_facets

# fix the x text and label
p_bars_facets +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = .5)) +
  labs(x = " Region",
       title = "US National Park Visitors by Year and Region") 
