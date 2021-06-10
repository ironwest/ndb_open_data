library(tidyverse)
library(sf)

dat <- sf::read_sf("data/map_data")

dat2 <- dat %>% filter(admin == "Japan")
  
write_rds(dat2, file = file.path("apps","data","processed","map.rds"), compress="gz")

ggplot(dat2) +
  geom_sf()
