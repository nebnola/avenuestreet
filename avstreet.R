require("tidyr")
require("dplyr")
require("ggplot2")
require("httr")


OVERPASS_URL <- "https://overpass-api.de/api/interpreter"

get.streets <- function(city.name, street.name, bbox = '', overpass.url){
  # Get all ways in a city which mach the regex specified by street.name
  # bbox can be specified to only include ways inside a bounding box in the
  # format (south, west, east, north) (useful to deal with multiple cities of
  # the same name)
  # if overpass.url is given, use this Overpass server
  
  body <- '[out:json];
  area[name="%s"];
  way[highway][name~"%s",i](area)%s;
  out ids geom;'
  body <- sprintf(body, city.name, street.name, bbox)
  
  if(missing(overpass.url)){
    overpass.url <- OVERPASS_URL
  }
  # make request:
  re <- httr::POST(overpass.url, body = body)
  if(re$status_code != 200){
    stop('Got status code ', re$status_code)
  }
  cont <- httr::content(re, as = 'parsed')
  
  # now convert the horrible nested list into a usable data frame without looping...
  
  # flatten list
  flattened <- unlist(cont$elements)
  streets <- data.frame(key = names(flattened), value = flattened) %>%
    separate(key, into=c("key", "subkey"), fill = 'right') %>%
    # add a column id and "extend" it to all coordinates
    mutate(id = ifelse(key =="id", value, NA)) %>%
    fill(id) %>%
    # we don't need id=... rows anymore
    filter(key == "geometry") %>%
    mutate(id = factor(id), value = as.numeric(value)) %>%
    # group rows so we can bring it into long format
    group_by(subkey) %>%
    mutate(point_id = row_number()) %>%
    pivot_wider(names_from = subkey, values_from = value) %>%
    # remove unneeded columns
    mutate(key = NULL, point_id = NULL)
  return(streets);
}

load.city <- function(city.name, bbox = '', overpass.url){
  # Get all the ways in a given city which end in "Street" or "Avenue"
  # and return a data frame
  
  streets <- get.streets(city.name, "Street$", bbox, overpass.url)
  avenues <- get.streets(city.name, "Avenue$", bbox, overpass.url)
  
  streets$name <- "Street"
  avenues$name <- "Avenue"
  
  return(rbind(streets, avenues))
}


adjust.lat <- function(streets){
  # adjust latitude such that lon and lat are locally a square grid
  # assumes that the extent of the map is much smaller than the globe
  avglat <- mean(streets$lat)
  streets$lat = streets$lat / abs(cos(avglat/180*pi))
  return(streets)
}

plot_map <- function(streets){
  streets <- adjust.lat(streets)  
  p <- ggplot(streets, aes(x=lon, y=lat, colour = name)) +
    geom_line(aes(group = id), size=0.2) +
    coord_fixed() + 
    theme_minimal() + 
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      legend.title = element_blank()
    )
  return(p)
}

plot_directions <- function(streets){
  streets <- adjust.lat(streets)
  diff <- streets %>%
    group_by(id) %>%
    mutate(latdiff = append(NA, diff(lat)),
           londiff = append(NA, diff(lon)),
           lat = NULL,
           lon = NULL) %>%
    drop_na(latdiff)
  
  diff$angle <- atan2(-diff$londiff, -diff$latdiff) / pi * 180 + 180  # geographic angle (clockwise from North)
  diff$length <- sqrt(diff$latdiff^2 + diff$londiff^2)
  diff <- diff %>%
    ungroup() %>%
    group_by(name)
  
  bw = 10   # bin width
  hb = bw/2 # half bin width
  
  diff$angle <- ifelse(diff$angle > 360 - hb, diff$angle - 360, diff$angle)
  dirs <- diff %>% count(angle = cut(angle,
                                     breaks = seq(-hb,360-hb,bw),
                                     labels = seq(0, 360 - bw, bw),
                                     include.lowest= TRUE,
                                     right=FALSE),
                         wt = length)
  dirs$angle <- as.numeric(levels(dirs$angle))[dirs$angle]
  reversedirs <- dirs
  reversedirs$angle <- (reversedirs$angle + 180) %% 360
  dirs <- inner_join(dirs, reversedirs, by = c("name", "angle")) %>%
    mutate(n = n.x + n.y,
           n.x = NULL,
           n.y = NULL)
  
  wraparound <- dirs %>% filter(angle == min(angle))
  wraparound$angle <- wraparound$angle + 360
  dirs <- rbind(dirs, wraparound)
  
  
  direction_formatter <- function(deg){
    cardinals <- c("N", "E", "S", "W")
    return(ifelse(deg%%90 == 0, cardinals[(deg %% 360)/90 + 1], deg))
  }
  
  p <- ggplot(dirs, aes(x = angle, y = n, colour = name)) +
    geom_line()+
    scale_x_continuous(breaks = seq(0,330,30), labels = direction_formatter) +
    ylim(0, NA) +
    coord_polar() +
    theme_minimal() + 
    theme(
      axis.title = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_blank()
    )
  return(p)
} 

export_map <- function(streets, filename, width=10){
  # export map in a convenient format
  
  # first, determine aspect ratio of plot
  adjusted <- adjust.lat(streets)
  maxwidth = max(adjusted$lon) - min(adjusted$lon)
  maxheight <- max(adjusted$lat) - min(adjusted$lat)
  ratio = maxheight/maxwidth
  height = ratio*width
  
  map <- plot_map(streets) + theme(legend.position = "none")
  ggsave(filename, plot=map, width = width, height = height, units="cm")
}

# Example
# manhattan <- load.city("Manhattan", "(40.546678,-74.450912,41.009439,-73.585739)")
# export_map(manhattan, "img/manhattan_map.png")
# compass <- plot_directions(manhattan) + theme(legend.position = "bottom",
#                                               rect = element_rect(fill = "transparent"))
# ggsave("img/manhattan_compass.png", plot=compass, width = 6, height = 8, units="cm", dpi = 500, bg="transparent")
