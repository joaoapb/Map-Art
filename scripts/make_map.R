# Load libraries
library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)


get_osm <- function(bb, key, value) {
  return(bb %>% 
           opq(timeout = 500) %>% 
           add_osm_feature(key = key,
                           value = value) %>%
           osmdata_sf())
}

make_map <- function(bb_vec, city, white_map) {
  bb <- matrix(bb_vec, 2, dimnames = list(c("x", "y"), c("min", "max")))
  
  # Airport
  airport_runway <- get_osm(bb, "aeroway", c("runway"))
  
  airport_taxiway <- get_osm(bb, "aeroway", c("taxiway")) 
  
  airport_others <- get_osm(bb, "aeroway", c("aerodrome", "aircraft_crossing", 
                                             "apron", "gate", "hangar", "helipad",
                                             "heliport", "terminal"))
  
  # Streets
  small_streets <- get_osm(bb, "highway", c("residential", "living_street"))
  
  big_streets <- get_osm(bb, "highway",c("motorway", "primary", 
                                         "motorway_link", "primary_link"))
  
  med_streets <- get_osm(bb, "highway", c("secondary", "tertiary", 
                                          "secondary_link", "tertiary_link"))
  
  footways <- get_osm(bb, "highway", c("footway"))
  
  service_roads <- get_osm(bb, "highway", c("service"))
  
  # Natural
  river <- get_osm(bb, "waterway", value = "river")
  
  lake <- get_osm(bb, "natural", value = "water")
  
  beach <- get_osm(bb, "natural", value = "beach")
  
  # Railway
  railway <- get_osm(bb, "railway", value="rail")
  
  font_add_google(name = "Lato", family = "lato") # add custom fonts
  showtext_auto()
  
  # Colors
  blue = "#9CC0F9"
  yellow = "#FEEFC3"
  light_grey = "#969696"
  dark_grey = "#111111"
  
  
  # The list of items follows the plotting order
  items = list(
    "river" = list(lines=river$osm_lines, color=blue, size = .7, alpha = .5, linetype="solid"),
    # "lake" = list(lines=lake$osm_points, color=blue, fill=blue, alpha = .4, size = 0, alpha = 0, linetype="solid"),
    # "beach" = list(lines=beach$osm_lines, color=yellow, size = .8, alpha = .8, linetype="solid"),
    
    "small_streets" = list(lines=small_streets$osm_lines, color=light_grey, size = .4, alpha = .6, linetype="solid"),
    "footways" = list(lines=footways$osm_lines, color=light_grey, size = .2, alpha = .4, linetype="solid"), 
    "service_roads" = list(lines=service_roads$osm_lines, color=light_grey, size = .2, alpha = .4, linetype="solid"), 
    
    "railway" = list(lines=railway$osm_lines, color=dark_grey, size = .5, alpha = .9, linetype="dotdash"),
    
    "airport_others" = list(lines=airport_others$osm_lines, color=dark_grey, size = .2, alpha = .6, linetype="solid"), 
    "airport_taxiway" = list(lines=airport_taxiway$osm_lines, color=dark_grey, size = .3, alpha = .7, linetype="solid"), 
    "airport_runway" = list(lines=airport_runway$osm_lines, color=dark_grey, size = .8, alpha = .9, linetype="solid"), 

    "med_streets" = list(lines=med_streets$osm_lines, color=dark_grey, size = .4, alpha = .8, linetype="solid"), 
    "big_streets" = list(lines=big_streets$osm_lines, color=dark_grey, size = .6, alpha = .9, linetype="solid"))
  

  items_names = c(
    "river", "lake", "beach",
    "small_streets", "footways", "service_roads", 
    "railway",
    "airport_others", "airport_taxiway", "airport_runway", 
    "med_streets", "big_streets")
  
  main_plot <- ggplot()
  for (item in items_names) {
    print(paste("Running item ", item, sep = ""))
    if (!is.null(items[item][[1]]$lines)) {
      main_plot <- main_plot + 
        geom_sf(data = items[item][[1]]$lines,
                inherit.aes = FALSE,
                color = items[item][[1]]$color,
                fill = items[item][[1]]$color,
                linetype = items[item][[1]]$linetype,
                size = items[item][[1]]$size,
                alpha = items[item][[1]]$alpha)
    }
  }
  
  message("Layers added successfuly")
  main_plot <- main_plot +
    # Limit the map to the city
    coord_sf(xlim = c(bb["x","min"], bb["x","max"]),
             ylim = c(bb["y","min"], bb["y","max"]),
             expand = FALSE)  +
    theme_void() + # get rid of background color, grid lines, etc.
    # Adjust the plotting area
    theme(plot.title = element_text(size = 160, family = "lato",
                                    face="bold", hjust=.5),
          plot.subtitle = element_text(family = "lato", size = 80, hjust=.5, 
                                       margin=margin(2, 0, 5, 0)),
          panel.border = element_rect(colour = "white", fill=NA, size=80),
          panel.background = element_rect(colour = "white", fill="white"),
          plot.background = element_rect(colour = "white", fill="white")) +
    labs(title = toupper(city),
         subtitle = sprintf("%s °N / %s °E",
                            round(mean(bb["y",]),3),
                            round(mean(bb["x",]),3)))
  
  message("Saving map")
  ggsave(paste("images/", city, " Map.jpg", sep=""), main_plot, device="jpg", 
         scale=1, width=30, height=40, units="cm", dpi=600, type = "cairo")
}

make_map(c(-43.43, -23.10, -43.12, -22.75), "Rio de Janeiro")
make_map(c(49.99, 8.48, 50.19, 8.74), "Frankfurt am Main")
make_map(c(-74.22, 40.52, -73.72, 40.92), "New York")
