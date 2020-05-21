# http://editerna.free.fr/wp/?p=130

library(tidyverse)
library(rworldmap)
library(readxl)
library(mapproj)
library(RColorBrewer)

d <- read_xlsx("dane.xlsx", sheet = 2) %>%
  select(-kraj)

d_long <- d %>%
  select(-pkb) %>%
  gather(wsk, wart, -country) %>%
  mutate(wsk=factor(wsk, 
                    levels = c("spi_2016", "zasp", "dobr", "awans"),
                    labels = c("Wskaźnik SPI 2016",
                               "Zaspokojenie fundamentalnych\n potrzeb człowieka",
                               "Fundamenty dobrobytu",
                               "Możliwości awansu społecznego\n i wolności osobiste"),
                    ordered = T))

d_pkb <- d %>%
  select(country, pkb)

worldMap <- getMap()

indEU <- which(worldMap$NAME %in% d$country)
#indEU <- which(worldMap$REGION == "Europe")

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

# pkd ---------------------------------------------------------------------

eu_pkb <- left_join(europeCoords, d_pkb, by=c("region"="country"))

ggplot() + geom_polygon(data = eu_pkb, 
                        aes(x = long, y = lat, 
                            group = region, fill = pkb),
                        colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
  scale_fill_gradientn(name = "PKB per capita", colours = brewer.pal(9,"RdYlGn")) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  ggsave("mapy/pkb20180323.png", width = 6, height = 6)


# SPI ---------------------------------------------------------------------

eu_spi <- left_join(europeCoords, d_long, by=c("region"="country")) %>%
  filter(!is.na(wsk))

ggplot() + geom_polygon(data = eu_spi, 
                        aes(x = long, y = lat, 
                            group = region, fill = wart),
                        colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
  facet_wrap(~ wsk) +
  scale_fill_gradientn(name = "", colours = brewer.pal(9,"RdYlGn")) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  ggsave("mapy/spi20180323.png", width = 6, height = 8)


ggplot() + geom_polygon(data = eu_spi, 
                        aes(x = long, y = lat, 
                            group = region, fill = wart),
                        colour = "black", size = 0.1) +
  coord_map(xlim = c(-13, 35),  ylim = c(32, 71)) +
  facet_wrap(~ wsk, nrow=1) +
  scale_fill_gradientn(name = "", colours = brewer.pal(9,"RdYlGn")) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(), 
        axis.title = element_blank(),
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  ggsave("mapy/spi20180323a.png", width = 12, height = 4)


# legenda

colors <- c("green", "yellow", "orange", "red")
var_q <- fivenum(d$pkb, na.rm = T)
int <- findInterval(d$pkb, var_q)
int_colors <- colors[int]
d$col <- int_colors

classInt::classIntervals(var = d$pkb, n = 4, style = "fixed", fixedBreaks = var_q)
