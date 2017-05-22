library(ggmap)
library(dplyr)
library(readr)
library(ggrepel)
library(maps)
library(ggsn)

coord <- read.delim("coord_kz_txt.txt", sep = "\t", header = T)
names(coord) <- c("sample", "y", "x")
city <- read_csv("city_extra_kz.csv")

df_kz <- coord %>% select(x, y) 
df_kz

bb1 <- attr(mapImageData, "bb")
bb22 <- data.frame(long = unlist(bb1[c(2, 4)]), lat = unlist(bb1[c(1,3)]))

mapImageData <- get_googlemap(center =  c(lon_kz = median(df_kz$x), 
                                          lat_kz = median(df_kz$y)), 
                              zoom = 5, scale = 2, language = "en-EN",
                              dark=.2, maptype = c("terrain"))

p <- ggmap(mapImageData,  extent = "panel")
p <- p + geom_point(aes(x = lon, y = lat), data= city, alpha = 1, 
                    color = 'red', size = 3,
                    pch = 16 )
p <- p + geom_label_repel(aes(x=lon, y=lat, label=city), data=city, 
                          size=3, color='black')

p <- p + geom_point(aes(x = x, y = y), data = df_kz, alpha = 0.9, 
                    color = 'orange', size = 3,
                    pch = 17 )+
    labs(x = NULL, y = NULL) +
    geom_label_repel(aes(x=x, y=y, label=sample), data=coord, 
                     size=3, color='blue') +
    ggsn::scalebar(data = bb22, dist = 100, dd2km = TRUE, model  = "WGS84", 
                   location = "topleft", st.size = 3,
                   anchor = c(
                       x = bb1$ll.lon + 0.1 * (bb1$ur.lon - bb1$ll.lon), 
                       y = bb1$ll.lat + 0.9 * (bb1$ur.lat - bb1$ll.lat)
                   )
    )

p <- p +  ggsn::north2(p, x=.65, y=.90, scale = .1, symbol=1)
p
