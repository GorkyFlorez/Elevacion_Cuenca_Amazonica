library(sf)
library(ggplot2)
library(raster)
SurAmerica = st_read("SHP/Sur_america.shp")  %>% st_as_sf()
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


Rios_Amazonas = st_read("SHP/Rios_Amazonas.geojson")  %>% st_as_sf()
Rios_Amazona <- st_transform(Rios_Amazonas  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))


library(elevatr)
elev = get_elev_raster(SurAmeric , z=4)
plot(elev)
Poligo_alt    <- crop(elev, SurAmeric )                           #
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, SurAmeric )
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope")
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
library(ggnewscale)

summary(Geo_data_frame$alt)
colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")
cortes <- c(200, 500,1000,2000,3000,4000,5000, 6500)


library(ggspatial)
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

library(rnaturalearth)                                   #Continentes
library(rnaturalearthdata)                               #Continentes especifico
world        <- ne_countries(scale= "small", returnclass = "sf") # Continentes del mundo
world.SA     <- subset(world, continent=="South America")     # Sur America

SurAmeric_xy <- cbind(world.SA, st_coordinates(st_centroid(world.SA$geometry)))

ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.1)+
  coord_sf(xlim = c(-81, -36 ), ylim = c(-30 ,12))+
  theme(panel.background = element_rect(fill = "#a2d2ff"))+
  annotate(geom = "text", x = -80, y = -20, hjust = 0, vjust = 1,
           label = "Pacific ocean",size = 3, family="serif", color =
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -45, y = -27, hjust = 0, vjust = 1,
           label = "Atlantic ocean",size = 3, family="serif", color =
             "black",  fontface="italic")+
  annotate(geom = "text", x = -48, y = 8, hjust = 0, vjust = 1,
           label = "Atlantic ocean",size = 3, family="serif", color =
             "black",  fontface="italic")+
  geom_sf_label(data = SurAmeric_xy , aes(label = name),
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"),
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))

Mapa = ggplot()+
  geom_raster(data = Geo_data_frame , aes(x,y, fill = alt) )+
  scale_fill_gradientn(colours = colores,
                       na.value = 'white',name="Elevacion\n(m.s.n.m)",
                       breaks = cortes ,
                       labels = c("[menor a - 270] ","[270 - 499]", "[500 - 999]", "[1000 - 1999]", "[2000 - 2999]",
                                  "[3000 - 3999]", "[4000 - 4999]", "[5000 -6500]"))+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.1)+

  geom_sf(data = Rios_Amazona ,  size=0.2, color="#a2d2ff")+

  coord_sf(xlim = c(-81, -36 ), ylim = c(-30 ,12))+

  theme_classic()+
  theme_ipsum_rc(grid="X")+
  guides(fill = guide_legend(
    title = " msnm.",
    direction = "horizontal",
    nrow = 1,
    keywidth = 1.75,
    keyheight = 0.5,
    label.position = "bottom",
    title.position = "right",
    override.aes = list(alpha = 1)
  ))+
  theme(legend.position = "bottom",

        axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        axis.title = element_text(face="bold", color="black"),
        panel.background = element_rect(fill = "#a2d2ff"),
        plot.title = element_text(size = 16, hjust = 0.5, family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11,  face = "italic", family="serif"),
        plot.caption = element_text(size = 9, family="serif", face = "italic"),

        plot.background = element_rect(fill = "white", color = NA),

        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=7, family="serif"),
        legend.title = element_text(size=8, family="serif", face='bold',hjust=0.5),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"), #ancho de cuadrados de referencia

        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+

  labs(x = 'Longitud', y = 'Latitud',
       title="Mapa de elevacion en la Region Hidrografica del Amazonas",
       subtitle="Red hidrológica de la cuenca del Amazonas en Sur América   \nLa cuenca hidrográfica del Amazonas cubre un área de 7 413 827 km²",
       caption="Gorky Florez  'Datos: Misión Topográfica de Radar del Transbordador (SRTM)'")+
  annotate(geom = "text", x = -80, y = -20, hjust = 0, vjust = 1,
           label = "Pacific ocean",size = 3, family="serif", color =
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -45, y = -27, hjust = 0, vjust = 1,
           label = "Atlantic ocean",size = 3, family="serif", color =
             "black",  fontface="italic")+
  annotate(geom = "text", x = -48, y = 8, hjust = 0, vjust = 1,
           label = "Atlantic ocean",size = 3, family="serif", color =
             "black",  fontface="italic")+
  geom_sf_label(data = SurAmeric_xy , aes(label = name),
                family="serif", color = "black",  fontface="italic", box.padding = unit(0.9, "lines"),
                size = 2, face = "bold",color = 'black',
                point.padding = unit(0.9, "lines"))





ggsave(plot = Mapa ,"Elevacion2.png",  units = "cm", width = 29, #ancho
       height = 29, #Largo
       dpi = 1200)





