#librerias
require(pacman)
pacman::p_load(
  sf,
  tidyverse,
  RColorBrewer,
  cowplot,
  ggspatial,
  rnaturalearth,
  rnaturalearthdata,
  ggplot2,
  raster,
  ggmap,
  osmdata,
  magick
)
rm(list = ls())


#Contadores de capas
args <- commandArgs(trailingOnly = TRUE)
capas_seleccionadas <- strsplit(args[1], ",")[[1]]
#API OSM
register_stadiamaps(key = "1ae805be-a31d-4893-b1c8-a3cc996fdbc6")
#grados y minutos
deg_to_dms <- function(deg) {
  d <- floor(deg)
  m <- floor((deg - d) * 60)
  return(paste0(d, "°", m, "'"))
}


#Capas
lim_estatales <- st_read("capas/Shapes/LimiteEstatal.geojson")
edos_cinturon <- st_read("capas/Shapes/EdosCinturonExtractivo.geojson")
cuencas <- st_read("capas/Shapes/Cuencas.geojson")
cuerp_agua <- st_read("capas/Shapes/CuerposAgua.geojson")
rios <- st_read("capas/Shapes/Rios.geojson")
acueductos <- st_read("capas/Shapes/Acueductos.geojson")
tunel_emisor_or <- st_read("capas/Shapes/TunelEmisorOriente.geojson")
tunel_emisor_ce <- st_read("capas/Shapes/TunelEmisorCentral.geojson")
gran_canal <- st_read("capas/Shapes/GranCanal.geojson")
pts_agua <- st_read("capas/Shapes/ptsAgua.geojson")
# Imágenes y texto
img_hidropes <- "img/logo.png"
img_conahcyt <- "img/logo_CONAHCYT-color.png"
img_colsan <- "img/logo_COLSAN-color.png"
text_titulo <- "Hidropesquisa"
text_right <- "Info del proyecto"


#Bounding box México
wrl <- ne_countries(scale = "medium", returnclass = "sf")
bbox_mexico <- st_bbox(c(
  xmin = -118,
  ymin = 8,
  xmax = -86,
  ymax = 35
), crs = st_crs(4326))
col_bb <- st_as_sfc(st_bbox(edos_cinturon))
#Extendido
bbox_mexico_corrected <- c(
  left = max(-180, bbox_mexico["xmin"]),
  bottom = max(-90, bbox_mexico["ymin"]),
  right = min(180, bbox_mexico["xmax"]),
  top = min(90, bbox_mexico["ymax"])
)

xlim_values <- extent(cuencas)[1:2]
ylim_values <- extent(acueductos)[3:4]
bbox_mexicoo <- st_bbox(c(
  xmin = xlim_values[1] - 0.5,
  ymin = ylim_values[1] - 0.5,
  xmax = xlim_values[2] + 0.2,
  ymax = ylim_values[2] + 0.2
), crs = st_crs(4326))
bbox_mexicoo_corrected <- c(
  left = max(-180, bbox_mexicoo["xmin"]),
  bottom = max(-90, bbox_mexicoo["ymin"]),
  right = min(180, bbox_mexicoo["xmax"]),
  top = min(90, bbox_mexicoo["ymax"])
)
#OpenStreetMap
map_base <- get_stadiamap(
  bbox = bbox_mexico_corrected, zoom = 4, maptype = "stamen_terrain"
)
map_osm <- get_stadiamap(
  bbox = bbox_mexicoo_corrected,
  zoom = 7,
  maptype = "stamen_terrain"
)

#Grafico macrolocalización
g1 <- ggmap(map_base) +
  geom_sf(
    data = col_bb, fill = NA, color = "red", size = 1.2, inherit.aes = FALSE
  ) +
  theme_bw() +
  coord_sf(
    xlim = c(bbox_mexico["xmin"], bbox_mexico["xmax"]),
    ylim = c(bbox_mexico["ymin"], bbox_mexico["ymax"])
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(
    labels = function(x) sapply(x, deg_to_dms), expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = function(y) sapply(y, deg_to_dms), expand = c(0, 0)
  )

#gráfico con mapa base
g2 <- ggmap(map_osm) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  scale_x_continuous(
    labels = function(x) sapply(x, deg_to_dms), expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = function(y) sapply(y, deg_to_dms), expand = c(0, 0)
  )

#gráfico con capas
g3 <- ggplot() +
  geom_rect(aes(
    xmin = bbox_mexico["xmin"] - 5,
    xmax = bbox_mexico["xmax"] + 5,
    ymin = bbox_mexico["ymin"] - 5,
    ymax = bbox_mexico["ymax"] + 5
  ), fill = "transparent", color = "transparent")
if ("LimiteEstatal" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = lim_estatales, size = 5, fill = NA, aes(color = "Límites estatales")
  )
}
if ("EdosCinturonExtractivo" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = edos_cinturon, size = 5, fill = NA, linetype = "dashed", aes(
      color = "Cinturón Extractivo"
    )
  )
}
if ("Cuencas" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = cuencas, size = 5, fill = NA, aes(color = "Cuencas")
  )
}
if ("CuerposAgua" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = cuerp_agua, size = 5, fill = "blue", aes(color = "Cuerpos de Agua")
  )
}
if ("Rios" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = rios, size = 5, aes(color = "Rios")
  )
}
if ("Acueductos" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = acueductos, size = 5, aes(color = "Acueductos")
  )
}
if ("TunelEmisorOriente" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = tunel_emisor_or, size = 5, aes(color = "Tunel Emisor Oriente")
  )
}
if ("TunelEmisorCentral" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = tunel_emisor_ce, size = 5, aes(color = "Tunel Emisor Central")
  )
}
if ("GranCanal" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(
    data = gran_canal, size = 5, aes(color = "Gran Canal")
  )
}
if ("ptsAgua" %in% capas_seleccionadas) {
  g3 <- g3 + geom_sf(data = pts_agua, aes(
    fill = Nombre,
    color = "Puntos de Agua"
  ), size = 2)
}
legend_order <- c(
  "Puntos de Agua", "Rios", "Acueductos",
  "Tunel Emisor Oriente", "Tunel Emisor Central",
  "Gran Canal", "Límites estatales",
  "Cinturón Extractivo", "Cuencas",
  "Cuerpos de Agua"
)
g3 <- g3 +
  scale_fill_manual(values = c(
    "Cuerpos de Agua" = "#0011D4",
    "Puntos de Agua" = "red"
  ), name = "Capa") +
  scale_color_manual(values = c(
    "Puntos de Agua" = "red",
    "Rios" = "#36C4DB",
    "Acueductos" = "#D4BB0D",
    "Tunel Emisor Oriente" = "#C04B20",
    "Tunel Emisor Central" = "#EB3E00",
    "Gran Canal" = "#EB8901",
    "Cuerpos de Agua" = "#0011D4",
    "Límites estatales" = "#676767",
    "Cinturón Extractivo" = "black",
    "Cuencas" =  "#329634"
  ), name = "Capa", breaks = legend_order) +
  guides(
    fill = guide_legend(order = 1, title = "Capa", byrow = FALSE),
    color = guide_legend(
      ncol = 2,
      order = 1,
      title = element_blank(),
      byrow = FALSE,
      keyheight = unit(0.4, "cm")
    )
  ) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  coord_sf(
    xlim = extent(cuencas)[1:2],
    ylim = extent(acueductos)[3:4]
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(
      fill = "transparent", color = "transparent"
    ),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    legend.box.background = element_rect(
      fill = "transparent", color = "transparent"
    ),
    panel.border = element_rect(color = "transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.28, -0.25),
    legend.key.size = unit(0.2, "cm"),
    legend.background = element_rect(
      fill = alpha("white", 1),
      colour = alpha("white", 0.4)
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(x = " ", y = " ")



#Rectangulo para los creditos
g4 <- ggplot() +
  geom_rect(
    aes(
      xmin = bbox_mexico["xmin"] - 5,
      xmax = bbox_mexico["xmax"] + 5,
      ymin = bbox_mexico["ymin"] - 5,
      ymax = bbox_mexico["ymax"] + 10
    ),
    fill = "white",
    color = "white"
  ) +
  theme_void()


#Logo hidropesquisa
g5 <- ggplot() +
  geom_rect(
    aes(
      xmin = bbox_mexico["xmin"] - 5,
      xmax = bbox_mexico["xmax"] + 5,
      ymin = bbox_mexico["ymin"] - 5,
      ymax = bbox_mexico["ymax"] + 10
    ),
    fill = "white",
    color = "white"
  ) +
  theme_void()



# Crear la combinación de gráficos
gg_inset <- ggdraw() +
  draw_plot(
    g2,
    x = 0, y = 0.15, width = 1, height = 0.85
  ) +
  draw_plot(
    g4, x = -0.2, y = 0.0215, width = 1.5, height = 0.2
  ) +
  draw_plot(
    g3,
    x = 0.045, y = 0.1, width = 0.965, height = 0.965
  ) +
  draw_plot(g1, x = 0, y = 0, width = 0.33, height = 0.24) +
  draw_image(
    img_conahcyt, x = 0.75, y = 0.01, width = 0.21, height = 0.21
  ) +
  draw_image(
    img_colsan, x = 0.75, y = 0.07, width = 0.2, height = 0.2
  ) +
  draw_plot(
    g5, x = 0.36, y = 0.162, width = 0.38, height = 0.05
  ) +
  draw_image(
    img_hidropes, x = 0.42, y = 0.152, width = 0.07, height = 0.07
  ) +
  draw_text(
    text_titulo, x = 0.5, y = 0.18, size = 14, hjust = 0, vjust = 0,
    color = "black", fontface = "bold"
  ) +
  draw_text(
    text_right, x = 0.76, y = 0.07, size = 10, hjust = 0, vjust = 0,
    color = "black"
  ) +
  draw_plot(
    g4, x = 0.991, y = 0.075, width = 0.02, height = 0.9
  ) +
  draw_plot(
    g4, x = -0.09, y = 0.934, width = 1.3, height = 0.01
  )

# Guardar el gráfico combinado
ggsave(
  plot = gg_inset,
  filename = "./mapa_final.png",
  units = "in",
  width = 8,
  height = 13,
  dpi = 300
)