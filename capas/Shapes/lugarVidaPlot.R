require(pacman)
pacman::p_load(
  sf,
  shiny,
  plotly,
  dplyr
)
rm(list = ls())

# Cargar datos
pts_extra <- st_read("capas/Shapes/ptsExtra.geojson") %>%
  filter(Estado == 2)

limite_estatal <- st_read("capas/Shapes/LimiteEstatal.geojson")
pts_con_estados <- st_join(pts_extra, limite_estatal, join = st_intersects)

# Interfaz
ui <- fluidPage(
  mainPanel(
    plotlyOutput("lugarVidaPlot")
  )
)

# Lógica del servidor
server <- function(input, output) {
  output$lugarVidaPlot <- renderPlotly({
    lugar_vida <- pts_con_estados %>% st_drop_geometry() %>% group_by(LugarVida, NOM_ENT) %>% summarise(count = n(), .groups = "drop")

    plot_ly(
      lugar_vida, x = ~LugarVida, y = ~count,
      color = ~NOM_ENT, type = 'bar',
      colors = 'Set1', text = ~paste(
      count, "puntos de vida en ", LugarVida, "en", NOM_ENT
    ),
            hoverinfo = "text") %>%
      layout(
        title = " ",
        xaxis = list(title = "Tipos de lugares por reproducción de la vida"
      ),
             yaxis = list(title = "",
                          tickmode = "linear",
                          dtick = 1),
             barmode = "stack",
             margin = list(l = 0, r = 0, t = 0, b = 0))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server, options = list(port = 4270))
