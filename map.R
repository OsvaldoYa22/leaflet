#Cargamos nuestras librerias
rm(list = ls())
library(cleangeo)
library(deldir)
library(dplyr)
library(foreign)
library(ggmap) 
library(ggplot2) 
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(mapproj)
library(maptools)
library(mapview)
library(raster)
library(readr)
library(rgdal)
library(spatstat) 
library(sp) 
library(tidyr)
library(webshot)
library(graphics)
library(leafem)

setwd("C:/Users/gyanezp/Desktop/Proyectos/3. Mapas HTML/Pruebas/Camaras 5C/")
#base = tipo de delitos
base <- read.csv("THALES_BASE_2019_2020_parte_1.csv", header = T,stringsAsFactors = F)
#SHP alcaldias
ALCALDIAS <- spTransform(shapefile("POLIGONOS/ALCALDIAS.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#SHP vialidades primarias
VIALIDADES_PRIMARIAS <- spTransform(shapefile("POLIGONOS/VIALIDADES CATEGO_PRIMARIA.shp"), "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#Ordenamos las variables de los tipos de delitos
orden <- order(unique(base$incidente_c4))
casos <- unique(base$incidente_c4)
casos[orden]

Robo_Vehiculo_con_Violencia <- base[which((base$incidente_c4) == "Robo-Vehiculo con Violencia"),]


tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: TRANSPARENT;
    font-weight: bold;
    font-size: 25px;
    color: #9F2241}"))
title <- tags$div(tag.map.title, HTML(paste("Robo a vehículos 2019<br>" )))


#Leyenda parte inferior irquierda
colors <- c("transparent","green","YELLOW","ORANGE","red")

labels <- c("<b>INCIDENCIA</b>",
            "BAJA",
            "MEDIA",
            "ALTA",
            "MUY ALTA"
            ) 

sizes <- c(0,10,10,10,10)
shapes <- c("circle","square","square","square","square")
borders <- c("transparent","green","YELLOW","ORANGE","red")


addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity,position = "bottomleft"))
}

#GENERAMOS MAPA--------------------------------------------------------------------------------------------------------
leaflet()  %>%
  addTiles() %>% addProviderTiles(providers$Esri.WorldGrayCanvas, group = "B&W") %>%
  addTiles(urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
           attribution = NULL, layerId = NULL, options = tileOptions(),group = "Color")%>%
  
  addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", 
           attribution = NULL, layerId = NULL, options = tileOptions(),group = "Satelital")%>%
  
  
  addFullscreenControl()%>%clearBounds()%>%
  addControl(title,position = "topleft",className="map-title")%>%
  leafem::addMouseCoordinates() %>% addFullscreenControl(position = "topleft", pseudoFullscreen = F) %>%
  #leafem::addLogo(img=ciudad$city,src="remote",position="bottomright",width=403,height=127)%>%
  addResetMapButton()%>%
  addMapPane("polygons",zIndex = 500)%>%
  addMapPane("ce",zIndex = 510)%>%
  addMapPane("li",zIndex=570)%>%
  addMapPane("lo",zIndex = 580)%>%
  
  addPolygons(data=ALCALDIAS,color = "black",fillColor = "transparent",fillOpacity =0.01,weight = 1,popup = ALCALDIAS$ALCALDIA,
              highlightOptions = highlightOptions(color = "red", weight = 1) , group="Alcaldias",options = pathOptions(pane="polygons"))%>%
  
  addPolylines(data=VIALIDADES_PRIMARIAS,color = "#8B0000",weight = 0.8, group="Primarias",options = pathOptions(pane="polygons"))%>%
  
  addCircles(data = Robo_Vehiculo_con_Violencia,lng = Robo_Vehiculo_con_Violencia$longitud,lat = Robo_Vehiculo_con_Violencia$latitud,color = "#9AC0CD" ,radius = 5,fillOpacity = T,
             popup = paste("<b>","Tipo : ","</b>",as.character(Robo_Vehiculo_con_Violencia$clas_con_f_alarma),"<br>",
                           "<b>","Categoria : ","</b>",as.character(Robo_Vehiculo_con_Violencia$incidente_c4),"<br>"),
             group = paste("Puntos","(",nrow(Robo_Vehiculo_con_Violencia),")"),options = pathOptions(pane="li"))%>%

  addWebGLHeatmap(data = Robo_Vehiculo_con_Violencia,lng = Robo_Vehiculo_con_Violencia$longitud,lat = Robo_Vehiculo_con_Violencia$latitud, group = "Mapa de calor",size=700,gradientTexture = "skyline",
                  opacity = 0.7 )%>%
  
  
  addLayersControl(overlayGroups = c( "&nbsp; <b>Vista </b> &nbsp; ",
                                      "Mapa de calor",
                                      paste("Puntos","(",nrow(Robo_Vehiculo_con_Violencia),")"),
                                      
                                      "&nbsp; <b>Vialidades</b> &nbsp; ",
                                      "Primarias",
                                      
                                      "&nbsp; <b>División </b> &nbsp; ",
                                      "Alcaldias",
                                      
                                      "&nbsp; <b>Capas</b> &nbsp; ",
                                      "B&W",
                                      "Color",
                                      "Satelital"
                                      
  ),
  options = layersControlOptions(collapsed = T))%>% 
  
  
  htmlwidgets::onRender(jsCode = htmlwidgets::JS("function(btn,map){ 
                                                 var lc=document.getElementsByClassName('leaflet-control-layers-overlays')[0]
                                                 
                                                 lc.getElementsByTagName('input')[0].style.display='none';
                                                
                                                 lc.getElementsByTagName('div')[0].style.fontSize='160%';
                                                 lc.getElementsByTagName('div')[0].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[0].style.color='white';
                                                 lc.getElementsByTagName('div')[0].style.backgroundColor='#9F2241';
                                                 
                                                 lc.getElementsByTagName('input')[3].style.display='none';
                                                
                                                 lc.getElementsByTagName('div')[3].style.fontSize='160%';
                                                 lc.getElementsByTagName('div')[3].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[3].style.color='white';
                                                 lc.getElementsByTagName('div')[3].style.backgroundColor='#9F2241';
                                                 
                                                 lc.getElementsByTagName('input')[5].style.display='none';
                                                
                                                 lc.getElementsByTagName('div')[5].style.fontSize='160%';
                                                 lc.getElementsByTagName('div')[5].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[5].style.color='white';
                                                 lc.getElementsByTagName('div')[5].style.backgroundColor='#9F2241';
                                                 
                                                 lc.getElementsByTagName('input')[7].style.display='none';
                                                
                                                 lc.getElementsByTagName('div')[7].style.fontSize='160%';
                                                 lc.getElementsByTagName('div')[7].style.textAlign='center';
                                                 lc.getElementsByTagName('div')[7].style.color='white';
                                                 lc.getElementsByTagName('div')[7].style.backgroundColor='#9F2241';
                                                 
                                                 
   ;
                                                 }
                                                 ")) %>%
  
  addLegendCustom(colors, labels, sizes, shapes, borders)%>%  
  
  hideGroup(c( "&nbsp; <b>Vista </b> &nbsp; ",
               
               paste("Puntos","(",nrow(Robo_Vehiculo_con_Violencia),")"),
               
               "&nbsp; <b>Vialidades</b> &nbsp; ",
               "Primarias",
               
               "&nbsp; <b>División </b> &nbsp; ",
               "Alcaldias",
               
               "&nbsp; <b>Capas</b> &nbsp; ",
               "B&W",
               "Color",
               "Satelital"
               
  ) )
