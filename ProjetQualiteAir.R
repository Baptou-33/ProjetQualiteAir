#Doc----------------------------------------------------------------------------
#https://www.ibm.com/docs/fr/spss-statistics/cd?topic=aggregate-functions-command
#https://r-charts.com/spatial/interactive-maps-leaflet/
#https://rstudio.github.io/leaflet/reference/easyButton.html#ref-examples
#https://stackoverflow.com/questions/62849300/r-leaflet-add-a-range-slider-to-filter-markers-without-shiny
#https://github.com/dwilhelm89/LeafletSlider



#Libraries----------------------------------------------------------------------
library(htmltools)
library(leaflet)
library(leaflet.extras2)
library(leafpop)
library(ggplot2)



#File---------------------------------------------------------------------------
#Set working directory
setwd("/autofs/unityaccount/cremi/bdureau/S2/CMIISI/Projet 2025-2026")
setwd("C:/Users/bapti/OneDrive/Documents/Fichiers/Travail/L1/S2/ProjetQualiteAir")

#Import file
mesures = read.csv2("JanvierFevrier.csv", sep = ",")

#Keep only important data
mesures = cbind(mesures[1:2], mesures[7:8], mesures[10], mesures[14:15], mesures[22:23])

#Format file
mesures$valeur = as.numeric(mesures$valeur)
mesures$valeur.brute = as.numeric(mesures$valeur.brute)
mesures$Latitude = as.numeric(mesures$Latitude)
mesures$Longitude = as.numeric(mesures$Longitude)
names(mesures)[names(mesures) == "nom.site"] = "Noms"



#Vars---------------------------------------------------------------------------

#Define vars that could be often used
valeurs = mesures$valeur
valeursBrutes = mesures$valeur.brute
Noms = mesures$Noms
latitudes = aggregate(mesures$Latitude ~ Noms, data = mesures, FUN = mean)
longitudes = aggregate(mesures$Longitude ~ Noms, data = mesures, FUN = mean)
coordonnees = cbind(latitudes, longitudes$`mesures$Longitude`)
names(coordonnees) = c("Noms", "Latitude", "Longitude")

#For graphs
france_moyenne_totale = mean(valeursBrutes)

france_moyennes_jf_heure = aggregate(valeursBrutes ~ as.POSIXct(mesures$Date.de.début), FUN = mean)
names(france_moyennes_jf_heure) = c("Dates", "valeur")

france_moyennes_semaine_jour = aggregate(valeursBrutes ~ as.POSIXlt(mesures$Date.de.début)$wday, FUN = mean)
names(france_moyennes_semaine_jour) = c("Dates", "valeur")

france_moyenne_jour_heure = aggregate(valeursBrutes ~ as.POSIXlt(mesures$Date.de.début)$hour, FUN = mean)
names(france_moyenne_jour_heure) = c("Dates", "valeur")

site_moyenne_totale = aggregate(valeursBrutes ~ Noms, FUN = mean)
names(site_moyenne_totale) = c("Noms", "valeur")

site_moyennes_semaine_jour = list()
for (site in 1:length(coordonnees[,1])){
  temp = subset(mesures, Noms == coordonnees[,1][site])
  if (all(sapply(temp$valeur.brute, is.na))){
    site_moyennes_semaine_jour[[site]] = data.frame(Dates = c(0, 1, 2, 3, 4, 5, 6), valeur = c(NA, NA, NA, NA, NA, NA, NA))
  }else{
    temp2 = aggregate(temp$valeur.brute ~ as.POSIXlt(temp$Date.de.début)$wday, FUN = mean)
    names(temp2) = c("Dates", "valeur")
    site_moyennes_semaine_jour[[site]] = temp2
  }
}
names(site_moyennes_semaine_jour) = coordonnees[,1]

site_moyenne_jour_heure = list()
for (site in 1:length(coordonnees[,1])){
  temp = subset(mesures, Noms == coordonnees[,1][site])
  if (all(sapply(temp$valeur.brute, is.na))){
    site_moyenne_jour_heure[[site]] = data.frame(Dates = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), valeur = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
  }else{
    temp2 = aggregate(temp$valeur.brute ~ as.POSIXlt(temp$Date.de.début)$hour, FUN = mean)
    names(temp2) = c("Dates", "valeur")
    site_moyenne_jour_heure[[site]] = temp2
  }
}
names(site_moyenne_jour_heure) = coordonnees[,1]



#Graphs-------------------------------------------------------------------------
#Function to display a graph
DISPLAY_GRAPH = function(graph_data){
  plot(graph_data$Dates, 
       graph_data$valeur, 
       type = "l", 
       xlab = "Date", 
       ylab = expression("Concentration en PM  "["2,5"]), 
       main = expression("Moyenne de la concentration horaire en PM"["2,5"]~" en France en Janvier - Février 2025."))
}

#Display the graph with inserted data
DISPLAY_GRAPH(france_moyennes_jf_heure)

#Map----------------------------------------------------------------------------

#Colors for the map
Colors_legend = data.frame(color = c("#872181", "#960032", "#FF5050", "#F0E641", "#50CCAA", "#50F0E6", "#ADADAD"), 
                           title = c("> 120", "90 - 120", "60 - 90", "40 - 60", "20 - 40", "<= 20", "Invalide"))
points_colors = colorBin(palette = Colors_legend$color,
                         reverse = TRUE, 
                         bins = c(-Inf, 0.1, 20.1, 40.1, 60.1, 90.1, 200.1, Inf), 
                         na.color = "#ADADAD")

#List if the graphs to display on station click
GRAPHE = function(name) {
  print(length(name))
  L = list()
  for (i in 1:length(name)) {;
    print(paste(100*i/length(name), "%"))
    temp = subset(mesures, Noms == name[i])
    L[[i]] = ggplot(data.frame(Date = as.POSIXct(temp$Date.de.début), Valeur = temp$valeur), aes(Date, Valeur)) + geom_line()
  }
  return(L)
}

#Data to display on the map
data = subset(mesures, Date.de.début == "2025-01-01 00:00:00")
data = mesures[which.max(mesures$valeur.brute), 1:9]
data = mesures[37785,]

#Display the map
map = leaflet(data = data) %>%
  setView(lng = 2, lat = 46.5, zoom = 6) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar(position = "bottomleft") %>%
  addMapPane("#ADADAD", zIndex = 401) %>%
  addMapPane("#50F0E6", zIndex = 402) %>%
  addMapPane("#50CCAA", zIndex = 403) %>%
  addMapPane("#F0E641", zIndex = 404) %>%
  addMapPane("#FF5050", zIndex = 405) %>%
  addMapPane("#960032", zIndex = 406) %>%
  addMapPane("#872181", zIndex = 407) %>%
  addCircleMarkers(lng = data$Longitude, 
                   lat = data$Latitude, 
                   popup = paste(data$Noms, "<hr>",
                                 "Moyenne horaire : ", round(data$valeur, 1), "µg/m<sup>3</sup>", "<br>",
                                 "Moyenne à cette heure en Janvier-Février : ", round(data$valeur, 1), "µg/m<sup>3</sup>", "<br>",
                                 "Moyenne à ce jour en Janvier-Février : ", round(data$valeur, 1), "µg/m<sup>3</sup>", "<br>",
                                 "Moyenne Janvier-Février : ", round(site_moyenne_totale$valeur, 1), "µg/m<sup>3</sup>", "<hr>",
                                 "Typologie : ", data$type.d.implantation, "<br>",
                                 "Influence : ", data$type.d.influence, "<hr>",
                                 popupGraph(GRAPHE(data$Noms))),
                   label = lapply(paste(data$Noms, " : ", round(data$valeur, 1), "µg/m<sup>3</sup>"), HTML), 
                   radius = 4, 
                   color = points_colors(data$valeur), 
                   opacity = 1, 
                   fillOpacity = 1, 
                   options = pathOptions(pane = points_colors(data$valeur))) %>%
  addLegend(position = "bottomright", 
            colors = Colors_legend$color, 
            opacity = 1, 
            labels = Colors_legend$title, 
            title = "<div style='text-align:center;'>
            PM<sub>2.5</sub><br>
            <span style='font-size:10px;'>µg/m<sup>3</sup></span>
            </div>") %>%
  addEasyButton(easyButton(icon = "fa-crosshairs", 
                           title = "Centrer la carte sur la position courante", 
                           onClick = JS("function(btn, map){ map.setView([46.5, 2], zoom = 6); }")))# %>%
  #addControl()
  #addTimeslider(data = data, 
  #              options = timesliderOptions(position = "topright",
  #                                          timeAttribute = "Date.de.début"))

#addEasyButtonBar(easyButton(), 
#                 easyButton())
#addLayersControl(position = "bottomleft", 
#                 overlayGroups = c("Métropole", "Martinique", "Guadeloupe", "Guyane", "La Réunion", "Mayotte")) # remplacé par addEasyButtonBar
map

