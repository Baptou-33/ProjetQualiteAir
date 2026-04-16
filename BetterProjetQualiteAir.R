#Doc----------------------------------------------------------------------------

#Code
#https://www.ibm.com/docs/fr/spss-statistics/cd?topic=aggregate-functions-command
#https://r-charts.com/spatial/interactive-maps-leaflet/
#https://rstudio.github.io/leaflet/reference/easyButton.html#ref-examples
#https://stackoverflow.com/questions/62849300/r-leaflet-add-a-range-slider-to-filter-markers-without-shiny
#https://github.com/dwilhelm89/LeafletSlider

#PM2.5
#https://www.santepubliquefrance.fr/les-actualites/2021/pollution-de-l-air-l-oms-revise-ses-seuils-de-reference-pour-les-principaux-polluants-atmospheriques
#https://www.statistiques.developpement-durable.gouv.fr/la-pollution-de-lair-par-les-particules-fines-de-diametre-inferieur-ou-egal-25-micrometres-pm25
#https://www.airparif.fr/les-particules-fines



#Libraries----------------------------------------------------------------------
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(leafpop)
library(ggplot2)
library(corrplot)
library(dplyr)



#File cleaning------------------------------------------------------------------

#Set working directory
#setwd("/autofs/unityaccount/cremi/bdureau/S2/CMIISI/Projet 2025-2026")
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

#Create a data frame with the values per station by time
longitudes = aggregate(mesures$Longitude ~ Noms, data = mesures, FUN = mean)
Station_data = subset(mesures, Noms == "A7 Salaise Ouest")$valeur.brute
for (i in longitudes$Noms[-1]) {
  Station_data = cbind(Station_data, subset(mesures, Noms == i)$valeur.brute)
}
Station_data = as.data.frame(Station_data)
names(Station_data) = longitudes$Noms

#Delete stations with only invalid data
stations_valides = names(Station_data)[colSums(is.na(Station_data)) != nrow(Station_data)]
Station_data = Station_data[, stations_valides]
mesures = subset(mesures, Noms %in% stations_valides)



#Graph function-----------------------------------------------------------------

#Function to display a graph
DISPLAY_GRAPH = function(graph_data, text){
  plot(graph_data$Dates, 
       graph_data$valeur, 
       type = "l", 
       xlab = "Date", 
       ylab = expression("Concentration en PM  "["2,5"]), 
       main = expression(text)) #"Moyenne de la concentration journalière en PM"["2,5"]~" en France en Janvier - Février 2025."
}



#Map function-------------------------------------------------------------------

#Colors for the map
COLOR_LEGEND = function(colors, legend){
  return(data.frame(color = c(colors, c("#ADADAD")),
                             title = c(legend, "Invalide")))
}

Colors_legend = COLOR_LEGEND(c("#872181", "#960032", "#FF5050", "#F0E641", "#50CCAA", "#50F0E6"),
                             c("> 120", "90 - 120", "60 - 90", "40 - 60", "20 - 40", "<= 20"))

POINTS_COLOR = function(values){
  return(colorBin(palette = Colors_legend$color,
                  reverse = TRUE,
                  bins = c(-Inf, values),
                  na.color = "#ADADAD"))
}

points_colors = POINTS_COLOR(c(0.1, 20.1, 40.1, 60.1, 90.1, 200.1, Inf))


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

#Display the map
map = function(data, title, infos) {leaflet(data = data) %>%
    setView(lng = 2, lat = 46.5, zoom = 6) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addScaleBar(position = "bottomleft") %>%
    addMapPane("#ADADAD", zIndex = 401) %>%
    addMapPane("#50F0E6", zIndex = 420) %>%
    addMapPane("#50CCAA", zIndex = 403) %>%
    addMapPane("#F0E641", zIndex = 404) %>%
    addMapPane("#FF5050", zIndex = 405) %>%
    addMapPane("#960032", zIndex = 406) %>%
    addMapPane("#872181", zIndex = 407) %>%
    addCircleMarkers(lng = data$Longitude, 
                     lat = data$Latitude, 
                     if (infos){
                       popup = paste(data$Noms, "<hr>",
                                   "Moyenne horaire : ", round(data$valeur, 1), "µg/m<sup>3</sup>", "<br>",
                                   "Moyenne à cette heure en Janvier-Février : ", round(data$valeur, 1), "µg/m<sup>3</sup>", "<br>",
                                   "Moyenne à ce jour en Janvier-Février : ", round(data$valeur, 1), "µg/m<sup>3</sup>", "<br>",
                                   "Moyenne Janvier-Février : ", round(site_moyenne_totale$valeur, 1), "µg/m<sup>3</sup>", "<hr>",
                                   "Typologie : ", data$type.d.implantation, "<br>",
                                   "Influence : ", data$type.d.influence, "<hr>",
                                   popupGraph(GRAPHE(data$Noms)))
                     },
                     label = lapply(paste(data$Noms, " : ", round(data$valeur, 1), "µg/m<sup>3</sup>"), HTML), 
                     radius = 4, 
                     color = points_colors(data$valeur), 
                     opacity = 1, 
                     fillOpacity = 1, 
                     options = pathOptions(pane = points_colors(data$valeur))) %>%
    addLegend(position = "bottomleft", 
              colors = Colors_legend$color, 
              opacity = 1, 
              labels = Colors_legend$title, 
              title = title) #%>%
#    addEasyButton(easyButton(icon = "fa-crosshairs", 
#                             title = "Centrer la carte sur la position courante", 
#                             onClick = JS("function(btn, map){ map.setView([46.5, 2], zoom = 6); }")))
}


#Vars---------------------------------------------------------------------------
#Vars
seuil_OMS = 15

implantation_moyennes = aggregate(mesures$valeur.brute ~ mesures$type.d.implantation, FUN = mean, na.rm = TRUE)
names(implantation_moyennes) = c("Implantation", "valeur")

#Keep only city related data
mesures = mesures[mesures$type.d.implantation %in% c("Urbaine", "Périurbaine"), ]

#Create a data frame with the coordinates of every station 
latitudes = aggregate(mesures$Latitude ~ Noms, data = mesures, FUN = mean)
longitudes = aggregate(mesures$Longitude ~ Noms, data = mesures, FUN = mean)
coordonnees = cbind(latitudes, longitudes$`mesures$Longitude`)
names(coordonnees) = c("Noms", "Latitude", "Longitude")

#France global mean
france_moyenne_totale = mean(mesures$valeur.brute, na.rm = TRUE)

#Global mean per site
site_moyenne_totale = aggregate(mesures$valeur.brute ~ mesures$Noms, FUN = mean, na.rm = TRUE)
names(site_moyenne_totale) = c("Noms", "valeur")

#Daily mean per site
site_moyennes_jour = data.frame()
for (site in coordonnees[,1]){
  temp = subset(mesures, Noms == site)
  temp2 = aggregate(temp$valeur.brute ~ as.Date(temp$Date.de.début), FUN = mean, na.rm = TRUE)
  names(temp2) <- c("Dates", "valeur")
  temp2$Noms <- site
  
  site_moyennes_jour = bind_rows(site_moyennes_jour, temp2)
}
names(site_moyennes_jour) = c("Dates", "valeur", "Noms")

#Number of days above the OMS recommendation
depassements_OMS = aggregate(site_moyennes_jour$valeur > seuil_OMS ~ site_moyennes_jour$Noms, FUN = sum, na.rm = TRUE)
names(depassements_OMS) = c("Station", "valeur")



#Display stats for report-------------------------------------------------------

#Introduction



#Description des données



#Statistiques
implantation_moyennes


france_moyenne_totale


site_moyenne_totale[order(site_moyenne_totale$valeur, decreasing = TRUE), ]


sum(site_moyenne_totale$valeur > 15)


Colors_legend = COLOR_LEGEND(c("#FF5050", "#50F0E6"),
                             c("> 15", "<= 15"))
points_colors = POINTS_COLOR(c(0, 15, Inf))
map(cbind(coordonnees, site_moyenne_totale[2]), "<div style='text-align:center;'>PM<sub>2.5</sub><br><span style='font-size:10px;'>µg/m<sup>3</sup></span></div>", FALSE)


depassements_OMS[order(depassements_OMS$valeur, decreasing = TRUE), ]


Colors_legend = COLOR_LEGEND(c("#872181", "#960032", "#FF5050", "#F0E641", "#50CCAA", "#50F0E6"),
                             c("> 25", "20-25", "15-20", "10-15", "5-10", "<5"))
points_colors = POINTS_COLOR(c(0, 5, 10, 15, 20, 25, Inf))
map(cbind(coordonnees, depassements_OMS[2]), "<div style='text-align:center;'><span style='font-size:10px;'>Nbr de <br>dépassements</span></div>", FALSE)


sum(depassements_OMS$valeur < 5)


map(cbind(coordonnees, site_moyenne_totale[2])[depassements_OMS$valeur<5, ], "<div style='text-align:center;'><span style='font-size:10px;'>Nbr de <br>dépassements</span></div>", FALSE)

