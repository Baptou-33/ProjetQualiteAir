#Libraries----------------------------------------------------------------------
library(htmltools)
library(leaflet)
library(leafpop)
library(ggplot2)



#File---------------------------------------------------------------------------

#Import file
mesures = read.csv2("Janvier.csv", sep = ",")

#Suppression des valeurs invalides -------------------------------------------------------- a voir plus tard si on les gère aussi
#mesures$valeur = as.numeric(gsub(",", ".", mesures$valeur))
#mesures$valeur.brute = as.numeric(gsub(",", ".", mesures$valeur.brute))
#mesures = mesures[!is.na(mesures$valeur.brute) & !is.na(mesures$valeur), ]

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
moyenne_totale = mean(valeursBrutes)
moyennes_totales = aggregate(valeursBrutes ~ as.POSIXct(mesures$Date.de.début), FUN = mean)
names(moyennes_totales) = c("Dates", "valeur")
moyennes_semaine_heure = aggregate(valeursBrutes ~ as.POSIXlt(mesures$Date.de.début)$wday, FUN = mean)
names(moyennes_semaine_heure) = c("Dates", "valeur")
moyenne_jour = aggregate(valeursBrutes ~ as.POSIXlt(mesures$Date.de.début)$hour, FUN = mean)
names(moyenne_jour) = c("Dates", "valeur")
moyenne_site = aggregate(valeursBrutes ~ mesures$Noms, FUN = mean)
names(moyenne_site) = c("Noms", "valeur")



#Graphs-------------------------------------------------------------------------

#Total
graphe_total= plot(moyennes_totales$Dates, 
                  moyennes_totales$valeur, 
                  type = "l", 
                  xlab = "Date", 
                  ylab = expression("Concentration en PM  "["2,5"]), 
                  main = expression("Moyenne de la concentration en PM"["2,5"]~" en France en fonction du temps."))

#Par semaine
graphe_semaine = plot(moyennes_semaine_heure$Dates, 
                      moyennes_semaine_heure$valeur, 
                      type = "l", 
                      xlab = "Jour de la semaine (0 = Dimanche, 1 = Lundi ...)", 
                      ylab = expression("Concentration en PM  "["2,5"]), 
                      main = expression("Moyenne de la concentration en PM"["2,5"]~" en France en fonction du temps."))

#Par jour
graphe_jour = plot(moyenne_jour$Dates, 
                   moyenne_jour$valeur, 
                   type = "l", 
                   xlab = "Heure", 
                   ylab = expression("Concentration en PM  "["2,5"]), 
                   main = expression("Moyenne de la concentration en PM"["2,5"]~" en France en fonction du temps."))


#Map----------------------------------------------------------------------------

#Data to display on the map
data = subset(mesures, Date.de.début == "2025-01-01 00:00:00")
NOM = "Chateauroux Sud"

#Colors for the map
Colors_legend = data.frame(color = c("#872181", "#960032", "#FF5050", "#F0E641", "#50CCAA", "#50F0E6", "#ADADAD"), 
                           title = c("> 120", "90 - 120", "60 - 90", "40 - 60", "20 - 40", "<= 20", "Invalide"))
points_colors = colorBin(palette = Colors_legend$color,
                         reverse = TRUE, 
                         bins = c(-Inf, 0.1, 20.1, 40.1, 60.1, 90.1, 200.1, Inf), 
                         na.color = "#ADADAD")

GRAPHE = function(name) {
  print(length(name))
  L = list()
  for (i in 1:length(name)) {;
    print(paste(100*i/length(name), "%"))
    subset_name = subset(mesures, mesures$Noms == name[i])
    L[[i]] = ggplot(data.frame(Date = as.POSIXct(subset_name$Date.de.début), valeur = subset_name$valeur), aes(Date, valeur)) + geom_line()
  }
  return(L)
}


#Display the map
#https://r-charts.com/spatial/interactive-maps-leaflet/
map = leaflet(data = data) %>%
  setView(lng = 2, lat = 46.5, zoom = 6) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar(position = "bottomleft") %>%
  addMapPane("Invalide", zIndex = 401) %>%
  addMapPane("<= 20", zIndex = 402) %>%
  addMapPane("20 - 40", zIndex = 403) %>%
  addMapPane("40 - 60", zIndex = 404) %>%
  addMapPane("60 - 90", zIndex = 405) %>%
  addMapPane("90 - 120", zIndex = 406) %>%
  addMapPane("> 120", zIndex = 407) %>%
  addCircleMarkers(lng = data$Longitude, 
                   lat = data$Latitude, 
                   #popup = paste(data$Noms, "<hr>", round(data$valeur, 1), "µg/m<sup>3</sup>"), 
                   popup = paste(data$Noms,popupGraph(GRAPHE(data$Noms))),
                   #label = lapply(paste(data$Noms, " : ", round(data$valeur, 1), "µg/m<sup>3</sup>"), HTML), 
                   radius = 4, 
                   color = points_colors(data$valeur), 
                   opacity = 1,
                   fillOpacity = 1 ) %>%
                   #options = pathOptions(pane = Colors_legend$title)
                   #group = "Métropole"
  addLegend(position = "bottomright", 
            colors = Colors_legend$color, 
            opacity = 1, 
            labels = Colors_legend$title, 
            title = "<div style='text-align:center;'>
            PM<sub>2.5</sub><br>
            <span style='font-size:10px;'>µg/m<sup>3</sup></span>
            </div>") %>%
  #https://rstudio.github.io/leaflet/reference/easyButton.html#ref-examples
  addEasyButton(easyButton(icon = "fa-crosshairs", 
                           title = "Centrer la carte sur la position courante", 
                           onClick = JS("function(btn, map){ map.setView([46.5, 2], zoom = 6); }")))# %>%
  #addEasyButtonBar(easyButton(), 
  #                 easyButton())
  #addLayersControl(position = "bottomleft", 
  #                 overlayGroups = c("Métropole", "Martinique", "Guadeloupe", "Guyane", "La Réunion", "Mayotte")) # remplacé par addEasyButtonBar
map


