#File---------------------------------------------------------------------------

#Import file
mesures = read.csv2("Janvier.csv", sep = ",")

#Suppression des valeurs invalides -------------------------------------------------------- a voir plus tard si on les gère aussi
mesures$valeur = as.numeric(gsub(",", ".", mesures$valeur))
mesures$valeur.brute = as.numeric(gsub(",", ".", mesures$valeur.brute))
mesures = mesures[!is.na(mesures$valeur.brute) & !is.na(mesures$valeur), ]

#Format file
mesures$valeur = as.numeric(mesures$valeur)
mesures$valeur.brute = as.numeric(mesures$valeur.brute)
mesures$Latitude = as.numeric(mesures$Latitude)
mesures$Longitude = as.numeric(mesures$Longitude)



#Vars---------------------------------------------------------------------------

#Define vars that could be often used
capteurs = mesures$nom.site
valeurs = mesures$valeur
valeursBrutes = mesures$valeur.brute
noms = mesures$nom.site
latitudes = aggregate(mesures$Latitude ~ noms, data = mesures, FUN = mean)
longitudes = aggregate(mesures$Longitude ~ noms, data = mesures, FUN = mean)
coordonnees = cbind(latitudes, longitudes$`mesures$Longitude`)
names(coordonnees) = c("Noms", "Latitudes", "Longitudes")

#For graphs
moyenne_totale = mean(valeursBrutes)
moyennes_totales = aggregate(valeursBrutes ~ as.POSIXct(mesures$Date.de.début), FUN = mean)
names(moyennes_totales) = c("Dates", "Values")
moyennes_semaine_heure = aggregate(valeursBrutes ~ as.POSIXlt(mesures$Date.de.début)$wday, FUN = mean)
names(moyennes_semaine_heure) = c("Dates", "Values")
moyenne_jour = aggregate(valeursBrutes ~ as.POSIXlt(mesures$Date.de.début)$hour, FUN = mean)
names(moyenne_jour) = c("Dates", "Values")
moyenne_site = aggregate(valeursBrutes ~ mesures$nom.site, FUN = mean)
names(moyenne_site) = c("Noms", "Values")



#Map----------------------------------------------------------------------------

#Data to display on the map
data = moyenne_site

#Colors for the map
Colors_legend = data.frame(color = c("#872181", "#960032", "#FF5050", "#F0E641", "#50CCAA", "#50F0E6", "#ADADAD"), 
                           title = c("> 120", "90 - 120", "60 - 90", "40 - 60", "20 - 40", "<= 20", "Invalide"))
points_colors = colorBin(palette = Colors_legend$color,
                         reverse = TRUE, 
                         bins = c(-Inf, 0.1, 20.1, 40.1, 60.1, 90.1, 200.1, Inf), 
                         na.color = "#ADADAD")

#Display the map
#https://r-charts.com/spatial/interactive-maps-leaflet/
library(htmltools)
library(leaflet)
map = leaflet(width = 688.5, height = 650, data = data) %>%
  setView(lng = 2, lat = 46.5, zoom = 6) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addScaleBar(position = "bottomleft") %>%
  addCircleMarkers(lng = coordonnees$Longitudes, 
                   lat = coordonnees$Latitudes, 
                   popup = coordonnees$Noms, 
                   label = lapply(paste(coordonnees$Noms, " : ", round(data$Values, 1), "µg/m<sup>3</sup>"), HTML), 
                   labelOptions = labelOptions(html = TRUE), 
                   radius = 4, 
                   color = points_colors(data$Values), 
                   opacity = 1,
                   fillOpacity = 1) %>%
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



#Graphs-------------------------------------------------------------------------

#Total
plot(moyennes_totales$Dates, 
     moyennes_totales$Values, 
     type = "l", 
     xlab = "Date", 
     ylab = expression("Concentration en PM  "["2,5"]), 
     main = expression("Moyenne de la concentration en PM"["2,5"]~" en France en fonction du temps."))

#Par semaine
plot(moyennes_semaine_heure$Dates, 
     moyennes_semaine_heure$Values, 
     type = "l", 
     xlab = "Jour de la semaine (0 = Dimanche, 1 = Lundi ...)", 
     ylab = expression("Concentration en PM  "["2,5"]), 
     main = expression("Moyenne de la concentration en PM"["2,5"]~" en France en fonction du temps."))

#Par jour
plot(moyenne_jour$Dates, 
     moyenne_jour$Values, 
     type = "l", 
     xlab = "Heure", 
     ylab = expression("Concentration en PM  "["2,5"]), 
     main = expression("Moyenne de la concentration en PM"["2,5"]~" en France en fonction du temps."))

