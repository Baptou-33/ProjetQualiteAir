#Import file
mesures = read.csv2("Janvier.csv", sep = ",")

#Suppression des valeurs invalides
mesures$valeur = as.numeric(gsub(",", ".", mesures$valeur))
mesures$valeur.brute = as.numeric(gsub(",", ".", mesures$valeur.brute))
mesures = mesures[!is.na(mesures$valeur.brute) & !is.na(mesures$valeur), ]


#Format file
#mesures$Date.de.début = as.POSIXlt(mesures$Date.de.début)
#mesures$Date.de.début = as.POSIXct(mesures$Date.de.début)
mesures$valeur = as.numeric(mesures$valeur)
mesures$valeur.brute = as.numeric(mesures$valeur.brute)
mesures$Latitude = as.numeric(mesures$Latitude)
mesures$Longitude = as.numeric(mesures$Longitude)

#Define vars that could be often used
capteurs = mesures$nom.site
valeurs = mesures$valeur
valeursBrutes = mesures$valeur.brute
noms = mesures$nom.site
latitudes = aggregate(mesures$Latitude ~ noms, data = mesures, FUN = mean)
longitudes = aggregate(mesures$Longitude ~ noms, data = mesures, FUN = mean)
coordonnees = cbind(latitudes, longitudes$`mesures$Longitude`)
names(coordonnees) = c("Noms", "Latitudes", "Longitudes")
Colors_legend = data.frame(c("#ADADAD", "#50F0E6", "#50CCAA", "#F0E641", "#FF5050", "#960032", "#872181"), c("", 0, 20, 40, 60, 90, 120))
colnames(Colors_legend) = c("color", "value")

#For the curves
moyenne_totale = mean(valeursBrutes)
moyennes_totales = aggregate(valeur.brute ~ as.POSIXct(mesures$Date.de.début), data = mesures, FUN = mean)
moyennes_semaine_heure = aggregate(valeur.brute ~ as.POSIXlt(mesures$Date.de.début)$wday , data = mesures, FUN = mean)
moyenne_jour = aggregate(valeur.brute ~ as.POSIXlt(mesures$Date.de.début)$hour , data = mesures, FUN = mean)



#Display the map
#https://r-charts.com/spatial/interactive-maps-leaflet/

library(leaflet)
map = leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addCircleMarkers(lng = coordonnees$Longitudes, 
                   lat = coordonnees$Latitudes, 
                   popup = coordonnees$Noms, 
                   label = coordonnees$Noms, 
                   radius = 3, 
                   color = rgb(255, 0, 0, maxColorValue = 255), 
                   opacity = 1)
  #labelOptions(interactive = TRUE)
  #popupOptions()
  #markerOptions()
  #addLegend()
map



#Total
plot(moyennes_totales$`as.POSIXct(mesures$Date.de.début)`, moyennes_totales$valeur.brute, type="l",xlab="Date",ylab="Concentration en PM25",main="Evolution de la moyenne de la concentration en PM25 en fonction du temps.")

#Par semaine
plot(moyennes_semaine_heure$`as.POSIXlt(mesures$Date.de.début)$wday`, moyennes_semaine_heure$valeur.brute, type="l",xlab="Jour de la semaine (0 = Dimanche, 1 = Lundi ...)",ylab="Concentration en PM25",main="Evolution de la moyenne de la concentration en PM25 en fonction du temps.")

#Par jour
plot(moyenne_jour$`as.POSIXlt(mesures$Date.de.début)$hour`, moyenne_jour$valeur.brute, type="l",xlab="Heure",ylab="Concentration en PM25",main="Evolution de la moyenne de la concentration en PM25 en fonction du temps.")























#Données
#mesures
#str(mesures)
#dim(mesures)
#nrow(mesures)
#names(mesures)
#head(mesures)
#moyennes_total_heure 