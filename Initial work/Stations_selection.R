library(terra)
library(stringr)


stations_ret <- vect("retained_station_with_correction_more.gpkg")

stations <- vect("geodata_station_with_correction_more.gpkg")

UH <- vect("../frenchMap/Re_ territoires des UH/UH_Pole2_avec_OM_JNA.shp")

UH_points_retained <- extract(UH, project(stations_ret, crs(UH)))

UH_wanted_station <- stations_ret[which(UH_points_retained$cdreseau == "POH014"),]

Name_to_look <- c(
  "Fleurey", 
  "Vuillecin",
  "Chenecey",
  "Nohain",
  "Bas-En-Basset", 
  "Villerest - Aval", 
  "Villerest - Pont De Villerest",
  "Rigny-sur-Arroux",
  "Verneuil",
  "Saint-Haon - Le Nouveau Monde",
  "Langeac",
  "Meung-sur-Loire",
  "Nazelles-Négron",
  "Vierzon",
  "Bourges - L'ormediot",
  "Selles-sur-Cher",
  "Brinon-sur-Sauldre",
  "Châtillon-sur-Cher",
  "Meusnes - Le Gué Au Loup",
  "Buzançais",
  "Conie-Molitard"
)

name_match <- c()
name_Found <- c()
for(name in stations$Name_station){
  for(name_checked in Name_to_look){
    cond <- grep(tolower(name_checked), tolower(name))
    if(length(cond) > 0){
      name_match <- c(name_match,name)
      name_Found <- c(name_Found,name_checked)
      print(name)
      print(name_checked)
      
    }
  }
}

Kept <- stations[which(stations$Name_station %in% name_match),]

all_kept_stations <- vect(c(Kept, UH_wanted_station))


Commemt_to_look <- c(
  "Vegetation",
  "Végétation",
  "Herbe",
  "Herbier"
)

name_match <- c()
name_Found <- c()
for(row in 1:nrow(stations)){
  station <- stations[row,]
  for(comment_checked in Commemt_to_look){
    cond <- grep(tolower(comment_checked), tolower(station$Commentaire))
    if(length(cond) > 0){
      name_match <- c(name_match,station$Name_station)
      print(station$Name_station)
      print(comment_checked)
      
    }
  }
}

grass_station <- stations[which(stations$Name_station %in% name_match),]

all_kept_stations <- vect(c(Kept, UH_wanted_station, grass_station))

unique_stations <- unique(all_kept_stations$Name_station)

all_kept_stations <- all_kept_stations[match(unique_stations,all_kept_stations$Name_station),]

plot(all_kept_stations)
writeVector(all_kept_stations, "Test_kept_stations_with_grass.gpkg", overwrite=T)
