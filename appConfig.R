

machineName <- as.character(Sys.info()['nodename'])

if(machineName == 'FANCY-DP'){
  dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
  senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
}else{
  dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
  senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
}

DefaultSensor <- 'Rainfall'
DefaultDaysSince <- 30
