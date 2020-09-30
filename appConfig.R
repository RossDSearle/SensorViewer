

machineName <- as.character(Sys.info()['nodename'])

if(machineName == 'WALCOT-SL'){
  dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
  senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
}else{
  dbPath <- "/srv/plumber/SensorFederator/DB/SensorFederator.sqlite"
  senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
}

DefaultSensor <- 'Rainfall'
DefaultDaysSince <- 30

DEBUG=T

if(DEBUG){
    defUsr<- 'ross.searle@csiro.au'
    defPwd <- '127a2ec00989b9f7faf671ed470be7f8'
}else{
    defUsr<- ''
    defPwd <- ''
}


errorDuration <- 10000
errorPos <- "center"