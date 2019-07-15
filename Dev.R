senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
locID <- 'VicAg_Youanmite'
dtype = 'Soil-Moisture'

url <- paste0(senFedPath, "getSensorDataStreams?siteid=", locID,"&sensortype=", dtype,"&startdate=2018-01-20T09%3A00%3A00&enddate=2018-02-22T09%3A00%3A00&aggperiod=days")

print(url)
resp <- getURL(paste0(url))


ts <- convertJSONtoTS(resp)

index(ts)

p <- plot_ly(x = ~as.POSIXct(index(ts)), y = as.numeric(ts$`Soil-Moisture_30`), mode = 'lines', text = paste( "seconds from now in", Sys.timezone()))
p
p <- plot_ly(x = as.POSIXct(ds$t), y = ds$v, mode = 'scatter', text = paste( "days from today"))