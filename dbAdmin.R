library(RSQLite)
library(DBI)
library(digest)
library(RCurl)
library(jsonlite)
library(plotly)
library(xts)
library(stringr)

machineName <- as.character(Sys.info()['nodename'])
if(machineName == 'FANCY-DP'){
  dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
}else if (machineName == 'soils-discovery') {
  dbPath <- "/srv/shiny-server/SensorViewer/DB/SensorView.sqlite"
}else{
  dbPath <- ""
}
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)


sql <- 'select * from appUsers'
res <- dbSendQuery(con, sql)
df <- dbFetch(res)
df
nrecs <- dbGetRowCount(res)
dbClearResult(res)

sqlDelete <- paste0("Delete from appUsers")
res <- dbSendStatement(con, sqlDelete)


usrn <- 'a'
p
md5 <- digest(passwd, serialize=T)
authcon <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
sqlInsert <- paste0("Insert into appUsers ( usr, pwd) values ('", usrn, "', '", md5 ,"')")
print(sqlInsert)
res <- dbSendStatement(authcon, sqlInsert)
dbGetRowsAffected(res)
dbDisconnect(authcon)


mgtcon <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)

appuserTableCreate <- "CREATE TABLE 'appUsers' ( `usr` TEXT NOT NULL UNIQUE, `pwd` TEXT NOT NULL, PRIMARY KEY(`usr`) )"
res <- dbSendStatement(mgtcon, appuserTableCreate)
md5 <- digest('a', serialize=T)
sqlInsert <- paste0("Insert into appUsers ( usr, pwd) values ('", 'ross.searle@csiro.au', "', '", md5 ,"')")
res <- dbSendStatement(mgtcon, sqlInsert)

appUserLocationsCreate <- "CREATE TABLE 'appUserLocations' ( `usr` TEXT NOT NULL, `locationID` TEXT NOT NULL)"
res <- dbSendStatement(mgtcon, appUserLocationsCreate)
sqlInsert <- paste0("Insert into appUserLocations ( usr, locationID) values ('", 'ross.searle@csiro.au', "', '", 'Cosmoz_12' ,"')")
res <- dbSendStatement(mgtcon, sqlInsert)
sqlInsert <- paste0("Insert into appUserLocations ( usr, locationID) values ('", 'ross.searle@csiro.au', "', '", 'VicAg_Youanmite' ,"')")
res <- dbSendStatement(mgtcon, sqlInsert)




senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
loc <- 'VicAg_Youanmite'

url <- paste0(senFedPath,'getSensorInfo?siteid=', loc)

stnsRaw <- getURL(paste0(url))
stnsJ <- fromJSON(stnsRaw)
unique(stnsJ$DataType)


url <- "http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorDataStreams?siteid=VicAg_Youanmite&sensortype=Rainfall&startdate=2018-01-20T09%3A00%3A00&enddate=2018-04-22T09%3A00%3A00&aggperiod=days"
stnsRaw <- getURL(paste0(url))
stnsJ <- fromJSON(stnsRaw)
ds <- stnsJ$DataStream[[1]]
ds <- na.omit(ds)
d <- as.POSIXct(str_trim(ds$t) , format = "%Y-%m-%d %H:%M:%S")
ts <- xts(x=ds[,-1], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))
dygraphs::dygraph(ts)

ds <- na.omit(ds)

ts <- as.xts(ds)

p <- plot_ly(x = as.POSIXct(ds$t), y = ds$v, mode = 'lines', text = paste( "days from today"))
p


mdeaths cns <-names(df)
# change over of daylight saving causes this to throw an error if we don't get rid of resultant NAs in the index
d <- as.POSIXct(str_trim(ds$t) , format = "%Y-%m-%d %H:%M:%S")

ds[2,1] <- 22
ds[2,2] <- 27

ts <- xts(x=ds[,-1], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))
dygraphs::dygraph(ts)



