library(jsonlite)
library(curl)
library(httr)

getWebDataJSON <- function(url){
  resp <- GET(url, timeout = 300)
  response <- content(resp, "text", encoding = 'UTF-8')
  return(response)
}

getWebDataDF <- function(json){
  md <- fromJSON(json)
  return(md)
}

queryDB <- function(sql){
  con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RO)
  res <- dbSendQuery(con, sql)
  sns <- dbFetch(res)
  dbClearResult(res)
  dbDisconnect(con)
  return(sns)
  
}



getListofUserSensors <- function(con, usr){
  
  sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(usr), "'")
  res <- dbSendQuery(con, sql)
  sns <- dbFetch(res)
  dbClearResult(res)
  return(sns)
  updateSelectInput(session = session, inputId = 'mySensors', choices = sns$localName )
  updateSelectInput(session = session, inputId = 'pickSensor', choices = sns$localName )
  
}

getSensorIDFromLocalName <- function(usr, sensorName){
  sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(usr), "' and localName = '", sensorName, "'")
  sns <- queryDB(sql)
  #  res <- dbSendQuery(con, sql)
  # sns <- dbFetch(res)
  # dbClearResult(res)
  return(sensorID <- sns$locationID[1])
}



responseIsOK <- function(resp){
  o <- fromJSON(resp)
  val <- is.null(o$error)

  return(is.null(o$error))
}


has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

convertJSONtoDF <- function(resp){
  
  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))
  
  for (i in 1:nrow(xin)) {
   
    outDF <- cbind(outDF, xin$DataStream[[i]]$v)
    cnames[i+1] <-  paste0(xin$DataType[[i]], "_", xin$UpperDepth[[i]])
    
  }
  colnames(outDF) <- cnames

  return(outDF)
}





convertJSONtoTS <- function(resp){
  
  xin <- fromJSON(resp)
  outDF <- data.frame(xin$DataStream[[1]]$t)
  cnames<- c('DateTime', rep('x', nrow(xin)))
  
  for (i in 1:nrow(xin)) {
    
    outDF <- cbind(outDF, xin$DataStream[[i]]$v)
    cnames[i+1] <-  paste0(xin$DataType[[i]], "_", xin$UpperDepth[[i]])
    
  }
  colnames(outDF) <- cnames
  
  ds <- na.omit(outDF)
  d <- as.POSIXct(str_trim(ds$DateTime) , format = "%Y-%m-%d %H:%M:%S")
  ts <- xts(x=ds[,-1], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))
  

  
  return(ts)
}

