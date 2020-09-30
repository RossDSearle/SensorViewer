library(jsonlite)
library(curl)
library(httr)




getWebData <- function(url, verbose=F){
  
  rout <- NULL
  has_internet()
  # check for internet connection
  if(is.null(curl::nslookup("r-project.org", error = FALSE))){
    rout$Error=T
    rout$Message =  ''
    rout$Response= "Can't connect to the internet at the moment"
    rout$Url=url
    return(rout)
  }
  r <- GET(url, timeout = 300)
  
  # check for HTTP error codes  
  if(r$status_code!=200){
    rout$Error=T
    rout$Message =  http_status(r)$message
    rout$Response= ''
    rout$Url=url
    return(rout)
  }
  
  resp <- content(r, "text", encoding = 'UTF-8')
  o <- fromJSON(resp)
  
  if(!is.null(o$error)){
    rout$Error=T
    rout$Message =  http_status(r)$message
    rout$Response= o$error
    rout$Url=url
    return(rout)
  }
  
  if(verbose){
    print(url)
    print(head(o) )
  }
  rout$Error=F
  rout$Message =  http_status(r)
  rout$Response=content(r, "text", encoding = 'UTF-8')
  rout$Url=url
  return(rout)
}

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

