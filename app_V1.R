library(shiny)
library(shinydashboard)
library(shinyjs)
library(stringr)
library(RSQLite)
library(DBI)
library(digest)
library(RCurl)
library(dygraphs)
#library(rjson)
library(jsonlite)
library(xts)
library(DT)
library(plotly)
library(rAmCharts)
library(fullcalendar)
library(leaflet.extras)
library(rhandsontable)

source('appUtils.R')

#Sys.info()
#https://shiny.esoil.io/SensorViewer

usrID = 'ross.searle@csiro.au'
passwd = 'rossiscool'
usrID = 'a'
passwd = 'b'
#password_hash <- hashpw(passwd)

machineName <- as.character(Sys.info()['nodename'])
if(machineName == 'FANCY-DP'){
  dbPath <- "C:/Users/sea084/Dropbox/RossRCode/Git/SensorFederator/DB/SensorFederator.sqlite"
}else if (machineName == 'soils-discovery') {
  dbPath <- "/srv/shiny-server/SensorViewer/DB/SensorView.sqlite"
}else{
  dbPath <- ""
}
con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RO)

senFedPath <- 'http://esoil.io/SensorFederationWebAPI/SensorAPI/'
DefaultSensor <- 'Rainfall'

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

jsCode <- '
  shinyjs.getcookie = function(params) {
    var cookie = Cookies.get("id");
    if (typeof cookie !== "undefined") {
      Shiny.onInputChange("jscookie", cookie);
    } else {
      var cookie = "";
      Shiny.onInputChange("jscookie", cookie);
    }
  }
  shinyjs.setcookie = function(params) {
    Cookies.set("id", escape(params), { expires: 31536000});  
    Shiny.onInputChange("jscookie", params);
  }
  shinyjs.rmcookie = function(params) {
    Cookies.remove("id");
    Shiny.onInputChange("jscookie", "");
  }
'

#Cookies.set("id", escape(params), { expires: 0.5 });  

#Cookies.set("id", escape(params), { expires: 0.5 });  


###########  UI   ########################
ui <- dashboardPage(
  
  dashboardHeader(title = "Sensor Viewer" , titleWidth = 150 ),
  dashboardSidebar(collapsed = F, width = 150,
                   sidebarMenu(
                     menuItem("View Sensors", icon = icon("list-alt", lib = "glyphicon"), tabName = "viewSensors"),
                     menuItem("Add Sensors", tabName = "addSensors", icon = icon("cloud-download", lib = "glyphicon")),
                     menuItem("Login", tabName = "login", icon = icon("log-in", lib = "glyphicon")),
                     menuItem("Set Date Range", tabName = "dateRange", icon = icon("calendar", lib = "glyphicon"))
                   )
                   
                   
                   
                   
                   
  ),
  dashboardBody(
    
    tags$head(tags$link( rel="icon", type="image/png", href="favicon-32x32.png", sizes="32x32" ),
              tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js"),
              tags$script(src = "message-handler.js")
              
    ),
    #tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js")),
    useShinyjs(),
    extendShinyjs(text = jsCode),
    
    tabItems(
      tabItem(tabName = "addSensors",
              
             
             fluidRow(
               
               tabsetPanel(
                  
                 
                 
                 tabPanel("Add From Map", fluidRow(
                   # column(1),
                   column(3, selectInput("select_SensorType", "Sensor Type", c('All', 'Rainfall', 'Soil-Moisture'), width=150)),
                   column(2, actionButton("addSensor_btn","Add Sensor", width=100)),
                   column(3, actionButton("addSensor_btn","Add All in View", width=150))),
                   fluidRow(column(12 , leafletOutput("select_SelectMap", width = "450", height = "350"))),
                   fluidRow(column(12, rHandsontableOutput('select_SelectedSensorTable',  width = "450")))
                  ),
                 tabPanel("My Sensors", rHandsontableOutput('select_MySensorsTable',  width = "450"))
                 
             ))
              
              
             
      ),
      
      tabItem(tabName = "viewSensors",
              h2("View Sensors"),
              # fluidRow(
              #   infoBox("Summary", icon = icon("credit-card"), fill = F)
              # ),
              fluidRow(
                column(1),
                column(11,
                   selectInput("pickSensor", "Sensors:", list(c('None')))
                )),
                fluidRow(
                  column(1),
                  column(11,
                  selectInput("pickDataStreamType", "Data Stream:", list(c('None')))
                  )),
              fluidRow(
                verbatimTextOutput("debug")
              ),
              fluidRow(
                
                tabsetPanel(
                  tabPanel("Charts", 
                           fluidRow(
                             column(1),
                             column(11, amChartsOutput(outputId = "amchart"))
                             )),
                  #tabPanel("Plotly", plotlyOutput("plotLY")),
                  #tabPanel("Charts",  dygraphOutput("dygraph")),
                 
                  
                  tabPanel("Calendar",   fluidRow(
                    column(1),
                    column(11, fullcalendarOutput("Cal", width = "100%", height = "400px"))
                  )),
                  tabPanel("Table", 
                           tableOutput('SensorValsTable')
                           #rHandsontableOutput('SensorInfoTable')
                           ),
                  
                  tabPanel("Dev",  tableOutput('dev'))
                )
                
                  
              ),
              fluidRow(
               # DT::dataTableOutput("SensorValsTable")
               
              )
              
      ),
      
      tabItem(tabName = "dateRange",
              dateRangeInput('sensorDateRange',label = 'Date range : yyyy-mm-dd',start =  as.Date('2017-05-27'), end = as.Date('2017-06-29'))
      ),
      
      tabItem(tabName = "login",
              
              
              textInput('username', 'User', placeholder = 'ross.searle@csiro.au'),
              passwordInput('password', 'Password', placeholder = 'rossiscool'),
              actionButton('login', 'Login'),
              actionButton('logout', 'Logout'),
              uiOutput(outputId = "textusrID")

              
              
      )
    )
    
  )
)

######  Servver Code   ############

server <- function(input, output, session) {
  
 
  d <- getURL("http://esoil.io/SensorFederationWebAPI/SensorAPI/getSensorLocations")
  sensorLocs <- fromJSON(d)
  RV <- reactiveValues()
  RV$sensorLocs <- sensorLocs
   
  
  ########       Authentication   ######################
  status <- reactiveVal(value = NULL)
  appAuth <- reactiveValues(loggedIn = F, currentUsr = '')
  appData <- reactiveValues(currentTS = NULL)
  
  
  
  js$getcookie()
  
  # check if a cookie is present and matching our super random sessionid  
  observe({
    
    
    cooky <- input$jscookie
    print(paste0('cookie = ', cooky ))
    #print ('DBSTUFF')
    
    if (!is.null(cooky) ) {
      
      lbits <- str_split( cooky, 'XXXXX')
     
      usrn <- lbits[[1]][1]
      pwd <- lbits[[1]][2]
      
      if(length(lbits[[1]]) == 2){
      
      print(paste0('user = ', usrn))
      
      updateTextInput(session, "username", value = usrn)
      updateTextInput(session, "password", value = pwd)

        sql <- paste0("Select usr, pwd from appUsers where usr = '", usrn, "'")
        print(sql)
        res <- dbSendQuery(con, sql)
        df <- dbFetch(res)

        nrecs <- dbGetRowCount(res)
        dbClearResult(res)

        if(nrecs > 0){
          #Check against password
          
          dbusr <- df[1,1]
          hash <- df[1,2]
          # print(hash)
          # print(pwd)
          print(identical(hash, digest(pwd, serialize=T)))
          
         if( identical(hash, digest(pwd, serialize=T))){
           
           appAuth$loggedIn <- T
           appAuth$currentUsr <- usrn
           status(paste0('in as ', usrn))
           
         }else{
           appAuth$loggedIn <- F
           appAuth$currentUsr <- ''
           status(paste0('out'))
         }
         
          
          
        }else{
          #insert a new user
          
          usrn <- str_to_lower(usrn)
            
            if (isValidEmail(usrn)){
              
              result = tryCatch({
                
                authcon <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
                md5 <- digest(pwd, serialize=T)
                sqlInsert <- paste0("Insert into appUsers ( usr, pwd) values ('", usrn, "', '", md5 ,"')")
                print(sqlInsert)
                res <- dbSendStatement(authcon, sqlInsert)
                dbGetRowsAffected(res)
                dbDisconnect(authcon)
                
                appAuth$loggedIn <- T
                appAuth$currentUsr <- usrn
                status(paste0('in as ', usrn))
                
              }, warning = function(w) {
                #warning-handler-code
              }, error = function(e) {
                print(e)
                print(paste0("Couldn't add the user ", usrn ))
                appAuth$loggedIn <- F
                appAuth$currentUsr <- ''
                status(paste0("Couldn't add the user ", usrn ))
                
              }, finally = {
                dbClearResult(res)
              }
              )
            }else
              appAuth$loggedIn <- F
            appAuth$currentUsr <- ''
              status('out. It looks like the user name you provided is not a valid email address')
              
            }
          
      
      #status(paste0('in as ', lbits[[1]][1]))
    }else{
      appAuth$loggedIn <- F
      appAuth$currentUsr <- ''
      status('out because no cookie (zero length) is present on your device. Feel free to log back in manually')
    }
    }else{
      appAuth$loggedIn <- F
      appAuth$currentUsr <- ''
      status('out because no cookie is present on your device. Feel free to log back in manually')
    }
  })
  
  observeEvent(input$login, {

ck <- paste0(input$username,'XXXXX', input$password)
print(paste0('Setting cookie = ', ck))
      js$setcookie(ck)
  })
  
  observeEvent(input$logout, {
    appAuth$loggedIn <- F
    status('out')
    js$rmcookie()
  })
  
  output$textusrID <- renderText({
    HTML(paste0("<br><p><b>You are logged ",status(),"</b></p>"))
  })



  #############  Udate location Info   ####################################    
  
 observe({
   if(appAuth$loggedIn){
     sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "'")
     res <- dbSendQuery(con, sql)
     sns <- dbFetch(res)
     dbClearResult(res)
     updateSelectInput(session, "pickSensor", choices =  sns$locationID)
   }
 })
  
  observe({
    if(appAuth$loggedIn){
      url <- paste0(senFedPath,'getSensorInfo?siteid=VicAg_Youanmite')
      stnsRaw <- getURL(paste0(url))
      stnsJ <- fromJSON(stnsRaw) 
      #appData$currentTS <- stnsJ
      sns <- unique(stnsJ$DataType)
      updateSelectInput(session, "pickDataStreamType", choices =  sns, selected = DefaultSensor)
    }
  })

  observe({
    
    dtype <- input$pickDataStreamType
    locID <- input$pickSensor

    if(dtype != 'None' & locID != ''){
      
      dnowYMD <- format(Sys.time(), "%Y-%m-%d")
      isoEDate <- paste0(format(Sys.time(), "%Y-%m-%d"), 'T00:00:00')
      
      edp <- strptime(isoEDate, "%Y-%m-%dT%H:%M:%S")
      back <- (60 * 60 * 24 * 30 * 2) + 1 
      py <- edp - back
      isoSDate <- str_replace_all(as.character(py), ' ', 'T')

         url <- paste0(senFedPath, "getSensorDataStreams?siteid=", locID,"&sensortype=", dtype,"&startdate=", isoSDate, '&enddate=', isoEDate, "&aggperiod=days")
         print(url)
         stnsRaw <- getURL(paste0(url))
         ts <- convertJSONtoTS(stnsRaw)

        if(nrow(ts) > 0){
            appData$currentTS <- ts
        }
     }
  })
  
  

  
 
  output$SensorValsTable <- renderTable({ 
    
    ts <- data.frame(DateTime= as.character(index(appData$currentTS)), coredata(appData$currentTS),row.names=NULL)
   colnames(ts) <- c('Date', 'Rainfall')
   
   tso <-  ts[rev(order(as.Date(ts$Date))),]
   colnames(ts)[1] <- c('Date')
   
   fd <- format(as.Date(ts$Date), "%d-%m-%Y")
   DF <- data.frame(Date=fd, tso[,-1])
   
   DF 
 }, digits = 1)  
  
  
  
  output$amchart <- renderAmCharts({

    req(appData$currentTS)
    
    ts <- data.frame(DateTime= as.character(index(appData$currentTS)), coredata(appData$currentTS),row.names=NULL)
    
    colnames(ts) <- c('Date', 'Vals')
    if(input$pickDataStreamType == 'Rainfall'){
      
      ts$Date <- as.character(ts$Date)
      ts$Vals[ts$Vals == 0] <- NA
      amBarplot(x = "Date", y = "Vals", data = ts, labelRotation = -45,show_values = TRUE, groups_color = c("#87cefa")) 
      
    }else{
      
      ts$Date <- as.POSIXct(as.character(ts$Date))
      amTimeSeries(ts, 'Date', c('Vals'))
    }
    
    
    
    
  })
  
  
  
  output$Cal <- renderFullcalendar({
    
    ts <- data.frame(DateTime= as.character(index(appData$currentTS)), coredata(appData$currentTS),row.names=NULL)
    colnames(ts) <- c('Date', 'Vals')
    ts$Vals[ts$Vals == 0] <- NA
    ts <- na.omit(ts)
    bits <- str_split(as.character(ts$Date), ' ')
    dts <- sapply(bits, function (x) x[1])
    
    
    cdata = data.frame(title =ts$Vals,
                       start = dts,
                       end = dts,
                       color = c("blue"))
    
    
    fullcalendar(cdata)
    
    
  })
  
  
  
  output$select_SelectMap <- renderLeaflet({
    
    #mapext <- c(130,  150, -50, -15)
   
    req(RV$sensorLocs)
    
    df <- RV$sensorLocs
    leaflet(df) %>%
      
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
       setView(lng = 135, lat = -28, zoom = 4) %>%
      # addMouseCoordinates(style = "basic") %>%
      # addHomeButton(extent(mapext), 'Zoom to Full') %>%
       addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                        #popup = ~SiteName,
                        radius = 10,
                         color = 'yellow',
                         stroke = FALSE, fillOpacity = 1,
                         layerId=df$SiteID)
    
  })
  
  
 
  
  
  # output$select_SelectedSensorTable <- renderTable({ 
  #   
  #   click <-input$select_SelectMap_marker_click
  #   
  #   if(is.null(click))
  #     return()
  #   
  #   print(paste0('Click = ', click$id))
  #   
  #   df <- RV$sensorLocs
  #   
  #   dfs <- df[df$SiteID == click$id,  ]
  #   t(dfs)
  #   
  # })
  # 
  
  output$dev <- renderTable({ 
    
    RV$sensorLocs
     
  }, digits = 1)  
  
  
  output$select_SelectedSensorTable  = renderRHandsontable({
    
    req(RV$sensorLocs)
    
    
    click <- input$select_SelectMap_marker_click
    if(is.null(click))
      return()
    
    df <- RV$sensorLocs
    
    dfs <- df[df$SiteID == click$id,  ]
    outdf <- data.frame(att= character(10), vals=character(10), stringsAsFactors = F)
    outdf[1,1] <- 'Site Name'
    outdf[1,2] <- dfs$SiteName
    outdf[2,1] <- 'Owner'
    outdf[2,2] <- dfs$Owner
    outdf[3,1] <- 'Contact'
    outdf[3,2] <- dfs$Contact
    outdf[4,1] <- 'Location'
    outdf[4,2] <- paste0('Lon = ', dfs$Longitude, '  Lat = ', dfs$Latitude)
    outdf[5,1] <- 'Access'
    outdf[5,2] <- dfs$Access
    outdf[6,1] <- 'Active'
    outdf[6,2] <- as.character( dfs$Active )
    outdf[7,1] <- 'Provider'
    outdf[7,2] <- dfs$Backend
    outdf[8,1] <- 'Start Date'
    outdf[8,2] <- dfs$StartDate
    outdf[9,1] <- 'End Date'
    outdf[9,2] <- dfs$EndDate
    outdf[10,1] <- 'Description'
    outdf[10,2] <- dfs$Description
    rhandsontable( outdf,  height = 600, manualColumnResize = F, readOnly = TRUE, rowHeaders = F, colHeaders = F) 
    
  })
  
  
  acm_defaults <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  observeEvent(input$select_SelectMap_marker_click, { # update the map markers and view on location selectInput changes
    
    p <- input$select_SelectMap_marker_click
    if(is.null(p))
      return()
    
    proxy <- leafletProxy("select_SelectMap")
    
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      #proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
      proxy %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  
  
  
  
  observeEvent(input$addSensor_btn, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  output$select_MySensorsTable  = renderRHandsontable({
    
    print('HHHHHHHHHHHHHHHHHHHHHHHHHH')
  sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "'")
  res <- dbSendQuery(con, sql)
  sns <- dbFetch(res)
  dbClearResult(res)
 
  rhandsontable( sns )
  
  })
  
  
  
}

shinyApp(ui, server)