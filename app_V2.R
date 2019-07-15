
#####   Sensor Viewer App   ###########

library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)

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
source('appConfig.R')
#source("helpers.R")

###   App Setup  #####


#https://shiny.esoil.io/SensorViewer


con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RO)


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

  dashboardHeader(title = "miSensors" , titleWidth = 150 ),
  
####    Dashboard  Sidebar  ####
   
  dashboardSidebar(
    
    tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('manipulateMenuItem', function(message){
        var aNodeList = document.getElementsByTagName('a');

        for (var i = 0; i < aNodeList.length; i++) {
          if(aNodeList[i].getAttribute('data-value') == message.tabName) {
            if(message.action == 'hide'){
              aNodeList[i].setAttribute('style', 'display: none;');
            } else {
              aNodeList[i].setAttribute('style', 'display: block;');
            };
          };
        }
      });
    "))),
    
    
    collapsed = F, width = 150,
                   sidebarMenu(id="mysidebar",
                     menuItem("View Sensors", icon = icon("list-alt", lib = "glyphicon"), tabName = "viewSensors"),
                     menuItem("Download Data", icon = icon("download-alt", lib = "glyphicon"), tabName = "extractdata", selected = T),
                     menuItem("Manage Sensors", tabName = "managesensors", icon = icon("cloud-download", lib = "glyphicon")),
                     menuItem("Login", tabName = "login", icon = icon("log-in", lib = "glyphicon")),
                     menuItem("Preferences", tabName = "preferences", icon = icon("calendar", lib = "glyphicon"))
                   )





  ),


####  Dashboard Body  ####

  dashboardBody(

    tags$head(tags$link( rel="icon", type="image/png", href="favicon-32x32.,png", sizes="32x32" ),
              tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js"),
              tags$script(src = "message-handler.js")

    ),
    #tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/js-cookie@2/src/js.cookie.min.js")),
    useShinyjs(),
    useShinyalert(),
    extendShinyjs(text = jsCode),

    tabItems(
      
####  View Sensors Tab  ####
      
      tabItem(tabName = "viewSensors",
              #h2("View Sensors"),
              # fluidRow(
              #   infoBox("Summary", icon = icon("credit-card"), fill = F)
              # ),
              fluidRow(column(1), column(11,selectInput("pickSensor", "Sensor Name", list(c('None'))))),
              fluidRow(column(1), column(11,selectInput("pickDataStreamType", "Sensor Type", list(c('None'))))),
              fluidRow(verbatimTextOutput("debug")),
              fluidRow(
                tabsetPanel(
                  tabPanel("Charts",
                           fluidRow(column(1), column(11,  htmlOutput("errMsgBox", inline = T))),
                           fluidRow(column(1), column(11, withSpinner(amChartsOutput(outputId = "amchart"))))),

                  tabPanel("Calendar",   fluidRow(column(1), column(11, withSpinner(fullcalendarOutput("Cal", width = "100%", height = "400px"))))),
                  tabPanel("Table",withSpinner( tableOutput('SensorValsTable')))
                )
              )
      ),
      
####  Manage Sensors Tab ####     
      tabItem(tabName = "managesensors",

                tabsetPanel(
                  tabPanel("Add Sensors", 
                           div(style="display: inline-block;vertical-align:top; width: 75px; padding-top: 23px;",  
                               actionBttn(
                                      inputId = "addSensor_btn",
                                      label = "Add ",
                                      color = "success",
                                      style = "simple",
                                      size = 'sm',
                                      icon = icon("thumbs-up"),
                                      block = F
                                    )),
                           div(style="display: inline-block;vertical-align:top; width: 20px;padding-top: 0px;"),
                           div(style="display: inline-block;vertical-align:top;  padding-top: 1px;",  textInput('mySensorName', '', width=240, placeholder = "Specify your own name if you want")),
                           

                    fluidRow(column(12 , leafletOutput("select_SelectMap", width = "350", height = "350"))),
                    fluidRow(column(12, rHandsontableOutput('select_SelectedSensorTable',  width = "350")))
                    
                  ),
                  tabPanel("My Sensors", 
                           div(style="display: inline-block;vertical-align:top; width: 260px; padding-top: 0px;",
                              selectInput("mySensors", "", list(c('None')))
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                           div(style="display: inline-block;vertical-align:top; width: 50px; padding-top: 23px;",  
                               
                               actionBttn(
                                 inputId = "deleteSensor_btn",
                                 label = "",
                                 color = "danger",
                                 style = "simple",
                                 size = 'sm',
                                 icon = icon("trash"),
                                 block = F
                               )),
                           fluidRow(column(12, rHandsontableOutput('mySensors_Table',  width = "350", height = 200)))
      ))),
      
      


####  Preferences Tab ####

      tabItem(tabName = "preferences",
              
              selectInput("prefDefaultSite", label = "Default location to display", choices = NULL),
              knobInput(
                inputId = "prefDaysToExtract",
                label = "Days since today to display",
                value = 30,
                min = 1,
                max = 365,
                displayPrevious = TRUE, 
                lineCap = "round",
                fgColor = "red",
                inputColor = "Red",
                bgColor = "#428BCA",
                immediate = F
              ),

              actionBttn(
                inputId = "prefSave",
                label = "Save ",
                color = "success",
                style = "simple",
                size = 'sm',
                icon = icon("save"),
                block = F
              )
      ),

####  Login Tab  ####

      tabItem(tabName = "login",

              textInput('username', 'User', placeholder = 'ross.searle@csiro.au'),
              passwordInput('password', 'Password', placeholder = 'rossiscool'),
              actionBttn(
                inputId = "login",
                label = "Login ",
                color = "success",
                style = "simple",
                size = 'sm',
                icon = icon("check"),
                block = F
              ),
              actionBttn(
                inputId = "logout",
                label = "Logout ",
                color = "danger",
                style = "simple",
                size = 'sm',
                icon = icon("times"),
                block = F
              ),

              uiOutput(outputId = "textusrID")
      ),
#####  Data Extraction Tab  ####

      tabItem(tabName = "extractdata" , 
              fluidPage(
                  #fluidRow(div( htmlOutput("message1"), style=paste0('color:', 'green', '; width: 850px;'))),
                  fluidRow(
                              column(5, 
                                    leafletOutput("extractDataMap", width = "650", height = "550"),
                                    br(),
                                    rHandsontableOutput("extractDataSiteInfoTable", width = "650")
                                    ),
                              column(7,
                                    div(style="display: inline-block;vertical-align:top; width: 250px;",selectInput("SelectedSiteDataDownload", "Select Site ", width = 300, choices = c(''))),
                                    div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                    div(style="display: inline-block;vertical-align:top; width: 200px;", selectInput("DataTypeDataDownload", "Select Site ", width = 300, choices = c('Rainfall', 'Soil-Moisture'))),
                                    div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                                    div(style="display: inline-block;vertical-align:top; width: 200px;", dateRangeInput('dateRange',label = 'Date range input: yyyy-mm-dd',start = Sys.Date() - 365, end = Sys.Date())),
                                    #withBusyIndicatorUI( actionButton("fetchSensorData","Ping Sensor")),
                                    rHandsontableOutput("sensorDataTable", width = "650")
                              ) 
                      )
            )
    )
   )
  )
)


######  Server Code   ############
server <- function(input, output, session) {

  
  
  print('xxxxxxxxxxxxx')
#####   Set Global Variables   #########
  
  
  print("This is a debig statement")

  d <- getURL(paste0(senFedPath,"getSensorLocations"))
  sensorLocs <- fromJSON(d)
  
 

  RV <- reactiveValues()
  RV$SelectedSite <- NULL
  RV$CurrentUser <- NULL
  RV$sensorLocs <- sensorLocs
  RV$pref_defSite <- NULL
  RV$pref_daysSince <- NULL
  RV$error <- NULL
  RV$AppType <- NULL
  RV$CurrentSensorData <- NULL
  
  
  #### Extract Data - Show selected site in table ####
  
  
  
  observeEvent(input$fetchSensorData, {
    
    #withBusyIndicatorServer("fetchSensorData", {
    
    dtype <- input$DataTypeDataDownload
    locID <- input$SelectedSiteDataDownload
    
    #if(has_internet()){
    # if(dtype != 'None' & locID != 'None' & locID != ''  & dtype != ''){
    
    #tryCatch({
    
    dnowYMD <- format(Sys.time(), "%Y-%m-%d")
    isoEDate <- paste0(format(Sys.time(), "%Y-%m-%d"), 'T00:00:00')
    
    edp <- strptime(isoEDate, "%Y-%m-%dT%H:%M:%S")
    # back <- (60 * 60 * 24 * RV$pref_daysSince) + 1
    back <- (60 * 60 * 24 * 20) + 1
    py <- edp - back
    isoSDate <- str_replace_all(as.character(py), ' ', 'T')
    print(head(sensorLocs))
    sensorID <- sensorLocs[sensorLocs$SiteName == locID,  ]$SiteID
    print(sensorID)
    
    url <- paste0(senFedPath, "getSensorDataStreams?siteid=", sensorID,"&sensortype=", dtype,"&startdate=", isoSDate, '&enddate=', isoEDate, "&aggperiod=days")
    
    
    print(url)
    resp <- getURL(paste0(url))
    if(responseIsOK(resp)){
      ts <- convertJSONtoTS(resp)
      print(head(ts))
      if(is.null(colnames(ts))){
        colnames(ts) <- c( dtype)
      }
      if(nrow(ts) > 0){
        RV$CurrentSensorData <- ts
        print(RV$CurrentSensorData)
        RV$error <- ''
      }
    }else{
      e <- fromJSON(resp)
      stop(e$error)
    }
    
    # }, error = function(err) {
    #   #shinyalert("Oops", err$message, type = 'error')
    #   
    #   RV$error <- 'It would appear there is no data to display for this sensor.'
    #   
    #   
    # },finally = {
    #   
    # })
    # }
    # }else{
    #   
    #   session$sendCustomMessage(type = 'errorMessage', message = paste0('There was a problem connecting to the internet'))
    # }
    
    #})
  })
  
  
  
  
  
  
  output$sensorDataTable  = renderRHandsontable({
    
    req(RV$CurrentSensorData)
    
    ts <- data.frame(DateTime= as.character(index(RV$CurrentSensorData)), coredata(RV$CurrentSensorData),row.names=NULL)
    tso <-  ts[rev(order(as.Date(ts$Date))),]
    fd <- format(as.Date(tso$Date), "%d-%m-%Y")
    DF <- data.frame(Date=fd, tso[,-1])
    colnames(DF) <- c('Date',colnames(appData$currentTS))
    
    rhandsontable(  DF,  height = 600, manualColumnResize = F, readOnly = TRUE, rowHeaders = F, colHeaders = F)
  })
  
  
  
  output$extractDataSiteInfoTable  = renderRHandsontable({
    
    req(RV$sensorLocs)
    
    click <- input$extractDataMap_marker_click
    if(is.null(click))
      return()
    
    df <- RV$sensorLocs
    
    dfs <- df[df$SiteID == click$id,  ]
    
    RV$SelectedSite <-  click$id
    
    
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
    
    updateSelectInput(session, 'SelectedSiteDataDownload', selected = dfs$SiteName)
    
    rhandsontable( outdf,  height = 600, manualColumnResize = F, readOnly = TRUE, rowHeaders = F, colHeaders = F)
    
  })
  
  
  observe({
    req(RV$sensorLocs)
    updateSelectInput(session, "SelectedSiteDataDownload", choices = RV$sensorLocs$SiteName )
  })
  
  
  # acm_defaults2 <- function(map, x, y) addCircleMarkers(map, x, y, radius=60, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  # 
  # observe({
  # 
  #   df <- RV$sensorLocs
  #   dfs <- df[df$SiteName == input$SelectedSiteDataDownload,  ]
  #   print(dfs$Longitude)
  #   proxy <- leafletProxy("extractDataMap")
  # 
  #     proxy %>% removeMarker(layerId="Selected")
  # 
  #     #proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
  #     proxy %>% acm_defaults(dfs$Latitude, dfs$Longitude)
  # })
  # 
  
  # acm_defaults3 <- function(map, x, y) addCircleMarkers(map, x, y, radius=6, color="black", fillColor="orange", fillOpacity=1, opacity=1, weight=2, stroke=TRUE, layerId="Selected")
  
  observeEvent(input$extractDataMap_marker_click, { # update the map markers and view on location selectInput changes
    
    p <- input$extractDataMap_marker_click
    if(is.null(p))
      return()
    
    proxy2 <- leafletProxy("extractDataMap")
    
    if(p$id=="Selected"){
      proxy2 %>% removeMarker(layerId="Selected")
    } else {
      #proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat)
      proxy2 %>% acm_defaults(p$lng, p$lat)
    }
  })
  
  
  #### Extract Data - Draw map ####
  
  output$extractDataMap <- renderLeaflet({
    
    req(RV$sensorLocs)
    
    
    leaflet() %>%
      clearMarkers() %>%
      addTiles(group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      
      setView(lng = 135, lat = -28, zoom = 4) %>%
      #addMouseCoordinates(style = "basic") %>%
      addEasyButton(
        easyButton(
          icon = 'fa-globe',
          title="Zoom to Full Extent",
          id='b2',
          onClick=JS("function(btn, map){map.setView([-28, 135], 4); }"))
      )%>%
      
      addControlGPS() %>%
      
      addLayersControl(
        baseGroups = c("Map", "Satelite Image"),
        #overlayGroups = c("Moisture Maps", "All Sensors"),
        options = layersControlOptions(collapsed = FALSE)
      ) #%>%
  })
  
  observe({
    
    req(RV$sensorLocs)
    
    sdf <- RV$sensorLocs
    
    labs <- lapply(seq(nrow(sdf)), function(i) {
      paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
              '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
              '<li>Backend : ', sdf[i, "Backend"], '</li>',
              '<li>Access : ', sdf[i, "Access"], '</li>',
              '<li>Site ID : ', sdf[i, "SiteID"], '</li>'
              #,'<li>Available Sensors : ',  paste(tsens, collapse = ', '), '</li>'
      )
    })
    
    colCnt <- length(unique(sdf[,"SensorGroup"]))
    colCats <- unique(sdf[,"SensorGroup"])
    colField <- sdf[,"SensorGroup"]
    factpal <-colorFactor(RColorBrewer::brewer.pal(colCnt, 'Spectral'), colField)
    
    proxy <- leafletProxy("extractDataMap", data = RV$sensorLocs) 
    proxy %>% clearMarkers()
    proxy %>% clearControls()
    proxy %>% addCircleMarkers(   lng = ~Longitude, lat = ~Latitude, 
                                  label = lapply(labs, HTML),
                                  stroke = FALSE, 
                                  fillOpacity = 1,
                                  color = factpal(sdf[,"SensorGroup"]), 
                                  radius = 4, 
                                  layerId=paste0(sdf$SiteID), 
                                  group = "Sensors" )
    proxy %>% setView(lng = center()[1],lat = center()[2],zoom = zoom())
    proxy %>% leaflet::addLegend("bottomleft", pal = factpal, values = colCats,title = input$SensorLabel)
  })
  
  center <- reactive({
    if(is.null(input$moistureMap_center)){
      return(c(135, -28))
    }else{
      return(input$moistureMap_center)
    }
  })   
  
  zoom <- reactive({
    ifelse(is.null(input$moistureMap_zoom),4,input$moistureMap_zoom)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

 
  

  
  #####    Determine if we are wanting the mobile or destop version  ########
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query[['apptype']])) {
      RV$AppType <- query[['apptype']]
    }else{
      RV$AppType <- 'mobile'
    }
    print(RV$AppType)
  })
 
  
  observe({
     if(RV$AppType != "mobile"){
       session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "extractdata"))
     }else{
       session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "extractdata"))
     }
  })

  
  
  #####    Authentication   ######################
  status <- reactiveVal(value = NULL)
  appAuth <- reactiveValues(loggedIn = F, currentUsr = '')
  appData <- reactiveValues(currentTS = NULL)

  js$getcookie()
  
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  observeEvent(input$mysidebar,
               {
                 
                 if( RV$AppType == 'mobile'){
                     # for desktop browsers
                     addClass(selector = "body", class = "sidebar-collapse")
                     # for mobile browsers
                     removeClass(selector = "body", class = "sidebar-open")
                 }
               })

  # check if a cookie is present and matching our super random sessionid
  observe({


    cooky <- input$jscookie


    if (!is.null(cooky) ) {

      lbits <- str_split( cooky, 'XXXXX')

      usrn <- lbits[[1]][1]
      pwd <- lbits[[1]][2]

      if(length(lbits[[1]]) == 2){


        updateTextInput(session, "username", value = usrn)
        updateTextInput(session, "password", value = pwd)

        sql <- paste0("Select usr, pwd from appUsers where usr = '", usrn, "'")

        res <- dbSendQuery(con, sql)
        df <- dbFetch(res)

        nrecs <- dbGetRowCount(res)
        dbClearResult(res)

        if(nrecs > 0){
          #Check against password

          dbusr <- df[1,1]
          hash <- df[1,2]

          if( identical(hash, digest(pwd, serialize=T))){

            appAuth$loggedIn <- T
            appAuth$currentUsr <- usrn
            status(paste0('in as ', usrn))
            RV$CurrentUser <- usrn

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
              sqlInsert <- paste0("Insert into appUsers ( usr, pwd, daysSince ) values ('", usrn, "', '", md5 ,"', DefaultDaysSince)")
 
              res <- dbSendStatement(authcon, sqlInsert)
              dbGetRowsAffected(res)
              dbDisconnect(authcon)

              appAuth$loggedIn <- T
              appAuth$currentUsr <- usrn
              status(paste0('in as ', usrn))

            }, warning = function(w) {
              #warning-handler-code
            }, error = function(e) {

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



  
  #####    Update location Info   ####################################

  
  observe({
    if(appAuth$loggedIn){
      sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "'")
      res <- dbSendQuery(con, sql)
      sns <- dbFetch(res)
      dbClearResult(res)
      #updateSelectInput(session, "pickSensor", choices =  sns$locationID)
      updateSelectInput(session = session, inputId = 'pickSensor', choices = sns$localName )
    }
  })

  observe({
    if(appAuth$loggedIn){
      
      req(input$pickSensor)
        sensorID <- getSensorIDFronLocalName(con, appAuth$currentUsr, input$pickSensor)
        url <- paste0(senFedPath,'getSensorInfo?siteid=', sensorID)
        stnsRaw <- getURL(paste0(url))
        stnsJ <- fromJSON(stnsRaw)
        sns <- unique(stnsJ$DataType)
        updateSelectInput(session, "pickDataStreamType", choices =  sns, selected = DefaultSensor)
      }
  })
  
  
  
  ####   Get data from a sensor ######

  observe({

    dtype <- input$pickDataStreamType
    locID <- input$pickSensor

    if(has_internet()){
        if(dtype != 'None' & locID != 'None' & locID != ''  & dtype != ''){
          
          tryCatch({
    
              dnowYMD <- format(Sys.time(), "%Y-%m-%d")
              isoEDate <- paste0(format(Sys.time(), "%Y-%m-%d"), 'T00:00:00')
        
              edp <- strptime(isoEDate, "%Y-%m-%dT%H:%M:%S")
              back <- (60 * 60 * 24 * RV$pref_daysSince) + 1
              py <- edp - back
              isoSDate <- str_replace_all(as.character(py), ' ', 'T')
        
              sensorID <- getSensorIDFronLocalName(con, appAuth$currentUsr, input$pickSensor)
              
              url <- paste0(senFedPath, "getSensorDataStreams?siteid=", sensorID,"&sensortype=", dtype,"&startdate=", isoSDate, '&enddate=', isoEDate, "&aggperiod=days")

              resp <- getURL(paste0(url))
              if(responseIsOK(resp)){
                  ts <- convertJSONtoTS(resp)
                  if(is.null(colnames(ts))){
                    colnames(ts) <- c( dtype)
                  }
                  if(nrow(ts) > 0){
                    appData$currentTS <- ts
                    RV$error <- ''
                  }
              }else{
                e <- fromJSON(resp)
                stop(e$error)
              }
              
        }, error = function(err) {
          #shinyalert("Oops", err$message, type = 'error')

          RV$error <- 'It would appear there is no data to display for this sensor.'
          
          
        },finally = {
          
        })
        }
    }else{
      
      session$sendCustomMessage(type = 'errorMessage', message = paste0('There was a problem connecting to the internet'))
      
    }
  })


output$errMsgBox <- renderText({
  paste0('<h3><span style="color: #ff0000;">', RV$error, '</span></h3>')
  
})


#### View Sensors - Output sensor Values table  #####

  output$SensorValsTable <- renderTable({

    
    ts <- data.frame(DateTime= as.character(index(appData$currentTS)), coredata(appData$currentTS),row.names=NULL)
    tso <-  ts[rev(order(as.Date(ts$Date))),]
    fd <- format(as.Date(tso$Date), "%d-%m-%Y")
    DF <- data.frame(Date=fd, tso[,-1])
    colnames(DF) <- c('Date',colnames(appData$currentTS))
    DF
  }, digits = 1)




#### View Sensors - Output sensor Chart #####

  output$amchart <- renderAmCharts({

    req(appData$currentTS)

    if(input$pickDataStreamType == 'Rainfall'){
      
      ts <- data.frame(DateTime= as.character(format(index(appData$currentTS), format="%d-%m-%Y")), coredata(appData$currentTS),row.names=NULL)
      
      colnames(ts) <- c('Date', 'Vals')
      ts$Vals[ts$Vals == 0] <- NA
      amBarplot(x = "Date", y = "Vals", data = ts, labelRotation = -45,show_values = TRUE, groups_color = c("#87cefa"))

    }else{

      ts <- data.frame(DateTime= as.character(index(appData$currentTS)), coredata(appData$currentTS),row.names=NULL)
      #colnames(ts) <- c('Date', 'Vals')
      print(head(ts))
      ts$Date <- as.POSIXct(as.character(ts$Date))
      print(colnames(ts)[-1])
      amTimeSeries(data=ts, col_date = 'Date', col_series = colnames(ts)[-1])
    }




  })


#### View Sensors - Output sensor Calendar  #####

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


####   Manage Sensors - Draw leaflet map  ######

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


####   Manage Sensors - Show sensor table ######

  output$select_SelectedSensorTable  = renderRHandsontable({

    req(RV$sensorLocs)


    click <- input$select_SelectMap_marker_click
    if(is.null(click))
      return()

    df <- RV$sensorLocs

    dfs <- df[df$SiteID == click$id,  ]

    RV$SelectedSite <-  click$id


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

    updateTextInput(session, 'mySensorName', value = dfs$SiteName)
    
    rhandsontable( outdf,  height = 600, manualColumnResize = F, readOnly = TRUE, rowHeaders = F, colHeaders = F)

  })
  
  
  observe({
    req(RV$SelectedSite)
    df <- RV$sensorLocs
    
    dfs <- df[df$SiteID == RV$SelectedSite,  ]
    updateTextInput(session, "mySensorName", value = dfs$SiteName)
    
  })

  ####   Manage Sensors - select sensor from map  ######

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
  
  
  ####   Manage Sensors - delete sensor  ######
  
  observeEvent(input$deleteSensor_btn, {
    
    req(input$mySensors)
    
    conInsert <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
    rs <- dbSendStatement(conInsert, "delete from appUserLocations where  usr = :x  and localName = :y")
    dbBind(rs, param = list(x = str_to_lower(appAuth$currentUsr), y = input$mySensors))
    dbGetRowsAffected(rs)
    dbClearResult(rs)
    dbDisconnect(conInsert)
    
    sns <- getListofUserSensors( con, appAuth$currentUsr)
    
    updateSelectInput(session = session, inputId = 'mySensors', choices = sns$localName )
    updateSelectInput(session = session, inputId = 'pickSensor', choices = sns$localName )
    updateSelectInput(session = session, inputId = 'prefDefaultSite', choices = sns$localName )
    
    shinyalert('Success', paste0('Site ', input$mySensors , ' removed from your list of locations'), type = 'success')
    
    
  })


  
  
  ####   Manage Sensors - Add sensor  ######  

  observeEvent(input$addSensor_btn, {

    req(RV$SelectedSite)
      
      sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "' and locationID = '", RV$SelectedSite, "'")
      res <- dbSendQuery(con, sql)
      sns <- dbFetch(res)
      dbClearResult(res)
      
      if(nrow(sns) > 0){
        shinyalert('Site Exists', "This location is already included in you list of sensors", type = 'warning')
        return()
      }

        result <- tryCatch({
        conInsert <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
    
        rs <- dbSendStatement(conInsert, "insert into appUserLocations ( usr, locationID, localName) values (:x, :y, :z)")
        dbBind(rs, param = list(x = RV$CurrentUser, y = RV$SelectedSite, z = input$mySensorName))
        dbGetRowsAffected(rs)
        dbClearResult(rs)
        
        sns <- getListofUserSensors( con, appAuth$currentUsr)
        
        updateSelectInput(session = session, inputId = 'mySensors', choices = sns$localName )
        updateSelectInput(session = session, inputId = 'pickSensor', choices = sns$localName )
        updateSelectInput(session = session, inputId = 'prefDefaultSite', choices = sns$localName )
        
        shinyalert('Success', paste0('Site ', input$mySensorName , ' successfully added to your list of locations'), type = 'success')
 
    }, error = function(err) {
      #shinyalert('Oops', paste0('There was a problem adding  ', input$mySensorName , '  to your list of locations'), type = 'error')
      shinyalert('Oops', paste0(err$message), type = 'error')

    },finally = {

      dbDisconnect(conInsert)

    })
  })
  
  
  
  output$select_MySensorsTable  = renderRHandsontable({
    rhandsontable( recs )
  })
  
  
  observe({
    
   req( RV$CurrentUser)
    
    sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "'")
    res <- dbSendQuery(con, sql)
    sns <- dbFetch(res)
    dbClearResult(res)
    
    sqlp <- paste0("select * from appUsers where usr = '", str_to_lower(appAuth$currentUsr), "'")
    resp <- dbSendQuery(con, sqlp)
    prefs <- dbFetch(resp)
    dbClearResult(resp)
    
    RV$pref_defSite <- prefs$defaultSite
    RV$pref_daysSince <- prefs$daysSince
    
    updateKnobInput(session = session,inputId = "prefDaysToExtract",value = RV$pref_daysSince)
    updateSelectInput(session = session, inputId = 'prefDefaultSite', choices = sns$localName, selected = RV$pref_defSite)
    updateSelectInput(session, 'pickSensor', choices = sns$localName, selected = RV$pref_defSite)
    updateSelectInput(session = session, inputId = 'mySensors', choices = sns$localName )
  })

 #### Manage Sensors - Show sensor info   ######  
  output$mySensors_Table  = renderRHandsontable({
    
    req(input$mySensors)
    
     sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "' and localName = '", input$mySensors, "'")
     
     res <- dbSendQuery(con, sql)
     sns <- dbFetch(res)
     dbClearResult(res)
     
     if(nrow(sns) > 0){

        df <- RV$sensorLocs
        
        dfs <- df[df$SiteID == sns$locationID,  ]
        
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
    
     }
  })
  
  
  
  ####   Preferences  ######
  
  observeEvent(input$prefSave, {
    
    defSite <- input$prefDefaultSite
    defDaysSince <- input$prefDaysToExtract
    
    tryCatch({
      
      conUpdate <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
      rs <- dbSendStatement(conUpdate, "Update appUsers SET daysSince = :y, defaultSite = :z where  usr = :x")
      dbBind(rs, param = list(x = str_to_lower(appAuth$currentUsr), y = defDaysSince, z = defSite))
      dbGetRowsAffected(rs)
      #dbClearResult(rs)
      dbDisconnect(conUpdate)
      
      RV$pref_daysSince <- defDaysSince
      RV$RV$pref_defSite <- defSite
      
      shinyalert("All Good", "Your preferences were successfully saved", type = 'success')
    }
    , error = function(err) {
      shinyalert("Oops", err$message, type = 'error')
    },finally = {
      
    })
    
  })
  
  
  
  

}

shinyApp(ui, server)