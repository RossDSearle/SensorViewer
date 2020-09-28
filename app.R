library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(dygraphs)
library(httr)
library(jsonlite)
library(xts)
library(shinybusy)
library(shinyjs)
library(rhandsontable)
library(DBI)
library(RSQLite)
library(glouton)

library(shinyalert)
 library(stringr)
# library(RSQLite)
# library(DBI)
 library(digest)
 library(RCurl)



# library(DT)
 library(plotly)
 library(rAmCharts)
#library(fullcalendar)
# library(leaflet.extras)
# library(rhandsontable)




defWidth = '380px'
loaderTime = 1
cookieName = 'miSensorsAppInfo'


debugging = F

machineName <- as.character(Sys.info()['nodename'])
print(machineName)
if(machineName == 'WALCOT-SL'){
  rootDir <<- 'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/SensorViewer'
}else{
  rootDir <- '/srv/shiny-server/SensorViewer'
}


setwd(rootDir)

source('appUtils.R')
source('appConfig.R')
source("helpers.R")

con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RO)


isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

myDownloadButton <- function(outputId, label = "Download"){
  tags$a(id = outputId, class = "btn btn-default shiny-download-link", href = "", 
         target = "_blank", download = NA, icon( "floppy-save", lib = "glyphicon"), label)
}


# jsCode <- '
# shinyjs.getcookie = function(params) {
# var cookie = Cookies.get("id");
# if (typeof cookie !== "undefined") {
# Shiny.onInputChange("jscookie", cookie);
# } else {
# var cookie = "";
# Shiny.onInputChange("jscookie", cookie);
# }
# }
# shinyjs.setcookie = function(params) {
# Cookies.set("id", escape(params), { expires: 31536000});
# Shiny.onInputChange("jscookie", params);
# }
# shinyjs.rmcookie = function(params) {
# Cookies.remove("id");
# Shiny.onInputChange("jscookie", "");}'


shiny::shinyApp(
  ui = f7Page(
    title = "miSensors",
    init = f7Init(skin = "auto", theme = "light", filled = T, color = 'lightblue'),
    tags$head(tags$link( rel="icon", type="image/png", href="wheat.png", sizes="32x32" ),
              tags$link( rel="apple-touch-icon", href="apple-touch-icon.png" )
              #tags$title("BCG AgDataShop"),
              #tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
              
              ),
    
    useShinyjs(),
    useShinyalert(),
  #  extendShinyjs(text = jsCode),
    use_glouton(),
    
    #add_busy_bar(color = "#FF0000", centered = FALSE, height = "18px"),
    #add_busy_spinner(spin = "fading-circle"),
  #  add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
    
    #title = NULL,
    preloader = F,
    loading_duration = loaderTime,
    f7TabLayout(
      panels = tagList(
         f7Panel(inputId='Sidepanel', side = "left", theme = "light", effect = "cover",
                 
                 #textInput('username', 'User', value = "", placeholder = 'ross.searle@csiro.au'),
                 #passwordInput('password', 'Password', placeholder = 'rossiscool'),
                 f7Text(inputId ='username', label='User', value = "ross.searle@csiro.au", placeholder = 'ross.searle@csiro.au'),
                 
                 f7Password(inputId = 'password', label = 'Password', value = "",  placeholder = 'rossiscool'),
                 
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
        #         
        #         # f7Link(label = "About BARS", src = "https://www.csiro.au/en/Research/AF/Areas/Boorowa-Agricultural-Research-Station", external = TRUE),
        #         # f7Link(label = "About CSIRO Ag & Food", src = "https://www.csiro.au/en/Research/AF", external = TRUE),
        #         # f7Link(label = "About CSIRO", src = "https://www.csiro.au", external = TRUE),
        #         # f7Link(label = "BoM Boowora", src = "http://www.bom.gov.au/places/nsw/boorowa/", external = TRUE),
        #         
        #         f7PanelItem(title = 'test', tabName = 'Weather'),
        #         f7Link(label = "BoM Boowora", src = "Weather", external = F)
        #         
        #         
        #         
                 ),
        f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
      ),
 
##################################  NAVIGATION BAR   ##################################      
      navbar = f7Navbar(
       # title = shiny::tags$div(style="background-image: url('Logos/HdrBkGrdImage.PNG');", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px"), "Boowora Agricultutral Research Station "),
        title = tags$div( tags$div(style="vertical-align:middle!important; text-align:left!important; display:inline-block;", "miSensors"), HTML('&nbsp&nbsp&nbsp'), tags$div(style="float: right;", tags$img(src = "Logos/csiro.png", width = "40px", height = "40px", align='right'))),
        hairline = F,
        shadow = T,
        left_panel = T,
        right_panel = F
      ),


##################################  UI - SOIL MOISTURE PROBE MAP  ##################################         
      
      f7Tabs(
        animated = T,
        #swipeable = TRUE,
        f7Tab(
          tabName = "View Sensors",
          icon = f7Icon("cloud_heavyrain_fill", old = TRUE),
          active = TRUE,
          f7Float( f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        
                        #fluidRow(column(1), column(11,selectInput("pickSensor", "Site Name", list(c('None'))))),
                        
                        f7Picker(inputId = 'pickSensor', label = "Site Name",  choices =  c('None'), toolbar=T, openIn='auto',scrollToInput=T,toolbarCloseText = "Prout",sheetSwipeToClose = TRUE),
                        selectInput("pickDataStreamType", "Sensor Type", list(c('None'))), 
                       
                        
                       
                        amChartsOutput(outputId = "amchart"),
                        HTML('<BR>')
                      )
            )
          ), side = "left" ),
          
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = "Current Soil Water Summary",
                        id = 'swgaugecard',
                        
                        
                      ))), side = "left" ),
          
          f7Float(  f7Shadow(
            intensity = 100,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = NULL,
                        
                        
                        
                      )
            )
          ), side = "left" ),
          
          f7Float( 
            f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),
                      f7Card(
                        title = 'Soil Water Bucket',
                        
                      )
            )
          ), side = "left" )
        ),

        
################################## UI - SOIL MOISTURE MAPS   ##################################           
        f7Tab(
          tabName = "Manage Sensors",
          icon = f7Icon("plus_square_fill_on_square_fill", old = F ),
          active = FALSE,
          f7Float( 
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            div( style=paste0("width: ", defWidth ,"; align='left'; vertical-align: middle;"),
                 f7Card(
                   title = NULL,
                   #f7DatePicker( "SMmapDate", label='Select Map Date', value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd" ),
                  # f7Select(inputId = 'SMDepthList', label = "Soil Depth",  choices =  soilDepthsDF$sdLabels),
                   HTML('<BR>'),
                   #div( style=paste0("width: 100px"),
                   f7Button(inputId = 'drawSMmapbtn', label = "Draw Soil Moisture Map", src = NULL, color = 'green', fill = TRUE, outline = F, shadow = T, rounded = T, size = NULL),
                   #),
                   HTML('<BR>'),
                   f7Progress(id = "pg1", value = 0, color = "blue"),
                   
                   
                   leafletOutput("moistureMap2", height = 400 )
                   
                 )
            )
          )
        ), side = "left"),
        
        
##################################  UI - WEATHER   ##################################          
        f7Tab(
          tabName = "Login",
          icon = f7Icon("lock_fill", old = F),
          active = FALSE,
          f7Float(  
            f7Shadow(
            intensity = 10,
            hover = TRUE,
            
            tags$div( style=paste0("width: ", defWidth),
                      
                       f7Card(
                        title = paste0("Todays Weather (", format(Sys.Date(), format="%B %d %Y"), ')' ),
                        
                        verbatimTextOutput("todaysRainfall"),
                        verbatimTextOutput("todaysMaxRainfall"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentTemperature"),
                        verbatimTextOutput("todaysMinTemperature"),
                        verbatimTextOutput("todaysMaxTemperature"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentHumidity"),
                        verbatimTextOutput("todaysMinHumidity"),
                        verbatimTextOutput("todaysMaxHumidity"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentWindspeed"),
                        verbatimTextOutput("todaysMinWindspeed"),
                        verbatimTextOutput("todaysMaxWindspeed"),
                        HTML('<BR>'),
                        verbatimTextOutput("todaysCurrentWindDirection")
                        # verbatimTextOutput("todaysMinHumidity"),
                        # verbatimTextOutput("todaysMaxHumidity")
                      ))), side = "left"), 
                      
          f7Float(  
            f7Shadow(
              intensity = 10,
              hover = TRUE,
              
<<<<<<< HEAD
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

              
              materialSwitch(inputId = "prefExtendedSetButton", value = F, label = 'Show extended locations'),
              
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
=======
              tags$div( style=paste0("width: ", defWidth),           
            f7Card(
              title = "Weather History",
              prettyRadioButtons(
                
                inputId = "WeatherHistoryButtons",
                label = "Variable:",
                
                c("Rainfall" = "Rainfall",
                  "Temperature" = "Temperature",
                  "Humidity" = "Humidity",
                  "Windspeed" = "Wind-Speed"),
                inline = TRUE,
                status = "success",
                animation = "pulse",
                bigger = T
>>>>>>> a52b3af3711d3875aac80973e1470e736f8e59c2
              ),
              dygraphOutput("WeatherHistoryChart", height = "300px")
            )))), side = "left"),
            
            
##################################  UI - SOIL DATA MAP   ##################################             
        
        f7Tab(
          tabName = "Preferences",
          icon = f7Icon("gear", old = F),
          active = FALSE,
          f7Float(
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            tags$div( style=paste0("width: ", defWidth),  
            f7Card(
              title = NULL,
              fluidRow( f7Select('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),  f7Select('SoilDepthList', "Select depth (cm)", choices=c('d1', 'd2', 'd3', 'd4'))),
              #f7Select('SoilPropList', "Select soil attribute", choices=c('clay', 'ecec', 'phc', 'soc')),
              HTML('<BR>'),
              leafletOutput("soilMap", height = 400),
              rHandsontableOutput('soilDataTable' )
              #tableOutput('soilDataTable' )
            )
            )
          )
          , side = "left")
        )
        
      )
    )
  ),


<<<<<<< HEAD
  d <- getURL(paste0(senFedPath,"getSensorLocations"))
  sensorLocs <- fromJSON(d)
  
    observe({
      
      req(RV$prefExtendedSet)
        print(RV$prefExtendedSet)
        if(as.numeric(RV$prefExtendedSet)==1){
          lurl <- paste0(senFedPath,"getSensorLocations?sensortype=", input$DataTypeDataDownload, "&extendedSet=T")
        }else{
          lurl <- paste0(senFedPath,"getSensorLocations?sensortype=", input$DataTypeDataDownload)
        }
        print(lurl)
        d <- getURL(lurl)
        sdf <- fromJSON(d)
        print(tail(sdf))
        
        RV$sensorLocs <- sdf
    })
  
  
  
 
  RV <- reactiveValues()
  RV$SelectedSite <- NULL
  RV$CurrentUser <- NULL
  RV$sensorLocs <- sensorLocs
  RV$sensorLocsDownload <- NULL
  RV$pref_defSite <- NULL
  RV$pref_daysSince <- NULL
  RV$prefExtendedSet <- NULL
  RV$error <- NULL
  RV$AppType <- NULL
  RV$CurrentSensorData <- NULL
  RV$pwd <- NULL
  
  
  
  
  #####    Parse supplied URL parameters and apply them  ########
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    RV$CurrentUser <- query[['usr']]
    RV$pwd <-  query[['pwd']]
    #shinyalert('Debug', RV$pwd, type = 'info')
=======
##################################  SERVER  ##################################   
  server = function(input, output, session) {
>>>>>>> a52b3af3711d3875aac80973e1470e736f8e59c2
    
    session$allowReconnect(TRUE)
   
    
    d <- getURL(paste0(senFedPath,"getSensorLocations"))
    sensorLocs <- fromJSON(d)
   # print(sensorLocs)
    
    RV <- reactiveValues()
    RV$SelectedSite <- NULL
  #  RV$CurrentUser <- NULL
    RV$sensorLocs <- sensorLocs
    RV$sensorLocsDownload <- NULL
    RV$pref_defSite <- NULL
    RV$pref_daysSince <- NULL
    RV$error <- NULL
    RV$AppType <- NULL
    RV$CurrentSensorData <- NULL
 #   RV$pwd <- NULL
    RV$Init <- T

    
    
    
    observe({
      
      print(appAuth$loggedIn)
      if(appAuth$loggedIn){
        sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "'")
        res <- dbSendQuery(con, sql)
        sns <- dbFetch(res)
        dbClearResult(res)
        print(sns)
        str(sns$localName)
        #updateSelectInput(session, "pickSensor", choices =  sns$locationID)
        updateF7Picker( inputId = 'pickSensor', choices = sns$localName, value = sns$localName[1] )
        
       updateF7Select( inputId = 'pickDataStreamType') 
      }
    })
    
    
    observe({
      if(!debugging){
        
        if(appAuth$loggedIn){
          
          req(input$pickSensor)
          sensorID <- getSensorIDFronLocalName(con, appAuth$currentUsr, input$pickSensor)
          url <- paste0(senFedPath,'getSensorInfo?siteid=', sensorID)
          stnsRaw <- getURL(paste0(url))
          stnsJ <- fromJSON(stnsRaw)
          sns <- unique(stnsJ$DataType)
          updateSelectInput(session, "pickDataStreamType", choices =  sns, selected = DefaultSensor)
        }
      }
    })
    
    
    
    #####    Authentication   ######################
    status <- reactiveVal(value = NULL)
    appAuth <- reactiveValues(loggedIn = F, currentUsr = '')
    appData <- reactiveValues(currentTS = NULL)
    RV$CurrentUser <- NULL
    RV$pwd <- NULL
    
    observe({
      
<<<<<<< HEAD
      setView(lng = 135, lat = -28, zoom = 4) %>%

      addLayersControl(
        baseGroups = c("Map", "Satelite Image"),
        #overlayGroups = c("Moisture Maps", "All Sensors"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  observe({
    
   
    
    req(input$DataTypeDataDownload)
    
    if(!debugging){
    
      lurl <- paste0(senFedPath,"getSensorLocations?sensortype=", input$DataTypeDataDownload)
      d <- getURL(lurl)
      sdf <- fromJSON(d)
    
    RV$sensorLocsDownload <- sdf
    #sdf <- RV$sensorLocs
    
    labs <- lapply(seq(nrow(sdf)), function(i) {
      paste0( '<li>Site Name : ', sdf[i, "SiteName"], '</li>',
              '<li>Provider : ', sdf[i, "SensorGroup"], '</li>',
              '<li>Backend : ', sdf[i, "Backend"], '</li>',
              '<li>Access : ', sdf[i, "Access"], '</li>',
              '<li>Site ID : ', sdf[i, "SiteID"], '</li>'
              #,'<li>Available Sensors : ',  paste(tsens, collapse = ', '), '</li>'
      )
=======
     
      if(RV$Init){
        print('Cookiestuff')
       r<-remove_cookie('miSensorsAppInfo')
      # print(r)
           add_cookie(name=cookieName, value='ross.searle@csiro.auXXXXXa')
      #   #print('cookieset')
          ck <- fetch_cookie(name=cookieName)
         # print(ck$miSensorsAppInfo)
          str(ck)
          RV$Init=F
          
          if(length(ck$miSensorsAppInfo)>0){
            lbits <- str_split( ck$miSensorsAppInfo, 'XXXXX')
            usrn <- lbits[[1]][1]
            pwd <- lbits[[1]][2]
            RV$CurrentUser <- usrn
            RV$pwd <- pwd
          }
      }
>>>>>>> a52b3af3711d3875aac80973e1470e736f8e59c2
    })
    
   
    
    # js$getcookie()
    # 
    # 
    # # addClass(selector = "body", class = "sidebar-collapse")
    # 
    # observeEvent(input$mysidebar,
    #              {
    #                
    #                if( RV$AppType == 'mobile'){
    #                  # for desktop browsers
    #                  addClass(selector = "body", class = "sidebar-collapse")
    #                  # for mobile browsers
    #                  removeClass(selector = "body", class = "sidebar-open")
    #                }
    #              })
    
    # check if a cookie is present 
    observe({

      
      if(!is.null(RV$CurrentUser) & !is.null(RV$pwd)){
        
        
        sql <- paste0("Select usr, pwd from appUsers where usr = '", RV$CurrentUser, "'")
       
        res <- dbSendQuery(con, sql)
        df <- dbFetch(res)
        
        nrecs <- dbGetRowCount(res)
        dbClearResult(res)
        
        if(nrecs > 0){
          #Check against password
          
          dbusr <- df[1,1]
          hash <- df[1,2]
          
          print(RV$pwd)
         
          if( identical(hash, digest(RV$pwd, serialize=T))){
            
            print(hash)
            appAuth$loggedIn <- T
            appAuth$currentUsr <- RV$CurrentUser
            status(paste0('in as ', RV$CurrentUser))
            #RV$CurrentUser <- usrn
            
          }else{
            appAuth$loggedIn <- F
            appAuth$currentUsr <- ''
            status(paste0('out'))
          }
          
        }
      }else if (!is.null(cooky) ) {
        
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
      print("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    })
    
    observeEvent(input$logout, {
      appAuth$loggedIn <- F
      status('out')
      js$rmcookie()
    })
    
    output$textusrID <- renderText({
      HTML(paste0("<br><p><b>You are logged ",status(),"</b></p>"))
    })
    
    
    
    
    
  }
<<<<<<< HEAD
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
p

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
  #  outdf[5,2] <- dfs$Access
    outdf[6,1] <- 'Active'
  #  outdf[6,2] <- as.character( dfs$Active )
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
       
         sql <- paste0("insert into appUserLocations ( usr, locationID, localName) values (:x, :y, :z)")
       #  sql <- paste0("insert into appUserLocations ( usr, locationID, localName) values ('", RV$CurrentUser, "', '",RV$SelectedSite, "', '", input$mySensorName, "')")
       #  shinyalert('Debug', sql, type = 'warning')
         rs <- dbSendStatement(conInsert, sql)
         
      rs <- dbSendStatement(conInsert, sql)
      dbBind(rs, param = list(x = RV$CurrentUser, y = RV$SelectedSite, z = input$mySensorName))
      dbGetRowsAffected(rs)
      dbClearResult(rs)

      sns <- getListofUserSensors( con, appAuth$currentUsr)
=======
)
>>>>>>> a52b3af3711d3875aac80973e1470e736f8e59c2






<<<<<<< HEAD
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
    RV$prefExtendedSet <- prefs$showExtended
    
    updateKnobInput(session = session,inputId = "prefDaysToExtract",value = RV$pref_daysSince)
    updateSelectInput(session = session, inputId = 'prefDefaultSite', choices = sns$localName, selected = RV$pref_defSite)
    updateSelectInput(session, 'pickSensor', choices = sns$localName, selected = RV$pref_defSite)
    updateSelectInput(session = session, inputId = 'mySensors', choices = sns$localName )
    updateMaterialSwitch(session = session, 'prefExtendedSetButton', value = RV$prefExtendedSet)
  })
=======
>>>>>>> a52b3af3711d3875aac80973e1470e736f8e59c2


<<<<<<< HEAD
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
    defextendedSet <- input$prefExtendedSetButton
    
    tryCatch({
      
      conUpdate <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)
      rs <- dbSendStatement(conUpdate, "Update appUsers SET daysSince = :y, defaultSite = :z, showExtended = :w where  usr = :x")
      dbBind(rs, param = list(x = str_to_lower(appAuth$currentUsr), y = defDaysSince, z = defSite, w = defextendedSet))
      dbGetRowsAffected(rs)
      #dbClearResult(rs)
      dbDisconnect(conUpdate)
      
      RV$pref_daysSince <- defDaysSince
      RV$RV$pref_defSite <- defSite
      RV$RV$extendedSet <- defextendedSet
      
      shinyalert("All Good", "Your preferences were successfully saved", type = 'success')
    }
    , error = function(err) {
      shinyalert("Oops", err$message, type = 'error')
    },finally = {
      
    })
    
  })
  
  
  
  

}
=======
>>>>>>> a52b3af3711d3875aac80973e1470e736f8e59c2

