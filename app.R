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
loaderTime = 0
cookieName = 'miSensorsAppInfo'


debugging = T

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
                        
                        f7Picker(inputId = 'pickSensor', label = "Site Name",  choices =  c('None'), toolbar=T, openIn='auto',scrollToInput=T,toolbarCloseText = "Done",sheetSwipeToClose = TRUE),
                        f7Picker(inputId = 'pickDataStreamType', label = "Sensor Type",  choices =  c('None'), toolbar=T, openIn='auto',scrollToInput=T,toolbarCloseText = "Done",sheetSwipeToClose = TRUE),
                        pickerInput(
                          inputId = 'pickSensor',
                          label = "Location", 
                          choices = c('None'),
                          options = list(mobile = T, inline=T, size = 0, width='50%')
                          
                          
                        ),
                        #selectInput("pickDataStreamType", "Sensor Type", list(c('None'))), 
                        
                        
                        
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
  
  
  ##################################  SERVER  ##################################   
  server = function(input, output, session) {
    
    session$allowReconnect(TRUE)
    
    
    d <- getURL(paste0(senFedPath,"getSensorLocations"))
    sensorLocs <- fromJSON(d)
    RV <- reactiveValues()
    RV$SelectedSite <- NULL
    RV$sensorLocs <- sensorLocs
    RV$sensorLocsDownload <- NULL
    RV$pref_defSite <- NULL
    RV$pref_daysSince <- NULL
    RV$error <- NULL
    RV$AppType <- NULL
    RV$CurrentSensorData <- NULL
    RV$Init <- T
    
    
    
    
    
    
    ####   Get data from a sensor ######
    
    observe({
      
      withBusyIndicatorServer("fetchSensorData", {
        
        if(!debugging){
          
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
                
                sensorID <- getSensorIDFromLocalName(con, appAuth$currentUsr, input$pickSensor)
                
                url <- paste0(senFedPath, "getSensorDataStreams?siteid=", sensorID,"&sensortype=", dtype,"&startdate=", isoSDate, '&enddate=', isoEDate, "&aggperiod=days&usr=SensorViewer&pwd=UbB0f7jXKQBXahyfU7cjOcaZEHUZSpE19dmX")
                print(url)
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
        }
      })
      
      
    })
    
    
    
    
    
    
    
######   Update pick lists   ###########    
    
    observe({
      
      if(appAuth$loggedIn){
        sql <- paste0("select * from appUserLocations where usr = '", str_to_lower(appAuth$currentUsr), "'")
        sns <- queryDB(sql)
        print(head(sns))
       # updateF7Picker( inputId = 'pickSensor', choices = sns$localName, value = sns$localName[1] )
        updatePickerInput(session=session, inputId = 'pickSensor', choices = sns$localName, selected = sns$localName[1] )
      }
    })
    
    
    observe({
     
      req(input$pickSensor,appAuth$loggedIn)
     # print('KKKKKKKKKKKKKKKKKKKKKKKK')
      sensorID <- getSensorIDFromLocalName(appAuth$currentUsr, input$pickSensor)

      url <- paste0(senFedPath,'getSensorInfo?siteid=', sensorID)
      print(url)
      stnsJ <- getWebDataDF(url)

      # stnsRaw <- getURL(paste0(url))
      # stnsJ <- fromJSON(stnsRaw)
       sns <- unique(stnsJ$DataType)
       #print(sns)

      #if(debugging){
        
        #if(appAuth$loggedIn){
          
          # req(input$pickSensor)
          # sensorID <- getSensorIDFronLocalName(con, appAuth$currentUsr, input$pickSensor)
          # url <- paste0(senFedPath,'getSensorInfo?siteid=', sensorID)
          # stnsRaw <- getURL(paste0(url))
          # stnsJ <- fromJSON(stnsRaw)
          # sns <- unique(stnsJ$DataType)
          #updateSelectInput(session, "pickDataStreamType", choices =  sns, selected = DefaultSensor)
          updateF7Picker( inputId = 'pickDataStreamType', choices = sns, value = sns[1] )
       # }
      #}
    })
    
    
    
    #####    Authentication   ######################
    status <- reactiveVal(value = NULL)
    appAuth <- reactiveValues(loggedIn = F, currentUsr = '')
    appData <- reactiveValues(currentTS = NULL)
    RV$CurrentUser <- NULL
    RV$pwd <- NULL
    RV$CookieExists <- NULL
    
    observe({
      
      
      
    })
    
    
    
    # check if a cookie is present 
    observe({
      
      
      if(RV$Init){

        r<-remove_cookie('miSensorsAppInfo')

        add_cookie(name=cookieName, value='ross.searle@csiro.auXXXXXa')
        #   #print('cookieset')
        ck <- fetch_cookie(name=cookieName)
        RV$Init=F
        
        if(length(ck$miSensorsAppInfo)>0){
          RV$CookieExists=T
          lbits <- str_split( ck$miSensorsAppInfo, 'XXXXX')
          usrn <- lbits[[1]][1]
          pwd <- lbits[[1]][2]
          RV$CurrentUser <- usrn
          RV$pwd <- pwd
        }else{
          RV$CookieExists=F
        }
      }
      
      if(!is.null(RV$CurrentUser) & !is.null(RV$pwd)){
        
        
        sql <- paste0("Select usr, pwd from appUsers where usr = '", RV$CurrentUser, "'")
        df <- queryDB(sql)
        print(paste0("query of db ", df))
        # res <- dbSendQuery(con, sql)
        # df <- dbFetch(res)
        
       # nrecs <- dbGetRowCount(res)
      #  dbClearResult(res)
        
        if(nrow(df) > 0){
          #Check against password
          
          dbusr <- df[1,1]
          hash <- df[1,2]
          

          
          if( identical(hash, digest(RV$pwd, serialize=T))){
            
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
      }else if (RV$CookieExists ) {
        
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
    
      print("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    })
    
    observeEvent(input$logout, {
      appAuth$loggedIn <- F
      status('out')
      
    })
    
    output$textusrID <- renderText({
      HTML(paste0("<br><p><b>You are logged ",status(),"</b></p>"))
    })
    
    
    
    
    
  }
)









