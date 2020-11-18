library(shiny)
library(ggvis)
library(dplyr)
library(shinydashboard)
library(DT)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(ggmap)
library(shinythemes)
library(plotly)
library(ggplot2)
Taiwan<- read.csv("www/Taiwan.csv")

data <- data.frame(Taiwan)
txt <- data.frame(台北市 =c(13290,	15799,	2495,	-9106	,-12811), 台東縣 = c(-1431,	-351,	-2018,	-1650,	-1086),宜蘭縣 =c(-139	,321,	-660	,-579	,-687),花蓮縣 =c(-1293,	-505,	-1447	,-1034,	-1380),金門縣=c(7602	,7010,	5076,	2315,	1698),南投縣= c(-2974	,-2907,	-4825	,-4327,	-3068),屏東縣=c(-6155,	-4369,	-6664,	-5461,	-4719),苗栗縣=c(1578,	1578,	-3220	,-4723,	-4140),桃園市=c(13862,	14305	,47452	,41983,	30695),高雄市=c(1218,	-885,	-74,	453,	-2350),基隆市=c(-2239,	-1837,	-972,	-5	,-342),連江縣=c(855	,341,	41	,48	,171),雲林縣=c(-3199,	-2436,	-5723,	-4760,	-3525),新北市=c(15624,	11889	,3826	,8564	,5767),新竹縣=c(6493,	7144,	4412,	5439,	3720),嘉義縣=c(-4494,	-4446	,-4944,	-4519	,-3256),嘉義市=c(-348,	11,	-517,	-492,	-536),彰化縣=c(-3855,	-4539,	-2402,	-1926	,-4155),澎湖縣=c(1557	,1358	,546,	959	,580)
                     , 年份 = c("102年","103年","104年","105年","106年"))
mybar <- read.csv("www/bgage.csv", header=TRUE)
mybar <- mybar[1:4]

#rent
dataframe <-read.csv("www/rent.csv")
fixInvase <- structure(list(Species= structure(dataframe$locs,
                       .Lable = dataframe$country, class="factor"),
                      latitude = as.numeric(dataframe$lat),
                      longitude = dataframe$lng,
                      pop = dataframe$pop),
                       .Names = c("Species", "latitude", "longitude","pop"),
                       row.names = c(NA, -9079L),class = "data.frame")

# store data
familymart.address<- read.csv("www/familymart.csv" 
                              , stringsAsFactors = FALSE
) 

seven_eleven.address <-read.csv("www/seven_eleven.csv")
seven_eleven.address <-data.frame(region=as.character(seven_eleven.address$address),
                                  lat = as.double(as.character(seven_eleven.address$lat)), 
                                  lng = seven_eleven.address$lng)                             

hi_life.address <- read.csv("www/hi_life.csv",stringsAsFactors = FALSE)
hi_life.address <-data.frame(region=as.character(hi_life.address$address),
                             lat = as.double(as.character(hi_life.address$lat)), 
                             lng = hi_life.address$lng)

colleage.address<-read.csv("www/colleage.csv",
                           stringsAsFactors = FALSE) 

highschool.address<-read.csv("www/highschool.csv",
                     stringsAsFactors = FALSE)

hospital.address<-read.csv("www/hospital.csv")

MRT.address<-read.csv("www/MRT.csv")

postoffice.address<-read.csv("www/post office.csv")


#ui-----------------------------------------------------------------------------------------------------------------------------------------

 ui <- shinyUI(navbarPage( "飲料店展店選址",theme = shinytheme("united"),
   id="nav",
   tabPanel("DataInfo",
                fluidRow(
                  datatable(data)
                )
   ),
        tabPanel("Maping",
                 column(width = 11,
                        box(title=h5("各縣市總人口數成長"),plotlyOutput("myhist", height = "300px", width = "70%")),
                        box(title = h5("各年齡層男女成長"),plotOutput("mytest",height = "300px",width = "125%"))
                        ),

                 column(10,
                        box(
                          title = "地圖", width = NULL, solidHeader = TRUE
                          
                        )),
                div(
                  tags$head(
                    includeCSS("C:/Users/user/Documents/project/style.css")),
                  title = "taiwan areas",
                  collapsible = TRUE,
                  width = "100%",
                  height = "100%",
                  leafletOutput("Map", width="100%",height="1000"),
                  absolutePanel(
                    selectInput("population","Country",label = h3("各縣市人口數成長:"),choices = list(台北市 ="台北市",台東縣 ="台東縣",宜蘭縣="宜蘭縣",花蓮縣="花蓮縣",金門縣="金門縣",南投縣="南投縣",屏東縣="屏東縣",苗栗縣="苗栗縣",桃園市="桃園市",高雄市="高雄市",基隆市="基隆市",連江縣="連江縣",雲林縣="雲林縣",新北市="新北市",新竹縣="新竹縣",嘉義縣="嘉義縣",嘉義市="嘉義市",彰化縣="彰化縣",澎湖縣="澎湖縣")),
                    selectInput("my_country", label = h3("各縣市年齡層男女成長:"), 
                                choices = as.list(levels(mybar$States))),
                  
                  id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 70, left = "auto", right = 20, bottom = "auto",
                  width = 330, height = "auto",
                   selectInput("region",label="Map of Taiwan", choices = c("",unique(as.character(data$Region)))),
                   selectInput("speciesInput", "Species", unique(fixInvase$Species)),
                  p("便利商店"),
                  inputPanel(
                    checkboxInput("familymart","familymart",value=FALSE),
                    checkboxInput("hi_life","hi_life",value=FALSE),
                    checkboxInput("seven_eleven","seven_eleven",value=FALSE)
                  ),
                  p("高中、大學"),
                  inputPanel(
                    checkboxInput("colleage","colleage",value=FALSE),
                    checkboxInput("highschool","highschool",value=FALSE)
                  ),
                  p("else"),
                  checkboxInput("hospital","hospital",value=FALSE),
                  checkboxInput("MRT","MRT",value=FALSE),
                  checkboxInput("post office","post office",value=FALSE)
                  )
                  
                 
                  
                  )
                
                  )
   
                  )
       
       
      
    )



# Server -----------------------------------------------------------------------------------------------------------------------------------
server <- shinyServer(
function(input,output, session){
  
# content <- paste0("<strong>,Estado:,</strong>", 
                  # colleage.address$name, 
                  # "<br>,<strong>PIB per, miles de pesos, 2008: </strong>", 
                  # colleage.address$address)

  output$myhist <- renderPlotly({
    myhist <- plot_ly(
      txt, x = ~年份 ,y = ~get(input$population), type = "bar")%>%
      layout(yaxis = list(title = '人口數'))
    
  })
  output$mytest <- renderPlot({
    
    dataSelect <- mybar[mybar$States == input$my_country,]
    dataSelect <- dataSelect[-2]
    ggplot(dataSelect, aes(x=年齡, y=人口數, fill=type)) +
      #ggtitle(paste("各年齡層男女成長", input$my_country, sep=" : ")) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      geom_bar(stat='identity', position='dodge')+
      geom_text(aes(label = 人口數), position=position_dodge(width=0.9), vjust=-0.25)
  })

 
  #load icon
  fm.icon <- iconList(
    fm = makeIcon("www/fm.png",30,30)
  )         
  hilife.icon <- iconList(
  hilife = makeIcon("www/hilife.png",30,30)
  )
  seven_eleven.icon <- iconList(
    seven_eleven = makeIcon("www/seven_eleven.png",30,30)
  )
  colleage.icon <- iconList(
    colleage = makeIcon("www/colleage.png",40,40)
  )
  highschool.icon <- iconList(
    highschool = makeIcon("www/highschool.png",40,40)
  )
  rent.icon <- iconList(
    rent = makeIcon("www/rent.png",40,40)
  )
  beverages <- iconList(
    beverages = makeIcon("www/beverages.png",40,40)
  )
  hospital <- iconList(
    hospital = makeIcon("www/hospital.png",40,40)
  )
  postoffice <- iconList(
    postoffice = makeIcon("www/postoffice.png",40,40)
  )  
    
    acm_defaults <- function(map, x, y) addCircles(map, x, y, radius=250, color="black", fillColor="orange",        fillOpacity=0.5,opacity=1, weight=2, stroke=TRUE, layerId="Selected")
    
    output$Map <- renderLeaflet({
      
      filtered <-fixInvase %>%filter(Species== input$speciesInput)
     
      
    Map<-leaflet(filtered) %>% 
      setView(lng =121.525 , lat=25.0391667, 16) %>% 
      addTiles() %>% 
      addMarkers(~as.numeric(longitude), ~as.numeric(latitude),clusterOptions = ~markerClusterOptions(),icon = ~rent.icon ,popup = "店面租屋處",popupOptions())%>%
    addCircles(data=data, radius= 6 , color="red", stroke=FALSE, fillOpacity=0.5 , weight =  10,layerId = ~Region)
      
   
    
    
    Map %>% addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(1); }")))%>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%addFullscreenControl()
    })
    
    
    
    
    
    # update the map markers and view on map clicks
    observeEvent(input$Map_marker_click, { 
      p <- input$Map_marker_click
      proxy <- leafletProxy("Map")
      
    ifelse(p$id=="Selected", proxy %>% removeMarker(layerId="Selected"),  proxy %>% setView(lng=p$lng, lat=p$lat, input$Map_zoom) %>% acm_defaults(p$lng, p$lat))
    
    })
    
    # update the location selectInput on map clicks
    observeEvent(input$Map_marker_click, { 
      p <- input$Map_marker_click
      if(!is.null(p$id)){
        if(is.null(input$region) || input$region!=p$id) updateSelectInput(session,"region", selected=p$id)
      }
    })
    
    # update the map markers and view on location selectInput changes
    observeEvent(input$region, { 
      p <- input$Map_marker_click
      p2 <- subset(Taiwan, Region==input$region)
      proxy <- leafletProxy("Map")
      if(nrow(p2)==0){
        proxy %>% removeMarker(layerId="Selected")
      } else if(length(p$id) && input$region!=p$id){
        proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
      } else if(!length(p$id)){
        proxy %>% setView(lng=p2$lon, lat=p2$lat, input$Map_zoom) %>% acm_defaults(p2$lon, p2$lat)
      }
    
    })
    
    #familymart mark
    observeEvent(input$familymart, {
      proxy <- leafletProxy('Map',session)
      proxy %>% clearControls()
      proxy %>% addMarkers(data= familymart.address ,~lng,~lat,
                            clusterOptions = ~markerClusterOptions() , 
                            icon = ~fm.icon , 
                            popupOptions(),
                            popup = "全家便利商店" ,
                            #popup = ~format(tags$(familymart.address$address)), 
                            group="family")
      if (input$familymart){proxy %>% showGroup("family") }
      else
      {
        updateCheckboxInput(session, 'familymart')
        proxy %>% hideGroup("family")
      }
    })
    
    #seven_eleven mark
    observeEvent(input$seven_eleven, {
      proxy <- leafletProxy('Map',data = seven_eleven.address)
      proxy %>% clearControls()
      proxy %>%  addMarkers(data=seven_eleven.address ,
                            lng=seven_eleven.address$lng,lat=seven_eleven.address$lat,
                            clusterOptions = markerClusterOptions() , 
                            icon = ~seven_eleven.icon ,popupOptions(),
                            popup = "7-11便利商店",group="seven_eleven")
     
      
      if (input$seven_eleven){proxy %>% showGroup("seven_eleven") }
      else
      {
        updateCheckboxInput(session, 'seven_eleven')
        proxy %>% hideGroup("seven_eleven")
      }
      
    })
    
    #hi_life mark
    observeEvent(input$hi_life, {
      proxy <- leafletProxy('Map',data = hi_life.address)
      proxy %>% clearControls()
      proxy %>%  addMarkers(data=hi_life.address ,lng=hi_life.address$lng,lat=hi_life.address$lat,
                            clusterOptions = markerClusterOptions() ,
                            icon = ~hilife.icon,popupOptions(),
                            popup = "萊爾富便利商店" ,
                            group="hi_life")
      if (input$hi_life){proxy %>% showGroup("hi_life") }
      else
      {
        updateCheckboxInput(session, 'hi_life')
        proxy %>% hideGroup("hi_life")
      }
    })

      
    #colleage mark
    observeEvent(input$colleage, {
      proxy <- leafletProxy('Map',data = colleage.address)
      proxy %>% clearControls()
      proxy %>%  addMarkers(data=colleage.address ,~lng,~lat,
                            clusterOptions = markerClusterOptions() ,
                            icon = ~colleage.icon ,popupOptions(), 
                            popup = paste("<b>","大專院校","<b>"),   
                                            group="colleage")
      if (input$colleage){proxy %>% showGroup("colleage") }
      else
      {
        updateCheckboxInput(session, 'colleage')
        proxy %>% hideGroup("colleage")
      }
    })
 
   #highschool
    observeEvent(input$highschool, {
      proxy <- leafletProxy('Map',data = highschool.address)
      proxy %>% clearControls()
      proxy %>%  addMarkers(data=highschool.address ,~lng,~lat,
                            clusterOptions = markerClusterOptions() ,
                            icon = ~highschool.icon ,popupOptions(), 
                            popup = paste("<b>","高中","<b>"),   
                            group="highschool")
      if (input$highschool){proxy %>% showGroup("highschool") }
      else
      {
        updateCheckboxInput(session, 'highschool')
        proxy %>% hideGroup("highschool")
      }
    })

#rent 
    output$results <- renderTable({
      filtered <-
        fixInvase %>%
        filter(Species == input$speciesInput
        )
      filtered
    })
   
           
   
     })

    
    
 
  
shinyApp(ui = ui, server = server)