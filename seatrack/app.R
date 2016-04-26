require(leaflet)
require(maps)
require(shiny)
require(RPostgreSQL)
require(magrittr)
#tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),

dbuser="postgjest"
dbpassword="gjestpost"

ui<-pageWithSidebar(headerPanel('Seatrack data download - VERSION 0.2'),
                   sidebarPanel(width=2, dateRangeInput("daterange", "Date range:",
                                                        start = Sys.Date() -365,
                                                        end   = Sys.Date()),
                                #selectInput('species', 'Species', c("All", "Common eider"),selected="All"),
                                uiOutput("choose_species"),
                                uiOutput("choose_colony"),

                                downloadButton('downloadData', 'Last ned CSV')),

                     mainPanel(
                       fluidRow(column(12, h4("Kartet viser 500 tilfeldige punkter av valgt data."),
                                              p("Spørringer etter stor mengd data kan ta tid, lukk vinduet å prøv igjen hvis appen låser seg"))),
                   fluidRow(column(12,leafletOutput("mymap", height=600))
                                                       )
                  ,
               fluidRow(column(1,offset=0,"Database dialog:"),column(11,verbatimTextOutput("nText"))))

)






server<-function(input, output, session) {

  datasetInput <- reactive({
    fields()$fields})

  output$downloadData <- downloadHandler(
    filename = function() { "Seatrack_data.csv" },
    content = function(file) {
      write.csv(datasetInput(), file, row.names=F)})


select_categories<-function(){

  drv<-("PostgreSQL")

  for(con in dbListConnections(PostgreSQL())){ dbDisconnect(con)}

  con<-dbConnect(drv,dbname="gisdata",host="ninsrv16.nina.no",user=dbuser,password=dbpassword)


  cat.query<-"SELECT DISTINCT species as species_cat, colony as colony_cat
  FROM seatrack.postable

  "

  suppressWarnings(res<-dbSendQuery(con, cat.query))

  categories<-fetch(res,-1)
  dbClearResult(res)

  categories
}


output$choose_species<- renderUI({
  selectInput('species', 'Species', c("All", as.character(select_categories()$species_cat)), selected="All")
})

output$choose_colony<- renderUI({
  selectInput('colony', 'Colony', c("All", as.character(select_categories()$colony_cat)), selected="All")
})



query<-reactive({
     if (is.null(input$species)){
     return(NULL)
     } else

  start_time<-as.character(input$daterange[1])
  end_time<-as.character(input$daterange[2])


  if (input$species=="All"){
    group.sub=""
    date_range<-paste("\n WHERE date_time::date >= '", start_time,"' ", "AND date_time::date <= '", end_time, "'",sep="")

  } else {
    group.sub<-paste("\n WHERE species = '",as.character(gsub("'", "''", input$species)), "'",sep="")
    date_range<-paste("\n AND date_time::date >= '", start_time,"' ", "AND date_time::date <= '", end_time, "'",sep="")

  }

  if (input$colony=="All"){
    colony.sub<-""
  } else {

    colony.sub<- paste0("\n AND colony = '", as.character(input$colony), "'")
    }

  limit<-"\n AND lon_smooth2 is not null
          AND lat_smooth2 is not null
  AND eqfilter3 = 1
  "
  ##ring_number as name, species, date_time, lat_smooth2 as lat, lon_smooth2 as lon

  fetch.q<-paste("SELECT *
  FROM seatrack.postable"
  ,group.sub,date_range, colony.sub, limit,sep="")

  fetch.q

  })



fields<-reactive({
  if (is.null(input$species)){
    return(NULL)
  } else


  drv<-("PostgreSQL")

  for(con in dbListConnections(PostgreSQL())){ dbDisconnect(con)}

  con<-dbConnect(drv,dbname="gisdata",host="ninsrv16.nina.no",user=dbuser,password=dbpassword)

  dbSendQuery(con, "SET CLIENT_ENCODING TO 'UTF8'")

  suppressWarnings(res<-dbSendQuery(con,as.character(query())))

  post.fields<-fetch(res,-1)
  dbClearResult(res)

 if(nrow(post.fields)<500){
   list(fields=post.fields, fields.subset=post.fields)
 } else
  list(fields=post.fields, fields.subset=post.fields[sample(x=1:nrow(post.fields),size=500),])

})


  output$mymap<-renderLeaflet({
    if (is.null(input$species)){
      return(NULL)

      } else


    if (nrow(fields()$fields)<1) {
      content<-paste(sep="<br/>", "<br> No Data! </br>", "Change data subset")
    leaflet() %>%
      addProviderTiles("MapQuestOpen.OSM") %>%
     # setView(10, 60, zoom = 4) %>%
      #addWMSTiles(
       # "http://ninsrv16/lm/lizmap/www/index.php/lizmap/service/?repository=jenstest&project=intercept_angle&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities",
        #layers = "Topografisk norgeskart 2",
        #options = WMSTileOptions(format = "image/png", transparent = TRUE),
        #attribution = "NINA wms"
      #)

      addPopups(10.406467, 63.414108, content,
                options = popupOptions(closeButton = FALSE)
      )
    } else

    if (nrow(fields()$fields)>500){
      my.fields<-fields()$fields.subset

      leaflet() %>%
        addProviderTiles("MapQuestOpen.OSM") %>%  # Add MapBox map tiles
         addCircleMarkers(radius=6, stroke= FALSE, fillOpacity=0.5, lng=my.fields$lon_smooth2, lat=my.fields$lat_smooth2
                    , popup=paste("Ring ID: ",as.character(my.fields$ring_number),"<br> Species: ", my.fields$species,
                                  "<br> Time: ", my.fields$date_time))
                    } else
  {
      leaflet() %>%
        addProviderTiles("MapQuestOpen.OSM") %>%
        addCircleMarkers(radius=6, stroke= FALSE, fillOpacity=0.5, lng=fields()$fields$lon_smooth2, lat=fields()$fields$lat_smooth2
                 , popup=paste("Ring ID: ",as.character(fields()[1]$ring_number),"<br> Species: ", fields()$fields$species,
                                 "<br> Time: ", fields()$fields$date_time)
        )

     }



  })


ntext<-reactive(
  query()

  )

 output$nText <- renderText({
     ntext()
 })

}

shinyApp(ui= ui, server= server)

