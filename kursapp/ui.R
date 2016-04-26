
library(shiny)
library(knitr)
library(rmarkdown)

shinyUI(fluidPage(
  br(),
  img(src = "logo_web.png", width = "300px", height = "50px"),
  br(),
  br(),
  titlePanel(h2("DISTANCE-SAMPLING-ANALYSER: KURSMODUL")),
  br(),
  hr(),
  
  br(),
  br(),
  br(),
  sidebarLayout(position="left", 
    sidebarPanel(
      helpText(h4("Oversikt"),p("Her dukker det opp hjelpefunksjoner som er tilpasset
               den siden du er på. Dette gjør det mulig for deg å laste opp kursfila
                og utforske datasettt på en enkel måte")),
      
      tags$hr(),
      conditionalPanel(
        "$('li.active a').first().html()==='Last opp kursfil'",
      
      helpText("Her kan du laste opp og se på kursfila. Dersom den ser merkelig ut 
                   skyldes dette trolig at du ikke har valgt riktig filformat i menyen til venstre. Sørg
               for at du har lastet opp fila i riktig format før du går videre"),  
        
      fileInput('file1', 'Se på kursfila',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      
      radioButtons('sep', 'Filformat',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ';'),
      br(),
      br()
      
      ),
      
      
      conditionalPanel(
        "$('li.active a').first().html()==='Histogram'",
        helpText("Velg hvor mange grupperinger (søyler) du ønsker i hisogrammet som vises til høyre på siden"),
        numericInput("bins1", 'Antall grupperinger', value=5, width="170px"),
        selectInput("histcol", "Farge på histogram", choices=c(Blå="cornflowerblue", Grå="grey", 
                                                               orange="darkgoldenrod"), width="170px")
        ),
      
      
      conditionalPanel(
        "$('li.active a').first().html()==='Summarisk oversikt'",
        helpText("Velg hvilken målestokk som er benyttet for de oppgitte linjeavstander og
                 linjelengder, og hvorvidt du ønsker oversikt linje for linje eller samlet"),
        br(),
        radioButtons("desc1", 'Vis oversikt', c(Linjevis='lin', Samlet='saml'), 'lin'),
        br(),
        radioButtons("LiAv", 'Linjeavstand er oppgitt i', c(Meter='meter', Centimeter='cm'), 'meter'),
        br(),
        radioButtons("Taks", 'Linjelengde er oppgitt i', c(Meter='meter', Kilometer='km'), 'meter'),
        br()
       
        
        
      ),
      
      conditionalPanel(
        "$('li.active a').first().html()==='Distance Sampling'",
        helpText("Her kan du gjøre diverse valg knyttet til analysene og 
                  hvordan du vil se på resultatene. Når du har gjort dine valg, 
                  trykk på knappen Analyser data"),
        br(),
        br(),
        radioButtons("Dens", 'Tetthet måles i', c(Kvadratmeter='meter', Kvadratkilometer='km'), 'km'),
        br(),
        br(),
        numericInput("bins2", 'Antall grupperinger', value=7, width="170px"),
        br(),
        br(),
        actionButton("go_analyse", "Analyser data!", icon("thumbs-up")),
        br()
        
        )
     
      
    ),
    mainPanel(width=5,
      tabsetPanel(type="pills",
        tabPanel("Hjem",
                 includeHTML(relative_to(getwd(), "kursapp/testpage.html"))
                 ),
        
        tabPanel("Last opp kursfil",
                 br(),
                 br(),
                 h4("Oversikt over kursfila"),
                 p(""),
                 br(),
                 tableOutput('contents')),

        tabPanel("Histogram",
                 br(),
                 br(),
                 h4("Ovsersikt over linjeavstander"),
                 br(),
                 plotOutput("distPlot")),

        
        tabPanel("Summarisk oversikt", 
                 br(),
                 br(),
                 h4("Summarisk oversikt over datasettet"),
                 br(),
                 br(),
                 tableOutput("Deskr")),
        
        tabPanel("Distance Sampling", 
                 br(),
                 br(),
                 h4("Distance sampling-analyser"),
                 br(),
                 br(),
                 tableOutput("Distance"),
                 br(), 
                 br(),
                 plotOutput("Distance3", width="600px"))
        
      )
    )
  ),
  hr(), 
  h5("Koden til denne appen kan lastes ned på", tags$a(href="https://github.com/ErlendNilsen/DS_kursapp", "GitHub")),
  h5("Har du spørsmål knyttet til denne applikasjonen finner du kontaktinformasjon", tags$a(href="http://honsefugl.nina.no/Innsyn/Home/Kurs", "her"))
)
)