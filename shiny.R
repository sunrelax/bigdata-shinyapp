library(shiny)
library(bslib)
library(RColorBrewer)
require(pals)

my_autocomplete_list <- c("Benevento","Salerno","Bari","Taranto","Lecce","Potenza",
"Matera","Caserta","Napoli","Avellino","Foggia","Brindisi","Cosenza","Crotone","Catanzaro"
,"Vibo Valentia","Reggio di Calabria","Trapani","Palermo","Messina","Agrigento","Caltanissetta"
,"Enna","Catania","Ragusa","Siracusa","Sassari","Nuoro","Cagliari","Oristano","Monza e della Brianza"
,"Fermo","Barletta-Andria-Trani","Imperia","La Spezia","Como","Milano","Reggio nell'Emilia","Verbano-Cusio-Ossola","Asti"
,"Lecco","Bergamo","Lodi","Aosta","Savona","Vicenza","Gorizia","Vercelli","Sondrio"
,"Brescia","Cremona","Torino","Novara","Bolzano","Belluno","Pavia","Mantova","Provincia Autonoma Bolzano"
,"Trento","Biella","Cuneo","Alessandria","Genova","Varese","Padova","Pordenone","Trieste"
,"Parma","Provincia Autonoma Trento","Verona","Treviso","Rovigo","Venezia","Udine"
,"Piacenza","Modena","Bologna","Ferrara","Ravenna","Forli'-Cesena","Rimini","Massa Carrara"
,"Lucca","Pistoia","Firenze","Prato","Livorno","Pisa","Arezzo","Siena","Grosseto"
,"Perugia","Terni","Pesaro e Urbino","Ancona","Macerata","Ascoli Piceno","Viterbo"
,"Rieti","Roma","Latina","Frosinone","Pescara","Chieti","Campobasso","L'Aquila","Teramo"
,"Isernia","Sud Sardegna")

ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  div(class = "container-fluid",
      div(class = "jumbotron text-center bg-primary text-white",
          h1("Analisi della Percentuale di Stranieri in Italia", class = "display-4"),
          p("Esplora l'evoluzione del fenomeno migratorio dal 2002 al 2019", class = "lead")
      ),
      div(class = "row justify-content-center mt-4",
          div(class = "col-lg-4",
              div(class = "card border-info",
                  div(class = "card-body",
                      h4("Seleziona un Anno", class = "card-title text-info"),
                      sliderInput(inputId = "year", sep ="",label = NULL, min = 2002, max = 2019, value = 2002)
                  )
              ),
              div(class = "card border-info",
                  div(class = "card-body",
                      h4("Inserisci una Provincia", class = "card-title text-info"),
                      selectizeInput(
                        inputId = 'provincia',
                        label = '',
                        choices = my_autocomplete_list,
                        selected = NULL,
                        multiple = FALSE, # allow for multiple inputs
                        options = list(create = FALSE) # if TRUE, allows newly created inputs
                      ),
                      
                      textOutput("stranieri"),
                      textOutput("totale"),
                      textOutput("perc")
                  )
              ),
              br(),
              br(),
              br(),
              img(src="sapienza.png",
                  height=120,
                  width=247),
              img(src="Logo.jpg",
                  height=90,
                  width=140
              )
          ),
          div(class = "col-lg-8", 
              div(class = "card border-success",
                  div(class = "card-body",
                      h4("Distribuzione della Percentuale di Stranieri a livello provinciale", class = "card-title text-success"),
                      plotOutput(outputId = "distPlot", height = "700")
                  )
              )
          )
      )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  #importo le librerie che ci serviranno
  library(sf) #per importare dati spaziali
  library(dplyr) #per fare operazioni sui dataset importati
  library(ggplot2) #per fare i grafici
  library(tidyr) #per eseguire la trasformazione long-wide, vedere sotto
  
  #importiamo il dataset con le coordinate geografiche per provincia
  #coordinate=read_sf("/Users/giorgiatagliaferri/Desktop/esame Jona-Gubbiotti/ProvCM01012023_g/ProvCM01012023_g_WGS84.shp")
  coordinate=read_sf("ProvCM01012023_g_WGS84.shp")
  #selezioniamo solo le colonne che ci servono (nome e coordinate geografiche)
  coordinate=coordinate %>% select(c("DEN_UTS",
                                     "geometry"))
  
  #importiamo il dataset con la popolazione residente per ogni provincia e per ogni anno dal 2002 al 2019
  #il file contiene la popolazione residente totale, quella degli stranieri e quella degli italiani
  popolazione <- read.csv("popolazione_province_storico.csv",
                          encoding="utf-8")
  #selezioniamo solo le colonne che ci interessano
  popolazione=popolazione  %>% select(c("Territorio",
                                        "TIME",
                                        "Value",
                                        "Cittadinanza"))
  
  #il formato di questa tabella non è ottimale, vorrei avere per ogni provincia
  #e per ogni anno tre colonne. La prima con il numero di italiani, la seconda
  #con il numero di stranieri e la terza con il totale.
  #devo trasformare questa tabella, che è nel formato detto "long", nel formato detto "wide"
  popolazione_wide=popolazione %>% pivot_wider(names_from=Cittadinanza, values_from=Value)
  #rinomino alcune colonne
  names(popolazione_wide)=c("Territorio","Anno","italiano","straniero","totale")
  
  #creo la colonna che contiene il rapporto tra il numero di stranieri e il totale,
  #per ogni provincia e per ogni anno
  popolazione_wide$perc=popolazione_wide$straniero/popolazione_wide$totale*100
  
  #metto insieme la tabella con le coordinate geografiche e quella con i dati sulla
  #popolazione
  join=merge(y=popolazione_wide, x=coordinate, by.y="Territorio", by.x="DEN_UTS", all=F)
  
  #creo le classi di percentuale di stranieri, per poter visualizzare diversi colori
  #nei grafici
  join$perc_class=cut(join$perc, c(0, 3, 6, 9, 12, 15, 17.9))
  
  output$distPlot <- renderPlot({
    ggplot(join %>% filter(Anno==input$year)) + #filtro per l'anno t
      geom_sf(aes(fill = perc_class)) + #coloro per la classe
      theme_bw()+ #solo un'impostazione grafica
      labs(fill = "Percentuale di\nstranieri residenti\nsulla popolazione\ntotale (ISTAT)", ) +
      theme(legend.justification = "center",
            legend.direction = "horizontal", legend.position = "bottom")+
      ggtitle(label=paste("Anno",input$year))+
      scale_fill_manual(labels=c("x ≤ 3%", "3% < x ≤ 6%", "6% < x ≤ 9%", "9% < x ≤ 12%", "12% < x ≤ 15%", "x > 15%"), values = brewer.pal(6, "Blues")) #scelgo i colori delle classi
  })
  
  output$stranieri <- renderText({
    paste0("Residenti stranieri: ",
           format(as.numeric(as.data.frame(join %>% filter(Anno==input$year, DEN_UTS==input$provincia)) %>% select(straniero)),
                  big.mark=",")
    )
  })
  output$totale <- renderText({
    paste0("Popolazione residente totale: ",
           format(as.numeric(as.data.frame(join %>% filter(Anno==input$year, DEN_UTS==input$provincia)) %>% select(totale)),
                  big.mark=",")
    )
  })
  output$perc <- renderText({
    paste0("Percentuale di stranieri residenti: ",
           round(as.numeric(as.data.frame(join %>% filter(Anno==input$year, DEN_UTS==input$provincia)) %>% select(perc)),digits=1),
           "%")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

