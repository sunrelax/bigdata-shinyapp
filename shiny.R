library(shiny)
library(bslib)
library(RColorBrewer)
require(pals)

# Define UI for app that draws a histogram ----
ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  div(class="container-fluid",
      div(class="text-center text-lg-start",
        h3("Percentuale di stranieri residenti sulla popolazione totale (ISTAT, 2009 - 2019)", class="d-none d-lg-block"),
        h3("Percentuale di stranieri (ISTAT)", class="d-block d-lg-none")
      ),
      div(class="row",
          div(class="col-lg-2",
              div(class="well",
                  numericInput(inputId = "year", label = "Anno:", min = 2009, max = 2019, value = 2009)
              )
          ),
          div(class="col-lg-10", 
              plotOutput(outputId = "distPlot", height="800")
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
  coordinate=read_sf("ProvCM01012023_g_WGS84.shp")
  #selezioniamo solo le colonne che ci servono (nome e coordinate geografiche)
  coordinate=coordinate %>% select(c("DEN_UTS",
                                     "geometry"))
  #View(coordinate)
  
  #importiamo il dataset con la popolazione residente per ogni provincia e per ogni anno dal 2002 al 2019
  #il file contiene la popolazione residente totale, quella degli stranieri e quella degli italiani
  popolazione <- read.csv("popolazione_province_storico.csv",
                          encoding="utf-8")
  #selezioniamo solo le colonne che ci interessano
  popolazione=popolazione  %>% select(c("Territorio",
                                        "TIME",
                                        "Value",
                                        "Cittadinanza"))
  #View(popolazione)
  
  #il formato di questa tabella non è ottimale, vorrei avere per ogni provincia
  #e per ogni anno tre colonne. La prima con il numero di italiani, la seconda
  #con il numero di stranieri e la terza con il totale.
  #devo trasformare questa tabella, che è nel formato detto "long", nel formato detto "wide"
  popolazione_wide=popolazione %>% pivot_wider(names_from=Cittadinanza, values_from=Value)
  #rinomino alcune colonne
  names(popolazione_wide)=c("Territorio","Anno","italiano","straniero","totale")
  #View(popolazione_wide)
  
  #creo la colonna che contiene il rapporto tra il numero di stranieri e il totale,
  #per ogni provincia e per ogni anno
  popolazione_wide$perc=popolazione_wide$straniero/popolazione_wide$totale*100
  
  #metto insieme la tabella con le coordinate geografiche e quella con i dati sulla
  #popolazione
  join=merge(y=popolazione_wide, x=coordinate, by.y="Territorio", by.x="DEN_UTS", all=F)
  #View(join)
  
  #creo le classi di percentuale di stranieri, per poter visualizzare diversi colori
  #nei grafici
  join$perc_class=cut(join$perc, c(0, 3, 5, 8, 10, 12, 15, 17.9))
  
  #tramite un ciclo, faccio tanti grafici quanti sono gli anni
  #for(t in seq(2002,2019)){
  output$distPlot <- renderPlot({
    ggplot(join %>% filter(Anno==input$year)) + #filtro per l'anno t
    geom_sf(aes(fill = perc_class)) + #coloro per la classe
    theme_bw()+ #solo un'impostazione grafica
    #labs(title=t)+ #mette l'anno come titolo del grafico
    labs(fill = "Percentuale di\nstranieri residenti\nsulla popolazione\ntotale (ISTAT)", ) +
    scale_fill_brewer()+
    theme(legend.justification = "center",
            legend.direction = "horizontal", legend.position = "bottom")
    #scale_fill_manual(values=c(pal.bands(coolwarm), brewer.pal(10,”Purples“)[5], "yellow","yellow3", "orange","orange3","red","red3")) #scelgo i colori delle classi
  })
  
  
  
  #print(plot) #printo il grafico
  #}
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

