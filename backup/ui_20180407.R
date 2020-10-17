library(shiny)

dados<-read.csv2("NewsTx.csv") 

shinyUI(fluidPage(
  # Application title
  titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", 
               img(src='ob.jpg', align = "left",height=60,width=150))),
  br(""),
  hr(),
  h2("Transplante Renal"),
  
  
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      wellPanel(
      # Escolher o intervalo de anos
      sliderInput("ano", "Seleccione intervalo:",
                  min = 2003, max = 2016, step = 1, sep = "",
                  value = c(2003,2016)),
      
      radioButtons('tipo', 'Seleccione tipo de gráfico:', c('linhas', 'barras'),
                   inline = TRUE)),
      br(),
      br(),
      br(),
      br(),
      
      wellPanel(
      h4("Selecione indicadores"),
      
      selectInput('y1', 'Indicador 1:', names(dados)[-1], 
                  c("DCpmh","TxDCpmh","TxDVpmh","Tx_total_pmh")[2]),
      selectInput('y2', 'Indicador 2:', names(dados)[-1], 
                  c("DCpmh","TxDCpmh","TxDVpmh","Tx_total_pmh")[3])),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),

      wellPanel(
      h4("Escolha o ano ou faça 'play':"),
      
      sliderInput("num", "",
                  min = 2003, max = 2016,
                  value = 2009, step = 1, sep = "",
                  animate =
                    animationOptions(interval = 1600, loop = TRUE))
    )),

    # Show a plot of the indicators evolution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Gráficos", 
                           h4("Distribuição anual dos indicadores"),
                           plotOutput("evolPlot"), 
                           br(),
                           br(),
                           h4("Correlação entre os indicadores"),
                           plotOutput("corrPlot"),
                           br(),
                           br(),
                           h4("Evolução anual do 'indicador 1' segundo o número de dadores cadáver"),
                           plotOutput("movPlot")
                           ),
                  tabPanel("Tabela", tableOutput("tabela"), downloadButton("downloadData", "Download")),
                  tabPanel("Legenda", tableOutput("legenda"))
                  )
      )
  ),
  hr(),
  print("Those informations are based on the Global Observatory on Donation and Transplantation (GODT) data, produced by the WHO-ONT collaboration.
    "),
  hr()
  
))

