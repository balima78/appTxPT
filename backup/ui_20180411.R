
library(shiny)
library(shinythemes)
library(plotly)
library(shinydashboard)

# https://fontawesome.com/v4.7.0/icons/

#install.packages("shinythemes")

dados<-read.csv2("NewsTx.csv") 

shinyUI(

  fluidPage(
    # nome no browser
  headerPanel(title ="",
          windowTitle = "RAT | Oficina de BioEstatistica"), 
    
  # Título da página com imagem OB
  titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", 
               img(src='ob.jpg', align = "right",height=60,width=150))),
  a(href="http://bioestatisticas.wixsite.com/bioestatisticas/rat", 
    h1("Registo Aberto de Transplantes (RAT)")),
#  br(""),
  hr(),

  tagList(
#  shinythemes::themeSelector(),
    navbarPage(theme = shinytheme("cerulean"),
               "RAT",
               tabPanel("Tx Renal", icon = icon("heartbeat"),
                      h2("Transplante Renal"),
                      sidebarPanel(
                        conditionalPanel(
                          'input.rim === "Gráficos"',
                        wellPanel(
                          # Escolher o intervalo de anos
                          sliderInput("ano", "Seleccione intervalo:",
                                      min = 2003, max = 2016, step = 1, sep = "",
                                      value = c(2003,2016)),
                          radioButtons('tipo', 'Seleccione tipo de gráfico:', 
                                       c('linhas', 'barras'),
                                       inline = TRUE)),
                        br(),
                        br(),
                        br(),
                        br(),
                        wellPanel(
                          # escolher indicadores
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
                          # escolher ano ou fazer play
                          h4("Escolha o ano ou faça 'play':"),
                          sliderInput("num", "",
                                      min = 2003, max = 2016,
                                      value = 2009, step = 1, sep = "",
                                      animate = animationOptions(interval = 1600, loop = TRUE))
                          )),
                        conditionalPanel(
                          'input.rim === "Tabela"',
                          wellPanel(
                            # Escolher o intervalo de anos
                            sliderInput("anos", "Seleccione intervalo:",
                                        min = 2003, max = 2016, step = 1, sep = "",
                                        value = c(2003,2016))
                          )),
                        conditionalPanel(
                          'input.rim === "Legenda"',
                          helpText("Descrição das variáveis usadas nas análises gráficas e disponibilizadas na tabela para download.")
                        )
                        ),
                      # Painel principal de Tx Renal
                      mainPanel(
                        tabsetPanel(type = "tabs",
                                    id = 'rim',
                                    tabPanel("Gráficos", icon = icon("area-chart"),
                                             h4("Distribuição anual dos indicadores"),
                                             plotOutput("evolPlot"), 
                                             br(),
                                             br(),
                                             h4("Correlação entre os indicadores"),
                                             plotOutput("corrPlot"),
                                             br(),
                                             br(),
                                             h4("Evolução anual do 'indicador 1' segundo o número de dadores cadáver"),
                                             plotlyOutput("movPlot")
                                             ),
                                    tabPanel("Tabela", icon = icon("table"),
                                             DT::dataTableOutput("tabela"), downloadButton("downloadData", "Download")),
                                    tabPanel("Legenda", icon = icon("th-list"),
                                             tableOutput("legenda"))
                                    )
                        ),
                      hr(),
                      print("Those informations are based on the Global Observatory on Donation and Transplantation (GODT) data, produced by the WHO-ONT collaboration.
                            "),
                      hr()
                      ),
             navbarMenu("cPRA",
                     tabPanel("calculador", icon = icon("calculator"),
                              # título da sub-página
                              fluidRow(
                                h2("Valor calculado de Pesquisa de Reactividade Alogénica (cPRA)"),
                                h4("Seleccione os antigéneos HLA para os quais o candidato está sensibilizado:")),
                              # selecção de antigéneos
                              fluidRow(
                                checkboxGroupInput("hlaA", "HLA-A*",
                                                          c("A*01" = "A1","A*02" = "A2","A*03" = "A3","A*11" = "A11","A*23" = "A23","A*24" = "A24","A*25" = "A25","A*26" = "A26","A*29" = "A29","A*30" = "A30","A*31" = "A31","A*32" = "A32","A*33" = "A33","A*34" = "A34","A*36" = "A36","A*66" = "A66","A*68" = "A68","A*69" = "A69","A*74" = "A74","A*80" = "A80"),
                                                          inline = TRUE
                                                   ),
                              #  column(2,
                              #         checkboxGroupInput("hlaC", "HLA-C*",
                              #                            c("C01","C02","C03"))
                              #  ),
                                # column(2,
                                  checkboxGroupInput("hlaB", "HLA-B*",
                                                          c('B*07'='B7','B*08'='B8','B*13'='B13','B*14'='B14','B*15'='B15','B*18'='B18','B*27'='B27','B*35'='B35','B*37'='B37','B*38'='B38','B*39'='B39','B*40'='B40','B*41'='B41','B*42'='B42','B*44'='B44','B*45'='B45','B*46'='B46','B*47'='B47','B*48'='B48','B*49'='B49','B*50'='B50','B*51'='B51','B*52'='B52','B*53'='B53','B*54'='B54','B*55'='B55','B*56'='B56','B*57'='B57','B*58'='B58','B*67'='B67','B*73'='B73','B*78'='B78','B*81'='B81','B*82'='B82'),
                                                          inline = TRUE
                                                     ),
                                  checkboxGroupInput("hlaDR", "HLA-DRB1*",
                                                          c('DRB1*01'='DR1','DRB1*03'='DR3','DRB1*04'='DR4','DRB1*07'='DR7','DRB1*08'='DR8','DRB1*09'='DR9','DRB1*10'='DR10','DRB1*11'='DR11','DRB1*12'='DR12','DRB1*13'='DR13','DRB1*14'='DR14','DRB1*15'='DR15','DRB1*16'='DR16'),
                                                          inline = TRUE
                                                     )
                                    ),
                              # butões de acção limpa/calcula
                              fluidRow(
                              actionButton("limpa", "Limpar"),
                              br(),
                              h4("Anticorpos anti-HLA seleccionados:"),
        
                              textOutput("A"),
                              textOutput("C"),
                              textOutput("B"),
                              textOutput("DR"),
                              
                              br(),
                              
                              actionButton("calcula", "Calcular"), 
                              
                              # tableOutput("sensib"),
                              
                              h4("valor percentual de cPRA :"),
                              strong(textOutput("cpra")),
                              h5("(ver 'Nota técnica')")
                              ),
                              # disclaimer
                              fluidRow(
                              br(),
                              h5("O valor gerado pelo calculador cPRA, neste web-site, é apenas para uso informativo."),
                              h5(em("The value produced by the cPRA calculator on this web-site is for your informational use only."))
                              
                              )),
                     tabPanel("Nota técnica", icon = icon("sticky-note"),
                              p("O valor calculado de Pesquisa de Reactividade Alogénica (cPRA) é uma estimativa da probabilidade de CrossMatch virtual positivo tendo em conta as tipagens HLA de um determinado grupo de potenciais dadores [1,2]."),
                              p("Tendo por base os antigéneos HLA para os quais um candidato a transplante esteja sensibilizado, o cPRA mede a dificuldade de encontrar um possível dador para este candidato."),
                              p("Para o estimador cPRA aqui apresentado foram utilizadas as frequencias HLA (alélicas e haplóticas), de 37.993 dadores voluntários de medula óssea, previamente publicadas [3]."),
                              p("O cálculo do valor de cPRA foi feito tal como descrito por Kransdorf e colegas em 2017 [4]"),
                              h6(p("[1] - Bruno A Lima, Helena Alves. Seleção do Par Dador-Recetor em Transplante Renal:Resultados Comparativos de uma Simulação. Acta Med Port 30(12), 2017: 854-860"),
                                 p("[2] - Bruno A. Lima, Miguel Mendes, Helena Alves. Hypersensitized candidates to kidney transplantation in Portugal. Port J Nephrol Hypert. 27(2), 2013: 77-81"),
                                 p("[3] - Bruno A. Lima, Helena Alves. HLA-A, -C, -B, and -DRB1 allelic and haplotypic diversity in bone marrow volunteer donors from Northern Portugal. Organs, Tissues & Cells. Volume 16(1), 2013, March: 19-26"),
                                 p("[4] - Kransdorf E, Pando M, Gragert L, Kaplan B. HLA Population Genetics in Solid Organ Transplantation. Transplantation 101(9), 2017: 1971-1976"))
                              )
        
                     ),
             
             tabPanel("Mais", icon = icon("globe"),
                      h4("Interessa também comparar as actividades de transplantação em Portugal com as dos restantes países da União Europeia."),
                      a(href="https://bioestatisticas.shinyapps.io/appue28/", target="_blank",
                        img(src='link2.jpg', align = "left",height=60,width=150)))
             )
             
  ),

hr(),
fluidRow(
  column(8, icon("copyright"), 
         a("2018, 'Oficina de BioEstatística' - All Rights Reserved |"),
         icon("envelope-o"), a(href="mailto:bioestatisticas@gmail.com","bioestatisticas@gmail.com")
  )),
hr()

))

