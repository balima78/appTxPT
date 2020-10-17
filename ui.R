
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
  titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", target="_blank",
               img(src='ob.jpg', align = "right",height=60,width=150))),
  a(href="http://bioestatisticas.wixsite.com/bioestatisticas/rat", target="_blank",
    h1("Registo Aberto de Transplantes (RAT)")),
  
  tags$head(includeScript("gtagTxPT.js")),
  
  hr(),

  tagList(
#  shinythemes::themeSelector(),
    navbarPage(theme = shinytheme("cerulean"),
               title=div(img(src="openDoor1.jpg"), "RAT"), 
               tabPanel("Tx Renal", icon = icon("heartbeat"),
                        h3("Transplante Renal"),
                        p("Os resultados aqui apresentados foram publicados em:"),
                      a(href="http://repositorio.insa.pt/bitstream/10400.18/4713/1/Boletim_Epidemiologico_Observacoes_N18_2017_artigo5.pdf", 
                        target = "_blank",
                        h6("Bruno A Lima, Helena Alves. Evolução da atividade de transplantação renal em Portugal: dados públicos de 2003 a 2015. Observações - Boletim Epidemiológico, 6(18), 2017: 24-27")
                      ),
                      sidebarPanel(
                        conditionalPanel(
                          'input.rim === "Gráficos"',
                        wellPanel(
                          # Escolher o intervalo de anos
                          sliderInput("ano", "Seleccione intervalo:",
                                      min = 2003, max = 2019, step = 1, sep = "",
                                      value = c(2003,2019)),
                          radioButtons('tipo', 'Seleccione tipo de gráfico:', 
                                       c('linhas', 'barras'),
                                       inline = TRUE)),
                        br(),
                        br(),
                        br(),
                        br(),
                        wellPanel(
                          # escolher indicadores
                          h5("Selecione indicadores"),
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
                          h5("Escolha o ano ou faça 'play':"),
                          sliderInput("num", "",
                                      min = 2003, max = 2019,
                                      value = 2009, step = 1, sep = "",
                                      animate = animationOptions(interval = 1600, loop = TRUE))
                          )),
                        conditionalPanel(
                          'input.rim === "Tabela"',
                          wellPanel(
                            # Escolher o intervalo de anos
                            sliderInput("anos", "Seleccione intervalo:",
                                        min = 2003, max = 2019, step = 1, sep = "",
                                        value = c(2003,2019))
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
                                             h5("Distribuição anual dos indicadores"),
                                             plotOutput("evolPlot"), 
                                             br(),
                                             br(),
                                             h5("Correlação entre os indicadores"),
                                             plotOutput("corrPlot"),
                                             br(),
                                             br(),
                                             h5("Evolução anual do 'indicador 1' segundo o número de dadores cadáver"),
                                             plotlyOutput("movPlot")
                                             ),
                                    tabPanel("Tabela", icon = icon("table"),
                                             DT::dataTableOutput("tabela"), downloadButton("downloadData", "Download")),
                                    tabPanel("Legenda", icon = icon("th-list"),
                                             tableOutput("legenda"))
                                    )
                        ),
                      br(),
                      hr(),
                      print("Those informations are based on the Global Observatory on Donation and Transplantation (GODT) data, produced by the WHO-ONT collaboration.
                            ")
                      
                      ),
             navbarMenu("cPRA",
                     tabPanel("calculador", icon = icon("calculator"),
                              # título da sub-página
                              fluidRow(
                                h3("Valor calculado de Pesquisa de Reactividade Alogénica (cPRA)"),
                                h5("Seleccione os antigéneos HLA para os quais o candidato está sensibilizado:")
                                ),
                              # selecção de antigéneos
                              fluidRow(
                                checkboxGroupInput("hlaA", "HLA-A*",
                                                   c("A*01" = "A1","A*02" = "A2","A*03" = "A3","A*11" = "A11","A*23" = "A23","A*24" = "A24","A*25" = "A25","A*26" = "A26","A*29" = "A29","A*30" = "A30","A*31" = "A31","A*32" = "A32","A*33" = "A33","A*34" = "A34","A*36" = "A36","A*66" = "A66","A*68" = "A68","A*69" = "A69","A*74" = "A74","A*80" = "A80"),
                                                   inline = TRUE
                                                   ),
                                checkboxGroupInput("hlaC", "HLA-C*",
                                                   c('C*01'='C1','C*02'='C2','C*03'='C3','C*04'='C4','C*05'='C5','C*06'='C6','C*07'='C7','C*08'='C8','C*12'='C12','C*14'='C14','C*15'='C15','C*16'='C16','C*17'='C17','C*18'='C18'),
                                                   inline = TRUE),
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
                              # linha 1
                              fluidRow(
                              column(3, actionButton("limpa", "Limpar")),
                              column(1,actionButton("calcula", "Calcular",
                                                    icon("bomb"), 
                                                    style="color: #800000; background-color: #2bdfbb; border-color: #2b2d5c"))
                              ),
                              # linha 2
                              fluidRow(
                                column(3,h5("Anticorpos anti-HLA seleccionados:")),
                                column(3,h5("valor percentual de cPRA:"))
                                ),
                              # linha 3
                              fluidRow(
                                column(3,
                                       wellPanel(textOutput("A"),
                                       textOutput("C"),
                                       textOutput("B"),
                                       textOutput("DR"))),
                                column(2,
                                       wellPanel(strong(textOutput("cpra")), style="color: #ff0000"),
                                       h6("(ver 'Nota técnica')"))
                                ),
                              # disclaimer
                              fluidRow(
                                h5(div("Os valores gerados pelo calculador cPRA, neste web-site, são apenas para uso informativo."),
                                   em("Values produced by the cPRA calculator on this web-site are for your informational use only."))
                              ),
                              # linha 4
                              fluidRow(
                                h4("Percentual de Transplantabilidade (%T)"),
                                radioButtons("ab0", "Seleccione grupo sanguíneo (e volte a 'Calcular'):",
                                             c("A" = "A",
                                               "B" = "B",
                                               "AB" = "AB",
                                               "0" = "O"), 
                                             inline = T)
                                ),
                              fluidRow(column(2,
                                h5("valor %T (isogrupal):"),
                                wellPanel(strong(textOutput("pt")), style="color: #ff0000"),
                                h6("(ver 'Nota técnica')")
                                ),
                                column(2,
                                       h5("valor %T (compatível):"),
                                       wellPanel(strong(textOutput("ptc")), style="color: #ff0000")), 
                                column(8,
                                       plotlyOutput("relacaoPlot"),
                                       p("(valores para uma cohort simulada de 100 doentes)"))
                                
                                )
                              
                             
                              
                              # tableOutput("sensib"),
                              
                     ),
                     
                     tabPanel("Nota técnica", icon = icon("sticky-note"),
                              p("O valor calculado de Pesquisa de Reactividade Alogénica (cPRA) é uma estimativa da probabilidade de CrossMatch virtual positivo, considerando as tipagens HLA de um determinado grupo de potenciais dadores, tendo por base os antigéneos HLA para os quais o candidato a transplante está sensibilizado [1,2]."),
                              p("Para o estimador cPRA aqui apresentado foram utilizadas as frequências HLA (alélicas e haplóticas), de 37.993 dadores voluntários de medula óssea, previamente publicadas [3]."),
                              p("O cálculo do valor de cPRA foi feito tal como descrito por Kransdorf e colegas em 2017 [4]."),
                              p("Os valores percentuais de Transplantabilidade (%T), aqui apresentados, devolvem as percentagens de possíveis dadores que são AB0 idênticos (distribuição isogrupal) ou AB0 compatíveis (distribuição compatível) e têm antigénios HLA aceitáveis pelo potencial candidato."),
                              p("Para o cálculo dos valores %T foram utilizadas as frequências alélicas de grupos sanguíneos, previamente descritas para dadores de sangue em Portugal [5]. As respectivas frequências genotípicas dos grupos AB0 foram estimadas aplicando o princípio de Hardy-Weinberg."),
                              p("Em comparação com o valor de cPRA, o %T tem a vantagem de considerar tanto o sistema HLA como o sistema AB0 para estimar a possibilidade de encontar um dador compatível para um determinado candidato a transplante."),
                              h6(p("[1] - Bruno A Lima, Helena Alves. Seleção do Par Dador-Recetor em Transplante Renal:Resultados Comparativos de uma Simulação. Acta Med Port 30(12), 2017: 854-860"),
                                 p("[2] - Bruno A. Lima, Miguel Mendes, Helena Alves. Hypersensitized candidates to kidney transplantation in Portugal. Port J Nephrol Hypert. 27(2), 2013: 77-81"),
                                 p("[3] - Bruno A. Lima, Helena Alves. HLA-A, -C, -B, and -DRB1 allelic and haplotypic diversity in bone marrow volunteer donors from Northern Portugal. Organs, Tissues & Cells. Volume 16(1), 2013, March: 19-26"),
                                 p("[4] - Kransdorf E, Pando M, Gragert L, Kaplan B. HLA Population Genetics in Solid Organ Transplantation. Transplantation 101(9), 2017: 1971-1976"),
                                 p("[5] - Duran J, Chabert T, Rodrigues F, Pestana D. Distribuição dos grupos sanguíneos na população portuguesa. AB0 29, 2007: 5-17."))
                              )
        
                     ),
             
             tabPanel("Mais", icon = icon("globe"),
                      h5("Interessa também comparar as actividades de transplantação em Portugal com as dos restantes países da União Europeia."),
                      a(href="https://bioestatisticas.shinyapps.io/appue28/", target="_blank",
                        img(src='link2.jpg', align = "left",height=60,width=150)))
             )
             
  ),

hr(),
fluidRow(
  column(8, icon("copyright"), 
         a("2018, 'Oficina de BioEstatística' - All Rights Reserved |"),
         icon("envelope-o"), a(href="http://bioestatisticas.wixsite.com/bioestatisticas/contact","bioestatisticas@gmail.com")
         )),
hr()

))

