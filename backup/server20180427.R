
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)


# data.frame com os dados do NewsTx
dados<-read.csv2("NewsTx.csv") 

# função para calcular coeficiente de correlação e valor de p
corr_eqn <- function(x,y, method='pearson', digits = 3) {
  corr_coef <- round(cor.test(x, y, method=method)$estimate, digits = digits)
  corr_pval <- round((cor.test(x,y, method=method)$p.value), digits = digits)
  paste(method, 'r = ', corr_coef, ',', 'pval =', corr_pval)
}

#####################################
# função para calcular cPRA
vprac_1<-function(id = 1, sensib = acHLA,
                  A_freq = read.csv2("CEDACE/hla_A.csv"),
                  C_freq = read.csv2("CEDACE/hla_C.csv"),   ###
                  B_freq = read.csv2("CEDACE/hla_B.csv"),
                  DR_freq = read.csv2("CEDACE/hla_DRB1.csv"),
                  AC_freq = read.csv2("CEDACE/hla_AC.csv"), ###
                  AB_freq = read.csv2("CEDACE/hla_AB.csv"),
                  ADR_freq = read.csv2("CEDACE/hla_ADR.csv"),
                  CB_freq = read.csv2("CEDACE/hla_CB.csv"), ###
                  CDR_freq = read.csv2("CEDACE/hla_CDR.csv"), ###
                  BDR_freq = read.csv2("CEDACE/hla_BDR.csv"),
                  ABDR_freq = read.csv2("CEDACE/hla_ABDR.csv"),
                  ACB_freq = read.csv2("CEDACE/hla_ACB.csv"), ###
                  ACDR_freq = read.csv2("CEDACE/hla_ACDR.csv"), ###
                  CBDR_freq = read.csv2("CEDACE/hla_CBDR.csv"), ###
                  ACBDR_freq = read.csv2("CEDACE/hla_ACBDR.csv") ###
){
  if(!require(dplyr)) {
    message("instaling the 'dplyr' package")
    install.packages("dplyr")
    library(dplyr)
  }  
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% A_freq$AA)>=1){
    Sa<-sum(A_freq %>% 
              filter(AA %in% sensib[sensib$ID == id,"acs"]) %>% 
              select(freq))
  } else Sa<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% C_freq$CC)>=1){
    Sc<-sum(C_freq %>% 
              filter(CC %in% sensib[sensib$ID == id,"acs"]) %>% 
              select(freq))
  } else Sc<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% B_freq$BB)>=1){
    Sb<-sum(B_freq %>% 
              filter(BB %in% sensib[sensib$ID == id,"acs"]) %>% 
              select(freq))
  } else Sb<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR)>=1){
    Sdr<-sum(DR_freq %>% 
               filter(DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
               select(freq))
  } else Sdr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% A_freq$AA &
         sensib[sensib$ID == id,"acs"] %in% C_freq$CC) >=1){  
    Sac<-sum(AC_freq %>% 
               filter(AA %in% sensib[sensib$ID == id,"acs"] &
                        CC %in% sensib[sensib$ID == id,"acs"]) %>% 
               select(freq))
  } else Sac<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% A_freq$AA &
         sensib[sensib$ID == id,"acs"] %in% B_freq$BB) >=1){  
    Sab<-sum(AB_freq %>% 
               filter(AA %in% sensib[sensib$ID == id,"acs"] &
                        BB %in% sensib[sensib$ID == id,"acs"]) %>% 
               select(freq))
  } else Sab<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% A_freq$AA &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR) >=1){  
    Sadr<-sum(ADR_freq %>% 
                filter(AA %in% sensib[sensib$ID == id,"acs"] &
                         DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
                select(freq))
  } else Sadr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% C_freq$CC &
         sensib[sensib$ID == id,"acs"] %in% B_freq$BB) >=1){  
    Scb<-sum(CB_freq %>% 
               filter(CC %in% sensib[sensib$ID == id,"acs"] &
                        BB %in% sensib[sensib$ID == id,"acs"]) %>% 
               select(freq))
  } else Scb<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% C_freq$CC &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR) >=1){  
    Scdr<-sum(CDR_freq %>% 
                filter(CC %in% sensib[sensib$ID == id,"acs"] &
                         DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
                select(freq))
  } else Scdr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% B_freq$BB &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR) >=1){  
    Sbdr<-sum(BDR_freq %>% 
                filter(DDR %in% sensib[sensib$ID == id,"acs"] &
                         BB %in% sensib[sensib$ID == id,"acs"]) %>% 
                select(freq))
  } else Sbdr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% B_freq$BB &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR &
         sensib[sensib$ID == id,"acs"] %in% A_freq$AA)>=1){  
    Sabdr<-sum(ABDR_freq %>% 
                 filter(AA %in% sensib[sensib$ID == id,"acs"] &
                          BB %in% sensib[sensib$ID == id,"acs"] &
                          DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
                 select(freq))
  } else Sabdr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% B_freq$BB &
         sensib[sensib$ID == id,"acs"] %in% C_freq$CC &
         sensib[sensib$ID == id,"acs"] %in% A_freq$AA)>=1){  
    Sacb<-sum(ACB_freq %>% 
                filter(AA %in% sensib[sensib$ID == id,"acs"] &
                         BB %in% sensib[sensib$ID == id,"acs"] &
                         CC %in% sensib[sensib$ID == id,"acs"]) %>% 
                select(freq))
  } else Sacb<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% C_freq$CC &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR &
         sensib[sensib$ID == id,"acs"] %in% A_freq$AA)>=1){  
    Sacdr<-sum(ACDR_freq %>% 
                 filter(AA %in% sensib[sensib$ID == id,"acs"] &
                          CC %in% sensib[sensib$ID == id,"acs"] &
                          DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
                 select(freq))
  } else Sacdr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% B_freq$BB &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR &
         sensib[sensib$ID == id,"acs"] %in% C_freq$CC)>=1){  
    Scbdr<-sum(CBDR_freq %>% 
                 filter(CC %in% sensib[sensib$ID == id,"acs"] &
                          BB %in% sensib[sensib$ID == id,"acs"] &
                          DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
                 select(freq))
  } else Scbdr<-0
  
  if(sum(sensib[sensib$ID == id,"acs"] %in% C_freq$CC &
         sensib[sensib$ID == id,"acs"] %in% B_freq$BB &
         sensib[sensib$ID == id,"acs"] %in% DR_freq$DDR &
         sensib[sensib$ID == id,"acs"] %in% A_freq$AA)>=1){  
    Sacbdr<-sum(ACBDR_freq %>% 
                  filter(AA %in% sensib[sensib$ID == id,"acs"] &
                           CC %in% sensib[sensib$ID == id,"acs"] &
                           BB %in% sensib[sensib$ID == id,"acs"] &
                           DDR %in% sensib[sensib$ID == id,"acs"]) %>% 
                  select(freq))
  } else Sacbdr<-0
  
  vpraf<-1-(1-Sa-Sc-Sb-Sdr+Sac+Sab+Scb+Scdr+Sbdr+Sadr-Sabdr-Sacb-Sacdr-Scbdr+Sacbdr)^2
  vpraf
  
} 

#####################################

##### gráfico cPRA e %Ts ##############
cpras<-read.csv2("cpras.csv")
names(cpras)<-c("X","ln","cPRA","AB0","pT.isogrupal","pT.compativel")
cpras$cPRA<-round(cpras$cPRA,2)
cpras$pT.isogrupal<-round(cpras$pT.isogrupal,2)
cpras$pT.compativel<-round(cpras$pT.compativel,2)

gls<-ggplot(cpras) +
  geom_line(aes(x=ln,y=cPRA, label = AB0, color = "cPRA")) +
  geom_line(aes(ln,pT.isogrupal, label = AB0, color = "%T (isogrupal)")) +
  geom_line(aes(ln,pT.compativel, label = AB0, color = "%T (compatível)")) +
  scale_color_discrete(name ="") +
  ylab("valor percentual (%)") + xlab("") + 
  ggtitle("Relação entre os valores de cPRA e de Transplantabilidade (%T)")


############ probabilidades de alelos AB0 ########
pa<-0.292588
pb<-0.056754
po<-0.650657
#################

shinyServer(function(input, output, session) {

  dadosx<-reactive({dados %>% filter(Ano >= input$ano[1] & Ano <= input$ano[2])})
  dadosxs<-reactive({dados %>% filter(Ano >= input$anos[1] & Ano <= input$anos[2])})
  
  dadost<-reactive({
    gather(dadosx(),"Indicadores", "valor",2:13) %>%
      filter(Indicadores == input$y1 | Indicadores == input$y2)
  })
  

  # Nav1
  ## tab1
  output$evolPlot <- renderPlot({

    b1<-ggplot(dadost(),aes_string(x="Ano", y = "valor", fill= "Indicadores")) +
      geom_bar(stat = "identity", position=position_dodge(), na.rm = T) +
      scale_x_continuous(breaks = input$ano[1]:input$ano[2]) +
      ylab("") + 
      scale_fill_manual(values=c("red","blue")) +
      theme_minimal() 
    
    l1<-ggplot(dadost()) + geom_line(aes_string(x="Ano", y="valor", colour="Indicadores"), size = 1.3) +
      geom_point(aes_string(x="Ano", y="valor", colour="Indicadores"), size = 4, alpha = 0.25) +
      geom_point(aes_string(x="Ano", y="valor", colour="Indicadores"), size = 1.75, color = "white") +
      scale_colour_manual(values=c("red","blue")) +
      scale_x_continuous(breaks = input$ano[1]:input$ano[2]) +
      ylab("") + 
      theme_minimal()
    #l1<-ggplotly(l1)

    if(input$tipo == "linhas") {
      return(l1)
    }
    else if(input$tipo == "barras"){
      return(b1)
    }

  })
  

  
  output$corrPlot <- renderPlot({
      g2 <- ggplot(dadosx(), aes_string(x=input$y2, y=input$y1)) + 
      geom_point() + stat_smooth(method="loess", colour = "red") + 
        geom_label(
        x = 0.95 * max(dadosx()[input$y2]),
        y = 0.95 * max(dadosx()[input$y1]),
        label = corr_eqn(dadosx()[[input$y1]], dadosx()[[input$y2]],
                         method = 'spearman')
        )
      g2 + theme_minimal()
      
    }) 
  
  output$movPlot <- renderPlotly({

    dadosm<- dados %>% filter(Ano >= 2003 & Ano <= input$num)
    
    g3<-ggplot(dadosm, aes_string(x="Ano", y=input$y1)) +
      geom_line(colour = "blue", size = 1.3) +
      scale_x_continuous(breaks = 2003:2016, limits = c(2003,2016)) +
      scale_y_continuous(limits = c(min(dados[input$y1]*0.1, na.rm = T), 
                                    max(dados[input$y1]*1.1, na.rm = T))) +
      scale_size_continuous(limits=c(150,350),
                            breaks=c(200, 230, 260, 290, 320),
                            range = c(1,25)) +
      geom_point(aes(size = DadoresCadaver), alpha = 0.5, colour = "red") 

    
    ggplotly(g3 + theme_minimal())
    
  })
  
  ## tab2
  output$tabela <- DT::renderDataTable({
    dadosxs()
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dados", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(dadosxs(), 
                 file,row.names = T)
    }
  )
  
  ## tab3
  output$legenda <- renderTable({
    legenda<-data.frame(Codigo = names(dados),
                        Descricao = c("Ano",
                                      "População a meio do ano",
                                      "nº de dadores cadáver",
                                      "Dadores Cadáver por milhão de habitantes",
                                      "nº de trasplantes com dador cadáver",
                                      "transplantes com dador cadáver por milhão de habitantes",
                                      "nº de transplantes com dador vivo",
                                      "transplantes com dador vivo por milhão de habitantes",
                                      "nº de inscrições incidentes em Lista de Espera",
                                      "nº de doentes em Lista Activa a 31 de dezembro",
                                      "nº de mortes em Lista de Espera",
                                      "nº total de transplantes",
                                      "total de transplantes por milhão de habitantes"))
    legenda
    })
  
  # Nav2

  observe({
    if ( is.null(input$limpa) || input$limpa == 0)
      return()
    updateCheckboxGroupInput(session,"hlaA",
                             choices = c("A*01" = "A1","A*02" = "A2","A*03" = "A3","A*11" = "A11","A*23" = "A23","A*24" = "A24","A*25" = "A25","A*26" = "A26","A*29" = "A29","A*30" = "A30","A*31" = "A31","A*32" = "A32","A*33" = "A33","A*34" = "A34","A*36" = "A36","A*66" = "A66","A*68" = "A68","A*69" = "A69","A*74" = "A74","A*80" = "A80"),
                             inline = TRUE, 
                             selected=NULL)
    updateCheckboxGroupInput(session,"hlaC",
                             choices = c('C*01'='C1','C*02'='C2','C*03'='C3','C*04'='C4','C*05'='C5','C*06'='C6','C*07'='C7','C*08'='C8','C*12'='C12','C*14'='C14','C*15'='C15','C*16'='C16','C*17'='C17','C*18'='C18'),
                             inline = TRUE,
                             selected=NULL)
    updateCheckboxGroupInput(session,"hlaB",
                             choices = c('B*07'='B7','B*08'='B8','B*13'='B13','B*14'='B14','B*15'='B15','B*18'='B18','B*27'='B27','B*35'='B35','B*37'='B37','B*38'='B38','B*39'='B39','B*40'='B40','B*41'='B41','B*42'='B42','B*44'='B44','B*45'='B45','B*46'='B46','B*47'='B47','B*48'='B48','B*49'='B49','B*50'='B50','B*51'='B51','B*52'='B52','B*53'='B53','B*54'='B54','B*55'='B55','B*56'='B56','B*57'='B57','B*58'='B58','B*67'='B67','B*73'='B73','B*78'='B78','B*81'='B81','B*82'='B82'),
                             inline = TRUE,
                             selected=NULL)
    updateCheckboxGroupInput(session,"hlaDR",
                             choices = c('DRB1*01'='DR1','DRB1*03'='DR3','DRB1*04'='DR4','DRB1*07'='DR7','DRB1*08'='DR8','DRB1*09'='DR9','DRB1*10'='DR10','DRB1*11'='DR11','DRB1*12'='DR12','DRB1*13'='DR13','DRB1*14'='DR14','DRB1*15'='DR15','DRB1*16'='DR16'),
                             inline = TRUE, 
                             selected=NULL)
  })
  
  observe({
    if ( is.null(input$limpa) || input$limpa == 0)
      return()
    updateRadioButtons(session,"ab0",
                       choices = c("A" = "A",
                                   "B" = "B",
                                   "AB" = "AB",
                                   "0" = "O"), inline = T
    )
  })

  output$A<-renderText(input$hlaA)
  output$C<-renderText(input$hlaC)
  output$B<-renderText(input$hlaB)
  output$DR<-renderText(input$hlaDR)
  
  sens<-eventReactive(input$calcula, {
    a<-input$hlaA
    b<-input$hlaB
    c<-input$hlaC
    dr<-input$hlaDR
    
    acs<-c(a,b,c,dr)
    
    data.frame(ID=1, acs)
  })
  
  gi<-eventReactive(input$calcula, switch(input$ab0,
                                         A = pa^2+2*pa*po,
                                         B = pb^2+2*pb*po,
                                         AB = 2*pa*pb,
                                         O = po^2)
                   )
  
  gc<-eventReactive(input$calcula, switch(input$ab0,
                                         A = (pa+po)^2,
                                         B = (pb+po)^2,
                                         AB = 1,
                                         O = po^2)
                    )
  
  
  #output$sensib<-renderTable(sens())

  v <- reactiveValues(data = NULL)
  
  observeEvent(input$calcula, {
    v$data <- sens()
  })
  
  observeEvent(input$limpa, {
    v$data <- NULL
  })  
  
  output$cpra <- renderText({
    if (is.null(v$data)) return()
    paste(round(vprac_1(sensib = v$data)*100,2),"%")
  })
  

  output$pt <- renderText({
    if (is.null(v$data)) return()
    paste(round((as.numeric(gi()) * (1-vprac_1(sensib = v$data)))*100,2),"%")
  })
  
  output$ptc <- renderText({
    if (is.null(v$data)) return()
    paste(round((as.numeric(gc()) * (1-vprac_1(sensib = v$data)))*100,2),"%")
  })
  
  output$relacaoPlot <- renderPlotly({
    plotly_build(gls + 
                   theme_minimal() + 
                   theme(axis.title.x=element_blank(),
                         axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         legend.title = element_blank())
                 )
  })

})






