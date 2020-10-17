
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

dados<-read.csv2("NewsTx.csv") 

corr_eqn <- function(x,y, method='pearson', digits = 3) {
  corr_coef <- round(cor.test(x, y, method=method)$estimate, digits = digits)
  corr_pval <- round((cor.test(x,y, method=method)$p.value), digits = digits)
  paste(method, 'r = ', corr_coef, ',', 'pval =', corr_pval)
}


shinyServer(function(input, output) {

  
  dadosx<-reactive({dados %>% filter(Ano >= input$ano[1] & Ano <= input$ano[2])})
  
  dadost<-reactive({
    gather(dadosx(),"Indicadores", "valor",2:13) %>%
      filter(Indicadores == input$y1 | Indicadores == input$y2)
  })
  
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
  
  output$movPlot <- renderPlot({

    dadosm<- dados %>% filter(Ano >= 2003 & Ano <= input$num)
    
    g3<-ggplot(dadosm, aes_string(x="Ano", y=input$y1)) +
      geom_line(colour = "blue", size = 1.3) +
      scale_x_continuous(breaks = 2003:2016, limits = c(2003,2016)) +
      scale_y_continuous(limits = c(min(dados[input$y1]*0.9, na.rm = T), 
                                    max(dados[input$y1]*1.1, na.rm = T))) +
      scale_size_continuous(limits=c(150,350),
                            breaks=c(200, 230, 260, 290, 320),
                            range = c(1,25)) +
      geom_point(aes(size = DadoresCadaver), alpha = 0.5, colour = "red") 

    
    g3 + theme_minimal()
    
  }) 
 
  output$tabela <- renderTable({
    dadosx()
    })
  

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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dados", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(dadosx(), 
                 file,row.names = T)
    }
  )

  })






