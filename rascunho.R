library(ggplot2)
library(dplyr)
library(tidyr)
dados<-read.csv2("NewsTx.csv") 

dados<- dados %>% 
  filter(Ano >= 2003 & Ano <= 2016) 

dim(dados)





g1<-ggplot(dados,aes_string(x="Ano")) +
  geom_line(aes_string(y=input$y1), colour = "red", size=2) +
  geom_point(aes_string(y=input$y1), size=2) +
  scale_x_continuous(breaks = input$ano[1]:input$ano[2]) +
  ylab("") +
  geom_line(aes_string(y=input$y2), colour = "blue", size=2) +
  geom_point(aes_string(y=input$y2), size=2) + theme_bw()

#    g1 <- ggplot(dados, aes_string(x="Ano", y=input$y1)) + geom_point() +
#   geom_line(color = "blue") + 
#    scale_x_continuous(breaks = input$ano[1]:input$ano[2]) +
#   ylab("") +
#geom_line(aes_string(x="Ano", y=input$y2), color="red") +
#geom_point(aes_string(x="Ano", y=input$y2), color="black") 
#g1 + theme_bw()



dadost<-gather(dados,"indicador", "valor",2:13)
dadost<-filter(dadost, indicador == "TxDCpmh" | indicador == "TxDVpmh")

l1<-ggplot(dadost) + geom_line(aes_string(x="Ano", y="valor", colour="indicador")) +
  scale_colour_manual(values=c("red","green")) +
  scale_x_continuous(breaks = 2003:2016) +
  ylab("") + 
  theme_bw() #+  theme(legend.position="none")

b1<-ggplot(dadost,aes_string(x="Ano", y = "valor", fill= "indicador")) +
  geom_bar(stat = "identity", position=position_dodge(), na.rm = T) +
  scale_x_continuous(breaks = 2003:2016) +
  ylab("") + 
  scale_fill_manual(values=c("TxDCpmh"= "green","TxDVpmh"="black")) +
  theme_bw() #+  theme(legend.position="none")










dadosc<- dados %>% filter(Ano >= 2003 & Ano <= 2016) 

g2 <- ggplot(dados, aes_string(x="TxDCpmh", y="TxDVpmh")) + 
  geom_point() + stat_smooth(method="loess", colour = "red") 
g2 + theme_minimal()

cor(dadosc)

cor(dados[c("TxDCpmh","TxDVpmh")],method="spearman")[1,2]


dadosc["TxDCpmh"]



dadosc<- dados %>% filter(Ano >= 2003 & Ano <= 2016)
dados1<- dados %>% filter(Ano == 2009)

g3<-ggplot(dados, aes_string(x="Ano")) +
  geom_point(aes_string(y="TxDCpmh", size = "DadoresCadaver"), alpha = 0.5) +
  scale_x_continuous(breaks = 2003:2016, limits = c(2003,2016)) +
  geom_line(aes_string(y="TxDCpmh"), colour = "blue") +
  
  geom_point(aes_string(y="TxDVpmh", size = "DadoresCadaver"), alpha = 0.5) +
  geom_line(aes_string(y="TxDVpmh"), colour = "red") 

g3 + theme_minimal()



################ criar ficheiro cPRAs e percTs##########
## preparar dados
cpras<-read.csv2("cpras.csv")

cpras$ab0<-abo()

cpras <- cpras %>% mutate(pTiso = ifelse(ab0 == "A", 0.46582*(1-cPRA/100),
                                          ifelse(ab0 == "B", 0.07699*(1-cPRA/100),
                                                 ifelse(ab0 == "AB", 0.03433*(1-cPRA/100),
                                                        0.42287*(1-cPRA/100))
                                          )
)
)

cpras <- cpras %>% mutate(pTcomp = ifelse(ab0 == "A", ((1-0.056754)^2)*(1-cPRA/100),
                                         ifelse(ab0 == "B", ((1-0.292588)^2)*(1-cPRA/100),
                                                ifelse(ab0 == "AB",(1-cPRA/100),
                                                       (0.650657^2)*(1-cPRA/100))
                                         )
)
)

cpras$pTiso<-cpras$pTiso*100
cpras$pTcomp<-cpras$pTcomp*100

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

gls<-ggplot(cpras) +
  geom_line(aes(x=ln,y=cPRA, label = AB0, color = "cPRA")) +
  geom_line(aes(ln,pT.isogrupal, label = AB0, color = "%T (isogrupal)")) +
  geom_line(aes(ln,pT.compativel, label = AB0, color = "%T (compatível)")) +
  scale_color_discrete(name ="") +
  ylab("valor percentual (%)") + xlab("") + 
  ggtitle("Relação entre os valores de cPRA e de Transplantabilidade (%T)")

gls +
  theme_minimal() + theme(axis.title.x=element_blank(),
                            axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(),
                            legend.title = element_blank())
  
write.csv2(cpras,"cpras.csv")

###### código a passar para a app####
cpras<-read.csv2("cpras.csv")
cprast <- cpras %>% gather("Indicadores", "valor",c(2,4,5))

gls2<-ggplot(cprast) +
  geom_line(aes(x=ordem,y=valor,colour = Indicadores), size = 1.2) 

gls2 +
  scale_colour_discrete(name  ="Indicador",
                        breaks=c("cPRA", "pTcomp","pTiso"),
                        labels=c("cPRA", "%T (compatível)", "%T (isogrupal)")) +
  scale_shape_discrete(name  ="Indicador",
                       breaks=c("cPRA", "pTcomp","pTiso"),
                       labels=c("cPRA", "%T (compatível)", "%T (isogrupal)")) +
  ylab("valor percentual (%)")  +
  #geom_text(aes(ab0)) +
  theme_minimal() +   theme(axis.title.x=element_blank(),
                             axis.text.x=element_blank(),
                             axis.ticks.x=element_blank(),
                             legend.title = element_blank())


ggplotly(gls) 
plotly_build(gls + theme_minimal() + theme(axis.title.x=element_blank(),
                                          axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank(),
                                          legend.title = element_blank()))



###################
cpras<-read.csv2("cpras.csv")
names(cpras)<-c("X","ln","cPRA","AB0","pT.isogrupal","pT.compativel")
cpras$cPRA<-round(cpras$cPRA,2)
cpras$pT.isogrupal<-round(cpras$pT.isogrupal,2)
cpras$pT.compativel<-round(cpras$pT.compativel,2)

pa<-0.292588
pb<-0.056754
po<-0.650657

ggs<-ggplot(cpras) 
ggs <- ggs  + geom_line(aes(x=ln,y=cPRA, color = "cPRA")) +
  ylab("valor percentual (%)") + xlab("")

## 0 ##
g0<- ggs + geom_line(aes(x=ln,y=(1-cPRA/100)*po^2*100, color = "%T"))

## AB
gAB<-ggs + geom_line(aes(ln,round((1-cPRA/100)*100,2), color = "%T compatível")) +
  geom_line(aes(ln,round((1-cPRA/100)*(2*pa*pb)*100,2), color = "%T isogrupal"))


## A ##
gA<-ggs + geom_line(aes(ln,(1-cPRA/100)*((pa+po)^2)*100, color = "%T compatível")) +
  geom_line(aes(ln,(1-cPRA/100)*(pa^2+2*pa*po)*100, color = "%T isogrupal"))

## B ##
gB<-ggs + geom_line(aes(ln,(1-cPRA/100)*((pb+po)^2)*100, color = "%T compatível")) +
  geom_line(aes(ln,(1-cPRA/100)*(pb^2+2*pb*po)*100, color = "%T isogrupal"))

gAB0<-list(A=gA,B=gB,AB=gAB,O=g0)

plotly_build(gAB0[[3]] + 
               ggtitle(paste("relação entre cPRA e %T para grupo",names(gAB0)[3])) +
               theme_minimal() + 
               theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank(),
                     legend.title = element_blank()))