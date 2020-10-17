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