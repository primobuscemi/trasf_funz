library(shiny)
  library(shinydashboard)
  library(ggplot2)
  library(dplyr)
  library(data.table)
  library(readxl)
  library(DT)
  library(tidyr)
library(data.table)

 dati <- read_excel("/home/primo/Scrivania/dashboard/trasfusionale/trasf2018_19.xls") 
  dati <- separate(dati, data_mov, c("data", "ora"), sep= " ")
  dati$data <- as.Date(dati$data, format = "%d-%m-%Y")
  
  #Preparazione data-frame
  nsaccheperpaz <- tapply(dati$codice_anagrafico_individuale, dati$descr_reparto, table)



nsacxrep <- function(data){
    nsaccheperpaz <- tapply(dati$codice_anagrafico_individuale, dati$descr_reparto, table)
 dfns <- mapply(sum, nsaccheperpaz)
  nomins <- attr(dfns, "names")
  tblns <-data.table(nomins, mapply(sum, nsaccheperpaz))
  dfsacxrep <- data.frame(tblns)
  tblns <- rename(tblns, Reparti= nomins)
  tblns <- rename(tblns, N.= V2)
return(tblns)}

plot_npazxrep <- function(dati){
  nsaccheperpaz <- tapply(dati$codice_anagrafico_individuale, dati$descr_reparto, table)
  dfnp <- mapply(nrow, nsaccheperpaz)
  nominp <- attr(dfnp, "names")
  tblnp <-data.table(nominp, mapply(nrow, nsaccheperpaz))
  dfpazxrep <- data.frame(tblnp)
  dfpazxrep$nominp <- recode(dfpazxrep$nominp, "CH. GENERALE"= "CG", "CHIRURGIA VASCOLARE"= "CV", "CLINICA S.ANNA AG"= "CSA", "DAY HOSPITAL EMATOLOGIA" = "DHE", "EMODIALISI"= "ED", "ESTERNI TERRITORIO"= "ET", "HOSPICE"= "HOS", "LUNGODEGENZA" = "LD", "MEDICINA"= "MED", "MEDICINA DH"= "MDH","MEDICINA FISICA E RIABILITATIVA"= "MFR","NEONATOLOGIA"= "NEO", "NEUROLOGIA"= "NEU", "ONCOLOGIA"= "ONC","ORTOPEDIA I"= "ORT", "OSTETRICIA GINECOLOGIA"= "GEO", "OTORINOLARINGOIATRIA"= "ORL", "PEDIATRIA"= "PED", "PRONTO SOCCORSO MEDICO"= "PS", "RIANIMAZIONE" = "RIA", "THALASSEMIA"= "THA", "UROLOGIA"= "URO","UTIC"= "UTC", "UTIN"="UTN")
  tblnp <- rename(tblnp,Reparti= nominp)
  tblnp <- rename(tblnp, N.= V2)
  dfpazxrep$nominp <- recode(dfpazxrep$nominp, "CH. GENERALE"= "CG", "CHIRURGIA VASCOLARE"= "CV", "CLINICA S.ANNA AG"= "CSA", "DAY HOSPITAL EMATOLOGIA" = "DHE", "EMODIALISI"= "ED", "ESTERNI TERRITORIO"= "ET", "HOSPICE"= "HOS", "LUNGODEGENZA" = "LD", "MEDICINA"= "MED", "MEDICINA DH"= "MDH","MEDICINA FISICA E RIABILITATIVA"= "MFR","NEONATOLOGIA"= "NEO", "NEUROLOGIA"= "NEU", "ONCOLOGIA"= "ONC","ORTOPEDIA I"= "ORT", "OSTETRICIA GINECOLOGIA"= "GEO", "OTORINOLARINGOIATRIA"= "ORL", "PEDIATRIA"= "PED", "PRONTO SOCCORSO MEDICO"= "PS", "RIANIMAZIONE" = "RIA", "THALASSEMIA"= "THA", "UROLOGIA"= "URO", "UTIC"= "UTC", "UTIN"="UTN")
  return(ggplot(dfpazxrep, aes(x=reorder(nominp, -V2), y=V2))+ geom_bar(stat = "identity", color="darkblue", fill="lightblue") + xlab("Reparti")+ ylab("N. di pazienti trasfusi"))
}


plot_nsacxrep <- function(dati, n){
  dati <- filter(dati, cat_emc == "n")
   nsaccheperpaz <- tapply(dati$codice_anagrafico_individuale, dati$descr_reparto, table)
  dfns <- mapply(sum, nsaccheperpaz)
  nomins <- attr(dfns, "names")
  library(data.table)
  tblns <-data.table(nomins, mapply(sum, nsaccheperpaz))
  dfsacxrep <- data.frame(tblns)
  tblns <- rename(tblns, Reparti= nomins)
  tblns <- rename(tblns, N.= V2)
  dfsacxrep$nomins <- recode(dfsacxrep$nomins, "CH. GENERALE"= "CG", "CHIRURGIA VASCOLARE"= "CV", "CLINICA S.ANNA AG"= "CSA", "DAY HOSPITAL EMATOLOGIA" = "DHE", "EMODIALISI"= "ED", "ESTERNI TERRITORIO"= "ET", "HOSPICE"= "HOS", "LUNGODEGENZA" = "LD", "MEDICINA"= "MED", "MEDICINA DH"= "MDH","MEDICINA FISICA E RIABILITATIVA"= "MFR","NEONATOLOGIA"= "NEO", "NEUROLOGIA"= "NEU", "ONCOLOGIA"= "ONC","ORTOPEDIA I"= "ORT", "OSTETRICIA GINECOLOGIA"= "GEO", "OTORINOLARINGOIATRIA"= "ORL", "PEDIATRIA"= "PED", "PRONTO SOCCORSO MEDICO"= "PS", "RIANIMAZIONE" = "RIA", "THALASSEMIA"= "THA", "UROLOGIA"= "URO", "UTIC"= "UTC", "UTIN"="UTN")
  
  print(ggplot(dfsacxrep, aes(x=reorder(nomins, -V2), y=V2))+ geom_bar(stat = "identity", color="darkred", fill="brown3") + xlab("Reparti")+ ylab("N. di sacche trasfuse"))
  
}

  prova <- function(data, value="numeric"){
  dati <- dati %>%  filter(emc)
  filtrati <- dati[dati$emc]
  print(dati$emc)
}

  #funzione per 
  trasf <- function(data, n){
    data <- data %>% filter(cod_emc==n) 
   p <-  ggplot(data) + geom_bar(mapping = aes(x = volume))
   pb <-  ggplot(data) + geom_boxplot(mapping = aes(x = strumento, y= volume))
    m <-  mean(data$volume)
  dev_s <- sd(data$volume)
  summa <- summary(data$volume)
print(p)
print(pb)
print(m)
print(dev_s)
 print(summa)     }

