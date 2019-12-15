# Pacchetti
library(dplyr)
library(ggplot2)
library(data.table)

nsaccheperpaz <- tapply(trasf2018_19$codice_anagrafico_individuale, trasf2018_19$descr_reparto, table)
# numero di sacche per singolo paziente in ogni reparto
nsaccheperpaz
# numero di sacche per reparto
mapply(sum, nsaccheperpaz)
#numero di pazienti trasfusi per reparto
mapply(nrow, nsaccheperpaz)
# media, deviazione standard e mediana delle distribuzioni delle sacche per paziente in ogni reparto
mapply(mean, nsaccheperpaz)
mapply(sd, nsaccheperpaz)
mapply(median, nsaccheperpaz)
#grafico n. di sacche nei vari reparti
dfns <- mapply(sum, nsaccheperpaz)
nomins <- attr(dfns, "names")
tblns <-data.table(nomins, mapply(sum, nsaccheperpaz))
dfsacxrep <- data.frame(tblns)

dfsacxrep$nomins <- recode(dfsacxrep$nomins, "CH. GENERALE"= "Ch. Gen", "CHIRURGIA VASCOLARE"= "Ch. Vasc.", "CLINICA S.ANNA AG"= "Clin. S. Anna", "DAY HOSPITAL EMATOLOGIA" = "DH, Emat.", "EMODIALISI"= "Emod.", "ESTERNI TERRITORIO"= "Est. Terr.", "HOSPICE"= "Hosp.", "LUNGODEGENZA" = "Lungod.", "MEDICINA"= "Med.", "MEDICINA DH"= "Med. DH","MEDICINA FISICA E RIABILITATIVA"= "Med. Fis. e riab.","NEONATOLOGIA"= "Neonat.", "NEUROLOGIA"= "Neurol.", "ONCOLOGIA"= "Oncol.","ORTOPEDIA I"= "Ortop.", "OSTETRICIA GINECOLOGIA"= "Ginecol. e Ost.", "OTORINOLARINGOIATRIA"= "ORL", "PEDIATRIA"= "Pediat.", "PRONTO SOCCORSO MEDICO"= "Pront. socc.", "RIANIMAZIONE" = "Rianim.", "THALASSEMIA"= "Thal.", "UROLOGIA"= "Urol.")

print(ggplot(dfsacxrep, aes(x=reorder(nomins, -V2), y=V2), xlab= "Reparti", ylab= "N. di sacche")+ geom_bar(stat = "identity", color="darkblue", fill="lightblue") + xlab("Reparti")+ ylab("N. di sacche"))

#grafico n. di pazienti trasfusi nei vari reparti


dfnp <- mapply(nrow, nsaccheperpaz)
nominp <- attr(dfnp, "names")
tblnp <-data.table(nominp, mapply(nrow, nsaccheperpaz))
dfpazxrep <- data.frame(tblnp)
dfpazxrep$nominp <- recode(dfpazxrep$nominp, "CH. GENERALE"= "Ch. Gen", "CHIRURGIA VASCOLARE"= "Ch. Vasc.", "CLINICA S.ANNA AG"= "Clin. S. Anna", "DAY HOSPITAL EMATOLOGIA" = "DH, Emat.", "EMODIALISI"= "Emod.", "ESTERNI TERRITORIO"= "Est. Terr.", "HOSPICE"= "Hosp.", "LUNGODEGENZA" = "Lungod.", "MEDICINA"= "Med.", "MEDICINA DH"= "Med. DH","MEDICINA FISICA E RIABILITATIVA"= "Med. Fis. e riab.","NEONATOLOGIA"= "Neonat.", "NEUROLOGIA"= "Neurol.", "ONCOLOGIA"= "Oncol.","ORTOPEDIA I"= "Ortop.", "OSTETRICIA GINECOLOGIA"= "Ginecol. e Ost.", "OTORINOLARINGOIATRIA"= "ORL", "PEDIATRIA"= "Pediat.", "PRONTO SOCCORSO MEDICO"= "Pront. socc.", "RIANIMAZIONE" = "Rianim.", "THALASSEMIA"= "Thal.", "UROLOGIA"= "Urol.")
print(ggplot(dfpazxrep, aes(x=reorder(nominp, -V2), y=V2), xlab= "Reparti", ylab= "N. di pazienti")+ geom_bar(stat = "identity", color="darkblue", fill="lightblue") + xlab("Reparti")+ ylab("N. di pazienti trasfusi"))

