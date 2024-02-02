
# PACKAGE HAVEN POUR IMPORTATION DES DONNÉES 

library(haven)
library(questionr)
library(dplyr)
library(survival)
library(survminer)
library(survey)


# IMPORTATION DES DONNÉES : TABLES INDIVIDU ET TABLE SEQUENTR/ LES 100 PREMIÈRES LIGNES

individu <- read_sas("DataIn/individu.sas7bdat",
                     col_select=c("IDENT","id","if","phd","FP1A","Q1","AGE10","CA9C","CA10C","fraetr09","regetab","pondef"))

sequen <- read_sas("DataIn/seqentr.sas7bdat",
                   col_select=c("IDENT","nseq","debut","duree","stat_emb","stat_fin","idnc"))

# TRIER LES OBSERVATIONS DE LA BASE SEQUEN SELON L'IDENTIFIANT ET LE PREMIER EDI #


sequen <- sequen %>% arrange(IDENT, nseq)


# TRIER LES OBSERVATIONS DE LA BASE INDIVIDU SELON L'IDENTIFIANT

individu <- individu %>% arrange(IDENT)



#CREATION DE LA VARIABLE RELATIVE A L'ACCÈS A UN EDI

table(sequen$stat_emb) # TRIE DE LA VARIABLE DU STATUT DE L'EMPLOI A L'EMBAUCHE

# RECODAGE DE LA VARIABLE EN BINAIRE AVEC 1 : ACCÈS A UN EDI ET 0 SINON
sequen$StatutEDI_deb <- case_when(
  sequen$stat_emb=="01" ~ 1,
  sequen$stat_emb=="02" ~ 1,
  sequen$stat_emb=="03" ~ 1,
  sequen$stat_emb=="04" ~ 1,
  sequen$stat_emb=="25" ~ 1,
  TRUE~0
)

table(sequen$StatutEDI_deb) # TRI A PLAT DE LA NOUVELLE 


# RECODAGE DE LA VARIABLE EN BINAIRE AVEC 1 : ACCÈS A UN EDI ET 0 SINON
sequen$StatutEDI_fin <- case_when(
  sequen$stat_fin=="01" ~ 1,
  sequen$stat_fin=="02" ~ 1,
  sequen$stat_fin=="03" ~ 1,
  sequen$stat_fin=="04" ~ 1,
  sequen$stat_fin=="25" ~ 1,
  TRUE~0
)

table(sequen$StatutEDI_fin) # TRI A PLAT DE LA NOUVELLE 


# VARIABLE RELATIVE A L'ACCÈS A UN EDI

sequen$EDI <- case_when(
  sequen$StatutEDI_deb==1 | sequen$StatutEDI_fin==1 ~ 1,
  TRUE~0
)


table(sequen$EDI) # TRI A PLAT DE LA VARIABLE EDI


# FILTRER ET NE CONSERVER QUE LES SEQUENCES CORRESPONDANTES A UN EDI #

sequen <- sequen %>% filter(EDI==1)


## SUPPRIMER LES DOUBLONS DE LA BASE SEQUEN ##
sequen$nseq <- as.numeric(sequen$nseq)

sequen <- sequen %>%group_by(IDENT)%>%filter(nseq==min(nseq))

sequen <- distinct(sequen, IDENT,.keep_all = TRUE)



## GESTION DES DATES DE DEBUT DE L'EDI ##

sequen$dateEDI <- case_when(
  sequen$StatutEDI_deb == 1 ~ sequen$debut,
  sequen$StatutEDI_fin == 1 & sequen$StatutEDI_deb == 0 ~ sequen$idnc
)


##### FUSION DES DONNÉES ENTRE INDIVIDU ET SEQUEN #######

sequen <- sequen %>% arrange(IDENT) # TRIONS LA BASE SEQUEN SUIVANT LES IDENTIFIANTS

individu <- individu %>% arrange(IDENT) # TRIONS LA BASE INDIVIDU SUIVANT LES IDENTIFIANTS


Fusion <- left_join(individu,sequen,by="IDENT")


#### GESTION DES DUREES ###

# LES INDIVIDUS N'AYANT PAS EU D'EDI on transforme les NA en 0 #

Fusion$EDI[is.na(Fusion$EDI)]<-0

# CALCUL DE LA DURÉE D'OBTENTION DE l'EDI #

#Fusion$temps <- 0
Fusion$dateEDI <- as.numeric(Fusion$dateEDI)

Fusion$temps <- ifelse(Fusion$EDI==1, (Fusion$dateEDI-Fusion$id), (Fusion$`if` -Fusion$id+1))



# GESTION DE LA PONDERATION #

Pond <- svydesign(ids = ~1, data = Fusion, weights = ~ Fusion$pondef)



#### KAPLAN-MEIR ###


ObjSurvie <- survfit(Surv(temps,EDI)~1,data = Fusion)


ObjSurvie

ggsurvplot(ObjSurvie)


svykm(Surv(temps,EDI)~1,design = Pond,se=TRUE)
