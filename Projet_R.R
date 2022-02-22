
setwd("/home/secke/Documents/R")
getwd()

############## lecture des données et extraction de la colonne Note ####################

donnee<-read.csv("Projet_Python.csv")

library(stringr)
note<-donnee['Note']
str(note)
head(note)
library(dplyr)
#fct<-c("Math","PC","SVT","Français","Anglais","HG")
#fct<-as.factor(fct)
#matiere<-split(note,fct,sep="#")


#head(matiere)
#cr=as.character(c("Math[20;19.5:05] #Francais[13;16:14] #Anglais[17;19:17] #PC[14;13:17] #SVT[12;19:10] #HG[16;19:17]"))
#split(cr,fct,sep="#")

######################### Programme principal ################################


#### Boucle pour séparer les matières ######
for (i in note) {
  matiere0<-str_split(i,"#")
  print(matiere0)
}

##### Variables #######
Nom_matiere=character()
Devoir1=numeric()
Devoir2=numeric()
Devoir3=numeric()
Exam=numeric()
Devoir11=list()
Devoir12=list()
Devoir13=list()
Examen=list()
Tab=data.frame()
TABLEAU=data.frame()


########## Boucles pour extraction finale des notes et noms des matières ################
for (m in seq(1:length(matiere0))) {
  for (indc in seq(1:length(matiere0[[m]]))) {
    splt_0=str_split(matiere0[[m]][indc],"\\[")
    nom_mat=splt_0[[1]][1]
    splt_1=str_split(splt_0[[1]][2],"\\]")
    notes_ttle=splt_1[[1]][1]
    splt_2=str_split(notes_ttle,"\\:")
    exam=splt_2[[1]][2]
    exam=as.numeric(exam)
    splt_3=str_split(splt_2[[1]][1],"\\;")
    d1=splt_3[[1]][1]
    d2=splt_3[[1]][2]
    d3=splt_3[[1]][3]
    d1=as.numeric(d1)
    d2=as.numeric(d2)
    d3=as.numeric(d3)
    Nom_matiere[indc]<-nom_mat
    Devoir1[indc]<-d1
    Devoir2[indc]<-d2
    Devoir3[indc]<-d3
    if (is.na(Devoir3[indc])) {Devoir3[indc]<-0}
    Exam[indc]<-exam
    #print(c(Nom_matiere,Devoir1,Devoir2,Devoir3,Exam))
    #print(splt_3[[1]][1])
  }
  Devoir11[[m]]=Devoir1
  Devoir12[[m]]=Devoir2
  Devoir13[[m]]=Devoir3
  Examen[[m]]=Exam
  
  Tab<-data.frame(Nom_matiere,Devoir11[[m]],Devoir12[[m]],Devoir13[[m]],Examen[[m]])
  TABLEAU<-bind_rows(TABLEAU,Tab)
  #Tab[,1]<-Nom_matiere
  #ESSAI<-rbind(Tab)
  #print(Tab)

}

################## Calcul et ajout de la moyenne des devoirs et de la moyenne générale ############

TABLEAU$Moyenne_Devoir<-rowMeans(TABLEAU[,c(2:4)],na.rm = TRUE)

TABLEAU$Moyenne_Generale<-(TABLEAU$Moyenne_Devoir+(2*TABLEAU$Examen..m..))/3

head(TABLEAU)
