library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(devtools)
library(plyr)
library(plotly)
library(matrixStats)
library(ggpubr)
library(rstatix)
library(ARTool)
library(stringr)
library(varhandle)
library(magicfor)
library(data.table)
getwd()
setwd("/Users/Aynaz/Documents/aynaz/Mots")
####audiogramme mediane
#fonction d'audiogramme mediane
# calcul de la distance euclidienne entre tous les audiogrammes
#"DATA" matrice contenant les audiogrammes(chaque ligne contient 1 audiogramme --> nb lignes = nb audiogrammes)
# Retourne le numéro de ligne correspondant à l'audiogramme médian
audiogramme.mediane = function(DATA)
{
  DISTANCE2 = matrix(0, nrow(DATA), nrow(DATA))
  for(ii in 1:nrow(DATA)){
    DIFF = t(DATA) - DATA[ii, ]
    DISTANCE2[,ii] = apply(DIFF^2, 2, sum) / (ncol(DATA) - 1)
  }
  Sum.of.distance <- apply(sqrt(DISTANCE2), 1, sum)
  return(order(Sum.of.distance)[1])
}
###ce librarie est les fonctions pour sauvegarder les résultats de boucle "for" 
library(magicfor)
#importer les données
sorted_df <- read.csv("Results_9999_raw_avec_distances_phonologiques_for_ML.csv", header=T, na.strings=c("","NA"))
#les 12 seuils + la colonne mot
sorted_df=sorted_df[,c(6:17,20)]
#supprimer les lignes ou les phonemes est bien reconnu
sorted_df_diff=sorted_df%>%filter(!str_detect(mot, "(.)\\1"))
#on crée une liste qui contient plusieurs dataframe pour chaque erreur 
#trouver les noms des erreurs et nomer chaque dataframe par leur propres noms (exemple [[1]]-> [[bd]])
sorted_df_list_del=sorted_df_diff %>% group_split(mot,.keep = FALSE)
sorted_df_list=sorted_df_diff %>% group_split(mot)
sorted_df_list_del=lapply(sorted_df_list_del,data.matrix)
###commecer le magic_for
#recuperer les noms des erreurs de la liste
magic_for(print, silent = TRUE)
for(i in 1:length(sorted_df_list)){
  my_vector<-sorted_df_list[[i]]$mot[1]
  print(my_vector)
}
#les noms des erreurs dans un dataframe 
magic_res=magic_result_as_dataframe()
names(sorted_df_list)<-magic_res$my_vector
#changer les noms de la liste
names(sorted_df_list_del)<-magic_res$my_vector
###appliquer le fonction d'audiogramme mediane sur la liste des erreurs
names=dput(names(sorted_df_list_del))
for (i in 1:length(names)) {
  print(i)
  print(sorted_df_list_del[[i]][audiogramme.mediane(sorted_df_list_del[[i]]),])
}
#savegarder ces audiogrammes medianes dans un dataframe 
df_res=magic_result_as_dataframe()
df_res$`sorted_df_list_del[[i]][audiogramme.mediane(sorted_df_list_del[[i]]),`
df_res_erreur=matrix(df_res[,4],nrow=12)
df_res_erreur=t(df_res_erreur)
df_res_erreur=as.data.frame(df_res_erreur)
df_res_erreur <- cbind(df_res_erreur, erreur = magic_res$my_vector)
#changer les noms des colonnes 
colnames(df_res_erreur)
colnames(df_res_erreur)[which(colnames(df_res_erreur) %in%
                                c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
#écrire un csv pour les résultats
write.csv(df_res_erreur,"/Users/Aynaz/Documents/aynaz/Mots/df_res_erreur.csv", row.names = FALSE)