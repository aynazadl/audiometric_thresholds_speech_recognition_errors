###pour chaquelibrarie, faut d'abord installer le package
#install.packages("tidyverse")
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
library(magrittr)
library(data.table)
### changement de working directory
getwd()
setwd("/Users/Aynaz/Documents/aynaz/Mots")
###importation des données, prendre les numéros de colonnes qu'on veut, changer le format des variables
sorted_df <- read.csv("Results_9999_raw_avec_distances_phonologiques_for_ML.csv", header=T, na.strings=c("","NA")) #à la place de df_pourcentage vous pouvez changer le nom de fichier
sorted_df=sorted_df[,c(2:13,17,18,19)]
str(sorted_df)
sorted_df<-sorted_df %>%mutate_if(is.integer,as.numeric)
sorted_df <- sorted_df %>%convert_as_factor( phoneme_cible,phoneme_reconnu,phoneme_cible_et_reconnu)
###le test statistique
data=sorted_df
result2<-NULL;
for (i in seq(1:12)) {
  a <- colnames(data)
  dtest <- dunn_test(data, as.formula(paste(a[i], a[13], sep="~")),p.adjust.method="bonferroni",detailed = TRUE)%>%
    filter(substr(group1, 1, 1) == substr(group2, 1, 1))
  result2 <- rbind(result2, dtest)
}
###
result21<-result2 %>% filter(str_detect(group1, "(.)\\1"))
result22<-result2 %>% filter(str_detect(group2, "(.)\\1"))
##prendre les colonnes qu'on veut et changer leurs noms
result22=result22[, c(".y.", "group2", "group1","n1","n2","estimate","statistic","p","method","p.adj","p.adj.signif")]
colnames(result22)[which(colnames(result22) %in%c(".y.","group2","group1","n1","n2","statistic","p","p.adj","p.adj.signif") )] <-c("seuil","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif")
colnames(result21)[which(colnames(result21) %in%c(".y.","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif") )] <-c("seuil","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif")
result22_new=rbind(result21, result22)
### changer le format des variables dans les résultats de test et changer les formats de valeurs pour p.adj.signif
str(result22_new)
result22_new$seuil= as.factor(result22_new$seuil)
result22_new <- result22_new %>%convert_as_factor(group2,p.adj.signif)
result22_new$p.adj.signif<-as.numeric(result22_new$p.adj.signif)
result22_new$p.adj.signif[result22_new$p.adj.signif== "5"] <- "non significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "1"] <- "significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "2"] <- "significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "3"] <- "significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "4"] <- "significant"
result22_new$p.adj.signif <- as.factor(result22_new$p.adj.signif)
result22_new$seuil <- factor(result22_new$seuil, levels = c("s_125", "s_250","s_500","s_750", "s_1000","s_1500","s_2000", "s_3000","s_4000","s_6000", "s_8000","s_10000"))
###on va filtrer les données de base par le p_value des erreurs qui sont jamais significatives
d=result22_new[,c(1,3,11)]
d1=subset(d, p.adj.signif=='non significant')
d2=d1 %>%group_by(group2) %>%summarise(count=n())
v=as.vector(d2$group2[d2$count==12])
b=dput(v)
result22_new=subset(result22_new, ! group2 %in% b)
###supprimer les lignes quand il n'y a pas d'erreur
g=unique(sorted_df$mot[str_detect(sorted_df$mot, "(.)\\1")])
c=dput(g)
sorted_df12=subset(sorted_df, ! mot %in% c)
sorted_df12=subset(sorted_df12, ! mot %in% b)
###trouver les erreurs le plus fréquant et trier le heatmap par les plus fréquant
x=names(sort(table(sorted_df12$mot)))
c1=dput(x)
result22_new$group2 <- factor(result22_new$group2, levels = c1)
###création le heatmap
result22_new$seuil=as.factor(result22_new$seuil)
erreur<-ggplot(result22_new, aes(x=seuil,y=group2, fill=statistic,alpha=factor(p.adj.signif))) +
  geom_tile(color = "black")+
  xlab("Seuil audiométrique") + ylab("Erreur phonetique")+ ggtitle("Relation entre les seuils audiométriques \n et les erreurs de reconnaissance de la parole : substitutions phonétiques") +
  scale_fill_distiller(palette = "PiYG", direction=1, limits=c(-25,22),name="Seuil audiometrique plus élevé \n (énergie moins forte)") +
  scale_alpha_discrete("Score de signifcation")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
  )
erreur
### exporter les audiogrammes avec le maximum score de vraisemblance
df_base<- read.csv("df_pourcentage.csv", header=T, na.strings=c("","NA"))
df_base=df_base[,2:20]
df_base<-df_base %>%mutate_if(is.integer,as.numeric)
df_base <- df_base %>%convert_as_factor( mot,phoneme_cible,phoneme_reconnu,n_audio)
df_max_vrai=df_base %>% group_by(mot) %>% slice(which.max(score_vrai))
df_max_vrai_filter=subset(df_max_vrai, ! mot %in% b)
df_max_vrai_filter=subset(df_max_vrai_filter, ! mot %in% c)
####
g=levels(sorted_df$phoneme_cible)
g1=dput(g)
List<- split(sorted_df,sorted_df$phoneme_cible)
table(List$b$phoneme_reconnu)
mylist<- read.csv("mylist.csv", header=T, na.strings=c("","NA"))
