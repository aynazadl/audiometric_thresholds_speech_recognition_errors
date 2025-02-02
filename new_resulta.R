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
library(ggprism)
#library(patchwork)
library(magrittr)
library(varhandle)
library(magicfor)
library(data.table)
library(fda.usc)
library(fda)
#library(FSA)
setwd("/Users/Aynaz/Documents/aynaz/Mots")
getwd()
#######
#importation des données merge de python sur R pour regarder combien d'erreur sont produit
df_merge<- read.csv("df_merge_.csv", header=T, na.strings=c("","NA"))
df_merge=df_merge[,-1]
df_merge_new=subset(df_merge,erreur_x=="tR" | erreur_x=="Rs"| erreur_x=="dg"| erreur_x=="kp"|
                      erreur_x=="fR"| erreur_x=="tp"| erreur_x=="vk"| erreur_x=="dt"| erreur_x=="vR"|
                      erreur_x=="vp"| erreur_x=="nb"| erreur_x=="bp"| erreur_x=="mb"| erreur_x=="Rf"|
                      erreur_x=="fk"| erreur_x=="vf"| erreur_x=="fp"| erreur_x=="nm"| erreur_x=="mn"|
                      erreur_x=="ml"| erreur_x=="sf"| erreur_x=="bv"| erreur_x=="bf"| erreur_x=="vt"|
                      erreur_x=="sp"| erreur_x=="RS"| erreur_x=="df"| erreur_x=="zg"| erreur_x=="vg"|
                      erreur_x=="dk"| erreur_x=="vZ"| erreur_x=="dp"| erreur_x=="kt"| erreur_x=="mz"| erreur_x=="bg")
df_merge1=df_merge %>%filter(substr(erreur_x, 1, 1) == substr(erreur_y, 1, 1))
df_merge1_new=df_merge1 %>%
  separate(x, c("125", "250","500","750", "1000","1500","2000", "3000","4000","6000", "8000","10000"), "-")
df_merge1_new=df_merge1_new[,c(1:13)]
df_merge1_new_melt <- melt(df_merge1_new,
                 id.vars = "erreur_x",
                 variable.name = "intensity",
                 value.name = "frequency")
df_merge1_new_melt$intensity=as.numeric(as.character(df_merge1_new_melt$intensity))
df_merge1_new_melt$frequency=as.numeric(df_merge1_new_melt$frequency)
df_merge1_new_melt$erreur_x=as.factor(df_merge1_new_melt$erreur_x)
ggplot(data=df_merge1_new_melt, aes(x=intensity, y = frequency)) + facet_wrap(~erreur_x)
ggplot(df_merge1_new_melt, aes(x=intensity, y=frequency))+
  geom_line()+
  facet_wrap(.~erreur_x)
df_merge2=df_merge1 %>%filter(substr(erreur_x, 1,2 ) == substr(erreur_y, 1, 2))
df_merge3=df_merge %>%filter(!str_detect(erreur_y, "(.)\\1"))
df_merge2_new=subset(df_merge2,erreur_x=="dp" | erreur_x=="kt"| erreur_x=="ml"| erreur_x=="tR"|erreur_x=="bv")
df_merge3=df_merge3[ grep("j", df_merge3$erreur_y, invert = TRUE) , ]
unique(df_merge3$erreur_y)
X1<- read.csv("result.csv", header=T, na.strings=c("","NA"))
df_verdi<-df_verdi %>%
  unite(x, c(X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29), sep = "-", remove = FALSE)
write.csv(df_verdi,"/Users/Aynaz/Documents/aynaz/Mots/df_verdi1.csv", row.names = FALSE)

df_file <- read.csv("df_file.csv", header=T, na.strings=c("","NA"))
df_file<-df_file %>%
  unite(x, c(X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29), sep = "-", remove = FALSE)
mean_func_audio1=format(mean_func_audio1, digits = 0)
write.csv(mean_func_audio1,"/Users/Aynaz/Documents/aynaz/Mots/mean_func_audio1.csv", row.names = TRUE)
####differente erreurs
df_base<- read.csv("df_pourcentage.csv", header=T, na.strings=c("","NA"))
sorted_df <- read.csv("df_pourcentage.csv", header=T, na.strings=c("","NA"))
write.csv(sorted_df,"/Users/Aynaz/Documents/aynaz/Mots/dataframe.csv", row.names = FALSE)
####
sorted_df=sorted_df[,c(6:17,20)]
str(sorted_df)
####
data=sorted_df
result2<-NULL;
for (i in seq(1:12)) {
  a <- colnames(data)
  dtest <- dunn_test(data, as.formula(paste(a[i], a[13], sep="~")),p.adjust.method="bonferroni",detailed = TRUE)%>%
    filter(substr(group1, 1, 1) == substr(group2, 1, 1))
  result2 <- rbind(result2, dtest)
}
d2<-NULL;
result21<-result2 %>% filter(str_detect(group1, "(.)\\1"))
result22<-result2 %>% filter(str_detect(group2, "(.)\\1"))

result22=result22[, c(".y.", "group2", "group1","n1","n2","estimate","statistic","p","method","p.adj","p.adj.signif")]
colnames(result22)[which(colnames(result22) %in%
                         c(".y.","group2","group1","n1","n2","statistic","p","p.adj","p.adj.signif") )] <-
  c("seuil","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif")
colnames(result21)[which(colnames(result21) %in%
                           c(".y.","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif") )] <-
  c("seuil","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif")
result22_new=rbind(result21, result22)
mean_label=aggregate(d[, 3], list(d$group2), mean)
max_label=aggregate(d[, 3], list(d$group2), max)
max_mean_label=cbind(mean_label,max_label)
max_mean_label=max_mean_label[,c(1,2,4)]
colnames(max_mean_label)=c("error","mean_z","max_z")
ggplot(max_mean_label, aes(x=error)) +
  geom_line( aes(y=mean_z), size=2) +
  geom_line( aes(y=max_z), size=2) +
  scale_y_continuous(
    name = "mean z score",
    sec.axis = sec_axis(~., name="max z score"))
####
result0<-NULL;
for (i in seq(1:12)) {
  a <- colnames(data)
  dtest <- dunn_test(data, as.formula(paste(a[i], a[13], sep="~")),p.adjust.method="bonferroni",detailed = TRUE)%>%
    filter(substr(group1, 1, 1) == substr(group2, 1, 1))
  result0 <- rbind(result0, dtest)
}
######
df_base=df_base[,2:20]
df_base<-df_base %>%
  mutate_if(is.integer,as.numeric)
df_base <- df_base %>%
  convert_as_factor( mot,phoneme_cible,phoneme_reconnu,n_audio)
df_max_vrai=df_base %>% group_by(mot) %>% slice(which.max(score_vrai))
df_max_vrai_filter=subset(df_max_vrai, ! mot %in% b)
df_max_vrai_filter=subset(df_max_vrai_filter, ! mot %in% c)
########
sorted_df <- read.csv("sorted_df.csv", header=T, na.strings=c("","NA"))
sorted_df=sorted_df[,-1]
str(sorted_df)
sorted_df<-sorted_df %>%
  mutate_if(is.integer,as.numeric)
sorted_df <- sorted_df %>%
  convert_as_factor( mot)
str(sorted_df)
#####
sorted_df %>% kruskal_effsize(X125 ~ mot)
#####
str(result22_new)
#result22_new$seuil<-stringr::str_replace(result22_new$seuil, '\\s_', '')
result22_new$seuil= as.factor(result22_new$seuil)
result22_new <- result22_new %>%
  convert_as_factor(group2,p.adj.signif)
result22_new$p.adj.signif<-as.numeric(result22_new$p.adj.signif)
result22_new$p.adj.signif[result22_new$p.adj.signif== "5"] <- "non significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "1"] <- "significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "2"] <- "significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "3"] <- "significant"
result22_new$p.adj.signif[result22_new$p.adj.signif== "4"] <- "significant"
result22_new$p.adj.signif <- as.factor(result22_new$p.adj.signif)
#levels(result22_new$group2) <- gsub("(.)(.)", "\\1->\\2", levels(result22_new$group2))
result22_new<-subset(result22_new, select = -group1)
result22_new$seuil <- factor(result22_new$seuil, levels = c("s_125", "s_250","s_500","s_750", "s_1000","s_1500","s_2000", "s_3000","s_4000","s_6000", "s_8000","s_10000"))
#####
result22_new=result22_new %>% mutate(group2 = as.factor(gsub("->", "", group2)))
d=result22_new[,c(1,2,10)]
d1=subset(d, p.adj.signif=='non significant')
d2=d1 %>%group_by(group2) %>%summarise(count=n())
v=as.vector(d2$group2[d2$count==12])
b=dput(v)
result22_new=subset(result22_new, ! group2 %in% b)
#sorted_df11=subset(sorted_df, ! mot %in% b1)
g=unique(sorted_df$mot[str_detect(sorted_df$mot, "(.)\\1")])
c=dput(g)
sorted_df12=subset(sorted_df, ! mot %in% c)
sorted_df12=subset(sorted_df12, ! mot %in% b)
x=names(sort(table(sorted_df12$mot)))
c1=dput(x)
result22_new$group2 <- factor(result22_new$group2, levels = c1)
#result22_new1=subset(result22_new,  group2 %in% c1)
####
####differente traits phonologiques
####
df_trait <- read.csv("df_trait.csv", header=T, na.strings=c("","NA"))
df_trait=df_trait[,-1]
df_trait=df_trait[,-1]
str(df_trait)
df_trait <- df_trait %>%
  convert_as_factor(erreur_phonetique)
df_trait<-df_trait %>%
  mutate_if(is.integer,as.numeric)
data1=df_trait
result1<-NULL;
for (i in seq(1:12)) {
  a <- colnames(data1)
  dtest1 <- dunn_test(data1, as.formula(paste(a[i], a[13], sep="~")),
                     p.adjust.method="bonferroni")%>%
    filter(substr(group1, 1, 4) == substr(group2, 1,4))
  result1 <- rbind(result1, dtest1)
}
colnames(result1)[which(colnames(result1) %in%
                         c(".y.","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif") )] <- c("seuil","group1","group2","n1","n2","statistic","p","p.adj","p.adj.signif")
colnames(df_trait)
result1 <- result1 %>%
  convert_as_factor( seuil,p.adj.signif,group2)
result1$p.adj.signif<-as.numeric(result1$p.adj.signif)
result1$seuil<-stringr::str_replace(result1$seuil, '\\X', '')
result1$seuil= factor(sort(as.numeric(result1$seuil)))
result1$p.adj.signif[result1$p.adj.signif== "5"] <- "non significant"
result1$p.adj.signif[result1$p.adj.signif== "1"] <- "significant"
result1$p.adj.signif[result1$p.adj.signif== "2"] <- "significant"
result1$p.adj.signif[result1$p.adj.signif== "3"] <- "significant"
result1$p.adj.signif[result1$p.adj.signif== "4"] <- "significant"
result1$p.adj.signif <- as.factor(result1$p.adj.signif)
result1<-subset(result1, select = -group1)
str(result1)
write.csv(result1,"/Users/Aynaz/Documents/aynaz/Mots/result1.csv", row.names = FALSE)
result1 <- read.csv("result1.csv", header=T, na.strings=c("","NA"))
result
######
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
######
vars <- paste0("X", c(125,250,500,750,1000,1500,2000,3000,4000,6000,8000,10000))
sorted_df[paste0("m_", vars)] <- lapply(sorted_df[vars], ave, sorted_df[["mot"]])
sorted_df_mean=unique(sorted_df[,c(13:25)])
colnames(sorted_df_mean)[which(colnames(sorted_df_mean) %in%
                                c('mot',"m_X125","m_X250","m_X500","m_X750","m_X1000","m_X1500","m_X2000","m_X3000","m_X4000","m_X6000","m_X8000","m_X10000") )] <- c('mot',"X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000")
colnames(sorted_df_mean)
sorted_df_mean=sorted_df_mean %>% remove_rownames %>% column_to_rownames(var="mot")
sorted_df_mean=as.data.frame(melt(as.data.table(sorted_df_mean, keep.rownames = "Vars"), id.vars = "Vars"))
sorted_df_mean_same<-sorted_df_mean %>%
  filter(str_detect(Vars, "(.)\\1"))
sorted_df_mean_same<-sorted_df_mean_same[with(sorted_df_mean_same, order(Vars, variable)), ]
result<-merge(result, sorted_df_mean_same, by.x=c(".y.", "group1"), by.y=c("variable", "Vars"))
sorted_df_mean_notsame<-sorted_df_mean %>%
  filter(!str_detect(Vars, "(.)\\1"))
sorted_df_mean_notsame<-sorted_df_mean_notsame[with(sorted_df_mean_notsame, order(Vars, variable)), ]
result<-merge(result, sorted_df_mean_notsame, by.x=c(".y.", "group2"), by.y=c("variable", "Vars"))
result$diff=result$value.y-result$value.x
result$diff[result$p.adj.signif == "ns"] <- 0
result<-result %>% mutate(.y. = as.numeric(gsub("X", "", .y.)))
result=result[with(result, order(.y.)), ]
result_min<-as.data.frame(result %>%
                             group_by(group2) %>%
                             slice(which.min(diff)))
result_min=result_min[,c(2,14)]
colnames(result_min)[which(colnames(result_min) %in% "group2" )] <- "trait"
result<-merge(result, result_min, by.x= "group2", by.y="trait")
result$new_diff=result$diff.x-result$diff.y
ggplot(data=result, aes(x=result$.y., y = new_diff, colour = group2, group = group2)) + geom_line() + facet_wrap(~group2)+ylim(50,0)
######
######
result_verdi<-unique(result[,c(1,2,16)])
colnames(result_verdi)[which(colnames(result_verdi) %in%
                                c('group2',".y.","new_diff") )] <- c('erreur',"seuil","intensite")
colnames(result_verdi)
result_verdi=spread(result_verdi, seuil, intensite)
write.csv(result_verdi,"/Users/Aynaz/Documents/aynaz/Mots/result_verdi.csv", row.names = FALSE)
colnames(result_verdi)[which(colnames(result_verdi) %in%
                                c('erreur',"125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000") )] <- c('erreur',"X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000")
result_verdi<-result_verdi %>%
  unite(x, c(X125,X250,X500,X750,X1000,X1500,X2000,X3000,X4000,X6000,X8000,X10000), sep = "-", remove = FALSE)
write.csv(result_verdi,"/Users/Aynaz/Documents/aynaz/Mots/result_verdi.csv", row.names = FALSE)
#####
vars <- paste0("X", c(125,250,500,750,1000,1500,2000,3000,4000,6000,8000,10000))
df_trait[paste0("m_", vars)] <- lapply(df_trait[vars], ave, df_trait[["erreur_phonetique"]])
df_trait_mean=unique(df_trait[,c(13:25)])
colnames(df_trait_mean)[which(colnames(df_trait_mean) %in%
                                 c('erreur_phonetique',"m_X125","m_X250","m_X500","m_X750","m_X1000","m_X1500","m_X2000","m_X3000","m_X4000","m_X6000","m_X8000","m_X10000") )] <- c('erreur_phonetique',"X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000")
colnames(df_trait_mean)
df_trait_mean=df_trait_mean %>% remove_rownames %>% column_to_rownames(var="erreur_phonetique")
df_trait_mean=as.data.frame(melt(as.data.table(df_trait_mean, keep.rownames = "Vars"), id.vars = "Vars"))
result1<-merge(result1, df_trait_mean, by.x=c(".y.", "group1"), by.y=c("variable", "Vars"))
result1<-merge(result1, df_trait_mean, by.x=c(".y.", "group2"), by.y=c("variable", "Vars"))
result1$diff=result1$value.y-result1$value.x
result1$diff[result1$p.adj.signif == "ns"] <- 0
result1<-result1 %>% mutate(.y. = as.numeric(gsub("X", "", .y.)))
result1=result1[with(result1, order(.y.)), ]
result1_min<-as.data.frame(result1 %>%
  group_by(group2) %>%
  slice(which.min(diff)))
result1_min=result1_min[,c(2,12)]
colnames(result1_min)[which(colnames(result1_min) %in% "group2" )] <- "trait"
result1<-merge(result1, result1_min, by.x= "group2", by.y="trait")
result1$new_diff=result1$diff.x-result1$diff.y
ggplot(data=result1, aes(x=result1$.y., y = new_diff, colour = group2, group = group2)) + geom_line() + facet_wrap(~group2)+ylim(50,0)
#####
str(result1)
result1$seuil=as.factor(result1$seuil)
erreur_trait<-ggplot(result1, aes(x=seuil,y=group2, fill=statistic,alpha=factor(p.adj.signif))) +
  geom_tile(color = "black")+
  xlab("Seuil audiométrique") + ylab("Erreur phonologique")+ggtitle("Relation entre les seuils
  audiométriques \n et les erreurs de reconnaissance de la parole : ajout ou supression de traits phonologiques") +
  scale_fill_distiller(palette = "PiYG", direction=1, limits=c(-14,14),name="Seuil audiometrique plus
                       élevé \n (énergie moins forte)") +
  scale_alpha_discrete("Score de signifcation")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
  )
erreur_trait
#########################################################################
#########################################################################
sorted_df_diff=sorted_df%>%filter(!str_detect(mot, "(.)\\1"))
sorted_df_list_del=sorted_df_diff %>% group_split(mot,.keep = FALSE)
sorted_df_list=sorted_df_diff %>% group_split(mot)
sorted_df_list_del=lapply(sorted_df_list_del,data.matrix)
magic_for(print, silent = TRUE)
for(i in 1:length(sorted_df_list)){
 my_vector<-sorted_df_list[[i]]$mot[1]
 print(my_vector)
}
magic_res=magic_result_as_dataframe()
names(sorted_df_list)<-magic_res$my_vector
names(sorted_df_list_del)<-magic_res$my_vector
names=dput(names(sorted_df_list_del))
###
library(magrittr)
library(varhandle)
library(magicfor)
#par(mfrow = c(1,3))
#loop.vector <- 1:157
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
for (i in 1:length(names)) {
  print(i)
  print(sorted_df_list_del[[i]][audiogramme.mediane(sorted_df_list_del[[i]]),])
}
df_res=magic_result_as_dataframe()
df_res$`sorted_df_list_del[[i]][audiogramme.mediane(sorted_df_list_del[[i]]),`
df_res_erreur=matrix(df_res[,4],nrow=12)
df_res_erreur=t(df_res_erreur)
df_res_erreur=as.data.frame(df_res_erreur)
df_res_erreur <- cbind(df_res_erreur, erreur = magic_res$my_vector)
colnames(df_res_erreur)[which(colnames(df_res_erreur) %in%
                                c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
colnames(df_res_erreur)
write.csv(df_res_erreur,"/Users/Aynaz/Documents/aynaz/Mots/df_res_erreur.csv", row.names = FALSE)

#

for (i in 1:length(names)) {
  x <- sorted_df_list_del[[i]]
  matplot(t(x),
          type='l', lty=1,main = paste("erreur=", i), col = "light grey")
  matlines(x[audiogramme.mediane(x),],
           type='l', lty=1, col = rainbow(n))
}
####
bf=sorted_df_diff %>%filter(mot == 'bf')
bf=bf[,c(1:12)]
bf=data.matrix(bf)
DAfrTA=bf
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
audio_med=audiogramme.mediane(DATA)
audio_med
names=c("bd" ,"bf", "bg", "bp" ,"bR", "bv", "bz", "db", "df", "dg", "dk" ,"dp" ,"dR" ,"dt", "dv" ,"dz", "fk", "fp","fR", "fs", "gk", "kp",
        "kt" ,"lm", "ln", "mb", "ml", "mn", "mv", "mz" ,"nb", "nd", "ng" ,"nm", "pR" ,"Rf", "Rs" ,"RS" ,"sf", "Sf" ,"sk", "sp", "sR", "Ss",
        "tl", "tp" ,"tR", "vf", "vg" ,"vk", "vp", "vR", "vt" ,"vz", "vZ", "zg" ,"zR" ,"zs" ,"zZ")
par(mfrow = c(1,3))
loop.vector <- 1:59

for (i in 1:length(names)) {
    x <- sorted_df_list_del[[i]]
    matplot(t(x),
            type='l', lty=1,main = paste("erreur=", i), col = "light grey")
    matlines(x[audiogramme.mediane(x),],
             type='l', lty=1, col = rainbow(n))
  }
df_res<-df_res_erreur %>%
  unite(x, c(X125,X250,X500,X750,X1000,X1500,X2000,X3000,X4000,X6000,X8000,X10000), sep = "-", remove = FALSE)
write.csv(df_res,"/Users/Aynaz/Documents/aynaz/Mots/df_res.csv", row.names = FALSE)
####
matplot(t(DATA), type = "l")
n <- nrow(DATA)
n
matplot(t(DATA),
        type='l', lty=1, col = rainbow(n),
        xlab="frequence", ylab="seuil_audiometrique")
matplot(t(DATA),
        type='l', lty=1, col = "light grey",
        xlab="frequence", ylab="seuil_audiometrique")
matlines(DATA[audiogramme.mediane(DATA),],
         type='l', lty=1, col = rainbow(n))

DATA[audiogramme.mediane(DATA),]
sorted_df_list_del[["bd"]]
names=c("bd" ,"bf", "bg", "bp" ,"bR", "bv", "bz", "db", "df", "dg", "dk" ,"dp" ,"dR" ,"dt", "dv" ,"dz", "fk", "fp","fR", "fs", "gk", "kp",
       "kt" ,"lm", "ln", "mb", "ml", "mn", "mv", "mz" ,"nb", "nd", "ng" ,"nm", "pR" ,"Rf", "Rs" ,"RS" ,"sf", "Sf" ,"sk", "sp", "sR", "Ss",
       "tl", "tp" ,"tR", "vf", "vg" ,"vk", "vp", "vR", "vt" ,"vz", "vZ", "zg" ,"zR" ,"zs" ,"zZ")
for (i in 1:length(names)) {
  print(i)
  print(sorted_df_list_del[[i]][audiogramme.mediane(sorted_df_list_del[[i]]),])
}
df_res=magic_result_as_dataframe()
df_res$`sorted_df_list_del[[i]][audiogramme.mediane(sorted_df_list_del[[i]]),`
df_res_erreur=matrix(df_res[,4],nrow=12)
df_res_erreur=t(df_res_erreur)
df_res_erreur=as.data.frame(df_res_erreur)
df_res_erreur <- cbind(df_res_erreur, erreur = magic_res$my_vector)
colnames(df_res_erreur)[which(colnames(df_res_erreur) %in%
                                 c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
colnames(df_res_erreur)
write.csv(df_res_erreur,"/Users/Aynaz/Documents/aynaz/Mots/df_res_erreur.csv", row.names = FALSE)
###############
df_res_erreur <- read.csv("df_res_erreur.csv", header=T, na.strings=c("","NA"))
bd=df_res_erreur %>%filter(erreur == 'bd')
##############################################################################
#############################################################################
df_trait1=df_trait[(df_trait$erreur_phonetique %in% c("-anterieur1", "-arriere1" ,   "-continu1" ,  "-coronal1" ,     "-haut1"
,  "-nasal1"  ,    "-sonant1" ,  "-voise1"  ,   "+anterieur1"  , "+arriere1"
,  "+continu1" ,   "+coronal1" ,   "+haut1"   ,    "+nasal1"  ,  "+sonant1"
,  "+voise1")), ]
trait_df_list_del=df_trait1 %>% group_split(erreur_phonetique,.keep = FALSE)
trait_df_list=df_trait1 %>% group_split(erreur_phonetique)
trait_df_list_del=lapply(trait_df_list_del,data.matrix)
magic_for(print, silent = TRUE)
for(i in 1:length(trait_df_list)){
  my_vector<-trait_df_list[[i]]$erreur_phonetique[1]
  print(my_vector)
}
magic_res_1=magic_result_as_dataframe()
write.csv(magic_res_1,"/Users/Aynaz/Documents/aynaz/Mots/magic_res_1.csv", row.names = FALSE)
magic_res_1 <- read.csv("magic_res_1.csv", header=T, na.strings=c("","NA"))

magic_res_1
names(trait_df_list)<-magic_res_1$my_vector
names(trait_df_list_del)<-magic_res_1$my_vector
###
bd=df_trait%>%filter(erreur_phonetique == '+voise1')
bd=bd[,c(1:12)]
bd=data.matrix(bd)
DATA=bd
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
audio_med=audiogramme.mediane(DATA)
audio_med
matplot(t(DATA), type = "l")
n <- nrow(DATA)
n
matplot(t(DATA),
        type='l', lty=1, col = rainbow(n),
        xlab="frequence", ylab="seuil_audiometrique")
matplot(t(DATA),
        type='l', lty=1, col = "light grey",
        xlab="frequence", ylab="seuil_audiometrique")
matlines(DATA[audiogramme.mediane(DATA),],
         type='l', lty=1, col = rainbow(n))

DATA[audiogramme.mediane(DATA),]
#trait_df_list_del[["bd"]]
names=c("-anterieur1", "-arriere1" ,   "-continu1" ,  "-coronal1" ,     "-haut1"
        ,  "-nasal1"  ,    "-sonant1" ,  "-voise1"  ,   "+anterieur1"  , "+arriere1"
        ,  "+continu1" ,   "+coronal1" ,   "+haut1"   ,    "+nasal1"  ,  "+sonant1"
        ,  "+voise1")
for (i in 1:length(names)) {
  print(trait_df_list_del[[i]][audiogramme.mediane(trait_df_list_del[[i]]),])
}
df_res_trait=magic_result_as_dataframe()
df_res_trait$`trait_df_list_del[[i]][audiogramme.mediane(trait_df_list_del[[i]]),`
df_res_trait=matrix(df_res_trait[,3],nrow=12)
df_res_trait=t(df_res_trait)
df_res_trait=as.data.frame(df_res_trait)
df_res_trait <- cbind(df_res_trait, erreur = magic_res_1$my_vector)
colnames(df_res_trait)[which(colnames(df_res_trait) %in%
                                c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
colnames(df_res_trait)
write.csv(df_res_trait,"/Users/Aynaz/Documents/aynaz/Mots/df_res_trait.csv", row.names = FALSE)
#############################################################################
sorted_df_S=sorted_df[startsWith(as.character(sorted_df$mot), 'S'),]
sorted_df_S=errors[startsWith(as.character(errors), 't'),]
errors<-unique(substring(sorted_df$mot, 1))
myvalues <- c('b','d','f','g','k','l','m','n','p','R','t','v','z','S')

for (i in 1:length(myvalues)) {
  #print(i)
  assign(paste0("sorted_df_",i), sorted_df[startsWith(as.factor(sorted_df$mot), i),])
}
##############################################################################
###histogram
colNames <- names(sorted_df)[2:12]
p <- list()
for(i in colNames){
  p[[i]] <- ggplot(data=sorted_df, aes_string(x=i, y = mot)) +
    geom_histogram(binwidth=0.2, color="black", aes(fill=mot))+
    geom_bar(aes_string(x=class.names[i],fill=var.names[j]))
  do.call(grid.arrange,p)
}
my_names = c("group1", "group2")
colNames <- c("X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000")
my_plot_list <- list()
new_list = lapply(result_list, "[",  my_names)
for(i in new_list){
  print(i)
  dfMod[[i]] <- subset(sorted_df, mot %in% c(i))
  dfMod[[i]]$mot <- droplevels(dfMod[[i]]$mot)
  print(dfMod[[i]])
  for(j in 1:length(colNames)){
    p <- boxplot(colNames[j]~ dfMod[[i]]$mot,data=dfMod[[i]])
    my_plot_list[[j]] <- p
    do.call(grid.arrange,my_plot_list)
  }}
data=sorted_df
result_list <- list()
for (i in seq(1:12)) {
  a <- colnames(data)
  dtest <- dunn_test(data, as.formula(paste(a[i], a[13], sep="~")),p.adjust.method="bonferroni")%>%
    filter(substr(group1, 1, 1) == substr(group2, 1, 1)) %>%
    filter(str_detect(group1, "(.)\\1"))
  result_list[[i]] <- dtest
}
####
colNames <- c("X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000")
dfMod <- subset(sorted_df, mot %in% c(x1, x2))
dfMod$mot <- droplevels(dfMod$mot)
my_plot_list <- list()
for(i in 1:length(colNames)){
  p <- boxplot(colNames[i]~ mot,data=dfMod)
  my_plot_list[[i]] <- p
  do.call(grid.arrange,my_plot_list)
}
####
library(car)
lapply(1:12, function(x) leveneTest(sorted_df[,x] ~ sorted_df[,13]))
lapply(1:12, function(x) kruskal.test(sorted_df[,x], sorted_df[,13]))
leveneTest(weight ~ group)
res.kruskal <- sorted_df %>% kruskal_test(X125 ~ mot)
res.kruskal
pwc <- sorted_df %>%
  dunn_test(X125 ~ mot, p.adjust.method = "bonferroni")%>%
  filter(substr(group1, 1, 1) == substr(group2, 1, 1)) %>%
  filter(str_detect(group1, "(.)\\1"))
x1=result$group1
x2=result$group2
dfMod <- subset(sorted_df, mot %in% c(x1, x2))
dfMod$mot <- droplevels(dfMod$mot)
boxplot(X125 ~ mot,data=dfMod)
####################
library(ROSE)
sorted_df_imb <- read.csv("sorted_df_imb.csv", header=T, na.strings=c("","NA"))
str(sorted_df_imb)
sorted_df_imb<-sorted_df_imb %>%
  mutate_if(is.integer,as.numeric)
sorted_df_imb <- sorted_df_imb %>%
  convert_as_factor( mot)
str(sorted_df_imb)
####
ex_tp=subset (sorted_df_imb,mot %in% c("tp", "tt"))
tp <- ovun.sample(mot ~ ., data = ex_tp, method = "both", p=0.5,N=100, seed = 1)$data

ex_tR=subset (sorted_df_imb,mot %in% c("tR", "tt"))
tR <- ovun.sample(mot ~ ., data = ex_tR, method = "both", p=0.5,N=100, seed = 1)$data

ex_bf=subset (sorted_df_imb,mot %in% c("bf", "bb"))
bf <- ovun.sample(mot ~ ., data = ex_bf, method = "both", p=0.5,N=100, seed = 1)$data

ex_bp=subset (sorted_df_imb,mot %in% c("bp", "bb"))
bp <- ovun.sample(mot ~ ., data = ex_bp, method = "both", p=0.5,N=100, seed = 1)$data

ex_bv=subset (sorted_df_imb,mot %in% c("bv", "bb"))
bv <- ovun.sample(mot ~ ., data = ex_bv, method = "both", p=0.5,N=100, seed = 1)$data

ex_df=subset (sorted_df_imb,mot %in% c("df", "dd"))
df <- ovun.sample(mot ~ ., data = ex_df, method = "both", p=0.5,N=100, seed = 1)$data

ex_dg=subset (sorted_df_imb,mot %in% c("dg", "dd"))
dg <- ovun.sample(mot ~ ., data = ex_dg, method = "both", p=0.5,N=100, seed = 1)$data

ex_dk=subset (sorted_df_imb,mot %in% c("dk", "dd"))
dk <- ovun.sample(mot ~ ., data = ex_dk, method = "both", p=0.5,N=100, seed = 1)$data

ex_dp=subset (sorted_df_imb,mot %in% c("dp", "dd"))
dp <- ovun.sample(mot ~ ., data = ex_dp, method = "both", p=0.5,N=100, seed = 1)$data

ex_dt=subset (sorted_df_imb,mot %in% c("dt", "dd"))
dt <- ovun.sample(mot ~ ., data = ex_dt, method = "both", p=0.5,N=100, seed = 1)$data

ex_fk=subset (sorted_df_imb,mot %in% c("fk", "ff"))
fk <- ovun.sample(mot ~ ., data = ex_fk, method = "both", p=0.5,N=100, seed = 1)$data

ex_fp=subset (sorted_df_imb,mot %in% c("fp", "ff"))
fp <- ovun.sample(mot ~ ., data = ex_fp, method = "both", p=0.5,N=100, seed = 1)$data

ex_fR=subset (sorted_df_imb,mot %in% c("fR", "ff"))
fR <- ovun.sample(mot ~ ., data = ex_fR, method = "both", p=0.5,N=100, seed = 1)$data

ex_kp=subset (sorted_df_imb,mot %in% c("kp", "kk"))
kp <- ovun.sample(mot ~ ., data = ex_kp, method = "both", p=0.5,N=100, seed = 1)$data

ex_kt=subset (sorted_df_imb,mot %in% c("kt", "kk"))
kt <- ovun.sample(mot ~ ., data = ex_kt, method = "both", p=0.5,N=100, seed = 1)$data

ex_mb=subset (sorted_df_imb,mot %in% c("mb", "mm"))
mb <- ovun.sample(mot ~ ., data = ex_mb, method = "both", p=0.5,N=100, seed = 1)$data

ex_ml=subset (sorted_df_imb,mot %in% c("ml", "mm"))
ml <- ovun.sample(mot ~ ., data = ex_ml, method = "both", p=0.5,N=100, seed = 1)$data

ex_mn=subset (sorted_df_imb,mot %in% c("mn", "mm"))
mn <- ovun.sample(mot ~ ., data = ex_mn, method = "both", p=0.5,N=100, seed = 1)$data
####
ex_nb=subset (sorted_df_imb,mot %in% c("nb", "nn"))
nb <- ovun.sample(mot ~ ., data = ex_nb, method = "both", p=0.5,N=100, seed = 1)$data

ex_nm=subset (sorted_df_imb,mot %in% c("nm", "nn"))
nm <- ovun.sample(mot ~ ., data = ex_nm, method = "both", p=0.5,N=100, seed = 1)$data

ex_Rf=subset (sorted_df_imb,mot %in% c("Rf", "RR"))
Rf <- ovun.sample(mot ~ ., data = ex_Rf, method = "both", p=0.5,N=100, seed = 1)$data

ex_RS=subset (sorted_df_imb,mot %in% c("RS", "RR"))
RS <- ovun.sample(mot ~ ., data = ex_RS, method = "both", p=0.5,N=100, seed = 1)$data

ex_Rs=subset (sorted_df_imb,mot %in% c("Rs", "RR"))
Rs <- ovun.sample(mot ~ ., data = ex_Rs, method = "both", p=0.5,N=100, seed = 1)$data

ex_sf=subset (sorted_df_imb,mot %in% c("sf", "ss"))
sf <- ovun.sample(mot ~ ., data = ex_sf, method = "both", p=0.5,N=100, seed = 1)$data

ex_sp=subset (sorted_df_imb,mot %in% c("sp", "ss"))
sp <- ovun.sample(mot ~ ., data = ex_sp, method = "both", p=0.5,N=100, seed = 1)$data

ex_vf=subset (sorted_df_imb,mot %in% c("vf", "vv"))
vf <- ovun.sample(mot ~ ., data = ex_vf, method = "both", p=0.5,N=100, seed = 1)$data

vg=subset (sorted_df_imb,mot %in% c("vg", "vv"))
#vg <- ovun.sample(mot ~ ., data = ex_vg, method = "both", p=0.5,N=100, seed = 1)$data

ex_vk=subset (sorted_df_imb,mot %in% c("vk", "vv"))
vk <- ovun.sample(mot ~ ., data = ex_vk, method = "both", p=0.5,N=100, seed = 1)$data

ex_vp=subset (sorted_df_imb,mot %in% c("vp", "vv"))
vp <- ovun.sample(mot ~ ., data = ex_vp, method = "both", p=0.5,N=100, seed = 1)$data

ex_vR=subset (sorted_df_imb,mot %in% c("vR", "vv"))
vR <- ovun.sample(mot ~ ., data = ex_vR, method = "both", p=0.5,N=100, seed = 1)$data

ex_vt=subset (sorted_df_imb,mot %in% c("vt", "vv"))
vt <- ovun.sample(mot ~ ., data = ex_vt, method = "both", p=0.5,N=100, seed = 1)$data

ex_vZ=subset (sorted_df_imb,mot %in% c("vZ", "vv"))
vZ <- ovun.sample(mot ~ ., data = ex_vZ, method = "both", p=0.5,N=100, seed = 1)$data

ex_zg=subset (sorted_df_imb,mot %in% c("zg", "zz"))
zg <- ovun.sample(mot ~ ., data = ex_zg, method = "both", p=0.5,N=100, seed = 1)$data
table(vf$mot)
balanced=list("bf"=bf,  "bp" =bp, "bv" =bv,"df"=df,  "dg"=dg , "dk"=dk , "dp"=dp,  "dt"=dt, "fk"=fk,  "fp"=fk,  "fR"=fR,  "kp"=kp,  "kt"=kt,   "mb"=mb,  "ml"=ml,    "mn"=mn,  "nb"=nb,  "nm"=nm,    "Rf"=Rf,    "Rs"=Rs,  "RS"=Rs,  "sf"=sf,  "sp"=sp, "tp"=tp, "tR"=tR,   "vf"=vf,  "vg"=vg,  "vk"=vk,  "vp"=vp,  "vR"=vR,  "vt"=vt,    "vZ"=vZ,  "zg"=zg  )
for( i in seq_along(balanced)){
  balanced[[i]]$erreur <- as.factor(as.numeric(factor(balanced[[i]]$mot)))
}
####################################################################
sorted_df$label=as.integer(as.factor(sorted_df$mot))
sorted_df_label<- read.csv("sorted_df_label.csv", header=T, na.strings=c("","NA"))
sorted_df_label<-sorted_df_label %>%
  mutate_if(is.integer,as.numeric)
sorted_df_label <- sorted_df_label %>%
  convert_as_factor( mot)
sorted_df_label=sorted_df_label[,-1]
colnames(sorted_df_label)[which(colnames(sorted_df_label) %in%
                                 c("X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
colnames(sorted_df_label)
sorted_df_label=arrange(sorted_df_label,erreur)
seuil=unname(as.matrix(sorted_df_label[,-c(13,14)]))
row.names(seuil) = c(1:10050)
colnames(seuil)=c(1:12)
dim(seuil) #10050  12
frequence<- c(125,250,500,750,1000,1500,2000,3000,4000,6000,8000,10000) #tt
rangfreq<-c(125,10000)
main = c("audiogram curves")
xlab = c("frequence")
ylab = c("seuil")
names=list(main='audiogram curves',xlab='frequence',ylab='seuil')
A=list(list1=seuil,list2=frequence,list3=rangfreq,list4=names)
datacarlw=fdata(seuil, argvals = frequence, rangeval = rangfreq, names = names, fdata2d = TRUE)
fdataobj=datacarlw
fac0=as.factor(sorted_df_label$erreur)
ldata=list("df"=data.frame(fac0),"fdataobj"=fdataobj)
####
fdata_fac <- ldata$fdataobj
mdist <- metric.lp(ldata$fdataobj)
###
####
a0<-func.mean.formula(fdataobj~fac0,data=ldata)
plot(a0)
mean_func_audio=a0$data
unique(sorted_df_label$mot)
row.names(mean_func_audio)=c("RR", "RS", "Rf", "Rs", "SS", "Sf", "Ss", "bR", "bb", "bd", "bf", "bg", "bp", "bv", "bz", "dR", "db", "dd", "df", "dg", "dk", "dp", "dt", "dv", "dz", "fR", "ff", "fk", "fp", "fs", "gg", "gk", "kk", "kp", "kt", "ll", "lm", "ln", "mb", "ml", "mm", "mn", "mv", "mz", "nb",
"nd", "ng", "nm", "nn", "pR", "pp", "sR", "sf", "sk", "sp", "ss", "tR", "tl", "tp", "tt", "vR", "vZ", "vf", "vg", "vk", "vp", "vt", "vv", "vz", "zR", "zZ", "zg", "zs", "zz")
colnames(mean_func_audio)= c(125,250,500,750,1000,1500,2000,3000,4000,6000,8000,10000)
levels(ldata$df)
ldata_list<-NULL;
for (i in seq(1:74)) {
    fac=format(func.mean(ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
    ldata_list <- rbind(ldata_list, fac)
}
ldata_list1<-NULL;
for (i in seq(0:73)) {
  fac=format(func.trim.FM (ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list1 <- rbind(ldata_list1, fac)
}
ldata_list2<-NULL;
for (i in seq(0:73)) {
  fac=format(func.trim.mode(ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list2 <- rbind(ldata_list2, fac)
}
ldata_list3<-NULL;
for (i in seq(0:73)) {
  fac=format(func.trim.RP (ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list3 <- rbind(ldata_list3, fac)
}
ldata_list4<-NULL;
for (i in seq(0:73)) {
  fac=format(func.trim.RT(ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list4 <- rbind(ldata_list4, fac)
}
ldata_list5<-NULL;
for (i in seq(0:73)) {
  fac=format(func.trim.RPD (ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list5 <- rbind(ldata_list5, fac)
}
ldata_list6<-NULL;
for (i in seq(0:73)) {
  fac=format(func.med.FM(ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list6 <- rbind(ldata_list6, fac)
}
ldata_list7<-NULL;
for (i in seq(0:73)) {
  fac=format(func.med.mode (ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list7 <- rbind(ldata_list7, fac)
}
ldata_list8<-NULL;
for (i in seq(0:73)) {
  fac=format(func.med.RP(ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list8 <- rbind(ldata_list8, fac)
}
ldata_list9<-NULL;
for (i in seq(0:73)) {
  fac=format(func.med.RPD (ldata$fdataobj[ldata$df == i],deriv=c(0,1))$data[,1:12],digits=0)
  ldata_list9 <- rbind(ldata_list9, fac)
}
ldata_list10<-NULL;
for (i in seq(0:73)) {
  fac=format(func.med.RT(ldata$fdataobj[ldata$df == i])$data[,1:12],digits=0)
  ldata_list10 <- rbind(ldata_list10, fac)
}

fac <- ldata$fdataobj[ldata$df == 73]
plot(a6)
a1<-func.mean(fac)
a2<-func.trim.FM(fac)
a3<-func.trim.mode(fac)
a4<-func.trim.RP(fac)
a5<-func.trim.RPD(fac,deriv=c(0,1))
a6<-func.med.FM(fac)
a7<-func.med.mode(fac)
a8<-func.med.RP(fac)
a9<-func.med.RPD(fac,deriv=c(0,1))
a10<-func.med.RT(fac)
par(mfrow=c(1,2))
plot(c(a1,a2,a3,a4),main="Sans Smote: trimmed mean")
plot(c(a1,a6,a7,a8),main="Central tendency: median")
plot(a1)
####
mlearn<-ldata[["fdataobj"]]
glearn<-ldata[["df"]]
a1<-classif.depth(glearn,mlearn,depth="RP")
table(a1$group.est,glearn)
###
ldata1=list("df"=data.frame(glearn=ldata$df),"x"=ldata$fdataobj)
library(e1071)
# require e1071 package
res.svm=classif.svm(glearn~x,data=ldata1)
# require nnet package
library(nnet)
res.nnet=classif.nnet(glearn~x,data=ldata1,trace=FALSE)
# require rpart package
library(rpart)
res.rpart=classif.rpart(glearn~x,data=ldata1)
round(mean(res.svm$prob.classification),3)
####


df_filtered<-sorted_df_label %>% filter(!str_detect(mot, "(.)\\1"))
m=combn(unique(df_filtered$mot), m = 1)
m2 <- as.data.frame.matrix(m)
df_final <- lapply(m2, function(col){
  df_filtered %>% filter(mot %in% col)
})
#
for(i in df_final){print(i)}
lapply(df_final, function(x) plot(x$y2, x$mot))

#
str(df_filtered_ml)
df_filtered_ml<-df_filtered_ml %>%
  mutate_if(is.integer,as.numeric)
df_filtered_ml <- df_filtered_ml %>%
  convert_as_factor(mot)
colnames(df_filtered_ml)[which(colnames(df_filtered_ml) %in%
                                 c("X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
colnames(df_filtered_ml)
y=data.matrix(t(df_filtered_ml[1:12]))
typeof(y)
ml=fds(x = 1:12, y = y, xname = "seuil", yname = "intensite")
##########
########
my_names =c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
y=list()
erreur=list()
for (i in 1:length(df_final)) {
  df_final[[i]]<-df_final[[i]] %>%
    mutate_if(is.integer,as.numeric)
  df_final[[i]] <- df_final[[i]] %>%
    convert_as_factor(mot)
  colnames(df_final[[i]])[which(colnames(df_final[[i]]) %in%
  c("X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
    y[[i]]=data.matrix(t(df_final[[i]][, my_names]))
    erreur[[i]]=fds(x = 1:12, y = y[[i]], xname = "seuil", yname = "intensite")
}
for(i in 1:length(erreur)){
  #p <- plot(erreur[[i]])
  ggarrange(plot(erreur[[i]]))
}
ggarrange(bxp, dp, bp + rremove("x.text"),
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
plotlist <- list()
for(i in 1:length(erreur)){
  p <- plot(erreur[[i]])
  plotlist[[i]]=p
}
p <- grid.arrange(grobs=plotlist)

str(df_filtered_ml)
df_filtered_ml<-df_filtered_ml %>%
  mutate_if(is.integer,as.numeric)
df_filtered_ml <- df_filtered_ml %>%
  convert_as_factor(mot)
colnames(df_filtered_ml)[which(colnames(df_filtered_ml) %in%
                                 c("X125","X250","X500","X750","X1000","X1500","X2000","X3000","X4000","X6000","X8000","X10000") )] <- c("125","250","500","750","1000","1500","2000","3000","4000","6000","8000","10000")
colnames(df_filtered_ml)
y=data.matrix(t(df_filtered_ml[1:12]))
typeof(y)
ml=fds(x = 1:12, y = y, xname = "seuil", yname = "intensite")
#########################
res.famd <- FAMD(sorted_df12, graph = FALSE)
fviz_screeplot(res.famd)
# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_famd_var(res.famd, "quali.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_famd_ind(res.famd, col.ind = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
###############principal component analysis-PCA(ggbiplot) /erreur
ind <- createDataPartition(sorted_df$mot,p=0.80,list = F)
train1 <- sorted_df[ind,]
dim(train1)
test1 <- sorted_df[-ind,]
pc1 <- prcomp(train1[,-13],center = T,scale. = T)
pc1
plot(pc1,type="lines")
pairs.panels(pc1$x,gap=0,pch = 21)
#Biplot for Principal Components using ggplot2
g1<-ggbiplot(pc1, obs.scale = 1, var.scale = 1,
             groups = train1$mot, ellipse = TRUE, circle = TRUE,ellipse.prob = 0.68) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
ggplotly(g1)
#######
selected<-c('fR','kp')
sorted_df_subset=sorted_df[sorted_df$mot %in% selected,]
tmp1=as.matrix(sorted_df_subset)
y = factor(tmp1[,13])      #membership vector
x = as.matrix(tmp1[,1:12])
mode(x)="numeric"
dim(x)
table(y)
col=as.numeric(y)
matplot(1:12,t(x[1:20,]),col=col[1:20],type="l",
        xlab="Frequency",ylab="Log-periodogram")
legend("topright",legend=levels(y),lty=1,col=1:2)
pca=princomp(x[1:300,],cor=F)
plot(pca$score[,1:2],col=col[1:500],xlab="PC1 (24.9%)",ylab="PC2 (18.0%)",pch=col[1:5000],cex=0.7)
legend("topleft",legend=levels(y),pch=1:5,col=1:5)
###
install.packages("mda")
library(mda)
N=nrow(x) #1523

#use small training size as an illustration
data=as.data.frame(x); data$Y=y #full dataset
n=300
set.seed(1)
id=sample(1:N,size=n,replace=F)

pda.fit <- mda(Y~.,data=data[id,], method=gen.ridge)
pda.prd <- predict(pda.fit,data[-id,],type="class")
#Confusion table
table(pda.prd,data$Y[-id])
1-sum(pda.prd==data$Y[-id])/length(data$Y[-id])
#Visualize the first two penalized canonical variates
pda.var=predict(pda.fit,data[id,],type="variates",dimension=2)
plot(pda.var,xlab="Dim 1",ylab="Dim 2",col=col[id],pch=col[id],cex=0.7)
legend("bottomright",legend=levels(y),pch=1:5,col=1:5,cex=1.5)
###
fda.fit <- fda(Y~.,data=data[id,], method=mars)
fda.prd <- predict(fda.fit,data[-id,],type="class")
#Confusion table
table(fda.prd,data$Y[-id])
1-sum(fda.prd==data$Y[-id])/length(data$Y[-id])
plot(fda.fit,pcex=1)
legend("bottomright",legend=levels(y),pch=c("1","2","3","4","5"),cex=1.5)
###
fda.fit2 <- fda(Y~.,data=data[id,], method=bruto)
fda.prd2 <- predict(fda.fit2,data[-id,],type="class")
#Confusion table
table(fda.prd2,data$Y[-id])
#Misclassification rate
1-sum(fda.prd2==data$Y[-id])/length(data$Y[-id])
plot(fda.fit2,pcex=1)
legend("topleft",legend=levels(y),pch=c("1","2","3","4","5"),cex=1.5)
###
library(MASS)
lda.fit <- lda(x[id,],y[id])
lda.prd <- predict(lda.fit,x[-id,])
#Confusion table
table(lda.prd$class,y[-id])
#Misclassification rate
1-sum(lda.prd$class==y[-id])/length(y[-id])
lda.var <- predict(lda.fit,dimen=2)$x
plot(lda.var,xlab="Dim 1",ylab="Dim 2",col=col[id],pch=col[id],cex=0.7)
legend("topleft",legend=levels(y),pch=1:5,col=1:5,cex=1.5)

####corpus clustering
install.packages("dendextend")
install.packages("circlize")
library(dendextend)
library(circlize)

# Distance matrix
d=dist.mat
# Hierarchical clustering dendrogram
hc <- as.dendrogram(hclust(d))

# Colors and line customization
hc <- hc %>%
  color_branches(k = 10) %>%
  set("branches_lwd", 2) %>%  # Line width
  set("branches_lty", 2) %>%  # Line type
  color_labels(k = 10)

# Line styling of the dendrogram
circlize_dendrogram(hc,
                    labels_track_height = NA,
                    dend_track_height = 0.5)
###
X=table(df_filtered$erreurs_phono,df_filtered$mot)
res.ca <- CA (X, graph = FALSE)
fviz_ca_row (res.ca, col.row = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_screeplot (res.ca) +
  geom_hline (yintercept = 33.33, linetype = 2, color = "red")
fviz_ca_biplot (res.ca, repel = TRUE)
fviz_ca_row(res.ca, repel = TRUE)

df_filtered <- read.csv("df_filtered.csv", header=T, na.strings=c("","NA"))
df_filtered <- df_filtered %>%
  convert_as_factor(mot,erreurs_phono)
df_filtered_clus=df_filtered[,18:19]
tab <- table(df_filtered_clus$erreurs_phono, df_filtered_clus$mot)
mat <- as.data.frame.matrix(tab)
head(mat[,1:5])
dist.mat <- dist(mat)
clusters = hclust(dist(mat))
plot(clusters, hang = -1)
canberra <- dist(mat, method="canberra")
canberra.ward = hclust(dist(canberra), method="ward.D2")
plot(canberra.ward, hang = -1, main="")
cluster.classes <- rect.hclust(canberra.ward,6)
########
bf1=balanced[["bf"]]
bf1=arrange(bf1,erreur)
seuil1=unname(as.matrix(bf1[,-c(13,14)]))
row.names(seuil1) = c(1:100)
colnames(seuil1)=c(1:12)
dim(seuil1)
frequence1<- c(125,250,500,750,1000,1500,2000,3000,4000,6000,8000,10000) #tt
rangfreq1<-c(125,10000)
main1 = c("audiogram curves")
xlab1 = c("frequence")
ylab1= c("seuil")
names1=list(main1='audiogram curves',xlab1='frequence',ylab1='seuil')
A1=list(list1=seuil1,list2=frequence1,list3=rangfreq1,list4=names1)
datacarlw1=fdata(seuil1, argvals = frequence1, rangeval = rangfreq1, names = names1, fdata2d = TRUE)
fdataobj1=datacarlw1
fac01=as.factor(bf1$erreur)
ldata1=list("df"=fac01,"fdataobj"=fdataobj1)

####
fac12 <- ldata1$fdataobj[ldata1$df == 2]
a1<-func.mean(fac12)
a2<-func.trim.FM(fac12)
a3<-func.trim.mode(fac12)
a4<-func.trim.RP(fac12)
a5<-func.trim.RPD(fac12,deriv=c(0,1))
a6<-func.med.FM(fac12)
a7<-func.med.mode(fac12)
a8<-func.med.RP(fac12)
a9<-func.med.RPD(fac12,deriv=c(0,1))
a10<-func.med.RT(fac)
par(mfrow=c(1,1))
plot(c(a1,a2,a3,a4),main="Smote: trimmed mean")
plot(c(a1,a6,a7,a8),main="Central tendency: median")
library(caTools)
sample <- sample.split(bf1$erreur, SplitRatio = 0.5)
train_data <- subset(bf1, sample == TRUE)
test_data <- subset(bf1, sample == FALSE)
####
data(phoneme)
mlearn<-phoneme$learn[c(1:50,101:150,201:250),]
# Unsupervised classification
out.fd1=kmeans.fd(mlearn,ncl=3,draw=TRUE)
out.fd2=kmeans.fd(mlearn,ncl=3,draw=TRUE,method="exact")
# Different Depth function
ind=c(17,77,126)
out.fd3=kmeans.fd(mlearn,ncl=mlearn[ind,],draw=FALSE,
                  dfunc=func.trim.FM,par.dfunc=list(trim=0.1))
out.fd4=kmeans.fd(mlearn,ncl=mlearn[ind,],draw=FALSE,
                  dfunc=func.med.FM)
group=c(rep(1,50),rep(2,50),rep(3,50))
table(out.fd4$cluster,group)
