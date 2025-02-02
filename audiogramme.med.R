setwd("/Users/Aynaz/Documents/aynaz/Mots")
getwd()
library(lubridate)
library(caTools)
library(gam)
library(tidyverse)
library(readr)
library(dplyr)
library(lattice)
library(psych)
library(corrplot)
library(ggplot2)
library(car)
library(FactoMineR)
library(factoextra)
library(caret)
library(devtools)
library(openxlsx)
library(reshape2)
library(plyr)
library(scales)
library(grid)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(plotly)
library(matrixStats)
library(lme4)
library(ggpubr)
library(rstatix)
attach(new_nbr)
##################import data
new_nbr<-read.csv("new_nbr.csv",check.names = FALSE)
result1<-read.csv("result1.csv",check.names = FALSE)
#sorted_df<-read.csv("sorted_df.csv",check.names = FALSE)
sorted_df <- read.csv("sorted_df.csv", header=T, na.strings=c("","NA"))
sorted_df=sorted_df[,-1]
result1=result1[,-1]
new_nbr$frequence=as.factor(new_nbr$frequence)
str(new_nbr)
new_nbr %>%
  select(-c(intensité,frequence)) %>% 
  mutate_if(is.integer,as.numeric) %>%
  str()
p <- read.csv("p.csv",check.names = FALSE)
df50 <- read.csv("df50.csv",check.names = FALSE)
df50=df50[,-1]
df50 <- df50[ -c(1,14:17) ]
df$mot=as.factor(df$mot)
df50_1$id_audiogramme=as.factor(df50_1$id_audiogramme)
df=filter(df50, mot %in% c("tp", "dt","bp","bv","fp","fR","nm","vp","vf",'sf',"mn","kp"))
describe(df)
str(df)
write.csv(df,"/Users/Aynaz/Documents/aynaz/Mots/MyData.csv", row.names = FALSE)
df50_1 <- read.csv("mydata1.csv",check.names = FALSE)
df50_1=df50_1[,-1]
#########################nonparametric friedman (repeated mesures~ les audiogrammes)
nonpartest( erreur_fR  ~ frequence, data = new_nbr, permreps = 1000)
new_df <- read.csv("new_df.csv",check.names = FALSE)
names(new_nbr)[1]<-paste("intensite")
df_0=filter(new_nbr, intensite %in% c(0))
new_df=new_df[,-1]
str(new_nbr)
new_df$erreur=as.factor(new_df$erreur)
new_df$frequence=as.numeric(new_df$frequence)
xyplot(new_df$intensité ~ new_df$frequence, groups = erreur, data = new_df,
       pch = 16, auto.key = TRUE)
colnames(result1)
##########################test statistics(pairwise test of kruskal wallis)
colnames(result1)
colnames(sorted_df)
result=sorted_df[,c(2:13,18)]
str(result)
result$mot=as.factor(result$mot)
names <- c(2:13)
my.variables <- colnames(result)
for (i in 1:length(my.variables)){
  if(my.variables[i] == 'mot'){
    next
  }else{
  pwc<- result %>% 
    dunn_test(i ~ mot, p.adjust.method = "bonferroni") %>%
    filter(substr(group1, 1, 1) == substr(group2, 1, 1))
  print(pwc[i])}}
###################outliers-normality test
df125$s125=as.numeric(df125$s125)
str(result1)
#df750 %>%
 # group_by(mot) %>%
  #identify_outliers(s750)
#df750 %>%
  #group_by(mot) %>%
  #shapiro_test(s500)
#df750=result1[,c(1,5,15)]
pwc_10000<- sorted_df[,c(1,13,18)] %>% 
  dunn_test(seuil_10000 ~ mot, p.adjust.method = "bonferroni")%>% 
  filter(substr(group1, 1, 1) == substr(group2, 1, 1)) 
signif10000<-pwc_10000[!pwc_10000$p.adj.signif == "ns", ]
#################
ggboxplot(result1, x = "mot", y = "s250", add = "jitter")
###########test anova (parametric)
result=result1
#result=result[result$Rf != 2 & result$x2 != "e",]
names <- c(2:13)
result[,names] <- lapply(result[,names] ,as.numeric)
str(result)
result$intensite=as.numeric(result$intensite)
result$frequence=as.numeric(result$frequence)

#result=result1[,1:14]
result <- result %>%
  gather(key = "frequence", value = "intensite", '125', '250','500',
         '750','1000','1500','2000','3000','4000','6000','8000','10000')
for (i in c('Rf',
            'Rs', 'bp', 'dg', 'dt', 'fR', 'fp', 'kp', 'mb', 'ml', 'mn', 'nb', 'nm',
            'sR', 'sf', 'tR', 'tp', 'vR', 'vf', 'vk', 'vp', 'vt', 'zg')){
  plot <-ggboxplot(
    result, x = "frequence", y = "intensite",color = i,
   short.panel.labs = FALSE
  )
  print(plot)}
bxp <- ggboxplot(
  result, x = "frequence", y = "intensite",
  color = "mot", short.panel.labs = FALSE
)
bxp
sh<-result %>%
  group_by(erreurs_phono, frequence,Rf,
           Rs, bp, dg, dt, fR, fp, kp, mb, ml, mn, nb, nm,
           sR, sf, tR, tp, vR, vf, vk, vp, vt, zg) %>%
  shapiro_test(intensite)
res.aov <- anova_test(
  data = result, dv = intensite, wid = id_audiogramme,
  within = c(erreurs_phono, frequence, Rf)
)
get_anova_table(res.aov)
########################Unsupervised Learning 
####################factor analysis of Mixed Data(qualitative,quantitative)
res.famd <- FAMD(sorted_df, graph = FALSE)
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
########################principal component analysis-PCA(ggbiplot) /trait
df50_1 <- df50_1[ -c(2:13) ]
df50_1 <- df50_1[,-1]
df50_1$erreurs_phono=as.factor(df50_1$erreurs_phono)
df50_1$mot=as.factor(df50_1$mot)
rownames(df50_1) <- df50_1$id_audiogramme
res.ca <- CA(df50_1, graph = FALSE)
#
ind1 <- createDataPartition(df50_1$erreurs_phono,p=0.80,list = F)
train2 <- df50_1[ind1,]
dim(train2)
test2 <- df50_1[-ind1,]
pc2 <- prcomp(train2[,-13],center = T,scale. = T)
pc2
plot(pc2,type="lines")
pairs.panels(pc2$x,gap=0,pch = 21)
#Biplot for Principal Components using ggplot2
g2<-ggbiplot(pc2, obs.scale = 1, var.scale = 1,
            groups = train2$erreurs_phono, ellipse = TRUE, circle = TRUE,ellipse.prob = 0.68) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
ggplotly(g2)
##############multinomial logistic regression 
training.samples <- result$mot %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- result[training.samples, ]
test.data <- result[-training.samples, ]
model <- nnet::multinom(mot ~., data = train.data)
# Summarize the model
summary(model)
# Make predictions
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test.data$mot)
prd1 <- predict(model,train.data)
confusionMatrix(prd1,train.data$mot)
#########multinomial logistic regression after pca
pred <- predict(pc,train)
train_1 <- data.frame(pred,train[13])
pred1 <- predict(pc,test)
test_1 <- data.frame(pred1,test[13])
library(nnet)
set.seed(100)
mymodel <- multinom(mot~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11,data = train_1)
summary(mymodel)
prd <- predict(mymodel,train_1)
confusionMatrix(prd,train_1$mot)
prt <- predict(mymodel,test_1)
confusionMatrix(prt,test_1$mot)
#################################functional non parametric unsupervised classification
############go and search all the files in one document on pc
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){
  read.csv(i, header=FALSE, skip=4)
})
df <- do.call(rbind.data.frame, All)
write.csv(df,"all_postcodes.csv", row.names=FALSE)
P <- lapply(All, function (y) {select(y, -c(V1,V15:V19))})
# calcul de la distance euclidienne entre tous les audiogrammes
################################################################
#"DATA" matrice contenant les audiogrammes
#(chaque ligne contient 1 audiogramme -->
#nb lignes = nb audiogrammes)
#
#Retourne le numéro de ligne correspondant à l'audiogramme médian
###############################trying to write a function
par(mfrow = c(6, 2))
for (j in c){
  for (DATA in P[[j]]){
    audiogramme.mediane = function(DATA)
  {
    DISTANCE2 = matrix(0, nrow(DATA), nrow(DATA))
    for(ii in 1:nrow(DATA)){
      DIFF = t(DATA) - DATA[ii, ]
      DISTANCE2[, ii] = apply(DIFF^2, 2, sum) / (ncol(DATA) - 1)
    }
    Sum.of.distance <- apply(sqrt(DISTANCE2), 1, sum)
    return(order(Sum.of.distance)[1])
    }}}
############################facetgrid with for loop
loop.vector <- 1:4

for (i in loop.vector) { # Loop over loop.vector
  
  # store data in column.i as x
  x <- examscores[,i]
  
  # Plot histogram of x
  hist(x,
       main = paste("Question", i),
       xlab = "Scores",
       xlim = c(0, 100))
}
##################centrality notions of functional variables
DATA
audiogramme.mediane = function(DATA)
{
  DISTANCE2 = matrix(0, nrow(DATA), nrow(DATA))
  for(ii in 1:nrow(DATA)){
    DIFF = t(DATA) - DATA[ii, ]
    DISTANCE2[, ii] = apply(DIFF^2, 2, sum) / (ncol(DATA) - 1)
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
######################generalized additive models
df_v=filter(df, mot %in% c("vp","vf"))
write.csv(df_v,"/Users/Aynaz/Documents/aynaz/Mots/df_v.csv", row.names = FALSE)
df_v <- read.csv("df_v.csv",check.names = FALSE)
df_v$mot=as.factor(df_v$mot)
gam1 <- gam(mot~s(seuil_125)+s(seuil_250)+s(seuil_500)
            +s(seuil_750)+s(seuil_1000)+s(seuil_1500)+s(seuil_2000)
            +s(seuil_3000)+s(seuil_4000)+s(seuil_6000)+s(seuil_8000)+s(seuil_10000),family=binomial, data = df_v)
summary(gam1)
plot(gam1, se=T)     
levels(df_v$mot)
