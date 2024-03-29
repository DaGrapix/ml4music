#Mini projet
#Anthony Kalaydjian - Mathieu Occhipinti

setwd(getwd())
rm(list=ls())
graphics.off()

library(ggplot2)
library(plyr)
library(stats)
library(tidyverse)
library(cowplot)
library(ROCR)
library(MASS)
library(glmnet)
library(corrplot)
library(caret)
library(class)
library(doParallel)


####################################
############# Partie 1 #############
####################################


############################    Q1

## importation des données
data <- read.csv("Music_2023.txt",sep=";",header=TRUE)
dim(data)
#Les données ont bien été importées

#Remarque: on garde une copie du dataframe original
data.0 <- read.csv("Music_2023.txt",sep=";",header=TRUE)



## Analyse uni et bi variée

my.plot = function(x, Y, xlab="", ylim=c()){
  Y.bin <- ifelse(Y=="Jazz", 1, 0)
  #plot(x, Y.bin, xlab=xlab, col=Y.bin+1, pch=Y.bin+1, ylab="Y", xlim=ylim);
  boxplot(x~Y, xlab=xlab, horizontal=TRUE, ylim=ylim)
}
par(mfcol=c(1,1))


my.plot(x=data.0$PAR_TC, Y=data.0$GENRE, xlab="PAR_TC")
my.plot(x=data.0$PAR_3RMS_TCD, Y=data.0$GENRE, xlab="PAR_3RMS_TCD")
my.plot(x=data.0$PAR_MFCCV15, Y=data.0$GENRE, xlab="PAR_MFCCV15")
my.plot(x=data.0$PAR_1RMS_TCD, Y=data.0$GENRE, xlab="PAR_1RMS_TCD")
my.plot(x=data.0$PAR_PEAK_RMS_TOT, Y=data.0$GENRE, xlab="PAR_PEAK_RMS_TOT")
my.plot(x=data.0$PAR_ZCD, Y=data.0$GENRE, xlab="PAR_ZCD")

par(mfrow=c(2,4))
my.plot(x=data.0$PAR_ASE1, Y=data.0$GENRE, xlab="PAR_ASE1", ylim=c(-0.25, -0.05))
my.plot(x=data.0$PAR_ASE5, Y=data.0$GENRE, xlab="PAR_ASE5", ylim=c(-0.25, -0.05))
my.plot(x=data.0$PAR_ASE10, Y=data.0$GENRE, xlab="PAR_ASE10", ylim=c(-0.25, -0.05))
my.plot(x=data.0$PAR_ASE15, Y=data.0$GENRE, xlab="PAR_ASE15", ylim=c(-0.25, -0.05))
my.plot(x=data.0$PAR_ASE20, Y=data.0$GENRE, xlab="PAR_ASE20", ylim=c(-0.25, -0.05))
my.plot(x=data.0$PAR_ASE25, Y=data.0$GENRE, xlab="PAR_ASE25", ylim=c(-0.25, -0.05))
my.plot(x=data.0$PAR_ASE30, Y=data.0$GENRE, xlab="PAR_ASE30", ylim=c(-0.25, -0.05))
#On observe que les valeurs prises par ASE augmentent en moyenne jusqu'à la 20ième variable
#puis diminuent

par(mfcol=c(1,1))

ASE.names <- paste("PAR_ASE", c(1:34), sep="")
ASE.indices <- which(names(data.0) %in% ASE.names)

corr.ASE <- cor(data[,ASE.indices])
corrplot(corr.ASE, title="Corr ASE")
#corrélations plus élevées au niveau de la diagonale


SFM.names <- paste("PAR_SFM", c(1:24), sep="")
SFM.indices <- which(names(data.0) %in% SFM.names)

corr.SFM <- cor(data[,SFM.indices])
corrplot(corr.SFM, title="Corr SFM")
#idem, avec même des anti-corrélations lorsque l'on s'en éloigne assez


ASE.SFM.mix.indices <- c(paste("PAR_SFM", c(1:5), sep=""), ASE.names <- paste("PAR_ASE", c(1:5), sep=""))
corr.ASE.SFM.mix <- cor(data[,ASE.SFM.mix.indices])
corrplot(corr.ASE.SFM.mix)
#matrice quasi-diagonale par blocs -> variables SFM et ASE décorrélées


## Proportion des genres musicaux
proportion.classique <- mean(ifelse(data$GENRE=="Classical", 1, 0))
proportion.jazz <- mean(ifelse(data$GENRE=="Jazz", 1, 0))

proportion.classique
#0.5310893
proportion.jazz
#0.4689107
#Les deux catégories sont relativement équivalentes en taille, c'est bien pour la classification.


#On sauvegarde les indices variables à retirer
#Ici on retirera les variables en double
indices.retires <- c(148:167)

## Analyse de densité des variables PAR_SC, PAR_SCV et PAR_ASC_V
density_plot <- function(X,xlab,lxlab){
  density <- ggplot(data.0,aes(x=X)) + geom_density(col="blue") + xlab(xlab)
  log_density <- ggplot(data.0, aes(x=log(X))) + geom_density(col="red") + xlab(lxlab)
  plot_grid(density, log_density, labels=c("Densité","Densité log"), label_size=12, ncol=1, label_x=0, label_y=0, hjust=-0.5, vjust=-0.5)
}

density_plot(data.0$PAR_SC, xlab="PAR_SC", lxlab="log(PAR_SC)")
density_plot(data.0$PAR_SC_V, xlab="PAR_SC_V", lxlab="log(PAR_SC_V)")
density_plot(data.0$PAR_ASC_V, xlab="PAR_ASC_V", lxlab="log(PAR_ASC_V)")

## Transformation log
data.0$PAR_SC_V <- log(data.0$PAR_SC_V)
data.0$PAR_ASC_V <- log(data.0$PAR_ASC_V)

density_plot(data.0$PAR_SC, xlab="PAR_SC", lxlab="log(PAR_SC)")
density_plot(data.0$PAR_SC_V, xlab="PAR_SC_V", lxlab="log(PAR_SC_V)")


## Variables très corrélées
#Remarque: on a ici déjà retiré les doublons
data <- data.0[, -indices.retires]
corr <- cor(x=data[, -ncol(data)])


corr.0 <- cor(x=data.0[, -ncol(data.0)])
#selection des indices de la matrice de correlation > 0.99
high.corr.index.new <- which(corr>0.99, arr.ind = TRUE) %>% unname

#selection des indices appartenant a la matrice triangulaire inferieure stricte,
#pour retirer les doublons, ainsi que les éléments diagonaux.
lower.tri <- lower.tri(corr, diag=FALSE)
#ne sélectionne que les lignes dont les indices sont dans le triangle inférieur
high.corr.index.new <- high.corr.index.new[which(lower.tri[high.corr.index.new]==TRUE),]
high.corr.index.new

#nom des couples de variables très corrélées
correlated.variables <- matrix(c(names(data)[high.corr.index.new[,1]], 
                                 names(data)[high.corr.index.new[,2]]),
                                 nrow=nrow(high.corr.index.new))
correlated.variables

#Calcul des indices de ces variables dans le dataframe d'origine
name.list <- as.vector(correlated.variables)
high.corr.index <- matrix(which(names(data.0) %in% name.list), nrow=nrow(high.corr.index.new))
high.corr.index

#indices des variables corrélées
indices.corr <- c(high.corr.index[,1], high.corr.index[,2])
indices.corr

#plot des variables corrélées
corrplot(cor(data.0[,indices.corr]))

#On retirera high.corr.index[,1]
indices.retires <- c(indices.retires, high.corr.index[,1])



## Cas des variables PAR_ASE_M, PAR_ASE_MV, PAR_SFM_M et PAR_SFM_MV

#Remarque, on réutilise le dataframe original ici
indices.ASE <- c(4:37)
indices.ASEV <- c(39:72)
indices.SFM <- c(78:101)
indices.SFMV <- c(103:126)

par(mfrow=c(2,2))

data.mean.ASE <- apply(data.0[,indices.ASE], MARGIN=1, FUN=mean)
plot(x=data.mean.ASE, y=data.0$PAR_ASE_M, xlab="mean(PAR_ASE)", ylab="PAR_ASE_M")

data.mean.ASEV <- apply(data.0[,indices.ASEV], MARGIN=1, FUN=mean)
plot(x=data.mean.ASEV, y=data.0$PAR_ASE_MV, xlab="mean(PAR_ASEV)", ylab="PAR_ASE_MV")

data.mean.SFM <-apply(data.0[,indices.SFM], MARGIN=1, FUN=mean)
plot(x=data.mean.SFM, y=data.0$PAR_SFM_M, xlab="mean(PAR_SFM)", ylab="PAR_SFM_M")

data.mean.SFMV <-apply(data.0[,indices.SFMV], MARGIN=1, FUN=mean)
plot(x=data.mean.SFMV, y=data.0$PAR_SFM_MV, xlab="mean(PAR_SFMV)", ylab="PAR_SFM_MV")
#On les 4 variables étudiées sont bien les moyennes des différentes observations
#auxquelles elles sont associées, elles n'apportent pas d'information, on les retire.

#Indice des variables dans le dataframe original.
indices.4 <- which(names(data.0) %in% c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))

#On obtient ainsi la liste finale de toutes les variables que l'on va retirer du dataframe.
indices.retires <- c(indices.retires, indices.4)
indices.retires

length(indices.retires)
#On retirera donc 27 variables au total.


############################    Q2

#Renommage de GENRE, et variable booléenne
names(data.0)[ncol(data.0)] = "y"
data.0$y <- ifelse(data.0$y=="Jazz", 1, 0)

## Echantillon d'apprentissage
set.seed(103)
train = sample(c(TRUE,FALSE), nrow(data.0), rep=TRUE, prob=c(2/3,1/3))

#On retirera les variables plus tard
data.train.0 <- data.0[which(train),]
data.test.0 <- data.0[which(train==FALSE),]

#On retire les 27 variables
data <- data.0[, -indices.retires]
data.train <- data.train.0[, -indices.retires]
data.test <- data.test.0[, -indices.retires]

dim(data)
dim(data.train)
dim(data.test)





############################    Q3

## Estimation de modèle

#Définition de Mod0
Mod0 <- glm(y~PAR_TC+PAR_SC+PAR_SC_V+PAR_ASE_M,PAR_ASE_MV+PAR_SFM_M+PAR_SFM_MV, family=binomial, data=data.train.0)
summary(Mod0)

#Définition de ModT
ModT <- glm(y~., family=binomial, data=data.train)
summary(ModT)


#On récupère les p-value des tests de significativité des coefficients de ModT
p_value <- coef(summary(ModT))[-1,4]


#On sélectionne les variables dont le coefficient a un niveau de significativité de 5% et on crée la formule de notre modèle Mod1
index.var.Mod1 <- which(p_value>0.05)
var.Mod1 <- names(data[index.var.Mod1])
formula.Mod1 <- as.formula(paste("y ~", paste(var.Mod1, collapse="+")))
Mod1<-glm(formula <- formula.Mod1, family=binomial, data=data.train)
summary(Mod1)

#On sélectionne les variables dont le coefficient a un niveau de significativité de 20% et on crée la formule de notre modèle Mod2
index.var.Mod2 <- which(p_value>0.2)
var.Mod2 <- names(data[index.var.Mod2])
formula.Mod2 <- as.formula(paste("y ~", paste(var.Mod2, collapse="+")))
Mod2 <- glm(formula=formula.Mod2, family=binomial, data=data.train)
summary(Mod2)


## Méthode de selection de variable AIC

#stepAIC
#Attention, execution très longue, vous pouvez load step_only.RData pour
#obtenir la variable step
step <- stepAIC(ModT)
#load("step_only.RData")

#Selection des variables eliminees
removed.variables <- gsub('- ', '', step$anova$Step[-1])
variable.names <- names(data)[-ncol(data)]

#Selection des variables à garder
keep.indices <- ifelse(variable.names %in% removed.variables, FALSE, TRUE) %>% which()
variables.AIC <- variable.names[keep.indices]

#Creation de la formule
formula.AIC <- as.formula(paste("y ~",paste(variables.AIC, collapse= "+")))
formula.AIC

#Modèle final obtenu
ModAIC <- glm(formula=formula.AIC, family=binomial, data=data.train)




############################    Q4

## ModT apprentissage
predproba_train_ModT=predict(ModT,type="response") # ModT est le résultat de glm
pred_train_ModT = prediction(predproba_train_ModT,data.train$y) 
AUC_train_ModT<-round(performance(pred_train_ModT, 'auc')@y.values[[1]],2) #AUC



## ModT test
predproba_test_ModT=predict(ModT,newdata=data.test,type="response")
pred_test_ModT=prediction(predproba_test_ModT,data.test$y)
AUC_test_ModT<-round(performance(pred_test_ModT, 'auc')@y.values[[1]],2) #AUC



## Mod0 test

predproba_test_Mod0=predict(Mod0,newdata=data.test.0,type="response")
pred_test_Mod0=prediction(predproba_test_Mod0,data.test.0$y)
AUC_Mod0<-round(performance(pred_test_Mod0, 'auc')@y.values[[1]],2) #AUC

## Mod1 test

predproba_test_Mod1=predict(Mod1,newdata=data.test,type="response")
pred_test_Mod1=prediction(predproba_test_Mod1,data.test$y)
AUC_Mod1<-round(performance(pred_test_Mod1, 'auc')@y.values[[1]],2) #AUC

## Mod2 test

predproba_test_Mod2=predict(Mod2,newdata=data.test,type="response")
pred_test_Mod2=prediction(predproba_test_Mod2,data.test$y)
AUC_Mod2<-round(performance(pred_test_Mod2, 'auc')@y.values[[1]],2) #AUC


## ModAIC test

predproba_test_ModAIC=predict(ModAIC,newdata=data.test,type="response")
pred_test_ModAIC=prediction(predproba_test_ModAIC,data.test$y)
AUC_ModAIC<-round(performance(pred_test_ModAIC, 'auc')@y.values[[1]],2) #AUC

## Définition de nos légendes
l_train_ModT<-paste("Apprentissage ModT, AUC=",AUC_train_ModT,sep=" ")
l_test_ModT<-paste("Test  ModT, AUC=",AUC_test_ModT,sep=" ")
l_Mod0<-paste("Test Mod0, AUC=",AUC_Mod0,sep=" ")
l_Mod1<-paste("Test Mod1, AUC=",AUC_Mod1,sep=" ")
l_Mod2<-paste("Test Mod2, AUC=",AUC_Mod2,sep=" ")
l_AIC<-paste("Test ModAIC, AUC=",AUC_ModAIC,sep=" ")

## Tracé de nos courbes ROC

par(mfrow=c(1,1))

plot(performance(pred_train_ModT,"sens","fpr"),xlab="",col="black",main="Courbes ROC de nos différents modèles") # ROC
plot(performance(pred_test_ModT,"sens","fpr"),xlab="",col="purple",add=TRUE)
plot(performance(pred_test_Mod0,"sens","fpr"),xlab="",col="blue",add=TRUE)
plot(performance(pred_test_Mod1,"sens","fpr"),xlab="",col="yellow",add=TRUE)
plot(performance(pred_test_Mod2,"sens","fpr"),xlab="",col="green",add=TRUE)
plot(performance(pred_test_ModAIC,"sens","fpr"),xlab="",col="orange",add=TRUE)
abline(a=0, b=1, col="#33FF66",lty=2)
abline(v=0,col="#003300",lty=2)
abline(h=1,col="#003300",lty=2)


legend("bottomright",legend=c(l_train_ModT,l_test_ModT,l_Mod0,l_Mod1,l_Mod2,l_AIC,"Règle aléatoire","Règle Parfaite"),lty=c(1,1,1,1,1,1,2,2),col=c("black","purple","blue","yellow","green","orange","#33FF66","#003300"))




############################    Q5

## Calcul des probabilités sur l'échantillon d'apprentissage

predproba_train_Mod0=predict(Mod0,type="response") 
predproba_train_Mod1=predict(Mod1,type="response") 
predproba_train_Mod2=predict(Mod2,type="response") 
predproba_train_ModAIC=predict(ModAIC,type="response") 

## On met à 1 si la probabilité est >= 0.5, 0 sinon.

## ModT

class_train_ModT=ifelse(predproba_train_ModT>=0.5,1,0)
class_test_ModT=ifelse(predproba_test_ModT>=0.5,1,0)

## Mod0

class_train_Mod0=ifelse(predproba_train_Mod0>=0.5,1,0)
class_test_Mod0=ifelse(predproba_test_Mod0>=0.5,1,0)

## Mod1

class_train_Mod1=ifelse(predproba_train_Mod1>=0.5,1,0)
class_test_Mod1=ifelse(predproba_test_Mod1>=0.5,1,0)

## Mod2

class_train_Mod2=ifelse(predproba_train_Mod2>=0.5,1,0)
class_test_Mod2=ifelse(predproba_test_Mod2>=0.5,1,0)

## ModAIC

class_train_ModAIC=ifelse(predproba_train_ModAIC>=0.5,1,0)
class_test_ModAIC=ifelse(predproba_test_ModAIC>=0.5,1,0)

## Calcul des erreurs

## ModT
err_train_ModT<-round(mean(class_train_ModT!=data.train$y),3)
err_test_ModT<-round(mean(class_test_ModT!=data.test$y),3)



## Mod0

err_train_Mod0<-round(mean(class_train_Mod0!=data.train.0$y),3)
err_test_Mod0<-round(mean(class_test_Mod0!=data.test.0$y),3)

## Mod1

err_train_Mod1<-round(mean(class_train_Mod1!=data.train$y),3)
err_test_Mod1<-round(mean(class_test_Mod1!=data.test$y),3)

## Mod2 

err_train_Mod2<-round(mean(class_train_Mod2!=data.train$y),3)
err_test_Mod2<-round(mean(class_test_Mod2!=data.test$y),3)


## ModAIC

err_train_ModAIC<-round(mean(class_train_ModAIC!=data.train$y),3)
err_test_ModAIC<-round(mean(class_test_ModAIC!=data.test$y),3)

err_train<-c(err_train_ModT,err_train_Mod0,err_train_Mod1,err_train_Mod2,err_train_ModAIC)
err_test<-c(err_test_ModT,err_test_Mod0,err_test_Mod1,err_test_Mod2,err_test_ModAIC)


## On crée un dataframe qui résume nos erreurs 

err<-data.frame(Apprentissage=err_train,Test=err_test)

rownames(err)<-c("ModT","Mod0","Mod1","Mod2","ModAIC")

print(err)












####################################
############# Partie 2 #############
####################################
rm(list=ls())

setwd(getwd())

library(ggplot2)
library(plyr)
library(stats)
library(tidyverse)
library(cowplot)
library(ROCR)
library(MASS)
library(glmnet)
library(corrplot)
library(caret)
library(class)
library(doParallel)

#Importation et nettoyage des donnees
prepare.data <- function(){
  data <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  data.0 <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  
  data.0$PAR_SC_V <- log(data.0$PAR_SC_V)
  data.0$PAR_ASC_V <- log(data.0$PAR_ASC_V)
  
  indices.retires <- c(148:167)
  data <- data.0[, -indices.retires]
  
  corr <- cor(x=data[, -ncol(data)])
  high.corr.index.new <- which(corr>0.99, arr.ind = TRUE) %>% unname
  lower.tri <- lower.tri(corr, diag=FALSE)
  high.corr.index.new <- high.corr.index.new[which(lower.tri[high.corr.index.new]==TRUE),]
  correlated.variables <- matrix(c(names(data)[high.corr.index.new[,1]], 
                                   names(data)[high.corr.index.new[,2]]),
                                 nrow=nrow(high.corr.index.new))
  name.list <- as.vector(correlated.variables)
  high.corr.index <- matrix(which(names(data.0) %in% name.list), nrow=nrow(high.corr.index.new))
  
  indices.retires <- c(indices.retires, high.corr.index[,1])
  
  indices.4 <- which(names(data.0) %in% c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))
  indices.retires <- c(indices.retires, indices.4)
  
  names(data.0)[ncol(data.0)] = "y"
  data.0$y <- ifelse(data.0$y=="Jazz", 1, 0)
  
  ## Echantillon d'apprentissage
  set.seed(103)
  train = sample(c(TRUE,FALSE), nrow(data.0), rep=TRUE, prob=c(2/3,1/3))
  
  #On retirera les variables plus tard
  data.train.0 <- data.0[which(train),]
  data.test.0 <- data.0[which(train==FALSE),]
  
  #On retire les 27 variables
  data <- data.0[, -indices.retires]
  data.train <- data.train.0[, -indices.retires]
  data.test <- data.test.0[, -indices.retires]
  
  return(list(data=data, data.train=data.train, data.test=data.test, data.0=data.0, data.train.0=data.train.0, data.test.0=data.test.0))
}

df <- prepare.data()
data <- df$data
data.train <- df$data.train
data.test <- df$data.test
data.0 <- df$data.0
data.train.0 <- df$data.train.0
data.test.0 <- df$data.test.0

############################    Q1

corr <- cor(x=data[,-ncol(data)])

#selection des indices de la matrice de correlation > threshold
threshold <- c(0.75, 0.9)
high.corr.index <- sapply(threshold, FUN=function(x) (nrow(which(corr > x, arr.ind = TRUE)) - (ncol(data)-1))/2)
high.corr.index

# Meme après avoir retiré les variables de la partie 1,
# 108 couples de variables ont un coefficient de correlation > 75%
# 21  couples de variables ont un coefficient de correlation > 90%


x.train <- data.train[,-ncol(data.train)] %>% as.matrix()
y.train <- data.train[,ncol(data.train)]

x.test <- data.test[,-ncol(data.test)] %>% as.matrix()
y.test <- data.test[,ncol(data.test)]



############################    Q2
par(mfrow=c(1,1))


grid <- 10^seq(10, -2, length=100)

ridge.fit <- glmnet(x=x.train, y=y.train, alpha=0, lambda=grid, family="binomial")

ridge.fit$dim
plot(ridge.fit)
plot(ridge.fit, xvar = "lambda")
#On remarque que 2 voire 3 coefficients ont un comportement different des autres et explosent




## determination des coefficients qui explosent
coef.ridge = coef(ridge.fit)[-1,] # enlève le coefficient d'intercept qui n'apporte rien

attained.max <- apply(coef.ridge, MARGIN=1, FUN=function(x) max(abs(x)))
max.theta.values <- round(attained.max, digits=2)

#coefficient maximal
max.coefs <- c(which.max(max.theta.values))

#deuxième coefficient maximal
new.coef <- which(names(data) == names(which.max(max.theta.values[-max.coefs])))
max.coefs <- c(max.coefs, new.coef)

#troisième coefficient maximal
new.coef <- which(names(data) == names(which.max(max.theta.values[-max.coefs])))
max.coefs <- c(max.coefs, new.coef)

names(max.coefs) <- names(data)[max.coefs]
max.coefs

#Les variables associées à ces trois coefficients sont :
# PAR_SFMV24 la 126ème variable du dataset.
# PAR_SFMV2 la 104ème variable du dataset.
# PAR_THR_3RMS_10FR_VAR la 176ème variable du dataset.


## Plot de l'evolution des coefficients, en ayant retire ceux qui explosent
matplot(apply(abs(coef.ridge[-max.coefs,]) ,2 ,sum), t(coef.ridge[-max.coefs,]),
        col=1:10, lty=1:10, type="l", xlab="norme L1", ylab="coefficients")




## Plot de l'evolution des coefficients en fonction du coefficient de penalite
plot(ridge.fit, xvar = "lambda")


############################    Q3
set.seed(314)

grid <- 10^seq(10, -2, length=100)
cv.out <- cv.glmnet(x=x.train, y=y.train, family="binomial", lambda=grid, nfolds=10)
bestlam=cv.out$lambda.min
plot(cv.out)

bestlam


#On voit que la SCR minimum est atteinte pour lambda=0.01 sur la frontière du domaine.
#Il serait judicieux de ré-effectuer la validation-croisée pour des nouvelles valeurs de lambda.
#Le minimum étant sûrement atteint avant 10^0, on choisira une grille de 10^0 à 10^-5$.


set.seed(314)
grid <- 10^seq(0, -4, length=100)

#execution un peu longue
cv.out <- cv.glmnet(x=x.train, y=y.train, family="binomial", lambda=grid, nfolds=10)

bestlam <- cv.out$lambda.min
plot(cv.out)

bestlam
#on trouve 0.0008497534 qui n'est pas sur la frontière, c'est le lambda optimal.

##modele final
ridge.model <- glmnet(x=x.train, y=y.train, family="binomial", lambda=bestlam)

## Erreur d'apprentissage
probabilities.train <- ridge.model %>% predict(newx=x.train)
predicted.classes.train <- ifelse(probabilities.train > 0.5, 1, 0)
err.train.ridge <- mean(predicted.classes.train != y.train)
err.train.ridge
#0.08570425


## Erreur de généralisation
probabilities.test <- ridge.model %>% predict(newx=x.test)
predicted.classes.test <- ifelse(probabilities.test > 0.5, 1, 0)
err.test.ridge <- mean(predicted.classes.test != y.test)
err.test.ridge
#0.09713487



############################    Q4

#On considere toutes les variables
x.train.0 <- data.train.0[,-ncol(data.train.0)] %>% as.matrix()
y.train.0 <- data.train.0[,ncol(data.train.0)]

x.test.0 <- data.test.0[,-ncol(data.test.0)] %>% as.matrix()
y.test.0 <- data.test.0[,ncol(data.test.0)]


set.seed(4658)
grid <- 10^seq(10, -2, length=100)
cv.out.0 <- cv.glmnet(x=x.train.0, y=y.train.0, family = "binomial", lambda=grid, nfolds=10)
bestlam.0 <- cv.out.0$lambda.min
plot(cv.out.0)

bestlam.0
#meme problème


#nouvelle grid
set.seed(4658)
grid <- 10^seq(0, -4, length=100)

#calcul long
cv.out.0 <- cv.glmnet(x=x.train.0, y=y.train.0, family = "binomial", lambda=grid, nfolds=10)
bestlam.0 <- cv.out.0$lambda.min
plot(cv.out.0)

bestlam.0
#on trouve lambda=0.0008497534


##modele final
ridge.model.0 <- glmnet(x=x.train.0, y=y.train.0, family="binomial", lambda=bestlam.0)

## Erreur d'apprentissage
probabilities.train.0 <- ridge.model.0 %>% predict(newx=x.train.0)
predicted.classes.train.0 <- ifelse(probabilities.train.0 > 0.5, 1, 0)
err.train.ridge.0 <- mean(predicted.classes.train.0 != y.train.0)
err.train.ridge.0
#0.08746048


## Erreur de généralisation
probabilities.test.0 <- ridge.model.0 %>% predict(newx=x.test.0)
predicted.classes.test.0 <- ifelse(probabilities.test.0 > 0.5, 1, 0)
err.test.ridge.0 <- mean(predicted.classes.test.0 != y.test.0)
err.test.ridge.0
#0.09573725










####################################
############# Partie 3 #############
####################################

rm(list=objects())

setwd(getwd())

library(ggplot2)
library(plyr)
library(stats)
library(tidyverse)
library(cowplot)
library(ROCR)
library(MASS)
library(glmnet)
library(corrplot)
library(caret)
library(class)
library(doParallel)

#Importation et nettoyage des donnees
prepare.data <- function(){
  data <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  data.0 <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  
  data.0$PAR_SC_V <- log(data.0$PAR_SC_V)
  data.0$PAR_ASC_V <- log(data.0$PAR_ASC_V)
  
  indices.retires <- c(148:167)
  data <- data.0[, -indices.retires]
  
  corr <- cor(x=data[, -ncol(data)])
  high.corr.index.new <- which(corr>0.99, arr.ind = TRUE) %>% unname
  lower.tri <- lower.tri(corr, diag=FALSE)
  high.corr.index.new <- high.corr.index.new[which(lower.tri[high.corr.index.new]==TRUE),]
  correlated.variables <- matrix(c(names(data)[high.corr.index.new[,1]], 
                                   names(data)[high.corr.index.new[,2]]),
                                 nrow=nrow(high.corr.index.new))
  name.list <- as.vector(correlated.variables)
  high.corr.index <- matrix(which(names(data.0) %in% name.list), nrow=nrow(high.corr.index.new))
  
  indices.retires <- c(indices.retires, high.corr.index[,1])
  
  indices.4 <- which(names(data.0) %in% c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))
  indices.retires <- c(indices.retires, indices.4)
  
  names(data.0)[ncol(data.0)] = "y"
  data.0$y <- ifelse(data.0$y=="Jazz", 1, 0)
  
  ## Echantillon d'apprentissage
  set.seed(103)
  train = sample(c(TRUE,FALSE), nrow(data.0), rep=TRUE, prob=c(2/3,1/3))
  
  #On retirera les variables plus tard
  data.train.0 <- data.0[which(train),]
  data.test.0 <- data.0[which(train==FALSE),]
  
  #On retire les 27 variables
  data <- data.0[, -indices.retires]
  data.train <- data.train.0[, -indices.retires]
  data.test <- data.test.0[, -indices.retires]
  print(indices.retires)
  return(list(data=data, data.train=data.train, data.test=data.test, data.0=data.0, data.train.0=data.train.0, data.test.0=data.test.0))
}

df <- prepare.data()
data <- df$data
data.train <- df$data.train
data.test <- df$data.test

### Dataset reduit

x.train <- data.train[,-ncol(data.train)] %>% as.matrix()
y.train <- data.train[,ncol(data.train)] %>% as.factor()

x.test <- data.test[,-ncol(data.test)] %>% as.matrix()
y.test <- data.test[,ncol(data.test)] %>% as.factor()

knn.ctrl <- trainControl(method="cv", number=10)

## modele k=1
k.1 <- expand.grid(k=1)
knn.1 <- train(x=x.train, y=y.train, method="knn", trControl=knn.ctrl, tuneGrid=k.1, preProcess=c("center", "scale"))

print(knn.1)

#Erreurs
knn.pred.train.1 <- predict(knn.1, newdata = x.train)
err.train.knn.1 <- mean(knn.pred.train.1 != y.train)
err.train.knn.1
#0

knn.pred.test.1 <- predict(knn.1, newdata = x.test)
err.test.knn.1 <- mean(knn.pred.test.1 != y.test)
err.test.knn.1
#0.0580014

## Validation croisée
set.seed(556)
k.grid <- expand.grid(k=1:30)

#Calculs paralleles
cl <- makePSOCKcluster(7)
registerDoParallel(cl)

#choix du modèle optimal
knn <- train(x=x.train, y=as.factor(y.train), method="knn", trControl=knn.ctrl, tuneGrid=k.grid, preProcess=c("center", "scale"))

stopCluster(cl)


print(knn)
#meilleur modèle obtenu pour k=1
plot(knn)

#Erreurs
knn.pred.train <- predict(knn, newdata = x.train)
err.train.knn <- mean(knn.pred.train != y.train)
err.train.knn
#0

knn.pred.test <- predict(knn,newdata = x.test)
err.test.knn <- mean(knn.pred.test != y.test)
err.test.knn
#0.0580014

#Arreter tout calcul parallele
unregister.dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister.dopar()

### Dataset complet
data.0 <- df$data.0
data.train.0 <- df$data.train.0
data.test.0 <- df$data.test.0

x.train.0 <- data.train.0[,-ncol(data.train.0)] %>% as.matrix()
y.train.0 <- data.train.0[,ncol(data.train.0)] %>% as.factor()

x.test.0 <- data.test.0[,-ncol(data.test.0)] %>% as.matrix()
y.test.0 <- data.test.0[,ncol(data.test.0)] %>% as.factor()

knn.ctrl <- trainControl(method="cv", number=10)


## modele k=1
k.1 <- expand.grid(k=1)
knn.1.0 <- train(x=x.train.0, y=y.train.0, method="knn", trControl=knn.ctrl, tuneGrid=k.1, preProcess=c("center", "scale"))

print(knn.1.0)

#Erreurs
knn.pred.train.1.0 <- predict(knn.1.0, newdata = x.train.0)
err.train.knn.1.0 <- mean(knn.pred.train.1.0 != y.train.0)
err.train.knn.1.0
#0

knn.pred.test.1.0 <- predict(knn.1.0, newdata = x.test.0)
err.test.knn.1.0 <- mean(knn.pred.test.1.0 != y.test.0)
err.test.knn.1.0
#0.05101328

## Validation croisée
set.seed(556)
k.grid <- expand.grid(k=1:30)

#Calculs paralleles
cl <- makePSOCKcluster(7)
registerDoParallel(cl)

#choix du modèle optimal
knn.0 <- train(x=x.train.0, y=y.train.0, method="knn", trControl=knn.ctrl, tuneGrid=k.grid, preProcess=c("center", "scale"))

stopCluster(cl)


print(knn.0)
#meilleur modèle obtenu pour k=1
plot(knn.0)

#Erreurs
knn.pred.train.0 <- predict(knn.0, newdata = x.train.0)
err.train.knn.0 <- mean(knn.pred.train.0 != y.train.0)
err.train.knn.0
#0

knn.pred.test.0 <- predict(knn.0,newdata = x.test.0)
err.test.knn.0 <- mean(knn.pred.test.0 != y.test.0)
err.test.knn.0
#0.05101328




####################################
########### Prédictions ############
####################################

rm(list=objects())

setwd(getwd())

library(glmnet)
library(tidyverse)

#Importation et nettoyage des donnees
prepare.data <- function(){
  data <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  data.0 <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  
  data.0$PAR_SC_V <- log(data.0$PAR_SC_V)
  data.0$PAR_ASC_V <- log(data.0$PAR_ASC_V)
  
  indices.retires <- c(148:167)
  data <- data.0[, -indices.retires]
  
  corr <- cor(x=data[, -ncol(data)])
  high.corr.index.new <- which(corr>0.99, arr.ind = TRUE) %>% unname
  lower.tri <- lower.tri(corr, diag=FALSE)
  high.corr.index.new <- high.corr.index.new[which(lower.tri[high.corr.index.new]==TRUE),]
  correlated.variables <- matrix(c(names(data)[high.corr.index.new[,1]], 
                                   names(data)[high.corr.index.new[,2]]),
                                 nrow=nrow(high.corr.index.new))
  name.list <- as.vector(correlated.variables)
  high.corr.index <- matrix(which(names(data.0) %in% name.list), nrow=nrow(high.corr.index.new))
  
  indices.retires <- c(indices.retires, high.corr.index[,1])
  
  indices.4 <- which(names(data.0) %in% c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))
  indices.retires <- c(indices.retires, indices.4)
  
  names(data.0)[ncol(data.0)] = "y"
  data.0$y <- ifelse(data.0$y=="Jazz", 1, 0)
  
  ## Echantillon d'apprentissage
  set.seed(103)
  train = sample(c(TRUE,FALSE), nrow(data.0), rep=TRUE, prob=c(2/3,1/3))
  
  #On retirera les variables plus tard
  data.train.0 <- data.0[which(train),]
  data.test.0 <- data.0[which(train==FALSE),]
  
  #On retire les 27 variables
  data <- data.0[, -indices.retires]
  data.train <- data.train.0[, -indices.retires]
  data.test <- data.test.0[, -indices.retires]
  
  return(list(data=data, data.train=data.train, data.test=data.test, data.0=data.0, data.train.0=data.train.0, data.test.0=data.test.0))
}


prepare.data.unlabelled <- function(filename){
  data <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  data.0 <- read.csv("Music_2023.txt",sep=";",header=TRUE)
  
  data.unlabelled <- read.csv(filename,sep=";",header=TRUE)
  
  data.unlabelled$PAR_SC_V <- log(data.unlabelled$PAR_SC_V)
  data.unlabelled$PAR_ASC_V <- log(data.unlabelled$PAR_ASC_V)
  
  
  ##indices à retirer
  indices.retires <- c(148:167)
  data <- data.0[, -indices.retires]
  
  corr <- cor(x=data[, -ncol(data)])
  high.corr.index.new <- which(corr>0.99, arr.ind = TRUE) %>% unname
  lower.tri <- lower.tri(corr, diag=FALSE)
  high.corr.index.new <- high.corr.index.new[which(lower.tri[high.corr.index.new]==TRUE),]
  correlated.variables <- matrix(c(names(data)[high.corr.index.new[,1]], 
                                   names(data)[high.corr.index.new[,2]]),
                                 nrow=nrow(high.corr.index.new))
  name.list <- as.vector(correlated.variables)
  high.corr.index <- matrix(which(names(data.0) %in% name.list), nrow=nrow(high.corr.index.new))
  
  indices.retires <- c(indices.retires, high.corr.index[,1])
  
  indices.4 <- which(names(data.0) %in% c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))
  indices.retires <- c(indices.retires, indices.4)
  
  #On retire les variables
  data.unlabelled <- data.unlabelled[, -indices.retires]

  return(data.unlabelled)
}


df <- prepare.data()

#On entraîne le modèle sur tout les individus du dataset
data.train <- df$data

data.unlabelled <- prepare.data.unlabelled("Music_test.txt")


x.train <- data.train[,-ncol(data.train)] %>% as.matrix()
y.train <- data.train[,ncol(data.train)]

x.unlabelled <- data.unlabelled %>% as.matrix()

#Modèle de régression ridge
set.seed(314)
grid <- 10^seq(0, -4, length=100)
cv.out <- cv.glmnet(x=x.train, y=y.train, family = "binomial", lambda=grid, nfolds=10)
bestlam=cv.out$lambda.min

bestlam
#on trouve 0.0005857021 qui n'est pas sur la frontière, c'est le lambda optimal.

#model final
ridge.model <- glmnet(x=x.train, y=y.train, family="binomial", lambda=bestlam)

## Prédiction
probabilities.pred <- ridge.model %>% predict(newx=x.unlabelled)
predicted.classes.pred <- ifelse(probabilities.pred > 0.5, "Jazz", "Classical")

prediction <- c(predicted.classes.pred) %>% unname()
write.table(prediction, file="KALAYDJIAN-OCCHIPINTI_test.txt", sep=": ", row.names=FALSE, col.names=FALSE, quote=FALSE)