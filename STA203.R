#Mini projet
#Anthony Kalaydjian - Mathieu Occhipinti

library(ggplot2)
library(plyr)
library(stats)
library(tidyverse)
library(cowplot)
library(ROCR)
library(MASS)
library(glmnet)

setwd(getwd())
rm(list=ls())
graphics.off()


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
















## Proportion des genres musicaux
proportion.classique <- mean(ifelse(data$GENRE=="Classical", 1, 0))
proportion.jazz <- mean(ifelse(data$GENRE=="Jazz", 1, 0))

proportion.classique
proportion.jazz
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

#selection des indices de la matrice de correlation > 0.99
high.corr.index.new <- which(corr>0.99, arr.ind = TRUE) %>% unname

#selection des indices appartenant a la matrice triangulaire inferieure stricte,
#pour retirer les doublons, ainsi que les elements diagonaux.
lower.tri <- lower.tri(corr, diag=FALSE)
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
indices.4 <- which(names(data) %in% c("PAR_ASE_M", "PAR_ASE_MV", "PAR_SFM_M", "PAR_SFM_MV"))

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

#On sélectionne les variables dont le coefficient a un niveau de significativité de 20% et on crée la formule de notre modèle Mod1
index.var.Mod2 <- which(p_value>0.2)
var.Mod2 <- names(data[index.var.Mod2])
formula.Mod2 <- as.formula(paste("y ~", paste(var.Mod2, collapse="+")))
Mod2 <- glm(formula=formula.Mod2, family=binomial, data=data.train)
summary(Mod2)


## Méthode de selection de variable AIC

#stepAIC
## Attention, execution longue...
#step <- stepAIC(ModT)

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

load("step.RData")
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


predproba_train_Mod0=predict(Mod0,type="response") 
predproba_train_Mod1=predict(Mod1,type="response") 
predproba_train_Mod2=predict(Mod2,type="response") 
predproba_train_ModAIC=predict(ModAIC,type="response") 

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

err<-data.frame(Apprentissage=err_train,Test=err_test)

rownames(err)<-c("ModT","Mod0","Mod1","Mod2","ModAIC")

print(err)




















####################################
############# Partie 2 #############
####################################


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



grid <- 10^seq(10, -2, length=100)

ridge.fit <- glmnet(x=x.train, y=y.train, alpha=0, lambda=grid, family="binomial")

ridge.fit$dim
plot(ridge.fit)
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
matplot(apply(abs(coef.ridge[-max.coefs,]) ,2 ,sum), t(coef.ridge[-max.coefs,]), main="ridge",
        col=1:10, lty=1:10, type="l", xlab="norme L1", ylab="coefficients")




## Plot de l'evolution des coefficients en fonction du coefficient de penalite
plot(ridge.fit, xvar = "lambda")


############################    Q3
set.seed(314)

grid <- 10^seq(10, -2, length=100)
cv.out <- cv.glmnet(x=x.train, y=y.train, lambda=grid, nfolds=10)
bestlam=cv.out$lambda.min
plot(cv.out)

bestlam


#On voit que la SCR minimum est atteinte pour lambda=0.01 sur la frontière du domaine.
#Il serait judicieux de ré-effectuer la validation-croisée pour des nouvelles valeurs de lambda.
#Le minimum étant sûrement atteint avant 10^0, on choisira une grille de 10^0 à 10^-5$.


set.seed(314)
grid <- 10^seq(0, -5, length=100)

cv.out <- cv.glmnet(x=x.train, y=y.train, lambda=grid, nfolds=10)

bestlam=cv.out$lambda.min
plot(cv.out)

bestlam
#on trouve 0.0009326033 qui n'est pas sur la frontière, c'est le lambda optimal.


ridge.fit <- glmnet(x=x.train, y=y.train, alpha=0, lambda=bestlam, family="binomial")



## Erreur d'apprentissage
ridge.pred.train = predict(ridge.fit, s=bestlam, newx=x.train)
mean((ridge.pred.train - y.train)^2)
#14.26256


## Erreur de généralisation
ridge.pred.test = predict(ridge.fit, s=bestlam, newx=x.test)
mean((ridge.pred.test - y.test)^2)
#15.71024



############################    Q4

#On considere toutes les variables
x.train.0 <- data.train.0[,-ncol(data.train.0)] %>% as.matrix()
y.train.0 <- data.train.0[,ncol(data.train.0)]

x.test.0 <- data.test.0[,-ncol(data.test.0)] %>% as.matrix()
y.test.0 <- data.test.0[,ncol(data.test.0)]


set.seed(4658)
grid <- 10^seq(10, -2, length=100)
cv.out.0 <- cv.glmnet(x=x.train.0, y=y.train.0, lambda=grid, nfolds=10)
bestlam.0 <- cv.out.0$lambda.min
plot(cv.out.0)

bestlam.0
#meme situation


#nouvelle grid
set.seed(4658)
grid <- 10^seq(0, -5, length=100)
##Attention calcul long
cv.out.0 <- cv.glmnet(x=x.train.0, y=y.train.0, lambda=grid, nfolds=10)
bestlam.0 <- cv.out.0$lambda.min
plot(cv.out.0)

bestlam.0
#on trouve lambda=0.0007390722

ridge.fit.0 <- glmnet(x=x.train.0, y=y.train.0, alpha=0, lambda=bestlam.0, family="binomial")


## Erreur d'apprentissage
ridge.pred.train.0 = predict(ridge.fit.0, s=bestlam.0, newx=x.train.0)
mean((ridge.pred.train.0 - y.train.0)^2)
#15.49832


## Erreur de généralisation
ridge.pred.test.0 = predict(ridge.fit.0, s=bestlam.0, newx=x.test.0)
mean((ridge.pred.test.0 - y.test.0)^2)
#17.14912

#Les erreurs sont plus élevées ici,
#Ce modèle est moins bon que le précédent.


####################################
############# Partie 3 #############
####################################