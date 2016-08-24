library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation

library('randomForest') # classification algorithm
library("rpart")  # Tree decision 
library("rpart.plot") # displayin Tree decision 

library("gbm")  #gardiant boosting algorithm

# changer le dossier de travail
setwd('C:/Users/ylasmak/Desktop/ML/Titanic')

#Chargement des fichiers Train et Test
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

#On fusionne les deux matrice
full  <- bind_rows(train, test)

# Ajout des nouvelles variable à partir des données existante
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

full$Fsize <- full$SibSp + full$Parch + 1
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

full$Family <- paste(full$Surname, full$Fsize, sep='_')


full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Imputation des données : correction des données manquantes
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

C
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

full$Embarked[c(62, 830)] <- 'C'

#Visualisation du grpahe de mediane de la densité
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

sum(is.na(full$Age))

#Création des enumeration
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Intialize le fonction random
set.seed(129)

# imputation des données manquantes grace à la fonction mice
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

mice_output <- complete(mice_mod)

full$Age <- mice_output$Age

full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
table(full$Child, full$Survived)
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# Building Model
train <- full[1:891,]
test <- full[892:1309,]

md.pattern(full)

set.seed(754)

# Random Forest

# entrainement de l'algorithme random forest
 rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train ,ntree=1000)



#entrainement de l'algorithme

my_tree <- rpart(Survived ~ Age + Sex + Pclass  + FsizeD, data = train, method = "class", control=rpart.control(cp=0.0001))

summary(my_tree)

prp(my_tree, type = 3, extra = 100)



# entrainement de l'algorithme random forest gradian boosting

gbm_model = gbm(Survived ~ Age + Sex + Pclass  + FsizeD, data = train, 
          n.trees=1000,
          shrinkage=0.01,
          distribution="gaussian",
          interaction.depth=7,
          bag.fraction=0.9,
          cv.fold=10,
          n.minobsinnode = 50
)
gbm_perf <- gbm.perf(gbm_model, method = "cv")







