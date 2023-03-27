## L3 MIASHS - Année 2022-2023
## Université Rennes I
## Projet AS - Le trafic de taxis - New York au mois de janvier 2015

################################################################################################

## Répertoire de travail
setwd("C:/Users/lebre/OneDrive/Bureau/AS/Projet AS")
getwd()

## Packages
library(lubridate)
library(ggplot2)
library(viridis)
library(timeDate)
library(car)


################################################################################################
# I-Analyse descriptive ########################################################################
################################################################################################

# Importation des données
df=read.csv(file = "new_york_taxis_2014-2015.csv",sep = ",",row.names=1)
nrow(df)
df_complet = df

# conversion de timestamp en date + heure
df$timestamp <- ymd_hms(as.character(df$timestamp))

df <- df[(df$timestamp>"2015-01-01 00:30:00"),]
df$times=seq(1,nrow(df))
nrow(df)
summary(df)

df_complet$moisannee <- strftime(df_complet$timestamp, "%y-%m")

### Graphiques

# graphique des données complètes par mois
ggplot(df_complet, aes(x=moisannee,y=value)) + 
  geom_bar(stat = "identity",fill="navy") +
  theme_light()+
# légende
  labs(title = "Evolution du nombre de passagers de taxi - New York",
        subtitle = "2014-2015",
        caption = "Données : The NYC Taxi and Limousine Commission",
        x = "Année et mois", y = "Nombre total de passagers de taxi par mois")
  
# graphique des données brutes 
ggplot(df, aes(x=timestamp, y=value)) +   
  geom_line(size=0.5,color="darkblue") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Date", y = "Nombre total de passagers de taxi par tranche de 30 min")

# Régression linéaire : Nombre de passagers x Temps
reg <- lm(df$value~df$timestamp)
summary(reg) # p-value = 0.75 et R2=0 donc il n'y a pas de tendance linéaire notable au cours du mois

# graphique des données brutes en log 
ggplot(df, aes(x=timestamp, y=log(value))) +   
  geom_line(size=0.5,color="darkred") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Date", y = "Logarithme du nombre de passagers par tranche de 30 min")

# création d'une colonne jour
df$jour <- strftime(df$timestamp, "%d")

# création d'une colonne jour de la semaine (weekday)
df$weekday <- weekdays(df$timestamp)
df$weekday <- factor(df$weekday, levels = c('lundi', 'mardi', 'mercredi', 'jeudi','vendredi','samedi','dimanche'))

# création d'une colonne Semaine/week-end
df$swe <-isWeekend(df$timestamp)
df$swe <- factor(df$swe)
levels(df$swe)[levels(df$swe)==TRUE] <- "Week-end"
levels(df$swe)[levels(df$swe)==FALSE] <- "Semaine"

# création d'une colonne heure
df$heure <- strftime(df$timestamp, "%H")
# création d'une colonne jour + heure
df$jour_heure <- strftime(df$timestamp, "%Y-%m-%d %H")


# agrégation des données par jour de la semaine (weekday) (moyenne)
df_weekday <- aggregate(value ~ weekday,df,FUN = mean)
# agrégation des données par semaine/week-end (sum)
df_swe <- aggregate(value ~ swe,df,FUN = sum)
# agrégation des données par heure (moyenne)
df_heure <- aggregate(value ~ heure,df,FUN = mean)
# agrégation des données par jour + heure (somme) et ajout de colonnes intéressantes pour la suite
df_jour_heure <- aggregate(value ~ jour_heure,df,FUN = sum)
df_jour_heure$jour_heure <- ymd_h(as.character(df_jour_heure$jour_heure))
df_jour_heure$heure <- strftime(df_jour_heure$jour_heure, "%H")
df_jour_heure$jour <- strftime(df_jour_heure$jour_heure, "%d")
df_jour_heure$weekday <- weekdays(df_jour_heure$jour_heure)
df_jour_heure$weekday <- factor(df_jour_heure$weekday, levels =c('dimanche','samedi','vendredi',
                                                                 'jeudi','mercredi','mardi','lundi'))
df_jour_heure$swe <-isWeekend(df_jour_heure$jour_heure)
df_jour_heure$swe <- factor(df_jour_heure$swe)
levels(df_jour_heure$swe)[levels(df_jour_heure$swe)==TRUE] <- "Week-end"
levels(df_jour_heure$swe)[levels(df_jour_heure$swe)==FALSE] <- "Semaine"
summary(df_jour_heure)

# graphique des données par jour + heure
ggplot(df_jour_heure, aes(x=jour_heure, y=value)) + 
  geom_line(size=0.7,color="blue") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Date", y = "Nombre total de passagers de taxi par heure")

# graphique des données par jour
ggplot(df, aes(x=jour,y=value,group=jour)) + 
  geom_bar(stat = "identity",fill="purple") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour du mois de Janvier", y = "Nombre total de passagers de taxi par jour")

# graphique des données par heure
ggplot(df_weekday, aes(x=weekday, y=value)) + 
  geom_bar(stat = "identity",fill="darkblue") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour de la semaine", y = "Nombre moyen de passagers de taxi par jour de la semaine")

# graphique des données par heure
ggplot(df_heure, aes(x=heure, y=value)) + 
  geom_bar(stat = "identity",fill="darkred") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure de la journée", y = "Nombre moyen de passagers de taxi par heure")


# diagramme circulaire Week-end/Semaine
df_swe$pourcentage=round(df_swe$value/sum(df_swe$value)*100,2)
ggplot(df_swe,aes(x="", y=value, fill=swe)) +
  geom_bar(stat="identity", width=1)+
  geom_text(aes(label = paste(pourcentage,"%",sep ="")),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0)

# graphique des données par heure + jour de la semaine
ggplot(df_jour_heure, aes(x=heure,y=value,group=weekday,fill=weekday)) + 
  geom_bar(stat = "identity",position="dodge") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York ",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure", y = "Nombre total de passagers de taxi par heure")

# graphique des données par heure + semaine ou week-end
ggplot(df_jour_heure, aes(x=heure,y=value,group=swe,fill=swe)) + 
  geom_bar(stat = "identity",position="dodge") +
  theme_light()+
  # légende
  labs(title = "Evolution du nombre de passagers de taxi - New York",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure", y = "Nombre total de passagers de taxi par heure")

# carte de chaleur : Jour x Heure
ggplot(df_jour_heure,aes(jour,heure,fill=value)) +
  geom_tile()+
  theme_classic() +
  scale_fill_viridis(name="Nombre de passagers par heure",option ="viridis") +
  labs(title = "Evolution du nombre de passagers de taxi - New York",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Jour", y = "Heure")

# carte de chaleur : Weekday x Jour
ggplot(df_jour_heure,aes(heure,weekday,fill=value)) +
  geom_tile()+
  theme_classic() +
  scale_fill_viridis(name="Nombre de passagers moyen par heure",option ="plasma") +
  labs(title = "Evolution du nombre de passagers de taxi - New York",
       subtitle = "Janvier 2015",
       caption = "Données : The NYC Taxi and Limousine Commission",
       x = "Heure", y = "Jour de la semaine")


################################################################################################
# II-Moyennes mobiles ##########################################################################
################################################################################################

## Suppression des anomalies

# Le nouvel an : on le remplace par le jeudi suivant (les 10 premières heures)
prim8 <- which(df$jour=="08")[1]
df[1:20,]$value <- df[prim8:(prim8+19),]$value

# Le blizzard du 26, 27 et 28 janvier : on les remplace par les 19,20 et 21 du mois
prim26 <- which(df$jour=="26")[1]
prim19 <- which(df$jour=="19")[1]
df[prim26:(prim26+48*3),]$value <- df[prim19:(prim19+48*3),]$value

# création de la série temporelle
df_periode<- ts(data = df$value,start = 01+(1/48),end = 31+(48/48),frequency = 48)
length(df_periode)

## Méthode de la bande

# graphique de la série temporelle sans anomalies
plot(df_periode,type="l",main="Evolution du nombre de passagers sans anomalies",
     xlab="Jour du mois de janvier 2015",ylab="Nombre de passagers par tranche de 30 min")
# les droites ne sont pas paralleles donc il semble s'agir d'un modèle multiplicatif

## Méthode du profil

mat=matrix(data=df_periode,nrow=31,ncol=48,byrow=TRUE)
ymin=min(mat[1:31,])
ymax=max(mat[1:31,])
plot(mat[1,],type="l",col=1,ylim=c(ymin,ymax),main="Profils des 31 jours",
     xlab="Tranches de 30 min sur une journée",ylab="Nombre de passagers")
for(i in 2:31) {lines(mat[i,],type="l",col=i,lwd=2)}
# les courbes ne se superposent pas vraiment donc il semble s'agir d'un modèle multiplicatif 

## Test de Buys-Ballot pour déterminer le modèle

# calcul des moyenne par jour pour chaque observation
aggmean<- aggregate(df$value,list(jour=df$jour),mean)
# calcul des ecarts types par jour pour chaque observation
aggsd<-aggregate(df$value,list(jour=df$jour),FUN="sd")
# on effectue une regression lineaire 
buys_ballot<- lm(aggsd$x~aggmean$x)
reg_bb <- summary(buys_ballot)
reg_bb
plot(aggsd$x~aggmean$x,main="Moyennes des jours x Ecarts-types des jours",xlab="Moyenne du jour",ylab="Ecart-type du jour",lwd=2)
abline(reg_bb$coefficients[1],reg_bb$coefficients[2],col='red',lwd=2)

# p-value < 0.05 donc relation linéaire effective 
# On rejette l'hypothese nulle. Donc le modele est multiplicatif

## Transformation de la série mutliplicative : transformation de BOX-COX

lambda <- powerTransform(df$value)$lambda
df$mod <- ((df$value^lambda)-1)/lambda
# graphique de la série transformée
plot(df$mod,type="l",main="Evolution du nombre transformé de passagers sur le mois de Janvier 2015",
     ylab="Nombre transformé de passagers")

## MM  d'ordre 49

filter48=c(1/96,rep(1/48,47),1/96)
df$modf49=filter(df$mod, filter48, method = "convolution",sides = 2, circular = FALSE)

# graphique de la moyenne mobile d'odre 49
plot(df$mod,type='l',col='black',lwd=1,main="Evolution du nombre transformé de passagers x Moyenne Mobile d'ordre 49",
     ylab="Nombre transformé de passagers")
lines(df$modf49,type='l',col='blue',lwd=2)

# calcul des coefficients saisonniers 
df$modsais0=df$mod-df$modf49
modsais0=tapply(df$modsais0,df$jour,mean,na.rm=TRUE)
sais0moy=mean(modsais0)
sais0bis=modsais0-sais0moy # coefficients saisonniers finals
df$sais0bis=rep(sais0bis,48) # ajout des coefficients saisonniers au dataframe

# extrapolation de la tendance
df$desais=df$mod-df$sais0bis
# graphique de la série désaisonnalisée
plot(df$desais,type='l',main="Série transformée désaisonnalisée",
     ylab="Nombre transformé désaisonnalisé de passagers",col='darkblue')

reg1=lm(df$desais~df$times)
summary(reg1) # p-value < 0.05 donc relation linéaire effective 
# estimations de la tendance
df$tchap0=reg1$coefficients[1]+reg1$coefficients[2]*df$times
# previsions de la serie mod
df$prev0=df$tchap0+df$sais0bis

# graphique de la série transformée x prévision x tendance
plot(df$mod~df$times,type='l',col='black',lwd=1.5,main="Série transformée x Prévision x Tendance",
     ylab="Nombre transformé de passagers")
lines(df$prev0~df$times,type='l',col='blue',lwd=1.5)
lines(df$tchap0~df$times,type='l',col='red',lwd=2.5)


# estimation et previsions de la serie df
df$prev1=(lambda*df$prev0+1)^(1/lambda)

# graphique de la série x prévision
plot(df$value~df$times,type='l',col='black',lwd=1.5,main="Série du nombre de passagers x Prévision",
     ylab="Nombre de passagers")
lines(df$prev1~df$times,type='l',col='red',lwd=1.5)


################################################################################################
# III-Lissages #################################################################################
################################################################################################

# Il y a une tendance et une saisonnalité donc on effectue un lissage de Holt-Winters

# Lissage de Holt-Winters sans declaration des coefficients de lissage, Modele additif
HW1=HoltWinters(df_periode)
HW1
plot(HW1,lwd=2,col="black")
HW1fit=fitted(HW1)
HW1fit
p1=predict(HW1,12,prediction.interval=TRUE)
plot(HW1,p1,lwd=2,col="black")


# Lissage de Holt-Winters sans declaration des coefficients de lissage, Modele multiplicatif
HW2=HoltWinters(df_periode,seasonal="multiplicative")
HW2
plot(HW2,lwd=2,col="black")
HW2fit=fitted(HW2)
HW2fit
p2=predict(HW2,12,prediction.interval=TRUE)
plot(HW2,p2,lwd=2,col="black")

plot(HW1fit)
plot(HW2fit)

dif= HW1$SSE-HW2$SSE
dif # La somme des carrés des résidus du 1er modèle est plus faible
# Donc le premier lissage (additif) est le meilleur


