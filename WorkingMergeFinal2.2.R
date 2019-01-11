library("stringi", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#............................................Gemeindedaten.........................................
gemeinde <- read.csv("gemeinde.csv", stringsAsFactors = FALSE, colClasses = c(rep("character",14)))

gemeinde1 <- gemeinde[(gemeinde$Gem == ""),]
gemeinde <- gemeinde[!(gemeinde$VB == "" | gemeinde$Gem=="" ),]
gemeinde<- gemeinde[gemeinde$Gem != "",]
gemeinde$rs <- paste(gemeinde[,1],gemeinde[,2], gemeinde[,3],gemeinde[,5], sep = "" )
gemeinde$insgesamt <- gsub(" ", "", gemeinde$insgesamt, fixed = TRUE)
gemeinde$insgesamt <- as.numeric(gemeinde$insgesamt)
sum(gemeinde$insgesamt)  #komme auf 80 Millionen
write.csv(gemeinde, "gemeinde1.csv") 

#.................................................arbeitslosigkeit...........................................

arbeit <- read.csv("arbeitslosigkeit.csv",stringsAsFactors = FALSE, colClasses = c(rep("character",9)))
test <- function(x){
  if(nchar(x) < 8){
    y <- 0
  }
    else {
      y <- x
    }
    return(y)
}
arbeit$rsnew <- sapply(arbeit$schluessel, test)
arbeit <- arbeit[arbeit$rsnew != 0,] 

fun <- function(x){
  if(nchar(x) >= 8){
    y <- paste(substr(x,1,5),substr(x,nchar(x)-2,nchar(x)), sep = "")
  }
  if(nchar(x)==7){
    y <- paste(substr(x,1,5),substr(x,nchar(x)-1,nchar(x)), sep = "")
  }
  if(nchar(x)<=6)
  {
    y <- x
  }
  return(y)
}
arbeit$rsnew <- sapply(arbeit$schluessel, fun)
arbeit <- arbeit[,-c(1)]    

arbeit<- arbeit[arbeit$rsnew %in% gemeinde$rs, ]

arbeit$arbeitslos.gsamt <- gsub(" ", "", arbeit$arbeitslos.gsamt, fixed = TRUE)
arbeit$arbeitslos.gsamt <- as.numeric(arbeit$arbeitslos.gsamt)

arbeit[is.na(arbeit)] <- 0

master1 <- merge(arbeit, gemeinde, by.x=c("rsnew"), by.y = c("rs"))

#....................................................Wahldaten.Europa..................................
wahl <- read.csv("wahldaten2.csv", stringsAsFactors = FALSE)

wahl$Schluessel[is.na(wahl$Schluessel)] <- 0
wahl$Schluessel <- as.character(wahl$Schluessel)
wahl$Schluessel<- str_pad(wahl$Schluessel,7,pad="0",side="right")

funwahl <- function(x){
  if(nchar(x) == 8){
    y <- x
  }
  if(nchar(x) == 9){
     y <- paste0("0",x)
    
  }
  if(nchar(x)== 7){
    y <- paste0("0",x)
  }
  if(nchar(x) == 10){
    y <- paste0("0",x)
  }
  return(y)
}
wahl$Schluessel <- sapply(wahl$Schluessel,funwahl)
funwahl2 <- function(x){
  if(nchar(x) > 8){
    y <- paste(substr(x,1,5),substr(x,nchar(x)-2,nchar(x)), sep = "")
  }
  else{
    y <- x
  }
}
wahl$Wahlberechtigte..Anzahl. <- as.numeric(wahl$Wahlberechtigte..Anzahl.)
sum(wahl$Wahlbeteiligung..Prozent.)
wahl$Schluessel <- sapply(wahl$Schluessel,funwahl2)
max(nchar(wahl$Schluessel)) 
wahl$Schluessel[wahl$Schluessel =="01100000"] <- "11000000"
master <- merge(wahl, master1, by.x=c("Schluessel"), by.y = c("rsnew"))
#....................................................wahldaten DE und ALter ..............................
wahlbtw <- read.csv("wahl.btw.csv", stringsAsFactors = FALSE)
alter <- read.csv("alter.csv", stringsAsFactors = FALSE, sep=";")
master2 <- merge(wahlbtw, alter,  by.x=c("Schluessel"), by.y = c("X"))
master2$Schluessel <- as.character(master2$Schluessel)
master2$Schluessel<- str_pad(master2$Schluessel,7,pad="0",side="right")
master2$Schluessel <- sapply(master2$Schluessel, funwahl)
master2$Wahlberechtigte.btw <- as.numeric(master2$Wahlberechtigte.btw)
sum(master2$Wahlberechtigte.btw, na.rm = TRUE)
master2$Schluessel <- sapply(master2$Schluessel,funwahl2)
master2$Schluessel[master2$Schluessel=="01100000"] <- "11000000"

master3 <- merge(master1, master2, by.x=c("rsnew"), by.y = c("Schluessel"))
#..........................................................Master gemerged..................................
master <- merge(wahl, master3, by.x=c("Schluessel"), by.y = c("rsnew"))

master <- master[,-c(2,14,18,23,24,25,26,27)]
sum(master$insgesamt)
master.numeric <- master
master.numeric$Wahlberechtigte..Anzahl. <- as.numeric(master.numeric$Wahlberechtigte..Anzahl.)
master.numeric$Wahlbeteiligung..Prozent. <- as.numeric(gsub(",","\\.", master.numeric$Wahlbeteiligung..Prozent.))
master.numeric$Gueltige.Stimmen..Anzahl. <- as.numeric(master.numeric$Gueltige.Stimmen..Anzahl.)
master.numeric$Gueltige.Stimmen..CDU.CSU. <- as.numeric(master.numeric$Gueltige.Stimmen..CDU.CSU.)
master.numeric$Gueltige.Stimmen..SPD. <- as.numeric(master.numeric$Gueltige.Stimmen..SPD.)
master.numeric$Gueltige.Stimmen..Gruene. <- as.numeric(master.numeric$Gueltige.Stimmen..Gruene.)
master.numeric$Gueltige.Stimmen..FDP. <- as.numeric(master.numeric$Gueltige.Stimmen..FDP.)
master.numeric$Gueltige.Stimmen..Die.Linke. <- as.numeric(master.numeric$Gueltige.Stimmen..Die.Linke.)
master.numeric$Gueltige.Stimmen..AfD. <- as.numeric(master.numeric$Gueltige.Stimmen..AfD.)
master.numeric$Gueltige.Stimmen..Sonstige. <- as.numeric(master.numeric$Gueltige.Stimmen..Sonstige.)
master.numeric$flaeche <- as.numeric(gsub(",","\\.", master.numeric$flaeche))      
master.numeric$Laengengrad <- as.numeric(gsub(",","\\.", master.numeric$Laengengrad))
master.numeric$Breitengrad <- as.numeric(gsub(",","\\.", master.numeric$Breitengrad))
master.numeric$Wahlberechtigte.btw <- as.numeric(master.numeric$Wahlberechtigte.btw)
# In 3 Spalten sind zwischen den tausender Stellen Leerzeichen eingef??gt, die nicht in numeric umgewandelt werden k??nnen.
# Hierf??r wenden wir eine manipulierende Customfunction an, die nach dem selben Prinzip funktioniert, wie die RS Umwandlung.

# Nun k??nnen auch diese Variablen in numeric umgewandelt werden


master.numeric$maennlich <- gsub(" ", "", master.numeric$maennlich, fixed = TRUE)
master.numeric$weiblich <- gsub(" ", "", master.numeric$weiblich, fixed = TRUE)

#Duplikate in den rs-schluesseln werden entfernt

master.numeric <- subset(master.numeric, !duplicated(subset(master.numeric, select = c("Schluessel"))))

sum(master.numeric$Wahlberechtigte..Anzahl.)
sum(master.numeric$insgesamt)
sum(master.numeric$arbeitslos.gsamt)
sum(master.numeric$Wahlberechtigte.btw, na.rm = TRUE)

#---------------------------Enfernen von Outliern und ersetzen von NAs, wenn n??tig:----------------------------------

# ??berpr??fung der NAs, gegebenenfalls ersetzen durch 0. Evtl. entfernen von Outlier.
ncol(master.numeric)
colnames(master.numeric)

stimmen.parteien <- c("Gueltige.Stimmen..CDU.CSU.", "Gueltige.Stimmen..SPD.", "Gueltige.Stimmen..Gruene.", "Gueltige.Stimmen..FDP.", "Gueltige.Stimmen..Die.Linke.", "Gueltige.Stimmen..AfD.", "Gueltige.Stimmen..Sonstige.")
master.numeric[stimmen.parteien][is.na(master.numeric[stimmen.parteien])] <- 0

#Arbeitslose 15bis20 und sinnlose spalte entfernen
master.numeric <- within(master.numeric, rm("arbeitslos.15bis20", "X.y"))

#Arbeitslose: Function: wenn arbeitlos insgesamt 0 -> NA = 0.
arbeitslos.num <- c("arbeitslos.ausld", "arbeitslos15bis25", "arbeitslos.55bis65", "arbeitslos.langzeit")
master.numeric[, arbeitslos.num] <- sapply(master.numeric[, arbeitslos.num], as.numeric)

master.numeric$arbeitslos.ausld <- ifelse(master.numeric$arbeitslos.gsamt==0, 0,master.numeric$arbeitslos.ausld)
master.numeric$arbeitslos15bis25 <- ifelse(master.numeric$arbeitslos.gsamt==0, 0,master.numeric$arbeitslos15bis25)
master.numeric$arbeitslos.55bis65 <- ifelse(master.numeric$arbeitslos.gsamt==0, 0,master.numeric$arbeitslos.55bis65)

# Muss das Bittburger Land noch raus?

#---------------------------------------Besonderheiten in den Daten:---------------------------------------

#gr????ere Summe der Kategorien "Arbeitslose" als Arbeitslose insgesamt
subset(master.numeric$Name, master.numeric$arbeitslos.gsamt<master.numeric$arbeitslos.55bis65+master.numeric$arbeitslos15bis25+master.numeric$arbeitslos.ausld)

#mehr langzeitarbeitslose als arbeitslose insgesamt:
subset(master.numeric$Name, master.numeric$arbeitslos.langzeit>master.numeric$arbeitslos.gsamt)

#mehr arbeitslose als einwohner -> arbeitslosen zahlen als NAs
subset(master.numeric$Name, master.numeric$arbeitslos.gsamt>master.numeric$insgesamt)

#mehr wahlberechtigte (eu) als einwohner  (evtl nicht aus dem gleichen Jahr)
subset(master.numeric$Name, master.numeric$Wahlberechtigte..Anzahl.>master.numeric$insgesamt)
#subset(master.numeric$Name, master.numeric$Wahlberechtigte..Anzahl.>master.numeric$Insgesamt)

#mehr wahlberechtigte (btw) als einwohner (evtl. nicht aus dem gleichen Jahr)
subset(master.numeric$Name, master.numeric$Wahlberechtigte.btw>master.numeric$insgesamt)

#--------------------------Umwandlung der restlichen f??lschlich erzeugten char Variablen in num:----------------------------

var.num <- c("maennlich", "weiblich", "Gueltige.Zweitstimmen.btw", "CDU.CSU.btw", "SPD.btw", "GRUENE.btw", "FDP.btw", "LINKE.btw", "AfD.btw", "Sonstige.Parteien.btw", "Insgesamt..unter.3.Jahre", "Insgesamt..3.bis.unter.6.Jahre", "Insgesamt..6.bis.unter.10.Jahre", "Insgesamt..10.bis.unter.15.Jahre", "Insegsamt..15.bis.unter.18.Jahre", "Insgesamt..18.bis.unter.20.Jahre", "Insgesamt.20.bis.unter.25.Jahre", "Insgesamt..25.bis.unter.30.Jahre", "Insgesamt..30.bis.unter.35.Jahre", "Insgesamt.35.bis.unter.40.Jahre", "Insgesamt..40.bis.unter.45.Jahre", "Insgesamt..45.bis.unter.50.Jahre", "Insgesamt..50.bis.unter.55.Jahre", "Insgesamt..55.bis.unter.60.Jahre", "Insgesamt..60.bis.unter.65.Jahre", "Insgesamt..65.bis.unter.75.Jahre", "Insgesamt..75.Jahre.und.mehr", "Insgesamt")
master.numeric[, var.num] <- sapply(master.numeric[, var.num], as.numeric)
master.numeric$Wahlbeteiligung.btw <- as.numeric(gsub(",","\\.", master.numeric$Wahlbeteiligung.btw))

#NAs durch fehlende Wahldaten erzeugt -> macht bei der Analyse keinen Unterschied (hoffentlich)
subset(master.numeric$Name, is.na(master.numeric$Wahlbeteiligung.btw))

# Die Tabelle wird als CSV Datei exportiert -> brauchen wir eigentlich nicht mehr

#write.csv(master.numeric, "master.numeric.csv")

#Zus?tzliche Variablen als Prozent (noch sch?ner coden)

master.numeric$Arbeitslos_Prozent <- (master.numeric$arbeitslos.gsamt/ master.numeric$insgesamt)*100
master.numeric$Arbeitslos.ausld_Prozent <- (master.numeric$arbeitslos.ausld/ master.numeric$insgesamt)*100
master.numeric$Arbeitslos.15bis20_Prozent <- (master.numeric$arbeitslos.15bis20/ master.numeric$insgesamt)*100
master.numeric$Arbeitslos.15bis25_Prozent <- (master.numeric$arbeitslos15bis25/ master.numeric$insgesamt)*100
master.numeric$Arbeitslos.55bis65_Prozent <- (master.numeric$arbeitslos.55bis65/ master.numeric$insgesamt)*100
master.numeric$Arbeitslos.langzeit_Prozent <- (master.numeric$arbeitslos.langzeit/ master.numeric$insgesamt)*100

master.numeric$maennlich_Prozent <- (master.numeric$maennlich/ master.numeric$insgesamt)*100
master.numeric$weiblich_Prozent <- (master.numeric$weiblich/ master.numeric$insgesamt)*100

master.numeric$unter3_prozent <- (master.numeric$Insgesamt..unter.3.Jahre/ master.numeric$Insgesamt)*100

prozent <-function(x){(x/master.numeric$Insgesamt) * 100}

master.numeric$unter3_prozent <- prozent(master.numeric$Insgesamt..unter.3.Jahre)
master.numeric$dreibis6_prozent <- prozent(master.numeric$Insgesamt..3.bis.unter.6.Jahre)
master.numeric$sechsbis10_prozent <- prozent(master.numeric$Insgesamt..6.bis.unter.10.Jahre)
master.numeric$zehnbis15_prozent <- prozent(master.numeric$Insgesamt..10.bis.unter.15.Jahre)
master.numeric$fuehnzehnbis18_prozent <- prozent(master.numeric$Insegsamt..15.bis.unter.18.Jahre)
master.numeric$achtzehnbis20_prozent <- prozent(master.numeric$Insgesamt..18.bis.unter.20.Jahre)
master.numeric$zwanzigbis25_prozent <- prozent(master.numeric$Insgesamt.20.bis.unter.25.Jahre)
master.numeric$fuenfundzwanzigbis30_prozent <- prozent(master.numeric$Insgesamt..25.bis.unter.30.Jahre)
master.numeric$dreissigbis35_prozent <- prozent(master.numeric$Insgesamt..30.bis.unter.35.Jahre)
master.numeric$fuenfunddreissigbis40_prozent <- prozent(master.numeric$Insgesamt.35.bis.unter.40.Jahre)
master.numeric$vierzigbis45_prozent <- prozent(master.numeric$Insgesamt..40.bis.unter.45.Jahre)
master.numeric$fuenfundvierzigbis50_prozent <- prozent(master.numeric$Insgesamt..40.bis.unter.45.Jahre)
master.numeric$fuenfzigbis55_prozent <- prozent(master.numeric$Insgesamt..50.bis.unter.55.Jahre)
master.numeric$fuenfundfuenzigbis60_prozent <- prozent(master.numeric$Insgesamt..55.bis.unter.60.Jahre)
master.numeric$sechzigbis65_prozent <- prozent(master.numeric$Insgesamt..60.bis.unter.65.Jahre)
master.numeric$fuenfundsechzigbis75_prozent <- prozent(master.numeric$Insgesamt..65.bis.unter.75.Jahre)
master.numeric$ueber75_prozent <- prozent(master.numeric$Insgesamt..75.Jahre.und.mehr)

master.numeric$minderjaehrig_prozent <- master.numeric$unter3_prozent + master.numeric$sechsbis10_prozent+master.numeric$zehnbis15_prozent + master.numeric$fuehnzehnbis18_prozent







#------------------------------Random forest starting now-------------------------------

#alles laden

install.packages("randomForest")
install.packages("tree")

library(randomForest)
library(tree)


#ohne Nas
master.numeric2 = na.exclude(master.numeric)

#normaler tree test

tree.wahlbeteiligung = tree(Wahlbeteiligung..Prozent.~ flaeche + insgesamt + weiblich_Prozent + m?nnlich_Prozent + Wahlbeteiligung.btw + Arbeitslos.langzeit_Prozent + Arbeitslos.55bis65_Prozent + Arbeitslos.15bis25_Prozent + Arbeitslos.ausld_Prozent, data=master.numeric) 
summary(tree.wahlbeteiligung)

plot(tree.wahlbeteiligung)
text(tree.wahlbeteiligung, pretty =0)

#random forest

#subset als training data 

sample.wahlbeteiligung <- sample(1:nrow(master.numeric2),nrow(master.numeric2)/2)


rf.wahlbeteiligung = randomForest(Wahlbeteiligung..Prozent.~ flaeche + insgesamt  + weiblich_Prozent + m?nnlich_Prozent + Wahlbeteiligung.btw + Arbeitslos.langzeit_Prozent + Arbeitslos.55bis65_Prozent + Arbeitslos.15bis25_Prozent + Arbeitslos.ausld_Prozent + minderj?hrig_prozent + ?ber75_prozent, data= master.numeric2, subset = sample.wahlbeteiligung)

#testen auf testdaten
yhat.rf = predict(rf.wahlbeteiligung, newdata = master.numeric[-sample.wahlbeteiligung,])
wahlbeteiligung_test = master.numeric [-sample.wahlbeteiligung, "Wahlbeteiligung..Prozent."]
plot(yhat.rf, wahlbeteiligung_test)
abline(0,1)
mean((yhat.rf - wahlbeteiligung_test)^2) #geht irgendwie nicht :( sollte MSE sein


importance(rf.wahlbeteiligung)

plot(rf.wahlbeteiligung)

varImpPlot(rf.wahlbeteiligung)



