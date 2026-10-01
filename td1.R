###############################################
####td1 #####
################################################
rm(list = ls()) #pour vider l'environnement
ls() #pour savoir ce que contient l'environnement

#créer une sequence d'entier de 1 à 10
seqce <- seq(1,10)
#variantes
seq.int(1, 10)
x <- c()
for (i in 1:10) {
  x <- c(x, i)
}
x

#créer une séquence d'artiste
artiste <- c(
  "Bong Joon Ho",
  "Fincher",
  "Spielberg",
  "Tarantino",
  "Christopher Nolan",
  "Martin Scorsese",
  "David Fincher",
  "Hayao Miyazaki",
  "Pedro Almodóvar",
  "Denis Villeneuve"
)
domaine <- c(
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma",
  "cinéma"
)
class(domaine) #pour connaitre le type d'un objet . il y a aussi typeof()
class(seqce)

table(domaine) #pour compter le nombre d'occurence
table(artiste)

names(which.max(table(domaine))) #pour avoir le nom de l'occurence la plus importante

tableau <- data.frame(seqce,artiste,domaine)
table[4,] #pour sélectionner la ligne 4
reste <- table[-4,] #pour tout selectionner sauf la ligne 4
reste
tableau[tableau$seqce!=4,]
table[!(1:nrow(table) == 4), ]
tableau$domaine[1:5]
tableau[1:5,3]
tableau[1:5,"domaine"]

tableau[tableau$domaine=="cinéma",] #attention à ne pas oublier , pour récupérer toutes les colonnes
tableau[tableau$domaine!="cinéma",]

#####

villes <- read.csv2("data/villes.csv")
villes
?read.csv2

villes[villes$Region.2015 == villes$Region.2016, "Ville"] #pour afficher les villes qui n'ont pas changé de région

length(unique(villes$Region.2015))
length(unique(villes$Region.2016))

sum(villes$Region.2015 == villes$Region.2016)

round(tapply(villes$soleil, villes$Region.2016, mean)/24,0) #pour afficher le nb d'heures moyens d'ensoleillement par région
moyennes <- tapply(villes$soleil, villes$Region.2016, mean) / 24
barplot(sort(moyennes),col="burlywood")
for (i in 1:nrow(tableau)) {
  print(tableau[i, ])
}

# Vecteur des entiers de 1 à 10
seqce <- 1:10
# Matrice vide (remplie de zéros) de taille 10x10
multi <- matrix(0, nrow = 10, ncol = 10)
# Remplissage avec une double boucle
for (i in 1:10) {
  for (j in 1:10) {
    multi[i, j] <- seqce[i] * seqce[j]
  }
}
multi

#blabla pour tester des choses notamment avec git


#ici extraits du td2 de stats inférentielles
dbinom(0, size = 7, prob = 0.125)
pnorm(0.4225, mean = 0.4, sd = 0.05)
dbinom(2, size = 5, prob = 0.90) # calcule la proba pour X =2 
pnorm(0.4225, mean = 0.4, sd = 0.05)
pbinom(2, size = 5, prob = 0.90) # calcule la proba pour X <=2 
pnorm(2.5) - pnorm(-3)
p <- 1 - pnorm(2.5)
(1 - p)^9 * p
lambda <- 25000 / 6000
dpois(0, lambda)
ppois(1, lambda = 25000 / 6000)
pnorm(1.453)- pnorm(-1.453)


##### tp1 : rcmdr sans Rcmdr #############
install.packages("Rcmdr")
library(Rcmdr)

#exercice1
x<-c(0,1) 
fct_proba <- dbinom(x,1,0.5)
plot(x,fct_proba,lwd=5,type="h",xlim=c(-0.5,1.2),ylim=c(0,0.7))

rbinom(100, size = 1, prob = 0.5)#tirage aleatoire de 100valeurs utilisant cette loi de proba

curve(pbinom(x, size = 1, prob = 0.5),
      from = -1,
      to = 2,
      lwd=3,
      n = 1000,
      xlab = "x",
      ylab = "F(x)",
      main = "Fonction de répartition de B(1/2)")
Ech_bernoulli<- rbinom(100, size = 1, prob = 0.5)
table(Ech_bernoulli)

#exercice2

i <- 0:5
proba <- dbinom(i, size = 5, prob = 0.5)
plot(i, proba,
     type = "h",
     lwd = 4,
     xlab = "i",
     ylab = "P(Y = i)",
     main = "Loi binomiale B(5, 1/2)")
curve(pbinom(x, size = 5, prob = 0.5),
      from = -1,
      to = 6,
      lwd=3,
      n = 1000,
      xlab = "x",
      ylab = "F(x)",
      main = "Fonction de répartition de Y")
ech <- rbinom(50, size = 5, prob = 0.5)
ech <- c(ech, mean(ech))
ech_binomial <- ech
plot(ech_binomial)
table(ech_binomial)

#loi_de_poisson
z <- 0:5
p <- dpois(z, lambda = 3)
plot(z, p,
     type = "h",
     lwd = 3,
     xlab = "z",
     ylab = "P(Z = z)",
     main = "Loi de Poisson P(3)")
ppois(0:5, lambda = 3)
curve(ppois(x, lambda = 3),
      from = 0,
      to = 15,
      n = 1000,
      lwd=3,
      xlab = "z",
      ylab = "F(z)",
      main = "Fonction de répartition de Z")
ech <- rpois(30, lambda = 3)
ech <- c(ech, mean(ech))
ech_poisson <- ech

#exo4 loi_normale_standard

curve(dnorm(x, mean = 0, sd = 1),
      from = -4,
      to = 4,
      n = 1000,
      lwd = 3,
      xlab = "x",
      ylab = "f(x)",
      main = "Loi normale standard N(0,1)")
curve(pnorm(x, mean = 0, sd = 1),
      from = -4,
      to = 4,
      n = 1000,
      lwd = 3,
      xlab = "x",
      ylab = "F(x)",
      main = "Fonction de répartition de N(0,1)")
pnorm(c(0, 0.5, 1), mean = 0, sd = 1)
ech <- rnorm(100, mean = 0, sd = 1)
ech <- c(ech, mean(ech))

#exo5 loi normale et qi
1 - pnorm(115, mean = 100, sd = 12.5)
qnorm(0.90, mean = 100, sd = 12.5)
1-pnorm(145,mean=100,sd=12.5)
P145 <- pnorm(145, mean = 100, sd = 12.5,
              lower.tail = FALSE)

P140 <- pnorm(140, mean = 100, sd = 12.5,
              lower.tail = FALSE)

P145 / P140

#exo6 loi_de_student
curve(dt(x, df = 5),
      from = -5,
      to = 5,
      n = 1000,
      lwd = 3,
      xlab = "t",
      ylab = "f(t)",
      main = "Loi de Student t(5)")
curve(pt(x, df = 5),
      from = -5,
      to = 5,
      n = 1000,
      lwd = 3,
      xlab = "t",
      ylab = "F(t)",
      main = "Fonction de répartition de t(5)")
pt(0, df = 5)
pt(0.2, df = 5)
ech1 <- rt(100, df = 5)
ech2 <- rt(100, df = 5)

#Exo7

data("women", package = "datasets")
women
barplot(women$weight,
        xlab = "Femmes",
        ylab = "Poids (lb)",
        main = "Poids des femmes")
barplot(women$height,
        xlab = "Femmes",
        ylab = "Taille (inches)",
        main = "Taille des femmes")
hist(women$weight,col="burlywood")
hist(women$height,col="gray")
mean(women$weight)
sd(women$weight)
quantile(women$weight)
mean(women$height)
sd(women$height)
quantile(women$height)
