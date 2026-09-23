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