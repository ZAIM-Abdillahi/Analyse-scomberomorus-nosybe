#Prétraitement
# CHARGEMENT DE FICHIER
library(readxl)
SE <- read_excel("C:/Users/ZAIM/Downloads/YES.xlsx")
DN <- read_excel("bb/DN.xlsx")
Correction des mesures de longueur 
#Modele log-loh pour LFd (longueur droite)
modele_LFd <- lm(log(SE$LD)~ log(SE$W), data = SE)
#Modele log-loh pour LFc (longueur courbe)
modele_LFc <- lm(log(SE$LC)~ log(SE$W), data = SE)

# resumé du modele
summary(modele_LFc)
summary(modele_LFd)

# recuperation des coefficients  des deux modèles
coef_LFd <- coef(modele_LFd)
coef_LFc <- coef(modele_LFc)

# Estimation de LFc et LFd pour l'echantillion de base à partir du poids (w)
# log(LFd)= coef_LFd[1]+coef_LFd[2]*log (W)
# log(LFc)= coef_LFc[1]+coef_LFc[2]*log (W)
DN$LFd_estimee <- exp(coef_LFd[1]+coef_LFd[2]*log (DN$`W (kg)`))
DN$LFc_estimee <- exp(coef_LFc[1]+coef_LFc[2]*log (DN$`W (kg)`))

#calcule de difference entre les tailles observées et taille estimées
DN$diff_LFd <- abs(DN$`LF (cm)`-DN$LFd_estimee)
DN$diff_LFc <- abs(DN$`LF (cm)`-DN$LFc_estimee)

# Identifier la longueurla plus proche de LFd ou LFc
DN$taille_finale <- ifelse(DN$diff_LFd<DN$diff_LFc,
                           DN$LFd_estimee,
                           DN$LFc_estimee)

#enregistrement de fichier finale
library(writexl)
write_xlsx(DN,"D:/SERIE/DN.xlsx")


Distribution de la taille par mois
library(ggplot2)
library(dplyr)
DN$DC<-as.Date(DN$DC)
DN<-DN%>% mutate(month=format(DC,"%Y-%m"))
ggplot(DN,aes(x = DN$`LF (cm)`))+
  geom_histogram(binwidth = 1,fill="blue",color="black")+
  facet_wrap(~month,scales = "free_y")+
  labs(title="Distribution de la taille (LF)par mois",
       x="Longueur à la fourche",
       y="Frequence")+
  theme_minimal()

#test  de chapiro-Wilkpour chaque mois #
normality_results<-DN%>%
  group_by(month)%>%
  summarise(shapiro_p_value=shapiro.test(DN$`LF (cm)`)$p.value)
#Affichage des resultats
print(normality_results)
 

#histograme Amelioré#
ggplot(DN,aes(x = DN$`LF (cm)`))+
  geom_histogram(aes(y=..density..),binwidth = 1,fill="lightblue",color="black")+ #Histogrammeavec densité
  geom_density(color="red",size=1.2)+ #courbe de densité
  facet_wrap(~month,scales = "free_y")+#Facetter par mois
  labs(title="Distribution de la taille (LF)par mois",
       x="Longueur à la fourche",
       y="Frequence")+
  theme_minimal()

#Calculer le nombre des classes selon sturges
nb_classes_sturges <- 1+log2(nrow(DN))

#calculer la largeur des classes en fonction de l'amplitude et 
# du nombre de classes#
binwidth_sturges <-(max(DN$`LF (cm)`, na.rm=TRUE)- min(DN$`LF (cm)`,
                                                       na.rm=TRUE))/nb_classes_sturges

#Creation d'hystogramme avec le nombre de classe basésur sturges
ggplot(DN,aes(x = DN$`LF (cm)`))+
  geom_histogram(aes(y=..density..),binwidth =     binwidth_sturges,fill="lightblue",color="black")+ #Histogramme avec densité
  geom_density(color="red",size=1.2)+ #courbe de densité
  facet_wrap(~month,scales = "free_y")+#Facetter par mois
  labs(title="Distribution de la taille (LF)par mois",
       x="Longueur à la fourche",
       y="Frequence")+
  theme_minimal()

Corrrelation non lineaire ou relation taille poids et test de correlation
#transformation logarithmique pour la longueur et le poids
DN$log_longueur <- log(DN$`LF (cm)`)
DN$log_poids <- log(DN$`W (kg)`)
#Ajustement d'un modele lineaire pour le log(pods)~log(taille)
modele <- lm(log_poids~log_longueur,data = DN)
#resumé du model 
summary(modele)
#Estraction des coefficient a et b
coefficients <- coef(modele)
a <- exp(coefficients[1]) # a est l'exponentiel de l'intercept
b <- coefficients[2] # b  est la pente
cat("a=",a,"\n")
cat("b=",b,"\n")
## tracer la relation taille_poids sur l'echelle logarithmique
plot (DN$`LF (cm)`,DN$`W (kg)`,
      xlab ="Longueur à la fourche en cm",
      ylab = "poids en kg",
      main="Relation Taille-poids",
      pch=16)
# Ajoute de la courbe ajuster à partir  du modele
longueur_seq <- seq(min(DN$`LF (cm)`),max(DN$`LF (cm)`),length.out=100)
poids_predits <- a*longueur_seq^b
lines(longueur_seq,poids_predits,col="blue",lwd=2)

# Ajout de l'equation sur le graphique 
text(x= max(DN$`LF (cm)`)*0.7,
     y= max(DN$`W (kg)`)*0.8,
     labels = equation,
     col = "red",
     cex = 1.2)
r_squared <- summary(modele)$r.squared
r_squared_text <- paste("R2 = ",round(r_squared),seq="")


#calcule statistique t student
b <-coef(modele)[2]
sb <- summary(modele)$coefficients[2,2]
n <-nrow(DN)
t_value <- abs((b-3)/(sb/sqrt(n)))
t_value
print(t_value)

test_spearman <- cor.test(DN$`LF (cm)`,DN$`W (kg)`,method = "spearman")
print(test_spearman)
 
Transformation des données en fréquence de tailles

Length Class	Class Center	2023-10	2023-11	2023-12	2024-01	2024-02	2024-03
10-20	15	0	0	0	10	0	0
20-30	25	0	0	0	10	0	0
30-40	35	0	2	0	1	0	0
40-50	45	0	16	16	0	0	2
50-60	55	1	26	40	0	1	28
60-70	65	3	82	186	20	130	192
70-80	75	1	89	124	0	38	97
80-90	85	1	23	7	0	1	12
90-100	95	1	19	11	0	0	1
100-110	105	0	0	2	0	0	0



 
Relation Taille-Poids 
#transformation logarithmique pour la longueur et le poids
DN$log_longueur <- log(DN$`LF (cm)`)
DN$log_poids <- log(DN$`W (kg)`)
#Ajustement d'un modele lineaire pour le log(pods)~log(taille)
modele <- lm(log_poids~log_longueur,data = DN)
#resumé du model 
summary(modele)
#Estraction des coefficient a et b
coefficients <- coef(modele)
a <- exp(coefficients[1]) # a est l'exponentiel de l'intercept
b <- coefficients[2] # b  est la pente
cat("a=",a,"\n")
cat("b=",b,"\n")
## tracer la relation taille_poids sur l'echelle logarithmique
plot (DN$`LF (cm)`,DN$`W (kg)`,
      xlab ="Longueur à la fourche en cm",
      ylab = "poids en kg",
      main="Relation Taille-poids",
      pch=16)
# Ajoute de la courbe ajuster à partir  du modele
longueur_seq <- seq(min(DN$`LF (cm)`),max(DN$`LF (cm)`),length.out=100)
poids_predits <- a*longueur_seq^b
lines(longueur_seq,poids_predits,col="blue",lwd=2)

# Ajout de l'equation sur le graphique 
text(x= max(DN$`LF (cm)`)*0.7,
     y= max(DN$`W (kg)`)*0.8,
     labels = equation,
     col = "red",
     cex = 1.2)
r_squared <- summary(modele)$r.squared
r_squared_text <- paste("R2 = ",round(r_squared),seq="")

# test de spearman 
test_spearman <- cor.test(DN$`LF (cm)`,DN$`W (kg)`,method = "spearman")
print(test_spearman)
#resumé du modele
summary(modele)
#tester si b est differents de 3 (test de Walk)
b_estme <-coef(modele)["log(LF cm)"]
SE_b <- summary(modele)$ coefficients ["log(LF cm)"]
Z <- (b_estme)-3/sb
p_value <- 2*(1-pnorm(abs(Z)))
p_value
confint(modele)
  

library(TropFishR)
# Longueurs médianes des classes
midLengths <- c(15.5, 25.5, 35.5, 45.5, 55.5, 65.5, 75.5, 85.5, 95.5, 105.5)

# Fréquences par mois pour chaque classe de taille (y compris mars)
catch_matrix <- matrix(c(
  0, 0, 0, 0, 1, 3, 1, 1, 1, 0,  # Oct 2023
  0, 0, 2, 16, 26, 82, 89, 23, 19, 0,  # Nov 2023
  0, 0, 0, 16, 40, 186, 124, 7, 11, 2,  # Dec 2023
  10, 10, 1, 0, 0, 20, 0, 0, 0, 0,   # Jan 2024
  0, 0, 0, 0, 1, 130, 38, 1, 0, 0,   # Feb 2024
  0, 0, 0, 2, 28, 192, 97, 12, 1, 0  # Mar 2024
), nrow = length(midLengths), ncol = 6, byrow = FALSE)

# Dates correspondant aux mois
dates <- as.Date(c("2023-10-01", "2023-11-01", "2023-12-01", "2024-01-01", "2024-02-01", "2024-03-01"))
# Structurer les données pour ELEFAN
lfq_data <- list(
  midLengths = midLengths,
  catch = catch_matrix,
  dates = dates
)
# Exécuter ELEFAN
results <- ELEFAN(lfq_data)
plot(results)

results

# Afficher les résultats des paramètres de croissance
results$par
# Visualiser les résultats de la courbe de croissance
plot(results)
str(results$score_mat)
K_values <- seq(0.1, 1, length.out = length(results$score_mat))
plot(K_values, results$score_mat, type = "l", col = "blue",
     xlab = "Growth constant K (/year)", ylab = "Score", main = "K-Scan")

# Ajouter une grille pour rendre plus lisible
grid()

# tracé des fréquences par classe de taille
matplot(lfq_data$midLengths, lfq_data$catch, type = "b", pch = 1, col = 1:6,
        xlab = "Longueur (cm)", ylab = "Fréquence",
        main = "Fréquences de capture par classe de taille")
legend("topright", legend = as.character(lfq_data$dates), col = 1:6, pch = 1)


# Extraire les paramètres de croissance L∞ et K à partir des résultats ELEFAN
Linf <- results$par$Linf   # Longueur asymptotique L∞
K <- results$par$K         # Coefficient de croissance K

# Calcul de l'indice de performance phi'
phi_prime <- log10(K) + 2 * log10(Linf)

# Afficher le résultat
phi_prime

# Vérifiez les résultats du modèle ELEFAN
results$par$t0

#Calcule de to
# Calcul de log10(-t0) selon la formule
log10_t0 <- -0.3922 - 0.2752 * log10(Linf) - 1.038 * log10(K)

# Résoudre pour t0
t0 <- -10^log10_t0

# Afficher le résultat de t0
t0

# Longueurs médianes observées
midLengths <- c(15.5, 25.5, 35.5, 45.5, 55.5, 65.5, 75.5, 85.5, 95.5, 105.5)

# Calculer l'âge correspondant à chaque longueur médiane
ages <- t0 - (1 / K) * log(1 - (midLengths / Linf))

# Afficher les âges correspondants
ages
# Créer un data frame avec les MidLengths et les âges calculés
df <- data.frame(MidLengths = midLengths, Age = ages)

lengths_seq <- Linf * (1 - exp(-K * (ages - t0)))

# Tracer la courbe de croissance théorique
plot(df$Age ,df$MidLengths , type = "l", col = "blue", lwd = 2,
     xlab = "Âge (années)", ylab = "MiLongueur à la fourche-cm", 
     main = "Courbe de croissance de von Bertalanffy")

# Ajouter les points correspondant aux longueurs médianes observées
points(df$Age, df$MidLengths, col = "red", pch = 19)

# Ajouter une légende pour différencier la courbe théorique des points observés
legend("bottomright", legend = c("Courbe de croissance", "MidLengths observés"), 
       col = c("blue", "red"), lty = 1, pch = 19)

### Estimation des parameter d’exploitation## 

moy<- mean(data$`LF (cm)`)
min<- min(data$`LF (cm)`)
max<- max(data$`LF (cm)`)
# Calculer les quantiles pour la colonne LF (cm)
quantiles <- quantile(data$`LF (cm)`, probs = c(0.25, 0.50, 0.75))

# Afficher les quantiles
quantiles

# Tracer l'histogramme des longueurs (LF (cm))
hist(data$`LF (cm)`, breaks = 20, main = "Distribution des longueurs des poissons", xlab = "Longueur (cm)", ylab = "Fréquence")
# Ajouter une ligne verticale pour L' = 60 cm
abline(v = 60, col = "red", lwd = 2, lty = 2)
# Calculer la proportion de poissons ayant une longueur supérieure à 60 cm
proportion_above_60 <- sum(data$`LF (cm)` > 60) / nrow(data)
# Afficher la proportion
proportion_above_60
# Calculer les quantiles pour la longueur des poissons (LF (cm))
quantiles <- quantile(data$`LF (cm)`, probs = c(0.25, 0.50, 0.75))
# Afficher les quantiles
quantiles
# Filtrer les poissons dont la longueur est supérieure à L' = 60 cm
filtered_data <- data[data$`LF (cm)` > 60, ]
# Calculer L_m (la longueur moyenne des poissons capturés au-dessus de 60 cm)
L_m <- mean(filtered_data$`LF (cm)`)

# Afficher L_m
L_m
L_prime <- 60

# Calculer Z à l'aide de la formule de Beverton et Holt
Z <- K * (Linf - L_m) / (L_m - L_prime)

# Afficher Z
Z
T <- 28

# Calculer les logarithmes
log_Linf <- log10(Linf)
log_K <- log10(K)
log_T <- log10(T)

# Appliquer la formule de Pauly pour log10(M)
log_M <- -0.0066 - 0.279 * log_Linf + 0.6543 * log_K + 0.4634 * log_T

# Calculer M en exponentiant le résultat
M <- 10^log_M

# Afficher M
M
#Mortalité par pêche 
#Z=F+M F=Z-M
F <- Z-M
F

#exploitation E=F/Z

E<- F/Z
E

# Somme des captures par classe de taille
total_catch <- rowSums(lfq_data$catch)

# Vérification des captures agrégées
print(total_catch)
param <- list(
  midLengths = lfq_data$midLengths,  # Les classes de taille médianes
  Linf = Linf,                       # Paramètre Linf de croissance
  K = results$par$K,                 # Coefficient de croissance
  t0 = t0,                           # Théorique t0 (optionnel, par défaut -1)
  M = M,                             # Mortalité naturelle calculée
  a = 0.0000127,                             # Coefficient de la relation poids-longueur
  b = 2.95,                             # Exposant de la relation poids-longueur
  catch = total_catch                # Captures agrégées par classe de taille
)


vpa_results <- VPA(
  param = param,
  terminalF = 1.18,      # Mortalité par pêche terminale
  catch_columns = NA,   # Non utilisé si `catch` est un vecteur
  analysis_type = "VPA", # Type d'analyse (par défaut VPA)
  plot = TRUE           # Afficher les graphiques
)
# Ajouter ylim pour ajuster l'axe des ordonnées
plot(vpa_results, ylim = c(0, 7000))  # Adaptez 10000 à une valeur plus réaliste pour votre cas
plot(vpa_results, ylim = c(0, 70000)), ylab = "Population (individus)")

# Visualisation des résultats de la VPA
plot(vpa_results$midLengths, vpa_results$numbers, type = "b", col = "green",
     main = "Analyse de population virtuelle (VPA)",
     xlab = "Classes de longueur", ylab = "Abondance estimée")


















ANNEXE

Estimation des parameter d’exploitation 

moy<- mean(data$`LF (cm)`)
min<- min(data$`LF (cm)`)
max<- max(data$`LF (cm)`)
# Calculer les quantiles pour la colonne LF (cm)
quantiles <- quantile(data$`LF (cm)`, probs = c(0.25, 0.50, 0.75))

# Afficher les quantiles
quantiles

# Tracer l'histogramme des longueurs (LF (cm))
hist(data$`LF (cm)`, breaks = 20, main = "Distribution des longueurs des poissons", xlab = "Longueur (cm)", ylab = "Fréquence")
# Ajouter une ligne verticale pour L' = 60 cm
abline(v = 60, col = "red", lwd = 2, lty = 2)

 

# Calculer la proportion de poissons ayant une longueur supérieure à 60 cm
proportion_above_60 <- sum(data$`LF (cm)` > 60) / nrow(data)
# Afficher la proportion
proportion_above_60
proportion_above_60 <- sum(data$`LF (cm)` > 60) / nrow(data)
> # Afficher la proportion
> proportion_above_60
[1] 0.8717519
# Calculer les quantiles pour la longueur des poissons (LF (cm))
quantiles <- quantile(data$`LF (cm)`, probs = c(0.25, 0.50, 0.75))
# Afficher les quantiles
quantiles
# Calculer les quantiles pour la longueur des poissons (LF (cm))
> quantiles <- quantile(data$`LF (cm)`, probs = c(0.25, 0.50, 0.75))
> # Afficher les quantiles
> quantiles
     25%      50%      75% 
62.37042 67.09770 72.11867


# Filtrer les poissons dont la longueur est supérieure à L' = 60 cm
filtered_data <- data[data$`LF (cm)` > 60, ]
# Calculer L_m (la longueur moyenne des poissons capturés au-dessus de 60 cm)
L_m <- mean(filtered_data$`LF (cm)`)

# Afficher L_m
L_m
# Filtrer les poissons dont la longueur est supérieure à L' = 60 cm
> filtered_data <- data[data$`LF (cm)` > 60, ]
> View(filtered_data)
> # Calculer L_m (la longueur moyenne des poissons capturés au-dessus de 60 cm)
> L_m <- mean(filtered_data$`LF (cm)`)
> # Afficher L_m
> L_m
[1] 69.76728

# Définir les paramètres
K <- 0.49
L_inf <- 108.05
L_prime <- 60

# Calculer Z à l'aide de la formule de Beverton et Holt
Z <- K * (L_inf - L_m) / (L_m - L_prime)

# Afficher Z
Z
T <- 28

# Calculer les logarithmes
log_L_inf <- log10(L_inf)
log_K <- log10(K)
log_T <- log10(T)

# Appliquer la formule de Pauly pour log10(M)
log_M <- -0.0066 - 0.279 * log_L_inf + 0.6543 * log_K + 0.4634 * log_T

# Calculer M en exponentiant le résultat
M <- 10^log_M

# Afficher M
M
#Mortalité par pêche 
#Z=F+M F=Z-M
F <- Z-M
F

 #exploitation E=F/Z

E<- F/Z
E



# Étape 1 : Préparer les données
Recrutement
mois <- c("Oct-23", "Nov-23", "Dec-23", "Janv-24", "Fev-24", "Mars-24")
recrutement <- c(7, 257, 386, 41, 170, 332)

# Étape 2 : Convertir le recrutement en pourcentage
total_recrutement <- sum(recrutement)
recrutement_pourcentage <- (recrutement / total_recrutement) * 100

# Étape 3 : Créer l'histogramme avec les pourcentages
barplot(recrutement_pourcentage, names.arg = mois, col = "yellow", border = "black", 
        ylim = c(0, 40),  # Limiter l'axe Y entre 0 et 40 pour les pourcentages
        xlab = "Mois", ylab = "Recrutement (%)", main = "Recrutement par Mois (en %)")

# Étape 4 : Utiliser LOESS avec un paramètre span pour lisser la courbe
x_vals <- 1:length(mois)
loess_fit <- loess(recrutement_pourcentage ~ x_vals, span = 1)  # Augmenter span pour lisser la courbe
smoothed_vals <- predict(loess_fit)

spline_fit <- spline(x_vals, recrutement_pourcentage, n = 100)  # Augmenter 'n' pour lisser
lines(spline_fit$x, spline_fit$y, col = "red", lwd = 2, type = "l")



# Étape 1 : Préparer les données
mois <- c("Oct-23", "Nov-23", "Dec-23", "Janv-24", "Fev-24", "Mars-24")
recrutement <- c(7, 257, 386, 41, 170, 332)

# Étape 2 : Convertir le recrutement en pourcentage
total_recrutement <- sum(recrutement)
recrutement_pourcentage <- (recrutement / total_recrutement) * 100

# Étape 3 : Créer l'histogramme avec l'axe Y limité à 40%
barplot(recrutement_pourcentage, names.arg = mois, col = "yellow", border = "black", 
        ylim = c(0, 40),  # Limiter l'axe Y entre 0 et 40 pour les pourcentages
        xlab = "Mois", ylab = "Recrutement (%)", main = "Recrutement par Mois (en %)")

# Étape 4 : Ajouter une courbe lissée (Spline ou LOESS)
x_vals <- 1:length(mois)
loess_fit <- loess(recrutement_pourcentage ~ x_vals)  # LOESS pour lisser la courbe
smoothed_vals <- predict(loess_fit)

# Ajouter la courbe lissée (ligne bleue) sur l'histogramme
lines(x_vals, smoothed_vals, col = "red", lwd = 2, type = "l")  # 'type = "l"' pour une ligne

# Étape 1 : Préparer les données
mois <- c("Oct-23", "Nov-23", "Dec-23", "Janv-24", "Fev-24", "Mars-24")
recrutement <- c(7, 257, 386, 41, 170, 332)

# Étape 2 : Convertir le recrutement en pourcentage
total_recrutement <- sum(recrutement)
recrutement_pourcentage <- (recrutement / total_recrutement) * 100

# Étape 3 : Créer l'histogramme avec les pourcentages
barplot(recrutement_pourcentage, names.arg = mois, col = "yellow", border = "black", 
        ylim = c(0, 40),  # Limiter l'axe Y entre 0 et 40 pour les pourcentages
        xlab = "Mois", ylab = "Recrutement (%)", main = "Recrutement par Mois (en %)")

# Étape 4 : Utiliser LOESS avec un paramètre span pour lisser la courbe
x_vals <- 1:length(mois)
loess_fit <- loess(recrutement_pourcentage ~ x_vals, span = 1)  # Augmenter span pour lisser la courbe
smoothed_vals <- predict(loess_fit)

spline_fit <- spline(x_vals, recrutement_pourcentage, n = 100)  # Augmenter 'n' pour lisser
lines(spline_fit$x, spline_fit$y, col = "red", lwd = 2, type = "l")


Selectivité des engine de pêche

DN<-data
library(dplyr)
library(ggplot2)
# Calcul de la fréquence cumulée
DN_cum <- DN %>%
  group_by(DN$Engin, DN$`LF (cm)`) %>%
  summarise(freq = n()) %>%
  mutate(cum_freq = cumsum(freq) / sum(freq)) %>%
  ungroup()

ggplot(DN_cum, aes(x = `DN$\`LF (cm)\``, y = cum_freq, color =DN_cum$`DN$Engin`)) +
  geom_line(size = 1.2) +
  labs(title = "Courbes Sigmoïdes par Engin", x = "Longueur à la fourche (cm)", y = "Fréquence Cumulée") +
  theme_minimal()+facet_wrap(~DN_cum$`DN$Engin`, scales = "free_y")


L50_values <- DN_cum %>%
  group_by(DN_cum$`DN$Engin`) %>%
  summarise(L50 = approx(cum_freq, `DN$\`LF (cm)\``, xout = 0.5)$y)



ggplot(DN_cum, aes(x = `DN$\`LF (cm)\``, y = cum_freq, color =DN_cum$`DN$Engin`)) +
  geom_line(size = 1.2) +
  geom_vline(data = L50_values,aes(xintercept = L50), linetype = "dashed", color = "black")+
  labs(title = "Courbes Sigmoïdes par Engin", x = "Longueur à la fourche (cm)", y = "Fréquence Cumulée") +
  theme_minimal()+facet_wrap(~DN_cum$`DN$Engin`, scales = "free_y")

library(dplyr)
library(ggplot2)

# Supposons que tes données sont dans un dataframe DN_cum avec des colonnes "Engin", "LF (cm)", et "cum_freq"
# Si tu n'as pas les fréquences cumulées, il faut d'abord les calculer.

DN_cum <- DN %>%
  group_by(Engin, `LF (cm)`) %>%
  summarise(freq = n()) %>%
  mutate(cum_freq = cumsum(freq) / sum(freq)) %>%
  ungroup()

# Calculer L25, L75, et L50 par interpolation
L_values <- DN_cum %>%
  group_by(Engin) %>%
  summarise(
    L50 = approx(cum_freq, `LF (cm)`, xout = 0.5)$y,  # L50, fréquence cumulative de 50%
    L25 = approx(cum_freq, `LF (cm)`, xout = 0.25)$y,  # L25, fréquence cumulative de 25%
    L75 = approx(cum_freq, `LF (cm)`, xout = 0.75)$y   # L75, fréquence cumulative de 75%
  )

# Calculer le paramètre r pour chaque engin
L_values <- L_values %>%
  mutate(
    r = 2 / (L75 - L25),
    a = 2*log(3)/(L75 - L25)
  )

print(L_values)

INFLUENCE DE TAILLE DES CAPTURES PAR LES ENGINS DE PÊCHE

str(data)
summary(data$`LF (cm)`)
unique(data$Engin)
kruskal_test <- kruskal.test(`LF (cm)` ~ Engin, data = data)
# Extraire les informations principales dans un tableau
kruskal_results <- data.frame(
  Statistic = kruskal_test$statistic,
  Degrees_of_Freedom = kruskal_test$parameter,
  P_Value = kruskal_test$p.value
)

# Afficher le tableau
print(kruskal_results)


print(kruskal_test)
install.packages("dplyr")
install.packages("rcompanion")
library(dplyr)
library(rcompanion)
pairwise_tests <- pairwise.wilcox.test(
  data$`LF (cm)`,
  data$Engin,
  p.adjust.method = "bonferroni"
)
print(pairwise_tests)

# Extraire les p-valeurs ajustées sous forme de matrice
p_matrix <- pairwise_tests$p.value

# Transformer la matrice en un tableau structuré
library(reshape2)  # Pour manipuler les matrices
results_table <- melt(p_matrix, na.rm = TRUE)
colnames(results_table) <- c("Engin_1", "Engin_2", "Adjusted_P_Value")

# Afficher le tableau final
print(results_table)




boxplot(`LF (cm)` ~ Engin, data = data,
        main = "Distribution des tailles par type d'engin",
        xlab = "Type d'engin",
        ylab = "Taille des poissons (cm)")
heatmap(as.matrix(pairwise_tests$p.value), 
        Rowv = NA, Colv = NA, 
        scale = "none", 
        main = "p-values ajustées")

ESTIMATION DE LA TAILLE DE MATURITE SEXUELLE


# Installer les packages nécessaires (si non installés)
install.packages("readxl")      # Pour lire des fichiers Excel
install.packages("dplyr")       # Pour manipuler les données
install.packages("ggplot2")     # Pour les visualisations
install.packages("minpack.lm")  # Pour ajuster des modèles non linéaires

# Charger les librairies
library(readxl)
library(dplyr)
library(ggplot2)
library(minpack.lm)
# Ajouter une colonne "Mature" selon les catégories
data <- data %>%
  mutate(
    Mature = case_when(
      Sexe == "F" ~ 1,
      Sexe == "M" ~ 1,
      Sexe == "I" ~ 0,
      Sexe == "ND" ~ NA_real_
    )
  ) %>%
  filter(!is.na(Mature))  # Exclure les ND (Non Identifiés)


# Créer des classes de taille (bins de 5 cm)
data <- data %>%
  mutate(SizeClass = cut(`LF (cm)`, breaks = seq(0, max(`LF (cm)`, na.rm = TRUE) + 5, 5)))

# Calculer les proportions de maturité par classe de taille
proportion_maturity <- data %>%
  group_by(SizeClass) %>%
  summarise(
    MidPoint = mean(as.numeric(sub("\\((.+),(.+)\\]", "\\1", as.character(SizeClass))) +
                      as.numeric(sub("\\((.+),(.+)\\]", "\\2", as.character(SizeClass))) / 2, na.rm = TRUE),
    ProportionMature = mean(Mature)
  ) %>%
  filter(!is.na(MidPoint))  # Exclure les classes sans individus


# Vérifiez les données utilisées pour le modèle
print(proportion_maturity)

# Assurez-vous que 'MidPoint' et 'ProportionMature' contiennent des valeurs numériques et non NA
summary(proportion_maturity)
# Ajouter un décalage aux proportions égales à 0 ou 1
install.packages("minpack.lm")
logistic <- function(x, a, b) {
  1 / (1 + exp(-(a + b * x)))
}

library(minpack.lm)  # Assurez-vous que ce package est installé

# Ajouter un décalage pour éviter les proportions égales à 0 ou 1
proportion_maturity <- proportion_maturity %>%
  mutate(
    ProportionMature = ifelse(ProportionMature == 0, 0.001, ProportionMature),
    ProportionMature = ifelse(ProportionMature == 1, 0.999, ProportionMature)
  )

# Ajuster le modèle logistique avec nlsLM
fit <- nlsLM(ProportionMature ~ logistic(MidPoint, a, b),
             data = proportion_maturity,
             start = list(a = -5, b = 0.1))  # Valeurs initiales ajustables

# Résumé des paramètres ajustés
summary(fit)
L50 <- -coef(fit)["a"] / coef(fit)["b"]
print(L50)

# Calcul de L50
a <- coef(fit)["a"]
a
b <- coef(fit)["b"]
b
L50 <- -a / b
print(paste("L50 (taille à maturité) :", round(L50, 2), "cm"))

# Prédictions basées sur le modèle ajusté
proportion_maturity$Predicted <- predict(fit)

# Graphique
ggplot(proportion_maturity, aes(x = MidPoint, y = ProportionMature)) +
  geom_point(color = "blue", size = 3) +
  geom_line(aes(y = Predicted), color = "red", size = 1) +
  labs(
    title = "Ajustement de la courbe logistique",
    x = "Taille (MidPoint)",
    y = "Proportion Mature"
  ) +
  theme_minimal()
# Tracer la courbe et les points observés
ggplot(proportion_maturity, aes(x = MidPoint, y = ProportionMature)) +
  geom_point(color = "blue",size = 3) +
  geom_line(aes(y = Predicted), color = "red",size = 1) +
  geom_vline(xintercept = L50, linetype = "dashed", color = "green",size = 0.5) +
  labs(title = "Courbe de maturité et estimation de L50",
       x = "Longueur (cm)",
       y = "Proportion mature") +
  theme_minimal()

fit_glm <- glm(ProportionMature ~ MidPoint, family = binomial(link = "logit"), data = proportion_maturity)

# Calcul de L50
LF50_glm <- -coef(fit_glm)[1] / coef(fit_glm)[2]
print(paste("LF50 avec glm :", round(LF50_glm, 2), "cm"))

# Résumé du modèle
summary(fit_glm)
LF50_glm <- -coef(fit_glm)[1] / coef(fit_glm)[2]
print(paste("LF50 avec glm :", round(LF50_glm, 2), "cm"))
# Créer des points pour tracer la courbe
curve_data <- data.frame(
  MidPoint = seq(min(proportion_maturity$MidPoint), max(proportion_maturity$MidPoint), length.out = 500)
)
curve_data$Predicted <- predict(fit_glm, newdata = curve_data, type = "response")
library(ggplot2)

# Graphique des proportions observées et de la courbe ajustée
ggplot(proportion_maturity, aes(x = MidPoint, y = ProportionMature)) +
  geom_point(color = "blue", size = 3) +  # Points observés
  geom_line(data = curve_data, aes(x = MidPoint, y = Predicted), color = "red", size = 1) +  # Courbe ajustée
  geom_vline(xintercept = LF50_glm, linetype = "dashed", color = "green") +  # LF50
  labs(
    title = "Ajustement de la courbe logistique avec glm",
    x = "Taille (MidPoint)",
    y = "Proportion Mature"
  ) +
  theme_minimal()

ggplot(proportion_maturity, aes(x = MidPoint, y = ProportionMature)) +
  geom_point(color = "blue", size = 3) +  # Points observés
  geom_line(data = curve_data, aes(x = MidPoint, y = Predicted), color = "red", size = 1) +  # Courbe ajustée
  geom_vline(xintercept = LF50_glm, linetype = "dashed", color = "green", size = 0.5, vjust = -0.5) +  # L50
  labs(
    title = "Ajustement de la courbe logistique avec glm",
    x = "Taille (MidPoint)",
    y = "Proportion Mature"
  ) +
  xlim(0, 110) +  # Limite l'axe des abscisses à 110
  theme_minimal()

ggplot(proportion_maturity, aes(x = MidPoint, y = ProportionMature)) +
  geom_point(color = "blue", size = 3) +  # Points observés
  geom_line(data = curve_data, aes(x = MidPoint, y = Predicted), color = "red", size = 1) +  # Courbe ajustée
  geom_vline(xintercept = LF50_glm, linetype = "dashed", color = "green", size = 0.5, vjust = -0.5) +  # L50
  labs(
    title = "Ajustement de la courbe logistique avec glm",
    x = "Taille (MidPoint)",
    y = "Proportion Mature"
  ) +
  xlim(60, 110) +  # Limite l'axe des abscisses à 110
  theme_classic() +  # Choix d'un thème
  theme(
    legend.position = "none",  # Suppression de la légende
    axis.title = element_text(size = 12),  # Taille des titres d'axes
    axis.text = element_text(size = 10)  # Taille des étiquettes d'axes
  )

ggplot(proportion_maturity, aes(x = MidPoint, y = ProportionMature)) +
  geom_point(color = "blue", size = 3) +  # Points observés
  geom_line(data = curve_data, aes(x = MidPoint, y = Predicted), color = "black", size = 1) +  # Courbe ajustée
  geom_vline(xintercept = LF50_glm, linetype = "dashed", color = "green", size = 0.5) +  # L50
  geom_segment(aes(x = L50_glm, xend = L50_glm + 5, y = 0.05, yend = 0.05), color = "black", size = 0.5, arrow = arrow(length = unit(0.2, "cm"))) +  # Segment horizontal avec flèche
  geom_text(x = LF50_glm + 6, y = 0.05, label = paste("LF50 =", round(LF50_glm, 1)), hjust = 0, vjust = 0.5) +  # Texte à droite du segment, arrondi à 1 décimale
  labs(
    title = "Ajustement de la courbe logistique avec glm",
    x = "Taille LF (MidPoint)",
    y = "Proportion Mature"
  ) +
  xlim(60, 100) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )



EFFORT DE PÊCHE
data <- DN_aVEC_HEURE_MODIFIER
# Charger les bibliothèques nécessaires
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(cluster)
library(corrplot)
library(ggplot2)
# 2. Nettoyage et préparation des données
# Uniformiser les valeurs de la colonne MD (mode de propulsion)
data$MD <- tolower(data$MD)
data$MD <- ifelse(data$MD %in% c("moteur", "motorisé"), "Moteur",
                  ifelse(data$MD %in% c("voile", "pagaie"), "Voile", NA))

# Conversion des dates
data$DD_HD <- as.POSIXct(data$DD_HD, format = "%Y-%m-%d %H:%M:%S")
data$DA_HA <- as.POSIXct(data$DA_HA, format = "%Y-%m-%d %H:%M:%S")

# Calcul du temps total en mer
data$Time_Total_Hours <- as.numeric(difftime(data$DA_HA, data$DD_HD, units = "hours"))

# Temps de parcours selon le mode de propulsion et l'engin
data$Time_Parcours <- ifelse(data$MD == "Moteur", 2, 
                             ifelse(data$MD == "Voile" & data$Engin != "PH", 4, 
                                    ifelse(data$Engin == "PH", 0.67, NA)))

# Temps de pêche réel
data$Time_Peche_Heures <- data$Time_Total_Hours - data$Time_Parcours

# Effort ajusté (temps de pêche réel x nombre de pêcheurs)
data$Effort_Ajuste <- data$Time_Peche_Heures * data$`Nbr P/E`

# 3. Calcul de la CPUE brute
summary_data <- data %>%
  group_by(`Lieu d'embarquement`, Engin, MD) %>%
  summarise(
    Total_Catch_Weight = sum(`Toatal-Capt-kg`, na.rm = TRUE),
    Total_Effort_Ajuste = sum(Effort_Ajuste, na.rm = TRUE)
  ) %>%
  mutate(CPUE_Brute = Total_Catch_Weight / Total_Effort_Ajuste)

# 4. Visualisation des CPUE brutes
ggplot(summary_data, aes(x = Engin, y = CPUE_Brute, fill = Engin)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~`Lieu d'embarquement`) +
  labs(title = "CPUE Brute par Engin et point de débarquement",
       x = "Type d'Engin", y = "CPUE Brute (kg/heure-pêcheur)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

graphics.off()

# 5. Standardisation de l'effort
max_cpue <- max(summary_data$CPUE_Brute, na.rm = TRUE)
summary_data <- summary_data %>%
  mutate(Facteur_Correction = max_cpue / CPUE_Brute)

# Joindre le facteur de correction aux données originales
data <- data %>%
  left_join(select(summary_data, Engin, Facteur_Correction), by = "Engin")

# Calcul de l'effort standardisé
data$Effort_Standardise <- data$Effort_Ajuste * data$Facteur_Correction

# 6. Analyse avec effort standardisé
standardized_summary <- data %>%
  group_by(`Lieu d'embarquement.x`, Engin) %>%
  summarise(
    Total_Catch_Weight = sum(`Toatal-Capt-kg`, na.rm = TRUE),
    Total_Effort_Standardise = sum(Effort_Standardise, na.rm = TRUE)
  ) %>%
  mutate(CPUE_Standardisee = Total_Catch_Weight / Total_Effort_Standardise)

# Visualisation des CPUE standardisées
ggplot(standardized_summary, aes(x = Engin, y = CPUE_Standardisee, fill = Engin)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~`Lieu d'embarquement.x`) +
  labs(title = "CPUE Standarséé par engin et point de débarquement",
       x = "Type d'Engin", y = "CPUE Standardisée (kg/heure-pêcheur)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Analyse des relations entre variables
# Relation entre l'effort ajusté et les captures totales
relation_plot <- ggplot(data, aes(x = Effort_Ajuste, y = `Toatal-Capt-kg`, color = Engin)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Relation entre l'Effort Ajusté et les Captures",
       x = "Effort Ajusté (heures-pêcheurs)",
       y = "Captures Totales (kg)") +
  theme_minimal()

# Affichage
print(relation_plot)

# 8. Analyse des résidus et ajustements
model <- glm(CPUE_Brute ~ Engin + MD, data = summary_data, family = gaussian)
library(broom)
library(broom)
print (model)
table(data$Engin)  # Comptage des occurrences par catégorie

#Valeurs équilibrées et résidus 
#Créer un tableau des valeurs ajustées et des résidus
results <- data.frame(
  Observed = model$y,
  Fitted = model$fitted.values,
  Residuals = model$residuals
)
# Visualiser les premières lignes
head(results)
# Graphique Résidus vs Valeurs ajustées
plot(model$fitted.values, model$residuals,
     xlab = "Valeurs ajustées", ylab = "Résidus",
     main = "Résidus vs Valeurs ajustées")
abline(h = 0, col = "red", lty = 2)

# distribution des residus du modele: Histogramme des résidus
hist(model$residuals, breaks = 10, col = "blue",
     main = "Distribution des Résidus",
     xlab = "Résidus")

#Donnné utilisé dans le modele
# Données utilisées dans le modèle
data_used <- model$model

# Visualiser les premières lignes
head(data_used)
# Résumé détaillé des coefficients
model_summary <- tidy(model)
# Ajouter des informations supplémentaires (statistiques globales)
model_glance <- glance(model)

# Afficher les résultats sous forme de tableau
print(model_summary)
print(model_glance)


plot(residuals(model), main = "Résidus du modèle")
qqnorm(residuals(model))
qqline(residuals(model))

# 9. Test de normalité et homogénéité
test_normalite <- shapiro.test(summary_data$CPUE_Brute)
print(test_normalite)
#transformation logarithmique
summary_data$CPUE_Brute_log <- log(summary_data$CPUE_Brute)
qqnorm(summary_data$CPUE_Brute_log)
qqline(summary_data$CPUE_Brute_log)
shapiro.test(summary_data$CPUE_Brute_log)

# Vérifier le nombre d'observations par groupe
summary_data%>%
  group_by(Engin) %>%
  summarise(N = n())

# Filtrer les groupes avec au moins deux observations
filtered_summary_data <- summary_data %>%
  group_by(Engin) %>%
  filter(n() > 1)
# Appliquer le test de Bartlett sur les données filtrées
bartlett_test <- bartlett.test(CPUE_Brute_log ~ Engin, data = filtered_summary_data)
print(bartlett_test)


anova_brute <- aov(CPUE_Brute ~ Engin, data = filtered_summary_data)
summary(anova_brute)
par(mfrow = c(2, 2))  # Afficher 4 graphiques sur une seule fenêtre
plot(anova_brute)      # Graphe des résidus
shapiro.test(resid(anova_brute))

names(summary_data) <- gsub("Lieu d'embarquement", "Lieu_embarquement", names(summary_data))
anova_result <- aov(CPUE_Brute ~ `Lieu d'embarquement` + Engin, data = summary_data)

# 10. ANOVA pour comparer les moyennes des CPUE
anova_result <- aov(CPUE_Brute ~ Lieu_embarquement + Engin, data = summary_data)
anova_summary <- summary(anova_result)
print(anova_summary)

TukeyHSD(anova_summary)
TukeyHSD(anova_interaction, "Lieu_embarquement")
TukeyHSD(anova_interaction, "Engin")
plot(TukeyHSD(anova_interaction, "Lieu_embarquement"), las = 1)  # Graphique des résultats pour Lieu_embarquement
plot(TukeyHSD(anova_interaction, "Engin"), las = 1) # Graphique des résultats pour Engin
table(summary_data$Lieu_embarquement)
table(summary_data$Engin)

kruskal.test(CPUE_Brute ~ Lieu_embarquement, data = summary_data)
kruskal.test(CPUE_Brute ~ Engin, data = summary_data)


boxplot(CPUE_Brute ~ Lieu_embarquement, data = summary_data, 
        main = "CPUE par Lieu d'Embarquement", xlab = "Lieu d'Embarquement", ylab = "CPUE Brute")

boxplot(CPUE_Brute ~ Engin, data = summary_data, 
        main = "CPUE par Type d'Engin", xlab = "Engin", ylab = "CPUE Brute")

anova_interaction <- aov(CPUE_Brute ~ Lieu_embarquement * Engin, data = summary_data)
summary(anova_interaction)


# 11. Exploration des interactions
interaction_model <- glm(CPUE_Brute ~ Engin * MD, data = summary_data, family = gaussian)
interaction_summary <- summary(interaction_model)
print(interaction_model)
print(interaction_summary)

# 12. Analyse des regroupements (clustering)
numeric_data <- summary_data %>%
  select(CPUE_Brute, Total_Effort_Ajuste) %>%
  filter(!is.na(CPUE_Brute) & !is.na(Total_Effort_Ajuste)) %>%
  filter(is.finite(CPUE_Brute) & is.finite(Total_Effort_Ajuste)) %>%
  mutate(across(everything(), as.numeric))
clustering <- kmeans(numeric_data, centers = 3)
summary_data$Cluster <- as.factor(clustering$cluster)

ggplot(summary_data, aes(x = Total_Effort_Ajuste, y = CPUE_Brute, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering des villages/engins")

ggplot(summary_data, aes(x = Total_Effort_Ajuste, y = CPUE_Brute, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering des villages/engins")

# 13. Corrélation entre variables
correlation_matrix <- cor(select(data, where(is.numeric)), use = "complete.obs")
corrplot(correlation_matrix, method = "circle")

# 14. Exporter les données finales
write.csv(standardized_summary, "CPUE_Standardisee_Resume.csv", row.names = FALSE)

# 15. Résumé graphique de l'évolution mensuelle
data$Month_Year <- format(data$DD_HD, "%Y-%m")
monthly_effort <- data %>%
  group_by(Month_Year, Engin) %>%
  summarise(Effort_Ajuste_Total = sum(Effort_Ajuste, na.rm = TRUE))
monthly_effort$Month_Year <- as.Date(paste0(monthly_effort$Month_Year, "-01"))

ggplot(monthly_effort, aes(x = Month_Year, y = Effort_Ajuste_Total, fill = Engin)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Évolution Mensuelle de l'Effort Ajusté de Pêche",
       x = "Mois",
       y = "Effort Ajusté Total (heures-pêcheurs)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Effort totale ajustée et CPUE Brute

 

 
# 📌 Installer et charger les packages nécessaires
install.packages("TropFishR")
install.packages("readxl")
library(TropFishR)
library(readxl)

# 📌 **1️⃣ Importation et structuration des données**
midLengths <- c(15.5, 25.5, 35.5, 45.5, 55.5, 65.5, 75.5, 85.5, 95.5, 105.5)

# Fréquences de capture par classe de taille
catch_matrix <- matrix(c(
  0, 0, 0, 0, 1, 3, 1, 1, 1, 0,  
  0, 0, 2, 16, 26, 82, 89, 23, 19, 0,  
  0, 0, 0, 16, 40, 186, 124, 7, 11, 2,  
  10, 10, 1, 0, 0, 20, 0, 0, 0, 0,   
  0, 0, 0, 0, 1, 130, 38, 1, 0, 0,   
  0, 0, 0, 2, 28, 192, 97, 12, 1, 0  
), nrow = length(midLengths), ncol = 6, byrow = FALSE)

# Dates correspondant aux mois
dates <- as.Date(c("2023-10-01", "2023-11-01", "2023-12-01", "2024-01-01", "2024-02-01", "2024-03-01"))

# Structurer les données pour ELEFAN
lfq_data <- list(midLengths = midLengths, catch = catch_matrix, dates = dates)

# 📌 **2️⃣ Identification des cohortes avec ELEFAN GA**
results_ga <- ELEFAN_GA(
  lfq = lfq_data, seasonalised = FALSE,  
  low_par = list(Linf = 100, K = 0.2, t_anchor = 0, C = 0, ts = 0),
  up_par = list(Linf = 112, K = 0.7, t_anchor = 1, C = 1, ts = 1),
  popSize = 100, maxiter = 200, plot = TRUE
)

# 📌 **3️⃣ Extraction des paramètres de croissance**
Linf <- results_ga$par$Linf
K <- results_ga$par$K
t0 <- -10^(-0.3922 - 0.2752 * log10(Linf) - 1.038 * log10(K))

# Calcul de l'indice de performance φ'
phi_prime <- log10(K) + 2 * log10(Linf)
print(paste("Indice de performance φ' :", phi_prime))

# 📌 **4️⃣ Détermination de Tc et Tr**
data <- read_excel("F:/AA/DN aVEC HEURE MODIFIER.xlsx")

L_prime <- quantile(data$`LF (cm)`, probs = 0.25)  # Premier quartile
L_r <- min(data$`LF (cm)`)  # Longueur minimale observée

T_c <- t0 - (1 / K) * log(1 - (L_prime / Linf))
T_r <- t0 - (1 / K) * log(1 - (L_r / Linf))

print(paste("Âge à la première capture (T_c) :", round(T_c, 2)))
print(paste("Âge au recrutement (T_r) :", round(T_r, 2)))

# 📌 **5️⃣ Estimation de la mortalité naturelle (M)**
Tmax <- 22  

M_jensen <- 1.5 * K
M_gislason <- mean(1.73 * midLengths^(-1.61) * Linf^(1.44) * K) 
M_beverton <- 4.118 * K^0.73 * Linf^(-0.331)
M_hoenig <- 4.31 * Tmax^(-1.01)
M_hewitt_hoenig <- 4.3 / Tmax

M_values <- c(M_jensen, M_gislason, M_beverton, M_hoenig, M_hewitt_hoenig)
M <- median(M_values)

print(paste("Valeur médiane de M :", round(M, 2)))

# 📌 **6️⃣ Calcul de Z et F**
Lm <- mean(data$`LF (cm)`[data$`LF (cm)` > L_prime])

Z_BH <- K * ((Linf - Lm) / (Lm - L_prime))
Z_Hoenig <- 10^(1.46 - 1.01 * log10(Tmax))
Z_Djabali <- Z_BH * 1.1  

Z_values <- c(Z_BH, Z_Hoenig, Z_Djabali)
Z <- median(Z_values)
F <- Z - M
E <- F / Z

cat("Mortalité totale (Z) :", Z, "\n")
cat("Mortalité par pêche (F) :", F, "\n")
cat("Taux d'exploitation (E) :", E, "\n")

# 📌 **7️⃣ Calcul de B/R et Y/R avec Beverton & Holt**
W_inf <- 2.5  
S <- exp(-K * (T_c - t0))

F_values <- seq(0, 5, length.out = 50)
Y_R_values <- numeric(length(F_values))
B_R_values <- numeric(length(F_values))

for (i in 1:length(F_values)) {
  F <- F_values[i]
  Z <- F + M  
  Y_R <- (F * exp(-M * (T_c - T_r)) * W_inf *
            (1 / Z - 3 * S / (Z + K) + 3 * S^2 / (Z + 2*K) - S^3 / (Z + 3*K)))
  B_R <- ifelse(F > 0, Y_R / F, NA)  
  
  Y_R_values[i] <- Y_R
  B_R_values[i] <- B_R
}

F_MSY <- F_values[which.max(Y_R_values)]

# 📌 Chargement des bibliothèques
library(ggplot2)

# 📌 Création du dataframe
df <- data.frame(F = F_values, Y_R = Y_R_values, B_R = B_R_values)

# 📌 Création du graphique final avec légende complète
ggplot(df, aes(x = F)) +
  # Courbe du rendement par recrue (Y/R) en BLEU
  geom_line(aes(y = Y_R, color = "Y/R"), size = 1.5) +
  
  # Courbe de la biomasse par recrue (B/R) en VERT
  geom_line(aes(y = B_R * (max(Y_R_values, na.rm = TRUE) / max(B_R_values, na.rm = TRUE)), color = "B/R"), size = 1.5) +
  
  # Ligne verticale pour F_MSY en NOIR POINTILLÉ
  geom_vline(aes(xintercept = F_MSY, color = "F_MSY = 1.6"), linetype = "dashed", size = 1.2) +
  
  # Ligne verticale pour F_ACTUELLE en VIOLET POINTILLÉ
  geom_vline(aes(xintercept = 1.71, color = "F_actuel = 1.71"), linetype = "dashed", size = 1.2) +
  
  # Ligne horizontale pour B_MSY en ROUGE POINTILLÉ
  geom_hline(aes(yintercept = B_MSY * (max(Y_R_values, na.rm = TRUE) / max(B_R_values, na.rm = TRUE)), color = "B_MSY = 15% Bv = 0.13"), 
             linetype = "dotted", size = 1.2) +
  
  # 📌 Définition des axes bien alignés
  scale_y_continuous(
    name = "Rendement par recrue (Y/R)",  # Axe gauche (bleu)
    limits = c(0, max(Y_R_values, na.rm = TRUE)),  # Ajustement Y/R
    sec.axis = sec_axis(~ . * (max(B_R_values, na.rm = TRUE) / max(Y_R_values, na.rm = TRUE)), 
                        name = "Biomasse par recrue (B/R)")  # Axe droit bien aligné
  ) +
  
  # 📌 Labels et légende
  labs(
    title = "Production de biomasse par recrue (B/R) et rendement par recrue (Y/R)",
    x = "Mortalité par pêche (F)"
  ) +
  
  # 📌 Personnalisation des couleurs et styles
  scale_color_manual(
    values = c("Y/R" = "blue", 
               "B/R" = "green4",
               "F_MSY = 1.6" = "black", 
               "F_actuel = 1.71" = "purple",
               "B_MSY = 15% Bv = 0.13" = "red"),
    labels = c("Y/R", "B/R", 
               expression(F[MSY] == 1.6), 
               expression(F[actuel] == 1.71), 
               expression(B[MSY] == "15% Bv = 0.13"))
  ) +
  
  # 📌 Amélioration du style général
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "blue", size = 16, face = "bold"),  # Axe Y gauche en bleu
    axis.title.y.right = element_text(color = "green4", size = 16, face = "bold"),  # Axe Y droit en vert
    axis.text = element_text(size = 14),  # Taille du texte des axes
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Centrage et taille du titre
    legend.position = "top",  # Placement de la légende en haut
    legend.title = element_blank(),  # Supprime le titre de la légende
    legend.text = element_text(size = 14)  # Taille du texte de la légende
  ) +
  
  # 📌 Ajustement des marges pour un affichage propre
  theme(plot.margin = margin(10, 10, 10, 10))
