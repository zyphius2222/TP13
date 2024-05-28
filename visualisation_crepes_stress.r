# -*- coding: utf-8 -*-
# Chargement des bibliothèques nécessaires
library(ggplot2)

# Génération des données
set.seed(123)  # Pour la reproductibilité
jours <- 14:1
stress <- jours * 5  # Taux de stress augmente linéairement
crepes <- exp(jours / 4)  # Crêpes ingérées augmente exponentiellement

# Inverser l'ordre des crêpes pour correspondre aux jours décroissants
crepes <- rev(crepes)

# Création du dataframe
data <- data.frame(jours, stress, crepes)

# Calcul de la droite des moindres carrés
fit <- lm(crepes ~ jours, data = data)
eq <- paste0("y = ", round(coef(fit)[2], 2), "x + ", round(coef(fit)[1], 2))

# Création de la visualisation
p <- ggplot(data, aes(x = jours, y = crepes)) +
  geom_point(color = "darkgray", size = 3, aes(shape = "Points des données")) +  # Ajout des points gris foncé avec légende
  geom_line(color = "lightblue", size = 1, aes(linetype = "Courbe des données")) +  # Ajout de la courbe bleu clair avec légende
  geom_smooth(method = "lm", color = "darkblue", aes(linetype = "Droite des moindres carrés"), se = FALSE) +  # Ajout de la droite des moindres carrés en bleu foncé sans zone d'ombre
  labs(
    title = "Taux de crêpes à la Nutella ingérées en fonction du stress des examens",
    x = "Jours restants avant le premier examen",
    y = "Crêpes ingérées",
    caption = "Étude sur 2 semaines, jour par jour",
    shape = "Légende",
    linetype = "Légende"
  ) +
  annotate("text", x = 13, y = max(crepes), label = eq, color = "darkgray", size = 4, hjust = 0, 
           fontface = "bold", bg.color = "white", label.padding = unit(0.2, "lines"), label.r = unit(0.15, "lines"), label.size = 0.5) +  # Ajout de l'équation des moindres carrés encadrée
  theme_minimal() +
  scale_x_reverse() +  # Inversion de l'axe des x pour les jours décroissants
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "bottom"  # Positionnement de la légende en bas
  )

# Affichage du graphique
print(p)
