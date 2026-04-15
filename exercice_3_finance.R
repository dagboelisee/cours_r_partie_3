library(readxl)
library(dplyr)
library(tidyverse)

df_fin <- read_excel("data/personal_transactions.xlsx")


# Création des sous-ensembles (2 comptes et 2 mois favoris)

names(df_fin)
str(df_fin)
head(df_fin)

df_a_platinum <- df_fin %>% filter(`Account Name` == "Platinum Card")
df_a_silver <- df_fin %>% filter(`Account Name` == "Silver Card")

df_m_2018_01 <- df_fin %>% filter(Month == "2018-01")
df_m_2018_02 <- df_fin %>% filter(Month == "2018-02")


# Q3 : Fonction de filtrage
filtrer_depenses <- function(df) {
  stopifnot(is.data.frame(df)) # Vérification de l'argument [7]

  df_filtre <- df %>% filter(`Transaction Type` == "debit")
  return(df_filtre)
}

# Test : Combien de lignes sont filtrées ?
nrow(df_fin) - nrow(filtrer_depenses(df_fin)) #118

# Q4 : Fonction pour compter le nombre de transactions
compter_transactions <- function(df) {
  stopifnot(is.data.frame(df))
  return(nrow(df))
}

# Q5 : Fonction pour compter le nombre de catégories uniques (équivalent aux "boucles" [8])
compter_categories <- function(df) {
  stopifnot(is.data.frame(df))
  return(length(unique(df$Category)))
}

platinum_depenses <- filtrer_depenses(df_a_platinum)
silver_depenses <- filtrer_depenses(df_a_silver)
janvier_depenses <- filtrer_depenses(df_m_2018_01)
fevrier_depenses <- filtrer_depenses(df_m_2018_02)

compter_transactions(df_a_platinum)
compter_transactions(df_a_silver)
compter_transactions(df_m_2018_01)
compter_transactions(df_m_2018_02)

compter_categories(df_a_platinum)
compter_categories(df_a_silver)
compter_categories(df_m_2018_01)
compter_categories(df_m_2018_02)

# Q6 & Q7 : Trouver la dépense maximum [8, 9]
trouver_depense_max <- function(df) {
  stopifnot(is.data.frame(df))

  # On filtre d'abord les revenus pour ne chercher que dans les dépenses
  df <- filtrer_depenses(df)

  # On trouve la ligne avec le montant maximum
  max_row <- df %>% arrange(desc(Amount)) %>% slice(1)

  # On retourne une liste avec les informations demandées
  return(list(
    Compte = max_row$`Account Name`,
    Date = max_row$Date,
    Montant = max_row$Amount,
    Description = max_row$Description
  ))
}

# Q8 : Distribution par catégorie (équivalent aux jours de la semaine [9])
calcul_distribution_categorie <- function(df) {
  stopifnot(is.data.frame(df))

  df %>%
    filtrer_depenses() %>%
    group_by(Category) %>%
    summarise(Total = sum(Amount, na.rm = TRUE)) %>%
    arrange(desc(Total))
}

# Q9 : Visualisation de la distribution [10]
plot_distribution_categorie <- function(df) {
  stopifnot(is.data.frame(df))

  data_plot <- calcul_distribution_categorie(df)

  ggplot(data_plot, aes(x = reorder(Category, Total), y = Total)) +
    geom_col(fill = "steelblue") + # Diagramme en colonne [10]
    coord_flip() +
    labs(title = "Distribution des dépenses par catégorie", x = "Catégorie", y = "Montant Total") +
    theme_minimal()
}

plot_distribution_categorie(df_fin)

