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


