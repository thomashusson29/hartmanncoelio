#***************ANALYSE UNIVARIEE FDR DE CONVERSION*****************

df_conversion <- df_conversion %>%
  rename(
    H_antibio_IV_PO = `H_antibio_IV_+_PO`,
    I_indication_poussee_compliquee = `I_indication_poussee_compliquee`,
    I_indication_immunodepression = `I_indication_immunodepression`,
    I_indi cation_hemorragie = `I_indication_hemorragie`
  )


# 🎯 Recode de la variable cible : conversion (dans df_HartmannCoelio)
df_conversion <- df_HartmannCoelio %>% 
  filter(I_voie_coelio == 1)  # uniquement ceux chez qui une coelioscopie a ete tentee

df_conversion$conversion <- factor(df_conversion$I_conversion, levels = c(0,1), labels = c("Non converti", "Converti"))

# 🔠 Variables explicatives = celles du tableau 1
vars <- cols_to_include_in_table1

# 🧼 Convertir les binaires numeriques en facteurs simples
df_conversion[vars] <- lapply(df_conversion[vars], function(x) {
  if (is.numeric(x) && length(unique(x)) <= 2) factor(x, ordered = FALSE) else x
})

# 🔍 Detection des variables avec moins de 2 modalites dans df_conversion
vars_uniques <- sapply(df_conversion[vars], function(x) length(unique(na.omit(x))))
vars_problematiques <- names(vars_uniques[vars_uniques < 2])
vars_problematiques

# ✅ Exclusion des variables avec < 2 modalites
vars_clean <- setdiff(vars, vars_problematiques)

# ⚙️ Regressions univariees logistiques sur la conversion
univ_models <- lapply(vars_clean, function(var) {
  model <- glm(as.formula(paste("conversion ~", var)), data = df_conversion, family = binomial)
  result <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(variable = var)
  
  
  # Niveau de reference
  if (is.factor(df_conversion[[var]])) {
    ref_level <- levels(df_conversion[[var]])[1]
    ref_row <- tibble(
      variable = var,
      term = ref_level,
      OR = 1,
      IC_bas = NA,
      IC_haut = NA,
      `p-value` = NA
    )
    
    result_clean <- result %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate, conf.low, conf.high, p.value) %>%
      rename(OR = estimate, IC_bas = conf.low, IC_haut = conf.high, `p-value` = p.value) %>%
      mutate(variable = var)
    
    bind_rows(ref_row, result_clean)
    
  } else {
    result %>%
      filter(term != "(Intercept)") %>%
      select(term, estimate, conf.low, conf.high, p.value) %>%
      rename(OR = estimate, IC_bas = conf.low, IC_haut = conf.high, `p-value` = p.value) %>%
      mutate(variable = var)
  }
})

# 📋 Regroupement dans un seul tableau
univ_table <- bind_rows(univ_models) %>%
  select(variable, term, OR, IC_bas, IC_haut, `p-value`) %>%
  arrange(variable, term)

# ✨ Ajout indentation pour les modalites
univ_table <- univ_table %>%
  mutate(term_affiche = paste0("  ", term))

# 💄 Mise en forme
univ_table <- univ_table %>%
  mutate(
    OR = ifelse(is.na(OR) | OR == 0 | OR == Inf, NA, OR),
    IC_bas = ifelse(is.na(IC_bas) | IC_bas == 0 | IC_bas == Inf, NA, IC_bas),
    IC_haut = ifelse(is.na(IC_haut) | IC_haut == 0 | IC_haut == Inf | IC_haut > 1e5, NA, IC_haut),
    OR = sprintf("%.2f", OR),
    IC_bas = sprintf("%.2f", IC_bas),
    IC_haut = sprintf("%.2f", IC_haut),
    `p-value` = ifelse(is.na(`p-value`), "", 
                       ifelse(`p-value` < 0.001, "< 0.001", sprintf("%.3f", `p-value`)))
  ) %>%
  mutate(
    OR = ifelse(grepl("NA", OR), "—", OR),
    IC_bas = ifelse(grepl("NA", IC_bas), "—", IC_bas),
    IC_haut = ifelse(grepl("NA", IC_haut), "—", IC_haut)
  )

# 📊 Affichage final avec kableExtra
library(kableExtra)

colnames_kable <- c("Modalite", "OR", "IC\u00A095%\u00A0bas", "IC\u00A095%\u00A0haut", "P\u00A0value")

univ_table %>%
  select(term_affiche, OR, IC_bas, IC_haut, `p-value`) %>%
  kable(col.names = colnames_kable,
        align = "lcccc",
        caption = "Analyse univariee : OR et IC95% des facteurs de conversion chez les patients avec Hartmann coelioscopique") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  pack_rows(index = table(univ_table$variable))









#***************ANALYSE MULTIVARIEE*****************
# Sélection des variables appropriées
vars_multi_ajuste <- c("Sexe_Male","BMI_sup30", "A_ASA_sup2", "A_ATCD_neuro", "A_ATCD_Cardio", 
                       "A_ATCD_pneumo", "A_diabete", "A_tabac", "A_immunosuppression", 
                       "A_anticoagulants", "A_antiaggregants", "A_ATCD_laparo_mediane", 
                       "H_perforation", "H_abces", "I_scoreHinchey_3", "I_scoreHinchey_4", "I_indication_hemorragie", 
                       "I_indication_immunodepression", "I_indication_poussee_compliquee", "I_indication_smoldering",
                       "I_localisation_colondroit", "I_localisation_sigmoide")

vars_multi_ajuste <- c(
  "Sexe_Male", "A_ASA_sup2", "BMI", "A_ATCD_neuro", "A_ATCD_Cardio", 
  "A_ATCD_pneumo", "A_diabete", "A_tabac", "A_immunosuppression", 
  "A_anticoagulants", "A_antiaggregants", "A_ATCD_laparo_mediane", 
  "H_perforation", "H_colo", "I_scoreHinchey_4"
)

df_HartmannCoelio <- df_HartmannCoelio %>%
  mutate(BMI_sup30 = ifelse(BMI >= 30, 1, 0))

df_HartmannCoelio <- df_HartmannCoelio %>%
  mutate(BMI_sub18 = ifelse(BMI <= 18, 1, 0))

# Construction de la formule pour la régression multivariée avec Hinchey 3 et Hinchey 4 séparés
formule_multi_ajuste <- as.formula(
  paste("I_conversion ~", paste(vars_multi_ajuste, collapse = " + "))
)

# Modèle de régression logistique multivariée
model_multi_ajuste <- glm(formule_multi_ajuste, data = df_HartmannCoelio, family = binomial)

# Résumé du modèle
summary(model_multi_ajuste)

# Extraire les coefficients et les p-values
coefs <- summary(model_multi_ajuste)$coefficients[, c("Estimate", "Std. Error", "Pr(>|z|)")]
coefs <- as.data.frame(coefs)

# Calcul des intervalles de confiance (IC à 95%)
coefs$CI_lower <- coefs$Estimate - 1.96 * coefs$`Std. Error`
coefs$CI_upper <- coefs$Estimate + 1.96 * coefs$`Std. Error`

# Formater les p-values (affichage sous forme de <0.001 ou valeurs)
coefs$`Pr(>|z|)` <- ifelse(coefs$`Pr(>|z|)` < 0.001, "< 0.001", 
                           sprintf("%.3f", coefs$`Pr(>|z|)`))

# Formater les coefficients et intervalles
coefs$Estimate <- sprintf("%.2f", coefs$Estimate)
coefs$CI_lower <- sprintf("%.2f", coefs$CI_lower)
coefs$CI_upper <- sprintf("%.2f", coefs$CI_upper)

# Créer un dataframe pour le forest plot avec les résultats de la régression
forest_data <- data.frame(
  Variable = rownames(coefs),
  HR = exp(as.numeric(coefs$Estimate)),  # Calcul du Hazard Ratio
  CI_Lower = exp(as.numeric(coefs$CI_lower)),  # Limite inférieure de l'intervalle de confiance
  CI_Upper = exp(as.numeric(coefs$CI_upper)),  # Limite supérieure de l'intervalle de confiance
  p_value = coefs$`Pr(>|z|)`
)

# Préparer l'objet textuel pour afficher HR et IC
tabletext <- cbind(
  c("Variable", forest_data$Variable),
  c("HR [IC95%]", paste0(round(forest_data$HR, 2), " [", round(forest_data$CI_Lower, 2), " - ", round(forest_data$CI_Upper, 2), "]"))
)


# Créer le forest plot
library(forestplot)

forestplot(
  labeltext = tabletext,
  mean = c(NA, forest_data$HR),
  lower = c(NA, forest_data$CI_Lower),
  upper = c(NA, forest_data$CI_Upper),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.2,
  col = fpColors(box = "black", line = "black", summary = "black"),
  xlog = TRUE,  # Affichage de l'échelle logarithmique
  lwd.zero = 2,
  lwd.ci = 2,
  ci.vertices = TRUE,
  xlab = "Hazard Ratio (log-échelle)",
  title = "Modèle de régression logistique multivariée pour la conversion"
)












#----------------------------------------
#Analyse des caractéristiques convertis vs non convertis
#----------------------------------------

# Sélectionner les patients du groupe Hartmann Coelio
df_conversion_HartmannCoelio <- df_HartmannCoelio %>%
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio")

# Renommer les valeurs de la variable I_conversion
df_conversion_HartmannCoelio$I_conversion <- recode(df_conversion_HartmannCoelio$I_conversion,
                                                    `0` = "Pas de conversion",
                                                    `1` = "Conversion")

# Vérifier le résultat
table(df_conversion_HartmannCoelio$I_conversion)


# Création du tableau 2 non ajusté avec comparaison entre les patients convertis et non convertis
table2unadjusted <- df_conversion_HartmannCoelio %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table2),
    missing = "ifany",
    by = I_conversion,  # Variable de regroupement (conversion oui/non)
    percent = "column",  # Calcule les pourcentages sur l’ensemble de la colonne
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Effectifs et pourcentages pour variables catégorielles
      all_continuous() ~ "{median} ({p25} - {p75})"  # Mediane et IQR pour variables continues
    )  
  ) %>%  
  add_p()  # Ajoute les valeurs p pour les comparaisons


# Affichage du tableau
table2unadjusted

# Convertir le tableau en objet gt
table2unadjusted_gt <- as_gt(table2unadjusted)

# Enregistrer le tableau au format Word
gtsave(table2unadjusted_gt , file = "table2unadjusted.docx")






# Création du tableau 3 non ajusté avec médianes et IQR, comparaison entre les patients convertis et non convertis
table3unadjusted <- df_conversion_HartmannCoelio %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table3),
    missing = "ifany",
    by = I_conversion,  # Variable de regroupement (conversion oui/non)
    percent = "column",  # Calcule les pourcentages sur l’ensemble de la colonne
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Effectif et pourcentage pour variables catégorielles
      all_continuous() ~ "{median} [{p25} - {p75}]"  # Mediane et IQR pour variables continues
    )
  ) %>%
  add_p()  # Ajoute les valeurs p pour les comparaisons

# Affichage du tableau
table3unadjusted

# Convertir le tableau en objet gt
table3unadjusted_gt <- as_gt(table3unadjusted)

# Enregistrer le tableau au format Word
gtsave(table3unadjusted_gt , file = "table3unadjusted.docx")






# Création du tableau 4 non ajusté avec médianes et IQR, comparaison entre les patients convertis et non convertis
table4unadjusted <- df_conversion_HartmannCoelio %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table4),
    missing = "ifany",
    by = I_conversion,  # Variable de regroupement (conversion oui/non)
    percent = "column",  # Calcule les pourcentages sur l’ensemble de la colonne
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",  # Effectif et pourcentage pour variables catégorielles
      all_continuous() ~ "{median} [{p25} - {p75}]"  # Mediane et IQR pour variables continues
    )
  ) %>%
  add_p()  # Ajoute les valeurs p pour les comparaisons

# Affichage du tableau
table4unadjusted

# Convertir le tableau en objet gt
table4unadjusted_gt <- as_gt(table4unadjusted)

# Enregistrer le tableau au format Word
gtsave(table4unadjusted_gt , file = "table4unadjusted.docx")






#**********COURBE HARTMANN COELIO CONVERTIS OU NON RÉGLÉE*********
# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Préparer les données de survie pour la courbe Kaplan-Meier
df_survie <- df_HartmannCoelio %>% 
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio") %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 730)
  )

# Conversion en facteur
df_survie$I_conversion <- factor(df_survie$I_conversion, levels = c(0, 1), labels = c("Non Converti", "Converti"))

# Objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_conversion, data = df_survie)

# Test log-rank
test_logrank <- survdiff(surv_object ~ I_conversion, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentages de fermeture
taux_final_fermeture <- df_survie %>%
  group_by(I_conversion) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes comme dans tbl_summary (calculées sans censures, sur la base brute)
mediane_fermeture <- df_conversion_HartmannCoelio %>%
  group_by(I_conversion) %>%
  summarise(
    mediane_jours = median(S_duree_avec_stomie, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    I_conversion = recode(I_conversion,
                          "Pas de conversion" = "Non Converti",
                          "Conversion" = "Converti"),
    y_label = case_when(
      I_conversion == "Non Converti" ~ -1,
      I_conversion == "Converti" ~ -5
    ),
    label_median = paste0(mediane_jours, "j")
  )

# Fonction pour estimer la position verticale (survie transformée)
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])
}

# Ajouter la position verticale sur la courbe pour les annotations
mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
})

# Récupérer les couleurs utilisées automatiquement par ggsurvplot
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Générer la courbe principale avec toutes les annotations
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  break.time.by = 100,
  xlim = c(0, 730),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Conversion",
  legend.labs = c("Non Converti", "Converti"),
  fun = function(x) 100 * (1 - x)
)

# Ajouter les éléments sur la courbe
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  geom_text(data = taux_final_fermeture, aes(x = 730, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = label_median),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Afficher la courbe
print(p)
















# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

unique(m.final$I_geste_comparaison_1HartmannCoelio)

# Créer une variable pour distinguer les trois groupes
df_survie <- m.final %>% 
  filter(I_stomie == 1) %>%  # Tous les patients ont une stomie par définition
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),  # Assurez-vous que S_duree_avec_stomie est numérique
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),  # Créer la variable fermeture de stomie
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 365),  # 730 jours pour les patients sans fermeture
    # Créer une variable pour distinguer les trois groupes
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",  # Correction du nom
      TRUE ~ NA_character_  # Exclure les autres cas
    )
  )


# Convertir la variable groupe en facteur
df_survie$groupe <- factor(df_survie$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))

# Définir l'objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)

# Modèle de Kaplan-Meier pour la comparaison entre les trois groupes
km_fit <- survfit(surv_object ~ groupe, data = df_survie)

# Test du log-rank pour comparer les groupes
test_logrank <- survdiff(surv_object ~ groupe, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentage final de fermeture de stomie pour chaque groupe
taux_final_fermeture <- df_survie %>%
  group_by(groupe) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes fixées manuellement pour chaque groupe
mediane_fermeture <- data.frame(
  groupe = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  mediane_jours = c(143, 115, 120),  # Médianes pour chaque groupe, ajustez selon les données
  y_label = c(-1, -8, -15)
)

# Récupérer la survie au temps de médiane dans km_fit
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])  # 1 pour "Hartmann Non Converti", 2 pour "Hartmann Converti", 3 pour "RA Protégée"
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit,
                   i,
                   mediane_fermeture$mediane_jours[i])
})

# Générer la courbe pour récupérer les couleurs utilisées
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Tracer la courbe principale
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  break.time.by = 100,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Groupe",
  legend.labs = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  fun = function(x) 100 * (1 - x)
)

# Modifier uniquement les lignes des médianes
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  
  geom_text(data = taux_final_fermeture, aes(x = 365, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  
  # Lignes médianes exactement sur la courbe
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = paste0(mediane_jours, " j")),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Afficher la courbe
print(p)






























#**************COURBE 3 NIVEAUX KAPLAN**********


# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

unique(m.final$I_geste_comparaison_1HartmannCoelio)

# Créer une variable pour distinguer les trois groupes
df_survie <- m.final %>% 
  filter(I_stomie == 1) %>%  # Tous les patients ont une stomie par définition
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),  # Assurez-vous que S_duree_avec_stomie est numérique
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),  # Créer la variable fermeture de stomie
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 365),  # 730 jours pour les patients sans fermeture
    # Créer une variable pour distinguer les trois groupes
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",  # Correction du nom
      TRUE ~ NA_character_  # Exclure les autres cas
    )
  )


# Convertir la variable groupe en facteur
df_survie$groupe <- factor(df_survie$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))

# Définir l'objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)

# Modèle de Kaplan-Meier pour la comparaison entre les trois groupes
km_fit <- survfit(surv_object ~ groupe, data = df_survie)

# Test du log-rank pour comparer les groupes
test_logrank <- survdiff(surv_object ~ groupe, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentage final de fermeture de stomie pour chaque groupe
taux_final_fermeture <- df_survie %>%
  group_by(groupe) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes fixées manuellement pour chaque groupe
mediane_fermeture <- data.frame(
  groupe = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  mediane_jours = c(143, 115, 120),  # Médianes pour chaque groupe, ajustez selon les données
  y_label = c(-1, -8, -15)
)

# Récupérer la survie au temps de médiane dans km_fit
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])  # 1 pour "Hartmann Non Converti", 2 pour "Hartmann Converti", 3 pour "RA Protégée"
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit,
                   i,
                   mediane_fermeture$mediane_jours[i])
})

# Générer la courbe pour récupérer les couleurs utilisées
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Tracer la courbe principale
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  break.time.by = 100,
  conf.int = TRUE,
  xlim = c(0, 365),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Groupe",
  legend.labs = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  fun = function(x) 100 * (1 - x)
)

# Modifier uniquement les lignes des médianes
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  
  geom_text(data = taux_final_fermeture, aes(x = 365, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  
  annotate("text", x = 600, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  
  # Lignes médianes exactement sur la courbe
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = paste0(mediane_jours, " j")),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Afficher la courbe
print(p)













#**************COURBE 3 NIVEAUX QUI FONCTIONNE***********
# Charger les bibliothèques nécessaires
library(survival)
library(survminer)
library(dplyr)
library(ggplot2)

# Créer une variable pour distinguer les trois groupes
df_survie <- m.final %>% 
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 365),
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",
      TRUE ~ NA_character_
    )
  )

df_survie$groupe <- factor(df_survie$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))

# Objet de survie
surv_object <- Surv(time = df_survie$S_duree_avec_stomie, event = df_survie$fermeture_stomie)
km_fit <- survfit(surv_object ~ groupe, data = df_survie)

# Log-rank
test_logrank <- survdiff(surv_object ~ groupe, data = df_survie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentages de fermeture
taux_final_fermeture <- df_survie %>%
  group_by(groupe) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Médianes comme dans tbl_summary (non censurées)
mediane_fermeture <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(groupe) %>%
  summarise(mediane_jours = median(S_duree_avec_stomie, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    y_label = c(-1, -8, -15),
    label_median = paste0(mediane_jours, "j")
  )

# Position verticale des médianes
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)
  return(surv_percent[groupe])
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
})

# Récupérer les couleurs
p_temp <- ggsurvplot(km_fit, data = df_survie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Courbe principale
p <- ggsurvplot(
  km_fit,
  data = df_survie,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  break.time.by = 100,
  xlim = c(0, 365),
  xlab = "Temps (jours)",
  ylab = "Taux de rétablissement de la continuité (%)",
  legend.title = "Groupe",
  legend.labs = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"),
  fun = function(x) 100 * (1 - x)
)

# Ajouter les éléments
p$plot <- p$plot +
  guides(fill = "none", linetype = "none", color = guide_legend(override.aes = list(linetype = 1))) +
  geom_hline(aes(yintercept = pourcentage_fermeture),
             data = taux_final_fermeture, linetype = "dashed", size = 1,
             color = "black", show.legend = FALSE) +
  geom_text(data = taux_final_fermeture, aes(x = 365, y = pourcentage_fermeture,
                                             label = paste0(round(pourcentage_fermeture, 1), "%")),
            vjust = -0.5, size = 5, fontface = "bold", color = "black") +
  annotate("text", x = 300, y = 10,
           label = paste0("Log-rank p = ", p_value),
           size = 5, hjust = 0) +
  geom_segment(data = mediane_fermeture,
               aes(x = 0, xend = mediane_jours,
                   y = y_courbe, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_segment(data = mediane_fermeture,
               aes(x = mediane_jours, xend = mediane_jours,
                   y = 0, yend = y_courbe),
               linetype = "dashed", size = 1,
               color = colors_used, show.legend = FALSE) +
  geom_text(data = mediane_fermeture,
            aes(x = mediane_jours, y = y_label,
                label = label_median),
            color = colors_used, size = 5, fontface = "bold",
            hjust = 0.5, show.legend = FALSE)

# Affichage
print(p)















# Vérification des durées de stomie pour chaque groupe de conversion
df_survie_hartmann_coelio <- df_survie %>%
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann") %>%  # Filtrer uniquement pour Hartmann Coelio
  group_by(I_conversion) %>%
  summarise(
    moyenne_duree_stomie = mean(S_duree_avec_stomie, na.rm = TRUE),  # Calculer la moyenne de la durée de stomie
    mediane_duree_stomie = median(S_duree_avec_stomie, na.rm = TRUE),  # Calculer la médiane de la durée de stomie
    .groups = "drop"
  )

# Afficher les résultats
print(df_survie_hartmann_coelio)

df_survie_hartmann_coelio

