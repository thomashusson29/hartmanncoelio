# =================================================================
# SCRIPT FINAL : ANALYSE RA PROTÉGÉE COELIOSCOPIE vs LAPAROTOMIE
# =================================================================
# 
# RÉSULTATS ATTENDUS :
# - Appariement : 60 vs 57 patients
# - Analyse de survie : 53 vs 51 patients  
# - P-value : 0.165
# - Médianes : 96 jours (Coelio) vs 132 jours (Laparo)
# - Différence : +36 jours pour la laparotomie
# =================================================================

# Chargement des librairies
library(dplyr)
library(MatchIt)
library(mice)
library(survival)
library(survminer)
library(ggplot2)
library(tableone)
library(broom)
library(tidyr)

# =================================================================
# ÉTAPE 1 : SÉLECTION DES PATIENTS RA PROTÉGÉE UNIQUEMENT
# =================================================================

cat("=== ANALYSE SPÉCIFIQUE RA PROTÉGÉE ===\n")

# Sélection uniquement des résections anastomoses protégées
# IMPORTANT : Adapter les noms selon votre dataset
df_RA_only <- df_Coelio %>%
  filter(I_geste_comparaison_whole %in% c("Résection anastomose protégée coelio", 
                                          "Résection anastomose protégée laparo")) %>%
  mutate(
    Intervention_RA = case_when(
      I_geste_comparaison_whole == "Résection anastomose protégée coelio" ~ "RA_Coelio",
      I_geste_comparaison_whole == "Résection anastomose protégée laparo" ~ "RA_Laparo",
      TRUE ~ as.character(I_geste_comparaison_whole)
    )
  )

cat("Effectifs initiaux RA :\n")
print(table(df_RA_only$Intervention_RA))
cat("Total patients RA :", nrow(df_RA_only), "\n")

# =================================================================
# ÉTAPE 2 : PRÉPARATION POUR L'APPARIEMENT
# =================================================================

# Variable binaire pour MatchIt
dfps_RA <- df_RA_only %>%
  mutate(Treatment_RA = ifelse(Intervention_RA == "RA_Coelio", 0, 1))

# Variables d'appariement
matching_vars <- c("Age", "Sexe_Male", "BMI", "A_ASA_sup2", "A_tabac", 
                   "A_immunosuppression", "I_arret_ATC", "A_ATCD_laparo_mediane", 
                   "I_scoreHinchey_4")

available_vars <- matching_vars[matching_vars %in% names(dfps_RA)]
cat("\nVariables d'appariement disponibles :\n")
print(available_vars)

# =================================================================
# ÉTAPE 3 : IMPUTATION DES DONNÉES MANQUANTES
# =================================================================

# Identification des données manquantes
missing_summary <- dfps_RA %>%
  select(all_of(available_vars)) %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  filter(missing_count > 0)

cat("\nDonnées manquantes :\n")
print(missing_summary)

# Imputation si nécessaire
if(nrow(missing_summary) > 0) {
  cat("Imputation en cours...\n")
  cols_to_impute <- missing_summary$variable
  
  set.seed(123)
  imputed_data <- mice(dfps_RA[, cols_to_impute], 
                       m = 5, method = "pmm", seed = 123, printFlag = FALSE)
  completed_data <- complete(imputed_data)
  dfps_RA[cols_to_impute] <- completed_data
  cat("Imputation terminée.\n")
}

# Création des catégories d'âge
dfps_RA <- dfps_RA %>%
  mutate(Age_cat = cut(Age, breaks = c(-Inf, 50, 60, 70, Inf), 
                       labels = c("<50", "50-60", "60-70", ">70")))

final_matching_vars <- c("Age_cat", available_vars[available_vars != "Age"])

# =================================================================
# ÉTAPE 4 : APPARIEMENT PAR SCORE DE PROPENSION
# =================================================================

cat("\n=== APPARIEMENT PAR SCORE DE PROPENSION ===\n")

# Formule d'appariement
formula_str <- paste("Treatment_RA ~", paste(final_matching_vars, collapse = " + "))
cat("Formule :", formula_str, "\n")

# Appariement
m.out_RA <- matchit(
  formula = as.formula(formula_str),
  method = "nearest",
  ratio = 2,  # Tentative 1:2
  caliper = 0.20,
  data = dfps_RA,
  na.rm = TRUE
)

cat("\nRésultat de l'appariement :\n")
print(m.out_RA)

# Extraction des données appariées
m.final_RA <- match.data(m.out_RA)
m.final_RA <- m.final_RA %>%
  mutate(Intervention_RA = recode(Treatment_RA, `0` = "RA_Coelio", `1` = "RA_Laparo"))

cat("\nEffectifs après appariement :\n")
print(table(m.final_RA$Intervention_RA))

# =================================================================
# ÉTAPE 5 : VÉRIFICATION DE L'ÉQUILIBRE
# =================================================================

cat("\n=== VÉRIFICATION DE L'ÉQUILIBRE ===\n")

# Balance après appariement
table_after_RA <- CreateTableOne(vars = final_matching_vars,
                                 strata = "Intervention_RA", 
                                 data = m.final_RA,
                                 test = TRUE)
print(table_after_RA, smd = TRUE)

# =================================================================
# ÉTAPE 6 : ANALYSE DE SURVIE
# =================================================================

cat("\n=== ANALYSE DE SURVIE ===\n")

# Préparation des données de survie
# IMPORTANT : Adapter les noms des variables selon votre dataset
time_var <- "S_duree_avec_stomie"
event_var <- "S_fermeturestomie"

df_survie_RA <- m.final_RA %>%
  mutate(
    temps_survie = as.numeric(!!sym(time_var)),
    evenement = as.numeric(!!sym(event_var))
  ) %>%
  filter(!is.na(temps_survie), !is.na(evenement), temps_survie > 0)

cat("Effectifs pour l'analyse de survie :\n")
print(table(df_survie_RA$Intervention_RA))
cat("Total :", nrow(df_survie_RA), "patients\n")

# Ajout de l'ID de paire pour le clustering
if("subclass" %in% names(df_survie_RA)) {
  df_survie_RA$Pair_ID <- df_survie_RA$subclass
} else {
  df_survie_RA <- df_survie_RA %>%
    arrange(Intervention_RA, distance) %>%
    mutate(Pair_ID = rep(1:(nrow(.)/2), each = 2))
}

cat("Nombre de paires :", length(unique(df_survie_RA$Pair_ID)), "\n")

# =================================================================
# COURBE DE SURVIE
# =================================================================

# Objet de survie
surv_object_RA <- Surv(df_survie_RA$temps_survie, df_survie_RA$evenement)

# Modèle de Kaplan-Meier
km_fit_RA <- survfit(surv_object_RA ~ Intervention_RA, data = df_survie_RA)

# Courbe de survie
p_RA <- ggsurvplot(
  km_fit_RA,
  data = df_survie_RA,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  legend.labs = c("RA Protégée Coelioscopie", "RA Protégée Laparotomie"),
  palette = c("#00AFBB", "#E7B800"),
  title = "Probabilité cumulative de fermeture de stomie : RA Coelio vs RA Laparo",
  subtitle = "Analyse spécifique avec appariement par score de propension",
  xlab = "Temps (jours)",
  ylab = "Probabilité cumulative de fermeture",
  ggtheme = theme_bw(),
  break.x.by = 100,
  xlim = c(0, 365),
  fun = "event"  # Courbe inversée (probabilité de fermeture croissante)
)

print(p_RA)

##même courbe mais sans la "grille" dans le fond
p_RA_nogrid <- ggsurvplot(
  km_fit_RA,
  data = df_survie_RA,
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  legend.labs = c("RA Protégée coelio", "RA protégée laparo"),
  palette = c("#00AFBB", "#E7B800"),
  title = "Probabilité cumulative de fermeture de stomie : RA Coelio vs RA Lap
aro",
  subtitle = "Analyse spécifique avec appariement par score de propension",
  xlab = "Temps (jours)",
  ylab = "Probabilité cumulative de fermeture",
  ggtheme = theme_classic(), # Changement ici pour thème sans grille
  break.x.by = 100,
  xlim = c(0, 365),
  fun = "event"  # Courbe inversée (probabilité de fermeture croissante
)
print(p_RA_nogrid)


# =================================================================
# TESTS STATISTIQUES
# =================================================================

# Test log-rank
logrank_test_RA <- survdiff(surv_object_RA ~ Intervention_RA, data = df_survie_RA)
cat("\nTest log-rank :\n")
print(logrank_test_RA)

pval_RA <- 1 - pchisq(logrank_test_RA$chisq, 1)
cat("P-value :", round(pval_RA, 4), "\n")

# Médianes de survie
cat("\nMédianes de survie :\n")
print(km_fit_RA)

# =================================================================
# MODÈLES DE COX
# =================================================================

cat("\n=== MODÈLES DE COX ===\n")

# Modèle Cox simple
cox_simple_RA <- coxph(Surv(temps_survie, evenement) ~ Intervention_RA, 
                       data = df_survie_RA)

# Modèle Cox avec clustering
cox_clustered_RA <- coxph(Surv(temps_survie, evenement) ~ Intervention_RA, 
                          data = df_survie_RA,
                          cluster = Pair_ID)

cat("Modèle Cox avec clustering :\n")
print(summary(cox_clustered_RA))

# Extraction du HR avec clustering
coef_clustered <- summary(cox_clustered_RA)$coefficients
hr_clustered <- exp(coef_clustered[1, "coef"])
ci_low_clustered <- exp(coef_clustered[1, "coef"] - 1.96 * coef_clustered[1, "robust se"])
ci_high_clustered <- exp(coef_clustered[1, "coef"] + 1.96 * coef_clustered[1, "robust se"])
p_clustered <- coef_clustered[1, "Pr(>|z|)"]

cat(sprintf("\nHR avec clustering: %.2f [%.2f - %.2f], p = %.3f\n", 
            hr_clustered, ci_low_clustered, ci_high_clustered, p_clustered))

# =================================================================
# STATISTIQUES DESCRIPTIVES
# =================================================================

cat("\n=== STATISTIQUES DESCRIPTIVES ===\n")

stats_RA <- df_survie_RA %>%
  group_by(Intervention_RA) %>%
  summarise(
    N = n(),
    Mediane = round(median(temps_survie, na.rm = TRUE), 1),
    Q1 = round(quantile(temps_survie, 0.25, na.rm = TRUE), 1),
    Q3 = round(quantile(temps_survie, 0.75, na.rm = TRUE), 1),
    Moyenne = round(mean(temps_survie, na.rm = TRUE), 1),
    .groups = 'drop'
  )

print(stats_RA)

# Calcul de la différence de médianes
mediane_coelio <- stats_RA$Mediane[stats_RA$Intervention_RA == "RA_Coelio"]
mediane_laparo <- stats_RA$Mediane[stats_RA$Intervention_RA == "RA_Laparo"]
diff_mediane <- mediane_laparo - mediane_coelio

cat(sprintf("\nDifférence de médianes : +%.1f jours (Laparo plus lente)\n", diff_mediane))

# =================================================================
# RÉSUMÉ FINAL
# =================================================================

claudeAddin()


cat("\n================================================================\n")
cat("                    RÉSUMÉ FINAL\n")
cat("================================================================\n")

cat(sprintf("• Patients RA analysés : %d\n", nrow(df_survie_RA)))
cat(sprintf("• RA Coelio : %d patients, médiane %d jours\n", 
            sum(df_survie_RA$Intervention_RA == "RA_Coelio"), mediane_coelio))
cat(sprintf("• RA Laparo : %d patients, médiane %d jours\n", 
            sum(df_survie_RA$Intervention_RA == "RA_Laparo"), mediane_laparo))
cat(sprintf("• Différence : +%.1f jours pour la laparotomie\n", diff_mediane))
cat(sprintf("• P-value : %.3f\n", pval_RA))
cat(sprintf("• HR (avec clustering) : %.2f [%.2f-%.2f]\n", 
            hr_clustered, ci_low_clustered, ci_high_clustered))

if(pval_RA < 0.05) {
  cat("• RÉSULTAT : Différence SIGNIFICATIVE\n")
} else if(pval_RA < 0.20) {
  cat("• RÉSULTAT : TENDANCE vers une différence\n")
} else {
  cat("• RÉSULTAT : Pas de différence significative\n")
}

cat("================================================================\n")
cat("Script terminé avec succès !\n")
cat("================================================================\n")