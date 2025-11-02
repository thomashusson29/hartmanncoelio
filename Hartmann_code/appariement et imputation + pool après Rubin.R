##************Appariement selon score de propension*****
#analyse uni- puis multivariée pour identifier facteurs importants

#creer un nouveau df pour appariemment selon score de propension
dfps_HartmannCoelio <- df_HartmannCoelio

# 1️⃣ Conversion de la variable de traitement en binaire (0 = HartmannCoelio, 1 = RAlaparoProtegee)
dfps_HartmannCoelio <- dfps_HartmannCoelio %>%
  mutate(I_geste_comparaison_1HartmannCoelio = ifelse(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio", 0, 1))

# 2️⃣ Verification des valeurs uniques après transformation
unique(dfps_HartmannCoelio$I_geste_comparaison_1HartmannCoelio)  # Doit retourner uniquement 0 et 1

#Ca ne va pas marcher à cette etape là sauf si aucune donnee manquante
#permet d'afficher les "non finite values", qu'il faudra imputer avec la fonction mice (imputation multiple)

# Test pour identifier les variables avequic des valeurs manquantes
cat("=== IDENTIFICATION DES VARIABLES AVEC VALEURS MANQUANTES ===\n")
tryCatch({
  m.out_test <- matchit(
    formula = I_geste_comparaison_1HartmannCoelio ~ Age + Sexe_Male + BMI + A_ASA_sup2 + A_tabac + A_immunosuppression + I_arret_ATC + A_ATCD_laparo_mediane + I_scoreHinchey_4,
    method = "optimal",
    data = dfps_HartmannCoelio,
    na.rm = TRUE
  )
}, error = function(e) {
  cat("Erreur attendue - variables avec valeurs manquantes détectées\n")
  cat("Message:", e$message, "\n")
})

#non finite values sont
cat("Variables à imputer identifiées:\n")
cat("- Age\n")
cat("- BMI\n") 
cat("- A_tabac\n")
cat("- A_immunosuppression\n")
cat("- I_arret_ATC\n")
cat("- A_ATCD_laparo_mediane\n")

# Selection uniquement des colonnes à imputer
cols_to_impute <- c(
  "Age",
  "BMI",
  "A_tabac",
  "A_immunosuppression",
  "I_arret_ATC",
  "A_ATCD_laparo_mediane"
)

# ============================================================================
# IMPUTATION MULTIPLE AVEC MÉTHODE DE RUBIN
# ============================================================================

cat("\n=== IMPUTATION MULTIPLE AVEC MICE (m=5) ===\n")

# Imputation multiple des donnees manquantes avec mice() - MÉTHODE DE RUBIN
set.seed(123)  # Pour la reproductibilité
imputed_data <- mice(dfps_HartmannCoelio[, cols_to_impute], 
                     m = 5, 
                     method = "pmm", 
                     seed = 123, 
                     printFlag = FALSE)

# Vérifier l'imputation
cat("Imputation MICE terminée - 5 datasets créés\n")
cat("Méthode utilisée: Predictive Mean Matching (PMM)\n")

# Créer des listes pour stocker les résultats
results_list <- list()
matched_datasets <- list()
appariement_info <- list()

# ============================================================================
# BOUCLE SUR LES 5 DATASETS IMPUTÉS - MÉTHODE DE RUBIN
# ============================================================================

cat("\n=== ANALYSE SUR LES 5 DATASETS IMPUTÉS ===\n")

for(i in 1:5) {
  cat(paste("\n--- Traitement du dataset imputé", i, "/5 ---\n"))
  
  # 1. Completer le dataset imputé i
  completed_data <- complete(imputed_data, i)
  
  # 2. Créer le dataset complet avec les données imputées
  dfps_temp <- dfps_HartmannCoelio
  dfps_temp[cols_to_impute] <- completed_data
  
  # 3. Ajouter la catégorisation de l'âge
  dfps_temp <- dfps_temp %>%
    mutate(Age_cat = cut(Age, breaks = c(-Inf, 50, 60, 70, Inf), 
                         labels = c("<50", "50-60", "60-70", ">70")))
  
  # 4. APPARIEMENT sur ce dataset imputé
  cat("   Appariement en cours...\n")
  m.out_temp <- matchit(
    I_geste_comparaison_1HartmannCoelio ~ Age_cat + Sexe_Male + BMI + A_ASA_sup2 + 
      A_tabac + A_immunosuppression + I_arret_ATC + A_ATCD_laparo_mediane + I_scoreHinchey_4,
    method = "nearest",
    caliper = 0.20,
    data = dfps_temp,
    na.rm = TRUE
  )
  
  # 5. Récupérer les données appariées
  m.final_temp <- match.data(m.out_temp)
  
  # 6. Ajouter l'ID de paire et renommer la variable pour compatibilité
  m.final_temp <- m.final_temp %>%
    mutate(Pair_ID = as.factor(subclass)) %>%
    mutate(
      I_geste_comparaison_1HartmannCoelio = recode(
        I_geste_comparaison_1HartmannCoelio, 
        `0` = "HartmannCoelio", 
        `1` = "RAlaparoProtegee"
      )
    )
  
  # 7. Stocker les résultats d'appariement
  matched_datasets[[i]] <- m.final_temp
  
  # 8. Stocker les informations d'appariement
  n_pairs <- length(unique(m.final_temp$Pair_ID))
  n_patients <- nrow(m.final_temp)
  appariement_info[[i]] <- list(
    dataset_id = i,
    n_pairs = n_pairs,
    n_patients = n_patients,
    balance = summary(m.out_temp, standardize = TRUE)
  )
  
  # 9. ANALYSE DE SURVIE sur ce dataset apparié
  cat("   Analyse de survie en cours...\n")
  
  if("S_duree_avec_stomie" %in% names(m.final_temp) && "S_fermeturestomie" %in% names(m.final_temp)) {
    
    # Préparer les données de survie
    df_survie_temp <- m.final_temp %>%
      filter(I_stomie == 1) %>%
      mutate(
        S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
        fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
        S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 730)
      ) %>%
      filter(!is.na(S_duree_avec_stomie) & !is.na(fermeture_stomie))
    
    if(nrow(df_survie_temp) > 10) {  # Assez de patients pour l'analyse
      # Modèle de Cox robuste avec cluster sur les paires
      cox_model <- coxph(
        Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
        data = df_survie_temp
      )
      
      # Extraire les résultats
      coef_cox <- coef(cox_model)[1]  # Premier et seul coefficient
      se_cox <- sqrt(diag(vcov(cox_model)))[1]  # Première erreur standard
      hr_cox <- exp(coef_cox)
      
      # Stocker les résultats pour le pooling
      results_list[[i]] <- list(
        dataset_id = i,
        cox_coef = coef_cox,
        cox_se = se_cox,
        cox_hr = hr_cox,
        n_pairs = n_pairs,
        n_patients = n_patients,
        n_survie = nrow(df_survie_temp),
        events = sum(df_survie_temp$fermeture_stomie)
      )
      
      cat(paste("   Résultats - Paires:", n_pairs, "| Patients survie:", nrow(df_survie_temp), 
                "| HR:", round(hr_cox, 3), "\n"))
    } else {
      cat("   Pas assez de patients pour l'analyse de survie\n")
      results_list[[i]] <- NULL
    }
  } else {
    cat("   Variables de survie manquantes\n")
    results_list[[i]] <- NULL
  }
}

# ============================================================================
# POOLING DES RÉSULTATS SELON LA MÉTHODE DE RUBIN
# ============================================================================

cat("\n=== POOLING DES RÉSULTATS (MÉTHODE DE RUBIN) ===\n")

# Vérifier qu'on a des résultats valides
valid_results <- results_list[!sapply(results_list, is.null)]
m <- length(valid_results)

cat(paste("Nombre de datasets avec résultats valides:", m, "/5\n"))

if(m >= 3) {  # Au moins 3 datasets pour un pooling fiable
  
  # Extraire les coefficients et erreurs standard
  coefs <- sapply(valid_results, function(x) x$cox_coef)
  ses <- sapply(valid_results, function(x) x$cox_se)
  hrs <- sapply(valid_results, function(x) x$cox_hr)
  
  cat("Coefficients individuels:", round(coefs, 4), "\n")
  cat("Erreurs standard individuelles:", round(ses, 4), "\n")
  cat("HR individuels:", round(hrs, 3), "\n")
  
  # 1. ESTIMATION POOLÉE (moyenne des estimations)
  Q_bar <- mean(coefs)
  
  # 2. VARIANCE INTRA-IMPUTATION (moyenne des variances)
  U_bar <- mean(ses^2)
  
  # 3. VARIANCE INTER-IMPUTATION
  B <- var(coefs)
  
  # 4. VARIANCE TOTALE selon Rubin
  T <- U_bar + (1 + 1/m) * B
  
  # 5. DEGRÉS DE LIBERTÉ (méthode améliorée de Barnard & Rubin, 1999)
  lambda <- (1 + 1/m) * B / T  # fraction de variance manquante
  
  # Éviter la division par zéro
  if(B > 0) {
    df_old <- (m - 1) / lambda^2
    df_obs <- (U_bar / ((1 + 1/m) * B)) * (length(valid_results[[1]]$n_survie) - 1)  # approximation
    df_new <- (1/df_old + 1/df_obs)^(-1)
    df <- max(df_new, m - 1)  # Au minimum m-1 degrés de liberté
  } else {
    df <- Inf  # Pas de variance inter-imputation
  }
  
  # 6. STATISTIQUE T ET P-VALUE
  t_stat <- Q_bar / sqrt(T)
  
  if(is.finite(df)) {
    p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  } else {
    p_value <- 2 * pnorm(abs(t_stat), lower.tail = FALSE)  # Distribution normale si df infini
  }
  
  # 7. INTERVALLES DE CONFIANCE
  if(is.finite(df)) {
    t_crit <- qt(0.975, df = df)
  } else {
    t_crit <- qnorm(0.975)
  }
  
  ci_lower <- Q_bar - t_crit * sqrt(T)
  ci_upper <- Q_bar + t_crit * sqrt(T)
  
  # 8. HAZARD RATIO POOLÉ
  hr_pooled <- exp(Q_bar)
  hr_ci_lower <- exp(ci_lower)
  hr_ci_upper <- exp(ci_upper)
  
  # ============================================================================
  # RÉSULTATS FINAUX
  # ============================================================================
  
  cat("\n=== RÉSULTATS FINAUX (MÉTHODE DE RUBIN) ===\n")
  cat(paste("Nombre de datasets utilisés:", m, "\n"))
  cat(paste("Coefficient poolé (log-HR):", round(Q_bar, 4), "\n"))
  cat(paste("Erreur standard poolée:", round(sqrt(T), 4), "\n"))
  cat(paste("Hazard Ratio poolé:", round(hr_pooled, 3), "\n"))
  cat(paste("IC 95% HR: [", round(hr_ci_lower, 3), " - ", round(hr_ci_upper, 3), "]\n"))
  cat(paste("Statistique t:", round(t_stat, 3), "\n"))
  cat(paste("Degrés de liberté:", round(df, 1), "\n"))
  cat(paste("P-value:", ifelse(p_value < 0.001, "< 0.001", formatC(p_value, format = "f", digits = 4)), "\n"))
  cat(paste("Fraction de variance manquante (lambda):", round(lambda, 3), "\n"))
  
  # Interprétation de lambda
  if(lambda < 0.1) {
    cat("Interprétation: Impact faible de l'imputation (lambda < 0.1)\n")
  } else if(lambda < 0.3) {
    cat("Interprétation: Impact modéré de l'imputation (0.1 ≤ lambda < 0.3)\n")
  } else {
    cat("Interprétation: Impact substantiel de l'imputation (lambda ≥ 0.3)\n")
  }
  
  # Créer un data.frame avec les résultats finaux
  rubin_results <- data.frame(
    Method = "Multiple Imputation (Rubin)",
    N_datasets = m,
    Coefficient = Q_bar,
    SE = sqrt(T),
    HR = hr_pooled,
    CI_lower = hr_ci_lower,
    CI_upper = hr_ci_upper,
    t_statistic = t_stat,
    df = df,
    p_value = p_value,
    lambda = lambda,
    stringsAsFactors = FALSE
  )
  
  cat("\n=== COMPARAISON DES RÉSULTATS INDIVIDUELS ===\n")
  for(i in 1:m) {
    result <- valid_results[[i]]
    cat(paste("Dataset", result$dataset_id, ": HR =", round(result$cox_hr, 3), 
              "| Paires =", result$n_pairs, "| Patients survie =", result$n_survie, "\n"))
  }
  
  # Créer un tableau de comparaison
  comparison_table <- data.frame(
    Dataset = sapply(valid_results, function(x) x$dataset_id),
    HR = sapply(valid_results, function(x) round(x$cox_hr, 3)),
    Coefficient = sapply(valid_results, function(x) round(x$cox_coef, 4)),
    SE = sapply(valid_results, function(x) round(x$cox_se, 4)),
    N_pairs = sapply(valid_results, function(x) x$n_pairs),
    N_survie = sapply(valid_results, function(x) x$n_survie)
  )
  
  cat("\nTableau de comparaison:\n")
  print(comparison_table)
  
} else {
  cat("ERREUR: Pas assez de résultats valides pour le pooling (minimum 3 requis)\n")
  rubin_results <- NULL
  comparison_table <- NULL
}

# ============================================================================
# TESTS DE QUALITÉ DE L'APPARIEMENT (sur le premier dataset)
# ============================================================================

cat("\n=== TESTS DE QUALITÉ DE L'APPARIEMENT ===\n")

if(length(matched_datasets) > 0) {
  # Utiliser le premier dataset pour les tests d'équilibre
  m.out_test <- matchit(
    I_geste_comparaison_1HartmannCoelio ~ Age_cat + Sexe_Male + BMI + A_ASA_sup2 + 
      A_tabac + A_immunosuppression + I_arret_ATC + A_ATCD_laparo_mediane + I_scoreHinchey_4,
    method = "nearest",
    caliper = 0.20,
    data = complete(imputed_data, 1) %>%
      mutate(
        I_geste_comparaison_1HartmannCoelio = ifelse(dfps_HartmannCoelio$I_geste_comparaison_1HartmannCoelio == 0, 0, 1),
        Age_cat = cut(Age, breaks = c(-Inf, 50, 60, 70, Inf), labels = c("<50", "50-60", "60-70", ">70"))
      ),
    na.rm = TRUE
  )
  
  # Affichage des résultats d'équilibre
  cat("Résultats d'équilibre (Dataset 1):\n")
  print(summary(m.out_test, standardize = TRUE))
  
  # Tests de discrimination et calibration
  dfps_test <- complete(imputed_data, 1)
  dfps_test$I_geste_comparaison_1HartmannCoelio <- dfps_HartmannCoelio$I_geste_comparaison_1HartmannCoelio
  dfps_test <- dfps_test %>%
    mutate(Age_cat = cut(Age, breaks = c(-Inf, 50, 60, 70, Inf), labels = c("<50", "50-60", "60-70", ">70")))
  
  # AUC (discrimination)
  if(require(pROC, quietly = TRUE)) {
    roc_curve <- roc(dfps_test$I_geste_comparaison_1HartmannCoelio, m.out_test$distance, quiet = TRUE)
    auc_value <- auc(roc_curve)
    cat(paste("AUC (discrimination):", round(auc_value, 3), "\n"))
  }
  
  # Test de Hosmer-Lemeshow (calibration)
  if(require(ResourceSelection, quietly = TRUE)) {
    hosmer_result <- hoslem.test(dfps_test$I_geste_comparaison_1HartmannCoelio, m.out_test$distance)
    cat(paste("Test de Hosmer-Lemeshow p-value:", round(hosmer_result$p.value, 4), "\n"))
    if(hosmer_result$p.value > 0.05) {
      cat("Calibration: Bonne (p > 0.05)\n")
    } else {
      cat("Calibration: À améliorer (p ≤ 0.05)\n")
    }
  }
}

# ============================================================================
# SCRIPT D'ÉVALUATION DE LA QUALITÉ DE L'APPARIEMENT
# À exécuter après votre script d'imputation multiple avec méthode de Rubin
# ============================================================================

cat("🔍 === ÉVALUATION DE LA QUALITÉ DE L'APPARIEMENT ===\n\n")

# Vérifications préliminaires
if(!exists("m.final")) {
  stop("❌ Variable 'm.final' non trouvée. Exécutez d'abord le script d'imputation.")
}

if(!exists("matched_datasets")) {
  stop("❌ Variable 'matched_datasets' non trouvée. Exécutez d'abord le script d'imputation.")
}

# Charger les librairies nécessaires
suppressMessages({
  library(MatchIt)
  library(cobalt)
  library(dplyr)
  library(ggplot2)
  library(kableExtra)
  library(pROC)
  library(ResourceSelection)
})

# ============================================================================
# 1. INFORMATIONS GÉNÉRALES SUR L'APPARIEMENT
# ============================================================================

cat("📊 === INFORMATIONS GÉNÉRALES ===\n")

# Données de base
n_original <- nrow(dfps_HartmannCoelio)
n_matched <- nrow(m.final)
n_pairs <- length(unique(m.final$Pair_ID))

# Répartition par groupe
group_counts <- table(m.final$I_geste_comparaison_1HartmannCoelio)

cat(paste("Patients dans la cohorte originale :", n_original, "\n"))
cat(paste("Patients après appariement :", n_matched, "\n"))
cat(paste("Nombre de paires créées :", n_pairs, "\n"))
cat(paste("Taux d'appariement :", round(100 * n_matched / n_original, 1), "%\n"))
cat(paste("Répartition :", names(group_counts)[1], "=", group_counts[1], 
          ",", names(group_counts)[2], "=", group_counts[2], "\n\n"))

# ============================================================================
# 2. RECRÉER L'OBJET MATCHIT POUR L'ÉVALUATION
# ============================================================================

cat("⚙️ === RECONSTRUCTION DE L'APPARIEMENT POUR ÉVALUATION ===\n")

# Préparer les données pour l'évaluation (premier dataset imputé)
dfps_evaluation <- complete(imputed_data, 1)
dfps_evaluation$I_geste_comparaison_1HartmannCoelio <- dfps_HartmannCoelio$I_geste_comparaison_1HartmannCoelio
dfps_evaluation <- dfps_evaluation %>%
  mutate(Age_cat = cut(Age, breaks = c(-Inf, 50, 60, 70, Inf), 
                       labels = c("<50", "50-60", "60-70", ">70")))

# Refaire l'appariement pour l'évaluation
m.out_evaluation <- matchit(
  I_geste_comparaison_1HartmannCoelio ~ Age_cat + Sexe_Male + BMI + A_ASA_sup2 + 
    A_tabac + A_immunosuppression + I_arret_ATC + A_ATCD_laparo_mediane + I_scoreHinchey_4,
  method = "nearest",
  caliper = 0.20,
  data = dfps_evaluation,
  na.rm = TRUE
)

cat("✅ Objet d'évaluation créé\n\n")

# ============================================================================
# 3. ANALYSE DES SMD (STANDARDIZED MEAN DIFFERENCES)
# ============================================================================

cat("📏 === ANALYSE DES SMD (STANDARDIZED MEAN DIFFERENCES) ===\n")

# Obtenir le résumé de l'équilibre
balance_summary <- summary(m.out_evaluation, standardize = TRUE)
print(balance_summary)

# Extraire les SMD avant et après appariement
smd_before <- balance_summary$sum.all[, "Std. Mean Diff."]
smd_after <- balance_summary$sum.matched[, "Std. Mean Diff."]

# Créer un tableau de comparaison
variables <- rownames(balance_summary$sum.matched)
balance_table <- data.frame(
  Variable = variables,
  SMD_Before = round(smd_before, 3),
  SMD_After = round(smd_after, 3),
  Improvement = round(abs(smd_before) - abs(smd_after), 3),
  stringsAsFactors = FALSE
)

# Évaluer la qualité
smd_excellent <- sum(abs(smd_after) < 0.10, na.rm = TRUE)
smd_acceptable <- sum(abs(smd_after) >= 0.10 & abs(smd_after) < 0.25, na.rm = TRUE)
smd_poor <- sum(abs(smd_after) >= 0.25, na.rm = TRUE)
total_vars <- length(smd_after[!is.na(smd_after)])

cat("\n🎯 ÉVALUATION DES SMD :\n")
cat(paste("✅ Excellent équilibre (SMD < 0.10) :", smd_excellent, "/", total_vars, 
          "(", round(100*smd_excellent/total_vars, 1), "%)\n"))
cat(paste("⚠️  Équilibre acceptable (0.10 ≤ SMD < 0.25) :", smd_acceptable, "/", total_vars, 
          "(", round(100*smd_acceptable/total_vars, 1), "%)\n"))
cat(paste("❌ Équilibre insuffisant (SMD ≥ 0.25) :", smd_poor, "/", total_vars, 
          "(", round(100*smd_poor/total_vars, 1), "%)\n"))

# Interprétation globale
if(smd_poor == 0 && smd_acceptable <= 1) {
  cat("🎉 QUALITÉ GLOBALE : EXCELLENTE\n")
  quality_rating <- "Excellente"
} else if(smd_poor == 0) {
  cat("✅ QUALITÉ GLOBALE : BONNE\n")
  quality_rating <- "Bonne"
} else {
  cat("⚠️  QUALITÉ GLOBALE : À AMÉLIORER\n")
  quality_rating <- "À améliorer"
}

cat("\n📋 TABLEAU DÉTAILLÉ DES SMD :\n")
print(balance_table)

# ============================================================================
# 4. TESTS STATISTIQUES APRÈS APPARIEMENT
# ============================================================================

cat("\n🔬 === TESTS STATISTIQUES APRÈS APPARIEMENT ===\n")

# Récupérer les données appariées pour les tests
matched_data <- match.data(m.out_evaluation)
matched_data <- matched_data %>%
  mutate(Pair_ID = as.factor(subclass))

# Variables à tester
test_vars <- c("Age", "Sexe_Male", "BMI", "A_ASA_sup2", "A_tabac", 
               "A_immunosuppression", "I_arret_ATC", "A_ATCD_laparo_mediane", "I_scoreHinchey_4")

# Tests statistiques
test_results <- data.frame(
  Variable = character(0),
  Test_Type = character(0),
  P_Value = numeric(0),
  Interpretation = character(0),
  stringsAsFactors = FALSE
)

for(var in test_vars) {
  if(var %in% names(matched_data)) {
    tryCatch({
      if(is.numeric(matched_data[[var]])) {
        # Test de Wilcoxon pour variables continues
        test_result <- wilcox.test(matched_data[[var]] ~ matched_data$I_geste_comparaison_1HartmannCoelio, 
                                   paired = FALSE)
        test_type <- "Wilcoxon"
        p_value <- test_result$p.value
      } else {
        # Test de McNemar pour variables catégorielles (approximé par Chi2)
        test_result <- chisq.test(table(matched_data[[var]], matched_data$I_geste_comparaison_1HartmannCoelio))
        test_type <- "Chi-square"
        p_value <- test_result$p.value
      }
      
      interpretation <- ifelse(p_value > 0.05, "✅ Équilibré", "⚠️ Déséquilibré")
      
      test_results <- rbind(test_results, data.frame(
        Variable = var,
        Test_Type = test_type,
        P_Value = round(p_value, 4),
        Interpretation = interpretation,
        stringsAsFactors = FALSE
      ))
      
    }, error = function(e) {
      cat(paste("⚠️ Impossible de tester", var, ":", e$message, "\n"))
    })
  }
}

print(test_results)

# Résumé des tests
n_balanced <- sum(test_results$P_Value > 0.05, na.rm = TRUE)
n_total_tests <- nrow(test_results)

cat(paste("\n🎯 RÉSUMÉ DES TESTS STATISTIQUES :\n"))
cat(paste("Variables équilibrées (p > 0.05) :", n_balanced, "/", n_total_tests, 
          "(", round(100*n_balanced/n_total_tests, 1), "%)\n"))

# ============================================================================
# 5. TESTS DE DISCRIMINATION (AUC)
# ============================================================================

cat("\n🎯 === TEST DE DISCRIMINATION (AUC) ===\n")

tryCatch({
  # Calculer l'AUC
  roc_curve <- roc(dfps_evaluation$I_geste_comparaison_1HartmannCoelio, 
                   m.out_evaluation$distance, 
                   quiet = TRUE)
  auc_value <- as.numeric(auc(roc_curve))
  
  cat(paste("AUC (Area Under Curve) :", round(auc_value, 3), "\n"))
  
  # Interprétation
  if(auc_value <= 0.6) {
    cat("✅ DISCRIMINATION : Excellente (AUC ≤ 0.6)\n")
    auc_interpretation <- "Excellente"
  } else if(auc_value <= 0.8) {
    cat("✅ DISCRIMINATION : Bonne (0.6 < AUC ≤ 0.8)\n")
    auc_interpretation <- "Bonne"
  } else {
    cat("⚠️ DISCRIMINATION : Faible (AUC > 0.8)\n")
    auc_interpretation <- "Faible"
  }
  
  cat("💡 Note : Plus l'AUC est proche de 0.5, meilleur est l'appariement\n")
  
}, error = function(e) {
  cat("❌ Impossible de calculer l'AUC :", e$message, "\n")
  auc_value <- NA
  auc_interpretation <- "Non calculable"
})

# ============================================================================
# 6. TEST DE CALIBRATION (HOSMER-LEMESHOW)
# ============================================================================

cat("\n📊 === TEST DE CALIBRATION (HOSMER-LEMESHOW) ===\n")

tryCatch({
  # Test de Hosmer-Lemeshow
  hosmer_result <- hoslem.test(dfps_evaluation$I_geste_comparaison_1HartmannCoelio, 
                               m.out_evaluation$distance)
  
  cat(paste("Test de Hosmer-Lemeshow p-value :", round(hosmer_result$p.value, 4), "\n"))
  
  # Interprétation
  if(hosmer_result$p.value > 0.05) {
    cat("✅ CALIBRATION : Bonne (p > 0.05)\n")
    calibration_interpretation <- "Bonne"
  } else {
    cat("⚠️ CALIBRATION : À améliorer (p ≤ 0.05)\n")
    calibration_interpretation <- "À améliorer"
  }
  
  cat("💡 Note : p > 0.05 indique une bonne calibration du modèle\n")
  
}, error = function(e) {
  cat("❌ Impossible de réaliser le test de Hosmer-Lemeshow :", e$message, "\n")
  calibration_interpretation <- "Non calculable"
})

# ============================================================================
# 7. GRAPHIQUE LOVE PLOT
# ============================================================================

cat("\n📈 === CRÉATION DU LOVE PLOT ===\n")

tryCatch({
  # Créer le Love Plot
  love_plot <- love.plot(m.out_evaluation,
                         stats = "mean.diffs",
                         threshold = 0.1,
                         abs = TRUE,
                         var.order = "adjusted",
                         title = "Covariate Balance Before and After Matching",
                         subtitle = "Standardized Mean Differences") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # Afficher le graphique
  print(love_plot)
  
  # Sauvegarder
  ggsave("Love_Plot_Covariate_Balance.png", love_plot, 
         width = 10, height = 8, dpi = 300)
  ggsave("Love_Plot_Covariate_Balance.pdf", love_plot, 
         width = 10, height = 8)
  
  cat("✅ Love Plot sauvegardé : Love_Plot_Covariate_Balance.png/.pdf\n")
  
}, error = function(e) {
  cat("❌ Impossible de créer le Love Plot :", e$message, "\n")
})

# ============================================================================
# 8. RAPPORT DE QUALITÉ FINAL
# ============================================================================

cat("\n🎉 === RAPPORT DE QUALITÉ FINAL ===\n")

# Créer un tableau de synthèse
quality_summary <- data.frame(
  Critère = c("SMD < 0.10", "Tests statistiques", "Discrimination (AUC)", "Calibration (H-L)"),
  Résultat = c(
    paste(smd_excellent, "/", total_vars, " (", round(100*smd_excellent/total_vars, 1), "%)", sep=""),
    paste(n_balanced, "/", n_total_tests, " (", round(100*n_balanced/n_total_tests, 1), "%)", sep=""),
    ifelse(exists("auc_value") && !is.na(auc_value), paste(round(auc_value, 3)), "N/A"),
    ifelse(exists("hosmer_result"), paste("p =", round(hosmer_result$p.value, 3)), "N/A")
  ),
  Évaluation = c(
    ifelse(smd_excellent/total_vars >= 0.9, "✅ Excellent", 
           ifelse(smd_excellent/total_vars >= 0.7, "✅ Bon", "⚠️ À améliorer")),
    ifelse(n_balanced/n_total_tests >= 0.8, "✅ Bon", "⚠️ À améliorer"),
    ifelse(exists("auc_interpretation"), auc_interpretation, "N/A"),
    ifelse(exists("calibration_interpretation"), calibration_interpretation, "N/A")
  ),
  stringsAsFactors = FALSE
)

cat("📊 SYNTHÈSE DE LA QUALITÉ :\n")
print(quality_summary)

# Évaluation globale finale
excellent_criteria <- sum(grepl("Excellent", quality_summary$Évaluation))
good_criteria <- sum(grepl("Bon", quality_summary$Évaluation))
total_criteria <- sum(quality_summary$Évaluation != "N/A")

cat(paste("\n🏆 ÉVALUATION GLOBALE FINALE :\n"))
if(excellent_criteria >= 2 && good_criteria >= (total_criteria - excellent_criteria)) {
  cat("🎉 QUALITÉ D'APPARIEMENT : EXCELLENTE\n")
  final_quality <- "EXCELLENTE"
} else if(good_criteria >= round(total_criteria * 0.7)) {
  cat("✅ QUALITÉ D'APPARIEMENT : BONNE\n") 
  final_quality <- "BONNE"
} else {
  cat("⚠️ QUALITÉ D'APPARIEMENT : À AMÉLIORER\n")
  final_quality <- "À AMÉLIORER"
}

# ============================================================================
# 9. TEXTE POUR L'ARTICLE
# ============================================================================

cat("\n📝 === TEXTE POUR VOTRE ARTICLE ===\n")

cat("📄 SECTION METHODS :\n")
cat("\"Propensity score matching was performed using nearest neighbor matching\n")
cat("with a caliper of 0.20. Covariate balance was assessed using standardized\n")
cat("mean differences (SMD), with SMD < 0.10 considered excellent balance.\n")
cat("Matching quality was evaluated using the area under the ROC curve (AUC)\n")
cat("and Hosmer-Lemeshow calibration test.\"\n\n")

cat("📄 SECTION RESULTS :\n")
cat(paste("\"Propensity score matching successfully created", n_pairs, "matched pairs\n"))
cat(paste("from the original cohort of", n_original, "patients.\n"))

if(smd_excellent/total_vars >= 0.9) {
  cat("Excellent covariate balance was achieved with SMD < 0.10 for\n")
  cat(paste(round(100*smd_excellent/total_vars, 0), "% of all covariates (Table 1).\n"))
} else {
  cat(paste("Covariate balance was achieved with SMD < 0.10 for", smd_excellent, "out of", total_vars, "covariates.\n"))
}

if(exists("auc_value") && !is.na(auc_value)) {
  cat(paste("The area under the ROC curve was", round(auc_value, 2), ",\n"))
  cat("indicating", tolower(auc_interpretation), "matching quality.\n")
}

if(exists("hosmer_result")) {
  cat(paste("The Hosmer-Lemeshow test confirmed", tolower(calibration_interpretation), 
            "calibration (p =", round(hosmer_result$p.value, 3), ").\"\n"))
}

# ============================================================================
# 10. SAUVEGARDE DES RÉSULTATS
# ============================================================================

cat("\n💾 === SAUVEGARDE DES RÉSULTATS ===\n")

# Sauvegarder le tableau de balance
write.csv(balance_table, "Balance_Table_SMD.csv", row.names = FALSE)
cat("✅ Tableau de balance sauvegardé : Balance_Table_SMD.csv\n")

# Sauvegarder les résultats des tests
write.csv(test_results, "Statistical_Tests_Results.csv", row.names = FALSE)
cat("✅ Résultats des tests sauvegardés : Statistical_Tests_Results.csv\n")

# Sauvegarder le rapport de qualité
write.csv(quality_summary, "Quality_Assessment_Summary.csv", row.names = FALSE)
cat("✅ Rapport de qualité sauvegardé : Quality_Assessment_Summary.csv\n")

# Créer un rapport détaillé en format texte
sink("Quality_Assessment_Report.txt")
cat("RAPPORT D'ÉVALUATION DE LA QUALITÉ DE L'APPARIEMENT\n")
cat("=" , rep("=", 50), "\n")
cat("Date :", Sys.Date(), "\n\n")
cat("INFORMATIONS GÉNÉRALES :\n")
cat("Patients originaux :", n_original, "\n")
cat("Patients appariés :", n_matched, "\n") 
cat("Nombre de paires :", n_pairs, "\n")
cat("Taux d'appariement :", round(100 * n_matched / n_original, 1), "%\n\n")
cat("QUALITÉ FINALE :", final_quality, "\n")
sink()

cat("✅ Rapport détaillé sauvegardé : Quality_Assessment_Report.txt\n")

cat("\n🎊 === ÉVALUATION TERMINÉE ===\n")
cat("📋 Fichiers créés :\n")
cat("   - Balance_Table_SMD.csv\n")
cat("   - Statistical_Tests_Results.csv\n")
cat("   - Quality_Assessment_Summary.csv\n")
cat("   - Quality_Assessment_Report.txt\n")
cat("   - Love_Plot_Covariate_Balance.png/.pdf\n")
cat(paste("\n🏆 QUALITÉ FINALE DE L'APPARIEMENT :", final_quality, "\n"))

# ============================================================================
# PRÉPARATION POUR LA SUITE DU SCRIPT
# ============================================================================

cat("\n=== PRÉPARATION POUR LA SUITE DE L'ANALYSE ===\n")

# Pour la compatibilité avec le reste du script, utiliser le premier dataset apparié
if(length(matched_datasets) > 0) {
  m.final <- matched_datasets[[1]]
  
  # Vérification de la structure
  cat("Variable 'm.final' créée pour compatibilité avec le reste du script\n")
  cat(paste("Nombre de paires dans m.final:", length(unique(m.final$Pair_ID)), "\n"))
  cat(paste("Nombre total de patients dans m.final:", nrow(m.final), "\n"))
  
  # Vérifier la variable subclass pour compatibilité
  if(!"subclass" %in% names(m.final)) {
    m.final$subclass <- m.final$Pair_ID
  }
  
  cat("Structure de m.final:\n")
  str(m.final[, c("I_geste_comparaison_1HartmannCoelio", "Pair_ID", "subclass")])
  
} else {
  cat("ERREUR: Aucun dataset apparié disponible\n")
  m.final <- NULL
}

# Variables créées pour la suite de l'analyse:
cat("\n=== VARIABLES CRÉÉES ===\n")
cat("- rubin_results: Résultats poolés selon Rubin\n")
cat("- matched_datasets: Liste des 5 datasets appariés\n")
cat("- m.final: Premier dataset apparié (compatibilité)\n")
cat("- comparison_table: Comparaison des résultats individuels\n")
cat("- appariement_info: Informations détaillées sur chaque appariement\n")

# Résumé final
if(!is.null(rubin_results)) {
  cat(paste("\n🎯 RÉSULTAT PRINCIPAL:\n"))
  cat(paste("HR poolé =", round(rubin_results$HR, 3), 
            "[", round(rubin_results$CI_lower, 3), "-", round(rubin_results$CI_upper, 3), "]"))
  cat(paste(", p =", ifelse(rubin_results$p_value < 0.001, "< 0.001", 
                            formatC(rubin_results$p_value, format = "f", digits = 4)), "\n"))
}

cat("\n=== SCRIPT PRÊT POUR LA SUITE ===\n")
cat("Vous pouvez maintenant continuer avec vos tableaux et graphiques\n")


# ============================================================================
# SUITE DU SCRIPT APRÈS IMPUTATION MULTIPLE AVEC MÉTHODE DE RUBIN
# À utiliser après avoir exécuté le script d'imputation complet
# ============================================================================

# Vérification que les variables nécessaires existent
if(!exists("m.final") || !exists("rubin_results")) {
  stop("❌ Vous devez d'abord exécuter le script d'imputation multiple avec la méthode de Rubin")
}

cat("✅ Variables détectées:\n")
cat(paste("- m.final: Dataset apparié avec", nrow(m.final), "patients\n"))
cat(paste("- rubin_results: Résultats poolés disponibles\n"))
cat(paste("- matched_datasets: Liste de", length(matched_datasets), "datasets appariés\n"))

# ============================================================================
# TABLEAUX DESCRIPTIFS AJUSTÉS (avec données appariées)
# ============================================================================

cat("\n=== CRÉATION DES TABLEAUX DESCRIPTIFS AJUSTÉS ===\n")

#********APPARIE : tableaux********#
#Tests statistiques après score de propension

# 1 verification de la colonne "subclass" de m.final
cat("Vérification de l'appariement:\n")
table_subclass <- table(m.final$subclass)
cat(paste("Nombre de paires:", length(table_subclass), "\n"))
cat(paste("Total patients:", sum(table_subclass), "\n"))

# Vérifier que chaque paire a bien 2 patients (appariement 1:1)
if(all(table_subclass == 2)) {
  cat("✅ Appariement 1:1 correct - chaque paire a 2 patients\n")
} else {
  cat("⚠️  Attention: certaines paires n'ont pas exactement 2 patients\n")
  print(table(table_subclass))
}

# Creer une colonne identifiant les paires (déjà fait dans le script principal mais on vérifie)
if(!"Pair_ID" %in% names(m.final)) {
  m.final <- m.final %>%
    mutate(Pair_ID = as.factor(subclass))
}

# Vérifier la modification
cat("Vérification Pair_ID:\n")
print(head(m.final$Pair_ID, 10))

# ============================================================================
# TABLEAU 1 AJUSTÉ (Caractéristiques baseline)
# ============================================================================

cat("\n--- Création du Tableau 1 ajusté ---\n")

library(gtsummary)
library(dplyr)

table1adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table1),
    missing = "no",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    type = list(
      H_taille_abces ~ "continuous"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",  # Test de Wilcoxon pour les donnees continues
      all_categorical() ~ "mcnemar.test"  # Test de McNemar pour les donnees categoriques
    ),
    group = Pair_ID  # Utilisation de l'ID de paire
  )

# Affichage du tableau
print(table1adjusted)

# Convertir et sauvegarder
table1adjusted_gt <- as_gt(table1adjusted)
gtsave(table1adjusted_gt, file = "table1adjusted_rubin.docx")
cat("✅ Table 1 ajustée sauvegardée: table1adjusted_rubin.docx\n")

# ============================================================================
# TABLEAU 2 AJUSTÉ (Caractéristiques peropératoires)
# ============================================================================

cat("\n--- Création du Tableau 2 ajusté ---\n")

table2adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table2),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",
      all_categorical() ~ "mcnemar.test"
    ),
    group = Pair_ID
  )

print(table2adjusted)

table2adjusted_gt <- as_gt(table2adjusted)
gtsave(table2adjusted_gt, file = "table2adjusted_rubin.docx")
cat("✅ Table 2 ajustée sauvegardée: table2adjusted_rubin.docx\n")

# ============================================================================
# TABLEAU 3 AJUSTÉ (Résultats postopératoires)
# ============================================================================

cat("\n--- Création du Tableau 3 ajusté ---\n")

table3adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table3),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",
      all_categorical() ~ "mcnemar.test"
    ),
    group = Pair_ID
  )

print(table3adjusted)

table3adjusted_gt <- as_gt(table3adjusted)
gtsave(table3adjusted_gt, file = "table3adjusted_rubin.docx")
cat("✅ Table 3 ajustée sauvegardée: table3adjusted_rubin.docx\n")

# ============================================================================
# TABLEAU 4 AJUSTÉ (Suivi à long terme)
# ============================================================================

cat("\n--- Création du Tableau 4 ajusté ---\n")

table4adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table4),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous() ~ "paired.wilcox.test",
      all_categorical() ~ "mcnemar.test"
    ),
    group = Pair_ID
  )

print(table4adjusted)

table4adjusted_gt <- as_gt(table4adjusted)
gtsave(table4adjusted_gt, file = "table4adjusted_rubin.docx")
cat("✅ Table 4 ajustée sauvegardée: table4adjusted_rubin.docx\n")

# ============================================================================
# COURBE DE SURVIE AVEC RÉSULTATS POOLÉS DE RUBIN
# ============================================================================

cat("\n=== CRÉATION DE LA COURBE DE SURVIE ===\n")

# Couleurs personnalisées
colors <- c("#db1d88", "#4b6bd6")

library(survival)
library(survminer)
library(patchwork)
library(dplyr)

# 1) Préparation des données de survie
df_survie_apparie <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 730)
  ) %>%
  filter(!is.na(S_duree_avec_stomie) & !is.na(fermeture_stomie))

cat(paste("Patients avec stomie pour l'analyse de survie:", nrow(df_survie_apparie), "\n"))

# 2) Objet de survie et modèle de Kaplan-Meier
surv_object <- Surv(df_survie_apparie$S_duree_avec_stomie,
                    df_survie_apparie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                  data = df_survie_apparie)

# 3) Test log-rank (informatif, mais on utilisera les résultats poolés de Rubin)
test_logrank <- survdiff(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                         data = df_survie_apparie)
raw_p_value_logrank <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)

# IMPORTANT: Utiliser la p-value poolée de Rubin au lieu du log-rank simple
if(!is.null(rubin_results)) {
  p_value_display <- ifelse(rubin_results$p_value < 0.001, "< 0.001", 
                            formatC(rubin_results$p_value, format = "f", digits = 3))
  hr_display <- paste0("HR = ", round(rubin_results$HR, 2), " [", 
                       round(rubin_results$CI_lower, 2), "-", 
                       round(rubin_results$CI_upper, 2), "]")
  
  cat(paste("P-value utilisée (Rubin):", p_value_display, "\n"))
  cat(paste("HR poolé:", hr_display, "\n"))
} else {
  # Fallback sur le log-rank si pas de résultats Rubin
  p_value_display <- ifelse(raw_p_value_logrank < 0.001, "< 0.001",
                            formatC(raw_p_value_logrank, format = "f", digits = 3))
  hr_display <- "Non calculé"
  cat("⚠️  Utilisation du test log-rank simple (résultats Rubin non disponibles)\n")
}

# 4) Calculs pour le graphique
taux_final_fermeture <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie, na.rm = TRUE) * 100,
            .groups = "drop")

# Médianes pour les annotations
mediane_fermeture <- data.frame(
  I_geste_comparaison_1HartmannCoelio = c("HartmannCoelio", "RAlaparoProtegee"),
  mediane_jours = c(143.5, 115),
  y_label = c(-1, -4)
)

# Fonction pour récupérer la position sur la courbe
get_surv_at_time <- function(km_fit, groupe_name, temps) {
  # Convertir le nom du groupe en index
  group_levels <- names(km_fit$strata)
  group_index <- which(grepl(groupe_name, group_levels))
  
  if(length(group_index) == 0) {
    return(50)  # valeur par défaut
  }
  
  surv_data <- summary(km_fit, times = temps)$surv
  if(length(surv_data) >= group_index) {
    surv_percent <- 100 * (1 - surv_data[group_index])
    return(surv_percent)
  } else {
    return(50)  # valeur par défaut
  }
}

# Calculer les positions sur la courbe
mediane_fermeture$y_courbe <- sapply(seq_len(nrow(mediane_fermeture)), function(i) {
  get_surv_at_time(km_fit, 
                   mediane_fermeture$I_geste_comparaison_1HartmannCoelio[i],
                   mediane_fermeture$mediane_jours[i])
})

# 5) Création du graphique
p <- ggsurvplot(
  km_fit, 
  data = df_survie_apparie,
  fun = function(x) 100 * (1 - x),
  pval = FALSE,  # On affichera manuellement la p-value de Rubin
  palette = colors,
  conf.int = TRUE,
  xlim = c(0, 730),
  break.time.by = 100,
  risk.table = TRUE,
  risk.table.height = 0.25,
  risk.table.y.text.col = TRUE,
  risk.table.breaks = seq(0, 730, 100),
  risk.table.xlim = c(0, 730),
  xlab = "Time (days)",
  ylab = "Stoma reversal rate (%)",
  legend.title = "Surgical technique",
  legend.labs = c("Laparoscopic Hartmann", "Open Diverted anastomosis"),
  ggtheme = theme_classic()
)

# 6) Personnalisation du graphique
p$plot <- p$plot +
  guides(
    fill = "none",
    linetype = "none",
    color = guide_legend(override.aes = list(linetype = 1))
  ) +
  # Pourcentages finaux
  geom_text(
    data = taux_final_fermeture,
    aes(x = 730, y = pourcentage_fermeture,
        label = paste0(round(pourcentage_fermeture, 1), "%")),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  # P-value de Rubin et HR
  annotate("text", x = 300, y = 15,
           label = paste0("p = ", p_value_display, " (Rubin pooled)"),
           size = 4, hjust = 0) +
  annotate("text", x = 300, y = 10,
           label = hr_display,
           size = 4, hjust = 0) +
  # Note méthodologique
  annotate("text", x = 500, y = 5,
           label = "Multiple imputation analysis",
           size = 3, hjust = 0, style = "italic")

# 7) Assemblage final
combined_plot <- p$plot / p$table + plot_layout(heights = c(8, 2))

print(combined_plot)

# Sauvegarder le graphique
ggsave("courbe_survie_rubin.png", combined_plot, width = 12, height = 8, dpi = 300)
cat("✅ Courbe de survie sauvegardée: courbe_survie_rubin.png\n")

# ============================================================================
# ANALYSE DE SURVIE DÉTAILLÉE AVEC RÉSULTATS POOLÉS
# ============================================================================

cat("\n=== ANALYSE DE SURVIE DÉTAILLÉE ===\n")

# Extraire le taux de fermeture à 2 ans (730 jours)
if(length(summary(km_fit, times = 730)$surv) >= 2) {
  surv_2_ans <- summary(km_fit, times = 730)$surv
  taux_fermeture_730 <- 100 * (1 - surv_2_ans)
  names(taux_fermeture_730) <- c("Hartmann Coelio", "RA Protégée Laparo")
  
  cat("Taux de fermeture de stomie à 2 ans:\n")
  print(round(taux_fermeture_730, 1))
} else {
  cat("⚠️  Impossible de calculer les taux à 2 ans\n")
}

# Modèle de Cox sur le dataset apparié (pour comparaison avec Rubin)
cox_dataset1 <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ 
                        I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                      data = df_survie_apparie)

cat("\n--- Comparaison des résultats ---\n")
cat("Modèle de Cox (dataset apparié #1):\n")
cox_summary <- summary(cox_dataset1)
print(cox_summary)

if(!is.null(rubin_results)) {
  cat("\nRésultats poolés (Méthode de Rubin):\n")
  cat(paste("HR poolé:", round(rubin_results$HR, 3), "\n"))
  cat(paste("IC 95%: [", round(rubin_results$CI_lower, 3), "-", 
            round(rubin_results$CI_upper, 3), "]\n"))
  cat(paste("P-value poolée:", 
            ifelse(rubin_results$p_value < 0.001, "< 0.001", 
                   formatC(rubin_results$p_value, format = "f", digits = 4)), "\n"))
  cat(paste("Fraction de variance manquante (λ):", round(rubin_results$lambda, 3), "\n"))
  
  # Interprétation
  if(rubin_results$lambda < 0.1) {
    cat("➜ Impact faible de l'imputation multiple\n")
  } else if(rubin_results$lambda < 0.3) {
    cat("➜ Impact modéré de l'imputation multiple\n") 
  } else {
    cat("➜ Impact substantiel de l'imputation multiple\n")
  }
}

# ============================================================================
# TABLEAU DE SYNTHÈSE DES RÉSULTATS
# ============================================================================

cat("\n=== TABLEAU DE SYNTHÈSE ===\n")

# Créer un tableau comparatif
if(!is.null(rubin_results) && exists("cox_dataset1")) {
  
  synthese_resultats <- data.frame(
    Méthode = c("Dataset apparié #1", "Pooling de Rubin"),
    HR = c(round(exp(coef(cox_dataset1)), 3), round(rubin_results$HR, 3)),
    IC_inf = c(round(exp(confint(cox_dataset1))[1], 3), round(rubin_results$CI_lower, 3)),
    IC_sup = c(round(exp(confint(cox_dataset1))[2], 3), round(rubin_results$CI_upper, 3)),
    P_value = c(
      ifelse(summary(cox_dataset1)$coefficients[5] < 0.001, "< 0.001", 
             formatC(summary(cox_dataset1)$coefficients[5], format = "f", digits = 4)),
      ifelse(rubin_results$p_value < 0.001, "< 0.001", 
             formatC(rubin_results$p_value, format = "f", digits = 4))
    ),
    stringsAsFactors = FALSE
  )
  
  cat("Comparaison des méthodes d'analyse:\n")
  print(synthese_resultats)
  
  # Recommandation
  cat("\n🎯 RECOMMANDATION:\n")
  cat("Utilisez les résultats du 'Pooling de Rubin' pour votre publication.\n")
  cat("Cette méthode est statistiquement plus robuste car elle prend en compte\n")
  cat("l'incertitude due à l'imputation multiple des données manquantes.\n")
}

# ============================================================================
# SAUVEGARDE DES RÉSULTATS FINAUX
# ============================================================================

cat("\n=== SAUVEGARDE DES RÉSULTATS ===\n")

# Sauvegarder les résultats de Rubin
if(!is.null(rubin_results)) {
  write.csv(rubin_results, "resultats_rubin_pooled.csv", row.names = FALSE)
  cat("✅ Résultats poolés sauvegardés: resultats_rubin_pooled.csv\n")
}

# Sauvegarder le dataset apparié principal
write.csv(m.final, "dataset_apparie_principal.csv", row.names = FALSE)
cat("✅ Dataset apparié sauvegardé: dataset_apparie_principal.csv\n")

# Sauvegarder les informations de synthèse
if(exists("synthese_resultats")) {
  write.csv(synthese_resultats, "synthese_comparative.csv", row.names = FALSE)
  cat("✅ Synthèse comparative sauvegardée: synthese_comparative.csv\n")
}

cat("\n🎉 ANALYSE TERMINÉE AVEC SUCCÈS !\n")
cat("📋 Fichiers créés:\n")
cat("   - Tables descriptives: table[1-4]adjusted_rubin.docx\n")
cat("   - Courbe de survie: courbe_survie_rubin.png\n") 
cat("   - Résultats poolés: resultats_rubin_pooled.csv\n")
cat("   - Dataset apparié: dataset_apparie_principal.csv\n")
cat("   - Synthèse: synthese_comparative.csv\n")

cat("\n📊 RÉSULTATS PRINCIPAUX À RAPPORTER:\n")
if(!is.null(rubin_results)) {
  cat(paste("   HR =", round(rubin_results$HR, 2), 
            "[", round(rubin_results$CI_lower, 2), "-", round(rubin_results$CI_upper, 2), "]"))
  cat(paste(", p =", ifelse(rubin_results$p_value < 0.001, "< 0.001", 
                            formatC(rubin_results$p_value, format = "f", digits = 3)), "\n"))
  cat(paste("   Basé sur", rubin_results$N_datasets, "imputations multiples (méthode de Rubin)\n"))
}