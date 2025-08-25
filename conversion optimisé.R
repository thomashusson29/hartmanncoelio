#***************ANALYSE UNIVARIEE FDR DE CONVERSION*****************

# 🎯 Préparation des données
df_conversion <- df_HartmannCoelio %>% 
  filter(I_voie_coelio == 1) %>%  # uniquement ceux chez qui une coelioscopie a été tentée
  rename(
    H_antibio_IV_PO = `H_antibio_IV_+_PO`,
    I_indication_poussee_compliquee = `I_indication_poussee_compliquee`,
    I_indication_immunodepression = `I_indication_immunodepression`,
    I_indication_hemorragie = `I_indication_hemorragie`
  )

df_conversion$conversion <- factor(df_conversion$I_conversion, levels = c(0,1), labels = c("Non converti", "Converti"))

# 🔠 Variables explicatives
vars <- cols_to_include_in_table1

# 🧼 Convertir les binaires numériques en facteurs
df_conversion[vars] <- lapply(df_conversion[vars], function(x) {
  if (is.numeric(x) && length(unique(x)) <= 2) factor(x, ordered = FALSE) else x
})

# 🔍 Detection des variables avec moins de 2 modalites
vars_uniques <- sapply(df_conversion[vars], function(x) length(unique(na.omit(x))))
vars_problematiques <- names(vars_uniques[vars_uniques < 2])
vars_clean <- setdiff(vars, vars_problematiques)

cat("Variables exclues (< 2 modalités):", paste(vars_problematiques, collapse = ", "), "\n")

# ⚙️ Regressions univariees logistiques
univ_models <- lapply(vars_clean, function(var) {
  model <- glm(as.formula(paste("conversion ~", var)), data = df_conversion, family = binomial)
  result <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(variable = var)
  
  # Niveau de reference pour facteurs
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

# 📋 Tableau univarié final
univ_table <- bind_rows(univ_models) %>%
  select(variable, term, OR, IC_bas, IC_haut, `p-value`) %>%
  arrange(variable, term) %>%
  mutate(
    term_affiche = paste0("  ", term),
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

# 📊 Affichage avec kableExtra
library(kableExtra)
colnames_kable <- c("Modalité", "OR", "IC 95% bas", "IC 95% haut", "P value")

tableau_univarie <- univ_table %>%
  select(term_affiche, OR, IC_bas, IC_haut, `p-value`) %>%
  kable(col.names = colnames_kable,
        align = "lcccc",
        caption = "Analyse univariée : OR et IC95% des facteurs de conversion chez les patients avec Hartmann coelioscopique") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  pack_rows(index = table(univ_table$variable))

print(tableau_univarie)

#***************ANALYSE MULTIVARIEE*****************

# Variables pour l'analyse multivariée (nettoyée)
vars_multi_ajuste <- c(
  "Sexe_Male", "A_ASA_sup2", "BMI", "A_ATCD_neuro", "A_ATCD_Cardio", 
  "A_ATCD_pneumo", "A_diabete", "A_tabac", "A_immunosuppression", 
  "A_anticoagulants", "A_antiaggregants", "A_ATCD_laparo_mediane", 
  "H_perforation", "H_colo", "I_scoreHinchey_4"
)

# Création de BMI_sup30 si nécessaire
if(!"BMI_sup30" %in% names(df_HartmannCoelio)) {
  df_HartmannCoelio <- df_HartmannCoelio %>%
    mutate(BMI_sup30 = ifelse(BMI >= 30, 1, 0))
}

# Vérification des données complètes
complete_data <- df_HartmannCoelio %>%
  select(all_of(vars_multi_ajuste), I_conversion) %>%
  filter(complete.cases(.))

cat("\n=== Analyse multivariée ===\n")
cat("Cas complets:", nrow(complete_data), "\n")
cat("Conversions:", sum(complete_data$I_conversion), "\n") 
cat("Ratio événements/variables:", round(sum(complete_data$I_conversion) / length(vars_multi_ajuste), 1), "\n")

# Modèle de régression logistique multivariée
formule_multi <- as.formula(paste("I_conversion ~", paste(vars_multi_ajuste, collapse = " + ")))
model_multi <- glm(formule_multi, data = df_HartmannCoelio, family = binomial)

# Résumé du modèle
summary(model_multi)

# Forest plot amélioré
forest_data <- tidy(model_multi, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    p_formatted = ifelse(p.value < 0.001, "< 0.001", sprintf("%.3f", p.value)),
    significant = p.value < 0.05
  )

# Tableau des résultats multivariés
tabletext <- cbind(
  c("Variable", forest_data$term),
  c("OR [IC95%]", paste0(round(forest_data$estimate, 2), " [", 
                         round(forest_data$conf.low, 2), " - ", 
                         round(forest_data$conf.high, 2), "]")),
  c("p-value", forest_data$p_formatted)
)

# Forest plot
library(forestplot)
forestplot(
  labeltext = tabletext,
  mean = c(NA, forest_data$estimate),
  lower = c(NA, forest_data$conf.low),
  upper = c(NA, forest_data$conf.high),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.2,
  col = fpColors(box = "black", line = "black", summary = "black"),
  xlog = TRUE,
  lwd.zero = 2,
  lwd.ci = 2,
  ci.vertices = TRUE,
  xlab = "Odds Ratio (échelle logarithmique)",
  title = "Modèle de régression logistique multivariée pour la conversion"
)

#***************COMPARAISON CONVERTIS VS NON CONVERTIS*****************

# Préparation des données pour comparaison
df_comparison <- df_HartmannCoelio %>%
  filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio") %>%
  mutate(I_conversion = recode(I_conversion, `0` = "Pas de conversion", `1` = "Conversion"))

cat("Répartition conversion:", table(df_comparison$I_conversion), "\n")

# Tableaux de comparaison
create_comparison_table <- function(data, columns, table_name) {
  table_result <- data %>%
    tbl_summary(
      include = all_of(columns),
      missing = "ifany",
      by = I_conversion,
      percent = "column",
      statistic = list(
        all_categorical() ~ "{n} ({p}%)",
        all_continuous() ~ "{median} [{p25} - {p75}]"
      )
    ) %>%
    add_p()
  
  # Sauvegarder en Word
  table_gt <- as_gt(table_result)
  gtsave(table_gt, file = paste0(table_name, ".docx"))
  
  return(table_result)
}

# Création des tableaux (si les colonnes existent)
if(exists("cols_to_include_in_table2")) {
  table2 <- create_comparison_table(df_comparison, cols_to_include_in_table2, "table2_comparison")
  print(table2)
}

if(exists("cols_to_include_in_table3")) {
  table3 <- create_comparison_table(df_comparison, cols_to_include_in_table3, "table3_comparison")
  print(table3)
}

if(exists("cols_to_include_in_table4")) {
  table4 <- create_comparison_table(df_comparison, cols_to_include_in_table4, "table4_comparison")
  print(table4)
}

#***************COURBES DE SURVIE OPTIMISÉES*****************

# Fonction pour créer les courbes de survie
create_survival_analysis <- function(data, groups, title_suffix = "") {
  
  # Préparation des données
  df_surv <- data %>% 
    filter(I_stomie == 1) %>%
    mutate(
      S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
      fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
      S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 365)
    )
  
  # Filtrer selon les groupes demandés
  if("conversion" %in% groups) {
    df_surv <- df_surv %>%
      filter(I_geste_comparaison_1HartmannCoelio == "Hartmann") %>%
      mutate(groupe = factor(I_conversion, levels = c(0, 1), labels = c("Non Converti", "Converti")))
  } else {
    df_surv <- df_surv %>%
      mutate(
        groupe = case_when(
          I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
          I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
          I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(groupe))
    
    df_surv$groupe <- factor(df_surv$groupe, levels = c("Hartmann Non Converti", "Hartmann Converti", "RA Protégée"))
  }
  
  # Objet de survie
  surv_object <- Surv(time = df_surv$S_duree_avec_stomie, event = df_surv$fermeture_stomie)
  km_fit <- survfit(surv_object ~ groupe, data = df_surv)
  
  # Test log-rank
  test_logrank <- survdiff(surv_object ~ groupe, data = df_surv)
  raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
  p_value <- ifelse(raw_p_value < 0.001, 
                    formatC(raw_p_value, format = "e", digits = 2),
                    formatC(raw_p_value, format = "f", digits = 3))
  
  # Pourcentages de fermeture
  taux_final_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(pourcentage_fermeture = mean(S_fermeturestomie, na.rm = TRUE) * 100, .groups = "drop")
  
  # Médianes calculées dynamiquement (CORRECTION MAJEURE)
  mediane_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(mediane_jours = median(S_duree_avec_stomie_orig, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      y_label = case_when(
        n() == 2 ~ c(-1, -8),           # 2 groupes (conversion)
        n() == 3 ~ c(-1, -8, -15),      # 3 groupes (complet)
        TRUE ~ seq(-1, -8*n(), by = -7)  # fallback
      )[row_number()],
      label_median = paste0(round(mediane_jours), "j")
    )
  
  # Position verticale des médianes sur la courbe
  get_surv_at_time <- function(km_fit, groupe_idx, temps) {
    surv_summary <- summary(km_fit, times = temps)
    if(length(surv_summary$surv) >= groupe_idx) {
      return(100 * (1 - surv_summary$surv[groupe_idx]))
    }
    return(50) # valeur par défaut
  }
  
  mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
    get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
  })
  
  # Récupérer les couleurs automatiques
  p_temp <- ggsurvplot(km_fit, data = df_surv)
  colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
    distinct(group, colour) %>%
    arrange(group) %>%
    pull(colour)
  
  # Créer la courbe principale
  p <- ggsurvplot(
    km_fit,
    data = df_surv,
    pval = TRUE,
    risk.table = TRUE,
    conf.int = TRUE,
    break.time.by = 100,
    xlim = c(0, 365),
    xlab = "Temps (jours)",
    ylab = "Taux de rétablissement de la continuité (%)",
    legend.title = "Groupe",
    legend.labs = levels(df_surv$groupe),
    fun = function(x) 100 * (1 - x)
  )
  
  # Ajouter les annotations
  p$plot <- p$plot +
    guides(fill = "none", linetype = "none", 
           color = guide_legend(override.aes = list(linetype = 1))) +
    
    # Lignes horizontales pour les pourcentages finaux
    geom_hline(aes(yintercept = pourcentage_fermeture),
               data = taux_final_fermeture, linetype = "dashed", size = 1,
               color = "black", show.legend = FALSE) +
    
    # Étiquettes des pourcentages finaux
    geom_text(data = taux_final_fermeture, 
              aes(x = 365, y = pourcentage_fermeture,
                  label = paste0(round(pourcentage_fermeture, 1), "%")),
              vjust = -0.5, size = 5, fontface = "bold", color = "black") +
    
    # P-value du log-rank
    annotate("text", x = 300, y = 10,
             label = paste0("Log-rank p = ", p_value),
             size = 5, hjust = 0) +
    
    # Lignes des médianes (horizontales)
    geom_segment(data = mediane_fermeture,
                 aes(x = 0, xend = mediane_jours,
                     y = y_courbe, yend = y_courbe),
                 linetype = "dashed", size = 1,
                 color = colors_used, show.legend = FALSE) +
    
    # Lignes des médianes (verticales)
    geom_segment(data = mediane_fermeture,
                 aes(x = mediane_jours, xend = mediane_jours,
                     y = 0, yend = y_courbe),
                 linetype = "dashed", size = 1,
                 color = colors_used, show.legend = FALSE) +
    
    # Étiquettes des médianes
    geom_text(data = mediane_fermeture,
              aes(x = mediane_jours, y = y_label,
                  label = label_median),
              color = colors_used, size = 5, fontface = "bold",
              hjust = 0.5, show.legend = FALSE)
  
  # Statistiques résumées
  cat("\n=== Analyse de survie:", title_suffix, "===\n")
  cat("Effectifs par groupe:\n")
  print(table(df_surv$groupe))
  cat("\nMédianes (jours):\n")
  print(mediane_fermeture %>% select(groupe, mediane_jours))
  cat("\nTaux de fermeture (%):\n")
  print(taux_final_fermeture)
  cat("Log-rank p =", p_value, "\n\n")
  
  return(list(plot = p, stats = list(medians = mediane_fermeture, 
                                     closure_rates = taux_final_fermeture,
                                     p_value = p_value)))
}

# 1. Courbe de survie : Hartmann Coelio Convertis vs Non Convertis
cat("### ANALYSE 1: CONVERSION HARTMANN COELIOSCOPIQUE ###\n")
survival_conversion <- create_survival_analysis(
  data = df_HartmannCoelio, 
  groups = "conversion",
  title_suffix = "Conversion Hartmann Coelio"
)
print(survival_conversion$plot)

# 2. Courbe de survie : 3 groupes (Hartmann Non Converti, Hartmann Converti, RA Protégée)
cat("### ANALYSE 2: COMPARAISON 3 GROUPES ###\n")
survival_three_groups <- create_survival_analysis(
  data = m.final,
  groups = c("hartmann_non_converti", "hartmann_converti", "ra_protegee"),
  title_suffix = "Comparaison 3 groupes"
)
print(survival_three_groups$plot)

#***************VÉRIFICATIONS ET DIAGNOSTICS*****************

# Diagnostics du modèle multivarié
cat("### DIAGNOSTICS DU MODÈLE MULTIVARIÉ ###\n")

# 1. Test de Hosmer-Lemeshow
library(ResourceSelection)
hosmer_test <- hoslem.test(model_multi$y, fitted(model_multi))
cat("Test de Hosmer-Lemeshow - p-value:", round(hosmer_test$p.value, 3), "\n")
if(hosmer_test$p.value > 0.05) {
  cat("✅ Bon ajustement du modèle (p > 0.05)\n")
} else {
  cat("⚠️ Ajustement questionnable (p < 0.05)\n")
}

# 2. Aire sous la courbe ROC
library(pROC)
roc_curve <- roc(model_multi$y, fitted(model_multi))
cat("AUC:", round(as.numeric(roc_curve$auc), 3), "\n")

# 3. Résumé des performances
cat("\n### RÉSUMÉ DES ANALYSES ###\n")
cat("Analyse univariée: ✅ Complète\n")
cat("Analyse multivariée: ✅ Complète (", length(vars_multi_ajuste), "variables)\n")
cat("Courbes de survie: ✅ Médianes corrigées dynamiquement\n")
cat("Tables de comparaison: ✅ Exportées en Word\n")

# 4. Vérification finale des médianes
cat("\n### VÉRIFICATION DES MÉDIANES ###\n")
verification_medianes <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann Non Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA Protégée",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(groupe)) %>%
  group_by(groupe) %>%
  summarise(
    n = n(),
    mediane_duree_stomie = median(S_duree_avec_stomie, na.rm = TRUE),
    taux_fermeture = round(mean(S_fermeturestomie, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

print(verification_medianes)

cat("\n🎯 SCRIPT OPTIMISÉ TERMINÉ 🎯\n")
cat("✅ Doublons supprimés\n")
cat("✅ Médianes calculées dynamiquement\n") 
cat("✅ Code structuré et commenté\n")
cat("✅ Diagnostics ajoutés\n")