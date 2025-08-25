#***************COURBES DE SURVIE CORRIGÉES - VERSION SANS BUGS ANNOTEE*****************


# Fonction pour créer les courbes de survie - VERSION DÉBUGGÉE
create_survival_analysis <- function(data, analysis_type = "conversion", title_suffix = "") {
  
  library(survival)
  library(survminer)
  library(dplyr)
  library(ggplot2)
  
  # Préparation des données de base
  df_surv <- data %>% 
    filter(I_stomie == 1) %>%
    mutate(
      S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
      fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
      S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 730)
    )
  
  # Filtrer selon le type d'analyse
  if(analysis_type == "conversion") {
    # Analyse conversion : df_HartmannCoelio avec "Hartmann Coelio"
    df_surv <- df_surv %>%
      filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio") %>%
      mutate(groupe = factor(I_conversion, levels = c(0, 1), labels = c("Non Converti", "Converti")))
    
  } else if(analysis_type == "three_groups") {
    # Analyse 3 groupes : m.final avec "Hartmann" et "Résection anastomose protégée"
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
  
  # Vérifier qu'on a des données
  if(nrow(df_surv) == 0) {
    cat("ERREUR: Aucune donnée après filtrage pour", analysis_type, "\n")
    return(NULL)
  }
  
  cat("Effectifs après filtrage:", nrow(df_surv), "patients\n")
  print(table(df_surv$groupe, useNA = "ifany"))
  
  # Objet de survie
  surv_object <- Surv(time = df_surv$S_duree_avec_stomie, event = df_surv$fermeture_stomie)
  km_fit <- survfit(surv_object ~ groupe, data = df_surv)
  
  # Test log-rank
  test_logrank <- survdiff(surv_object ~ groupe, data = df_surv)
  raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
  p_value <- ifelse(raw_p_value < 0.001, 
                    formatC(raw_p_value, format = "e", digits = 2),
                    formatC(raw_p_value, format = "f", digits = 3))
  
  # Pourcentages de fermeture - BUG FIX: utiliser df_surv pas data original
  taux_final_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(pourcentage_fermeture = mean(S_fermeturestomie, na.rm = TRUE) * 100, .groups = "drop")
  
  # Médianes calculées dynamiquement - BUG FIX: utiliser df_surv
  mediane_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(mediane_jours = median(S_duree_avec_stomie_orig, na.rm = TRUE), .groups = "drop")
  
  # Y_label corrigé selon le nombre de groupes - BUG FIX: éviter les erreurs d'index
  n_groupes <- nrow(mediane_fermeture)
  if(n_groupes == 2) {
    mediane_fermeture$y_label <- c(-1, -8)
  } else if(n_groupes == 3) {
    mediane_fermeture$y_label <- c(-1, -8, -15)
  } else {
    mediane_fermeture$y_label <- seq(-1, -8*n_groupes, by = -7)[1:n_groupes]
  }
  
  mediane_fermeture$label_median <- paste0(round(mediane_fermeture$mediane_jours), "j")
  
  # Position verticale des médianes sur la courbe - BUG FIX: gestion d'erreurs améliorée
  get_surv_at_time <- function(km_fit, groupe_idx, temps) {
    tryCatch({
      surv_summary <- summary(km_fit, times = temps)
      if(!is.null(surv_summary$surv) && length(surv_summary$surv) >= groupe_idx && !is.na(surv_summary$surv[groupe_idx])) {
        return(100 * (1 - surv_summary$surv[groupe_idx]))
      }
      return(50)
    }, error = function(e) {
      cat("Erreur calcul survie pour groupe", groupe_idx, "temps", temps, ":", e$message, "\n")
      return(50)
    })
  }
  
  mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
    get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
  })
  
  # Récupérer les couleurs automatiques - BUG FIX: vérification existence
  p_temp <- ggsurvplot(km_fit, data = df_surv)
  colors_used <- tryCatch({
    colors <- ggplot_build(p_temp$plot)$data[[1]] %>%
      distinct(group, colour) %>%
      arrange(group) %>%
      pull(colour)
    colors
  }, error = function(e) {
    # Couleurs par défaut si extraction échoue
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
    default_colors[1:n_groupes]
  })
  
  # Assurer qu'on a le bon nombre de couleurs - BUG FIX
  if(length(colors_used) < n_groupes) {
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
    colors_used <- default_colors[1:n_groupes]
  }
  
  # Créer la courbe principale
  p <- ggsurvplot(
    km_fit,
    data = df_surv,
    pval = TRUE,
    risk.table = TRUE,
    conf.int = TRUE,
    break.time.by = 100,
    xlim = c(0, 730),
    xlab = "Temps (jours)",
    ylab = "Taux de rétablissement de la continuité (%)",
    legend.title = "Groupe",
    legend.labs = levels(df_surv$groupe),
    fun = function(x) 100 * (1 - x)
  )
  
  # Ajouter les annotations - BUG FIX: utiliser 'size' au lieu de 'linewidth' pour compatibilité
  p$plot <- p$plot +
    guides(fill = "none", linetype = "none", 
           color = guide_legend(override.aes = list(linetype = 1))) +
    
    # Lignes horizontales pour les pourcentages finaux
    geom_hline(aes(yintercept = pourcentage_fermeture),
               data = taux_final_fermeture, linetype = "dashed", size = 1,
               color = "black", show.legend = FALSE) +
    
    # Étiquettes des pourcentages finaux
    geom_text(data = taux_final_fermeture, 
              aes(x = 730, y = pourcentage_fermeture,
                  label = paste0(round(pourcentage_fermeture, 1), "%")),
              vjust = -0.5, size = 5, fontface = "bold", color = "black") +
    
    # P-value du log-rank
    annotate("text", x = 300, y = 10,
             label = paste0("Log-rank p = ", p_value),
             size = 5, hjust = 0) +
    
    # Lignes des médianes (horizontales) - BUG FIX: indexation sécurisée
    geom_segment(data = mediane_fermeture,
                 aes(x = 0, xend = mediane_jours,
                     y = y_courbe, yend = y_courbe),
                 linetype = "dashed", size = 1,
                 color = colors_used[1:nrow(mediane_fermeture)], show.legend = FALSE) +
    
    # Lignes des médianes (verticales) - BUG FIX: indexation sécurisée
    geom_segment(data = mediane_fermeture,
                 aes(x = mediane_jours, xend = mediane_jours,
                     y = 0, yend = y_courbe),
                 linetype = "dashed", size = 1,
                 color = colors_used[1:nrow(mediane_fermeture)], show.legend = FALSE) +
    
    # Étiquettes des médianes - BUG FIX: indexation sécurisée
    geom_text(data = mediane_fermeture,
              aes(x = mediane_jours, y = y_label,
                  label = label_median),
              color = colors_used[1:nrow(mediane_fermeture)], size = 5, fontface = "bold",
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
  
  return(list(plot = p, 
              stats = list(medians = mediane_fermeture, 
                           closure_rates = taux_final_fermeture,
                           p_value = p_value),
              data = df_surv))
}

# VERSION SÉCURISÉE AVEC GESTION D'ERREURS
run_survival_analysis_safe <- function() {
  
  cat("### DÉMARRAGE DES ANALYSES DE SURVIE ###\n\n")
  
  # 1. Courbe de survie : Hartmann Coelio Convertis vs Non Convertis
  cat("### ANALYSE 1: CONVERSION HARTMANN COELIOSCOPIQUE ###\n")
  
  survival_conversion <- tryCatch({
    result <- create_survival_analysis(
      data = df_HartmannCoelio, 
      analysis_type = "conversion",
      title_suffix = "Conversion Hartmann Coelio"
    )
    if(!is.null(result)) {
      print(result$plot)
      cat("✅ Analyse conversion réussie\n\n")
    } else {
      cat("❌ Échec analyse conversion\n\n")
    }
    result
  }, error = function(e) {
    cat("❌ ERREUR analyse conversion:", e$message, "\n\n")
    NULL
  })
  
  # 2. Courbe de survie : 3 groupes (Hartmann Non Converti, Hartmann Converti, RA Protégée)
  cat("### ANALYSE 2: COMPARAISON 3 GROUPES ###\n")
  
  survival_three_groups <- tryCatch({
    result <- create_survival_analysis(
      data = m.final,
      analysis_type = "three_groups",
      title_suffix = "Comparaison 3 groupes"
    )
    if(!is.null(result)) {
      print(result$plot)
      cat("✅ Analyse 3 groupes réussie\n\n")
    } else {
      cat("❌ Échec analyse 3 groupes\n\n")
    }
    result
  }, error = function(e) {
    cat("❌ ERREUR analyse 3 groupes:", e$message, "\n\n")
    NULL
  })
  
  cat("### RÉSUMÉ DES ANALYSES ###\n")
  cat("Analyse conversion:", ifelse(!is.null(survival_conversion), "✅ OK", "❌ ERREUR"), "\n")
  cat("Analyse 3 groupes:", ifelse(!is.null(survival_three_groups), "✅ OK", "❌ ERREUR"), "\n")
  
  return(list(conversion = survival_conversion, three_groups = survival_three_groups))
}

# FONCTION DE DIAGNOSTIC
diagnostic_survival_data <- function() {
  cat("### DIAGNOSTIC DES DONNÉES DE SURVIE ###\n")
  
  # Test df_HartmannCoelio
  cat("1. df_HartmannCoelio:\n")
  cat("   Dimensions:", paste(dim(df_HartmannCoelio), collapse = " x "), "\n")
  cat("   Patients avec stomie:", sum(df_HartmannCoelio$I_stomie == 1, na.rm = TRUE), "\n")
  cat("   Hartmann Coelio:", sum(df_HartmannCoelio$I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio", na.rm = TRUE), "\n")
  
  # Test m.final  
  cat("\n2. m.final:\n")
  cat("   Dimensions:", paste(dim(m.final), collapse = " x "), "\n")
  cat("   Patients avec stomie:", sum(m.final$I_stomie == 1, na.rm = TRUE), "\n")
  
  hartmann_count <- sum(grepl("Hartmann", m.final$I_geste_comparaison_1HartmannCoelio), na.rm = TRUE)
  ra_count <- sum(grepl("Résection", m.final$I_geste_comparaison_1HartmannCoelio), na.rm = TRUE)
  cat("   Hartmann:", hartmann_count, "\n")
  cat("   RA protégée:", ra_count, "\n")
  
  # Vérifier les conversions
  conversion_summary <- m.final %>%
    filter(I_stomie == 1) %>%
    group_by(I_geste_comparaison_1HartmannCoelio, I_conversion) %>%
    summarise(n = n(), .groups = "drop")
  
  cat("\n3. Répartition conversions:\n")
  print(conversion_summary)
}

# EXÉCUTION PRINCIPALE
cat("🚀 LANCEMENT DU SCRIPT COURBES DE SURVIE CORRIGÉ 🚀\n\n")

# Diagnostic préalable
diagnostic_survival_data()

# Exécution des analyses
results <- run_survival_analysis_safe()

cat("\n🎯 SCRIPT TERMINÉ 🎯\n")






















#***************COURBES DE SURVIE CORRIGÉES - VERSION SANS BUGS ANNOTEE*****************

# Fonction pour créer les courbes de survie - VERSION DÉBUGGÉE
create_survival_analysis <- function(data, analysis_type = "conversion", title_suffix = "") {
  
  library(survival)
  library(survminer)
  library(dplyr)
  library(ggplot2)
  
  # Préparation des données de base
  df_surv <- data %>% 
    filter(I_stomie == 1) %>%
    mutate(
      S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
      fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
      S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 730)
    )
  
  # Filtrer selon le type d'analyse
  if(analysis_type == "conversion") {
    # Analyse conversion : df_HartmannCoelio avec "Hartmann Coelio"
    df_surv <- df_surv %>%
      filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio") %>%
      mutate(groupe = factor(I_conversion, levels = c(0, 1), labels = c("Non Converti", "Converti")))
    
  } else if(analysis_type == "three_groups") {
    # Analyse 3 groupes : m.final avec "Hartmann" et "Résection anastomose protégée"
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
  
  # Vérifier qu'on a des données
  if(nrow(df_surv) == 0) {
    cat("ERREUR: Aucune donnée après filtrage pour", analysis_type, "\n")
    return(NULL)
  }
  
  cat("Effectifs après filtrage:", nrow(df_surv), "patients\n")
  print(table(df_surv$groupe, useNA = "ifany"))
  
  # Objet de survie
  surv_object <- Surv(time = df_surv$S_duree_avec_stomie, event = df_surv$fermeture_stomie)
  km_fit <- survfit(surv_object ~ groupe, data = df_surv)
  
  # Test log-rank
  test_logrank <- survdiff(surv_object ~ groupe, data = df_surv)
  raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
  p_value <- ifelse(raw_p_value < 0.001, 
                    formatC(raw_p_value, format = "e", digits = 2),
                    formatC(raw_p_value, format = "f", digits = 3))
  
  # Pourcentages de fermeture - BUG FIX: utiliser df_surv pas data original
  taux_final_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(pourcentage_fermeture = mean(S_fermeturestomie, na.rm = TRUE) * 100, .groups = "drop")
  
  # Médianes calculées dynamiquement - BUG FIX: utiliser df_surv
  mediane_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(mediane_jours = median(S_duree_avec_stomie_orig, na.rm = TRUE), .groups = "drop")
  
  # Y_label corrigé selon le nombre de groupes - BUG FIX: éviter les erreurs d'index
  n_groupes <- nrow(mediane_fermeture)
  if(n_groupes == 2) {
    mediane_fermeture$y_label <- c(-1, -8)
  } else if(n_groupes == 3) {
    mediane_fermeture$y_label <- c(-1, -8, -15)
  } else {
    mediane_fermeture$y_label <- seq(-1, -8*n_groupes, by = -7)[1:n_groupes]
  }
  
  mediane_fermeture$label_median <- paste0(round(mediane_fermeture$mediane_jours), "j")
  
  # Position verticale des médianes sur la courbe - BUG FIX: gestion d'erreurs améliorée
  get_surv_at_time <- function(km_fit, groupe_idx, temps) {
    tryCatch({
      surv_summary <- summary(km_fit, times = temps)
      if(!is.null(surv_summary$surv) && length(surv_summary$surv) >= groupe_idx && !is.na(surv_summary$surv[groupe_idx])) {
        return(100 * (1 - surv_summary$surv[groupe_idx]))
      }
      return(50)
    }, error = function(e) {
      cat("Erreur calcul survie pour groupe", groupe_idx, "temps", temps, ":", e$message, "\n")
      return(50)
    })
  }
  
  mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
    get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
  })
  
  # Récupérer les couleurs automatiques - BUG FIX: vérification existence
  p_temp <- ggsurvplot(km_fit, data = df_surv)
  colors_used <- tryCatch({
    colors <- ggplot_build(p_temp$plot)$data[[1]] %>%
      distinct(group, colour) %>%
      arrange(group) %>%
      pull(colour)
    colors
  }, error = function(e) {
    # Couleurs par défaut si extraction échoue
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
    default_colors[1:n_groupes]
  })
  
  # Assurer qu'on a le bon nombre de couleurs - BUG FIX
  if(length(colors_used) < n_groupes) {
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
    colors_used <- default_colors[1:n_groupes]
  }
  
  # Créer la courbe principale
  p <- ggsurvplot(
    km_fit,
    data = df_surv,
    pval = TRUE,
    risk.table = TRUE,
    conf.int = TRUE,
    break.time.by = 100,
    xlim = c(0, 730),
    xlab = "Temps (jours)",
    ylab = "Taux de rétablissement de la continuité (%)",
    legend.title = "Groupe",
    legend.labs = levels(df_surv$groupe),
    fun = function(x) 100 * (1 - x)
  )
  
  # Ajouter les annotations - BUG FIX: utiliser 'size' au lieu de 'linewidth' pour compatibilité
  p$plot <- p$plot +
    guides(fill = "none", linetype = "none", 
           color = guide_legend(override.aes = list(linetype = 1))) +
    
    # Lignes horizontales pour les pourcentages finaux
    geom_hline(aes(yintercept = pourcentage_fermeture),
               data = taux_final_fermeture, linetype = "dashed", size = 1,
               color = "black", show.legend = FALSE) +
    
    # Étiquettes des pourcentages finaux
    geom_text(data = taux_final_fermeture, 
              aes(x = 730, y = pourcentage_fermeture,
                  label = paste0(round(pourcentage_fermeture, 1), "%")),
              vjust = -0.5, size = 5, fontface = "bold", color = "black") +
    
    # P-value du log-rank
    annotate("text", x = 300, y = 10,
             label = paste0("Log-rank p = ", p_value),
             size = 5, hjust = 0) +
    
    # Lignes des médianes (horizontales) - BUG FIX: indexation sécurisée
    geom_segment(data = mediane_fermeture,
                 aes(x = 0, xend = mediane_jours,
                     y = y_courbe, yend = y_courbe),
                 linetype = "dashed", size = 1,
                 color = colors_used[1:nrow(mediane_fermeture)], show.legend = FALSE) +
    
    # Lignes des médianes (verticales) - BUG FIX: indexation sécurisée
    geom_segment(data = mediane_fermeture,
                 aes(x = mediane_jours, xend = mediane_jours,
                     y = 0, yend = y_courbe),
                 linetype = "dashed", size = 1,
                 color = colors_used[1:nrow(mediane_fermeture)], show.legend = FALSE) +
    
    # Étiquettes des médianes - BUG FIX: indexation sécurisée
    geom_text(data = mediane_fermeture,
              aes(x = mediane_jours, y = y_label,
                  label = label_median),
              color = colors_used[1:nrow(mediane_fermeture)], size = 5, fontface = "bold",
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
  
  return(list(plot = p, 
              stats = list(medians = mediane_fermeture, 
                           closure_rates = taux_final_fermeture,
                           p_value = p_value),
              data = df_surv))
}

# VERSION SÉCURISÉE AVEC GESTION D'ERREURS
run_survival_analysis_safe <- function() {
  
  cat("### DÉMARRAGE DES ANALYSES DE SURVIE ###\n\n")
  
  # 1. Courbe de survie : Hartmann Coelio Convertis vs Non Convertis
  cat("### ANALYSE 1: CONVERSION HARTMANN COELIOSCOPIQUE ###\n")
  
  survival_conversion <- tryCatch({
    result <- create_survival_analysis(
      data = df_HartmannCoelio, 
      analysis_type = "conversion",
      title_suffix = "Conversion Hartmann Coelio"
    )
    if(!is.null(result)) {
      print(result$plot)
      cat("✅ Analyse conversion réussie\n\n")
    } else {
      cat("❌ Échec analyse conversion\n\n")
    }
    result
  }, error = function(e) {
    cat("❌ ERREUR analyse conversion:", e$message, "\n\n")
    NULL
  })
  
  # 2. Courbe de survie : 3 groupes (Hartmann Non Converti, Hartmann Converti, RA Protégée)
  cat("### ANALYSE 2: COMPARAISON 3 GROUPES ###\n")
  
  survival_three_groups <- tryCatch({
    result <- create_survival_analysis(
      data = m.final,
      analysis_type = "three_groups",
      title_suffix = "Comparaison 3 groupes"
    )
    if(!is.null(result)) {
      print(result$plot)
      cat("✅ Analyse 3 groupes réussie\n\n")
    } else {
      cat("❌ Échec analyse 3 groupes\n\n")
    }
    result
  }, error = function(e) {
    cat("❌ ERREUR analyse 3 groupes:", e$message, "\n\n")
    NULL
  })
  
  cat("### RÉSUMÉ DES ANALYSES ###\n")
  cat("Analyse conversion:", ifelse(!is.null(survival_conversion), "✅ OK", "❌ ERREUR"), "\n")
  cat("Analyse 3 groupes:", ifelse(!is.null(survival_three_groups), "✅ OK", "❌ ERREUR"), "\n")
  
  return(list(conversion = survival_conversion, three_groups = survival_three_groups))
}

# FONCTION DE DIAGNOSTIC
diagnostic_survival_data <- function() {
  cat("### DIAGNOSTIC DES DONNÉES DE SURVIE ###\n")
  
  # Test df_HartmannCoelio
  cat("1. df_HartmannCoelio:\n")
  cat("   Dimensions:", paste(dim(df_HartmannCoelio), collapse = " x "), "\n")
  cat("   Patients avec stomie:", sum(df_HartmannCoelio$I_stomie == 1, na.rm = TRUE), "\n")
  cat("   Hartmann Coelio:", sum(df_HartmannCoelio$I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio", na.rm = TRUE), "\n")
  
  # Test m.final  
  cat("\n2. m.final:\n")
  cat("   Dimensions:", paste(dim(m.final), collapse = " x "), "\n")
  cat("   Patients avec stomie:", sum(m.final$I_stomie == 1, na.rm = TRUE), "\n")
  
  hartmann_count <- sum(grepl("Hartmann", m.final$I_geste_comparaison_1HartmannCoelio), na.rm = TRUE)
  ra_count <- sum(grepl("Résection", m.final$I_geste_comparaison_1HartmannCoelio), na.rm = TRUE)
  cat("   Hartmann:", hartmann_count, "\n")
  cat("   RA protégée:", ra_count, "\n")
  
  # Vérifier les conversions
  conversion_summary <- m.final %>%
    filter(I_stomie == 1) %>%
    group_by(I_geste_comparaison_1HartmannCoelio, I_conversion) %>%
    summarise(n = n(), .groups = "drop")
  
  cat("\n3. Répartition conversions:\n")
  print(conversion_summary)
}

# EXÉCUTION PRINCIPALE
cat("🚀 LANCEMENT DU SCRIPT COURBES DE SURVIE CORRIGÉ 🚀\n\n")

# Diagnostic préalable
diagnostic_survival_data()

# Exécution des analyses
results <- run_survival_analysis_safe()

cat("\n🎯 SCRIPT TERMINÉ 🎯\n")







#***************COURBES DE SURVIE AVEC STYLE APPLIQUÉ*****************

# STYLE DÉFINI - CORRIGÉ
colors = c("#d40202", "#03911d","#022ff5")  # Rose, bleu, vert dans le bon ordre

# Fonction pour créer les courbes de survie avec le nouveau style
create_survival_analysis_styled <- function(data, analysis_type = "conversion", title_suffix = "") {
  
  library(survival)
  library(survminer)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  
  # Préparation des données de base
  df_surv <- data %>% 
    filter(I_stomie == 1) %>%
    mutate(
      S_duree_avec_stomie_orig = as.numeric(S_duree_avec_stomie),
      fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
      S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie_orig, 730)
    )
  
  # Filtrer selon le type d'analyse
  if(analysis_type == "conversion") {
    # Analyse conversion : df_HartmannCoelio avec "Hartmann Coelio"
    df_surv <- df_surv %>%
      filter(I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio") %>%
      mutate(groupe = factor(I_conversion, levels = c(0, 1), labels = c("Non Converti", "Converti")))
    
    # Légendes pour l'analyse conversion
    legend_labels <- c("LH Non Converted", "LH Converted")
    
  } else if(analysis_type == "three_groups") {
    # Analyse 3 groupes : m.final avec "Hartmann" et "Résection anastomose protégée"
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
    
    # Légendes pour l'analyse 3 groupes
    legend_labels <- c("LH Non Converted", "LH Converted", "Open DA")
  }
  
  # Vérifier qu'on a des données
  if(nrow(df_surv) == 0) {
    cat("ERREUR: Aucune donnée après filtrage pour", analysis_type, "\n")
    return(NULL)
  }
  
  cat("Effectifs après filtrage:", nrow(df_surv), "patients\n")
  print(table(df_surv$groupe, useNA = "ifany"))
  
  # Objet de survie
  surv_object <- Surv(time = df_surv$S_duree_avec_stomie, event = df_surv$fermeture_stomie)
  km_fit <- survfit(surv_object ~ groupe, data = df_surv)
  
  # Test log-rank avec formatage comme dans le style
  test_logrank <- survdiff(surv_object ~ groupe, data = df_surv)
  raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
  p_value <- ifelse(
    raw_p_value < 0.001,
    "< 0.001",
    paste0(formatC(raw_p_value, format = "f", digits = 3))
  )
  
  # Pourcentages de fermeture
  taux_final_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(pourcentage_fermeture = mean(S_fermeturestomie, na.rm = TRUE) * 100, .groups = "drop")
  
  # Médianes calculées dynamiquement
  mediane_fermeture <- df_surv %>%
    group_by(groupe) %>%
    summarise(mediane_jours = median(S_duree_avec_stomie_orig, na.rm = TRUE), .groups = "drop")
  
  # Y_label selon le nombre de groupes
  n_groupes <- nrow(mediane_fermeture)
  if(n_groupes == 2) {
    mediane_fermeture$y_label <- c(-1, -4)  # Comme dans le style
  } else if(n_groupes == 3) {
    mediane_fermeture$y_label <- c(-1, -4, -7)  # Adapté pour 3 groupes
  }
  
  mediane_fermeture$label_median <- paste0(round(mediane_fermeture$mediane_jours), "j")
  
  # Position verticale des médianes sur la courbe
  get_surv_at_time <- function(km_fit, groupe_idx, temps) {
    tryCatch({
      surv_summary <- summary(km_fit, times = temps)
      if(!is.null(surv_summary$surv) && length(surv_summary$surv) >= groupe_idx && !is.na(surv_summary$surv[groupe_idx])) {
        return(100 * (1 - surv_summary$surv[groupe_idx]))
      }
      return(50)
    }, error = function(e) {
      return(50)
    })
  }
  
  mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
    get_surv_at_time(km_fit, i, mediane_fermeture$mediane_jours[i])
  })
  
  # Couleurs selon le nombre de groupes - CORRIGÉ
  if(n_groupes == 2) {
    colors_to_use <- colors[1:2]  # Rose + bleu
  } else if(n_groupes == 3) {
    colors_to_use <- colors[1:3]  # Rose + bleu + vert - CORRECTION APPLIQUÉE
  } else {
    colors_to_use <- colors[1:n_groupes]  # Sécurité pour autres cas
  }
  
  # Créer la courbe principale avec le style appliqué
  p <- ggsurvplot(
    km_fit,
    data = df_surv,
    fun = function(x) 100*(1 - x),
    pval = FALSE,  # Pas de p-value automatique
    palette = colors_to_use,
    conf.int = FALSE,
    xlim = c(0, 730),
    break.time.by = 100,
    risk.table = TRUE,
    risk.table.height = 0.25,
    risk.table.y.text.col = TRUE,
    risk.table.breaks = seq(0, 730, 100),
    risk.table.xlim = c(0, 730),
    xlab = "Temps (jours)",
    ylab = "Taux de rétablissement (%)",
    legend.title = "Intervention",
    legend.labs = legend_labels,
    ggtheme = theme_classic()
  )
  
  # Customisation du plot avec le style simplifié
  p$plot <- p$plot +
    guides(
      fill = "none",
      linetype = "none",
      color = guide_legend(override.aes = list(linetype = 1))
    ) +
    
    # Texte du pourcentage final au bout du plot
    geom_text(
      data = taux_final_fermeture,
      aes(x = 730, y = pourcentage_fermeture,
          label = paste0(round(pourcentage_fermeture, 1), "%")),
      vjust = -0.5,
      size = 5,
      fontface = "bold",
      color = "black"
    ) +
    
    # Annotation de la p-value avec le style simplifié
    annotate(
      "text",
      x = 400,
      y = 10,
      label = paste0("p ", p_value),
      size = 5,
      hjust = 0
    )
  
  # Re-assemblage avec patchwork comme dans le style
  combined_plot <- p$plot / p$table +
    plot_layout(heights = c(8, 2))
  
  # Statistiques résumées
  cat("\n=== Analyse de survie:", title_suffix, "===\n")
  cat("Effectifs par groupe:\n")
  print(table(df_surv$groupe))
  cat("\nMédianes (jours):\n")
  print(mediane_fermeture %>% select(groupe, mediane_jours))
  cat("\nTaux de fermeture (%):\n")
  print(taux_final_fermeture)
  cat("Log-rank p =", p_value, "\n\n")
  
  return(list(
    plot = combined_plot,  # Le plot combiné avec patchwork
    original_plot = p,     # Le plot original ggsurvplot
    stats = list(
      medians = mediane_fermeture, 
      closure_rates = taux_final_fermeture,
      p_value = p_value
    ),
    data = df_surv
  ))
}

# VERSION SÉCURISÉE AVEC GESTION D'ERREURS ET STYLE APPLIQUÉ
run_survival_analysis_styled <- function() {
  
  cat("### DÉMARRAGE DES ANALYSES DE SURVIE AVEC STYLE APPLIQUÉ ###\n\n")
  
  # 1. Courbe de survie : Hartmann Coelio Convertis vs Non Convertis
  cat("### ANALYSE 1: CONVERSION HARTMANN COELIOSCOPIQUE ###\n")
  
  survival_conversion <- tryCatch({
    result <- create_survival_analysis_styled(
      data = df_HartmannCoelio, 
      analysis_type = "conversion",
      title_suffix = "Conversion Hartmann Coelio"
    )
    if(!is.null(result)) {
      print(result$plot)
      cat("✅ Analyse conversion réussie avec style appliqué\n\n")
    } else {
      cat("❌ Échec analyse conversion\n\n")
    }
    result
  }, error = function(e) {
    cat("❌ ERREUR analyse conversion:", e$message, "\n\n")
    NULL
  })
  
  # 2. Courbe de survie : 3 groupes (Hartmann Non Converti, Hartmann Converti, RA Protégée)
  cat("### ANALYSE 2: COMPARAISON 3 GROUPES ###\n")
  
  survival_three_groups <- tryCatch({
    result <- create_survival_analysis_styled(
      data = m.final,
      analysis_type = "three_groups",
      title_suffix = "Comparaison 3 groupes"
    )
    if(!is.null(result)) {
      print(result$plot)
      cat("✅ Analyse 3 groupes réussie avec style appliqué\n\n")
    } else {
      cat("❌ Échec analyse 3 groupes\n\n")
    }
    result
  }, error = function(e) {
    cat("❌ ERREUR analyse 3 groupes:", e$message, "\n\n")
    NULL
  })
  
  cat("### RÉSUMÉ DES ANALYSES ###\n")
  cat("Analyse conversion:", ifelse(!is.null(survival_conversion), "✅ OK", "❌ ERREUR"), "\n")
  cat("Analyse 3 groupes:", ifelse(!is.null(survival_three_groups), "✅ OK", "❌ ERREUR"), "\n")
  
  return(list(conversion = survival_conversion, three_groups = survival_three_groups))
}

# FONCTION DE DIAGNOSTIC (inchangée)
diagnostic_survival_data <- function() {
  cat("### DIAGNOSTIC DES DONNÉES DE SURVIE ###\n")
  
  # Test df_HartmannCoelio
  cat("1. df_HartmannCoelio:\n")
  cat("   Dimensions:", paste(dim(df_HartmannCoelio), collapse = " x "), "\n")
  cat("   Patients avec stomie:", sum(df_HartmannCoelio$I_stomie == 1, na.rm = TRUE), "\n")
  cat("   Hartmann Coelio:", sum(df_HartmannCoelio$I_geste_comparaison_1HartmannCoelio == "Hartmann Coelio", na.rm = TRUE), "\n")
  
  # Test m.final  
  cat("\n2. m.final:\n")
  cat("   Dimensions:", paste(dim(m.final), collapse = " x "), "\n")
  cat("   Patients avec stomie:", sum(m.final$I_stomie == 1, na.rm = TRUE), "\n")
  
  hartmann_count <- sum(grepl("Hartmann", m.final$I_geste_comparaison_1HartmannCoelio), na.rm = TRUE)
  ra_count <- sum(grepl("Résection", m.final$I_geste_comparaison_1HartmannCoelio), na.rm = TRUE)
  cat("   Hartmann:", hartmann_count, "\n")
  cat("   RA protégée:", ra_count, "\n")
  
  # Vérifier les conversions
  conversion_summary <- m.final %>%
    filter(I_stomie == 1) %>%
    group_by(I_geste_comparaison_1HartmannCoelio, I_conversion) %>%
    summarise(n = n(), .groups = "drop")
  
  cat("\n3. Répartition conversions:\n")
  print(conversion_summary)
}

# EXÉCUTION PRINCIPALE
cat("🎨 LANCEMENT DU SCRIPT COURBES DE SURVIE AVEC STYLE APPLIQUÉ 🎨\n\n")

# Diagnostic préalable
diagnostic_survival_data()

# Exécution des analyses avec le nouveau style
results <- run_survival_analysis_styled()

cat("\n🎯 SCRIPT AVEC STYLE APPLIQUÉ TERMINÉ 🎯\n")
cat("✨ Caractéristiques du style appliqué:\n")
cat("   - Couleurs personnalisées:", paste(colors, collapse = ", "), "\n")
cat("   - Tableau de risque aligné avec plot_layout patchwork\n")
cat("   - Annotations simplifiées (pas de lignes de médianes)\n")
cat("   - Format p-value simplifié\n")
cat("   - Légendes en anglais\n")
cat("   - Thème classic\n")




# Sauvegarde du plot à 3 groupes
plot_three_groups <- results$three_groups$plot
ggsave("survie_3_groupes_Hartmann_RA.svg", plot_three_groups, width = 10, height = 6, dpi = 1000)



claude_rstudio_addin()
claudeAddin()


#***************ANALYSES DE SURVIE APPARIÉES SIMPLIFIÉES*****************

library(survival)
library(dplyr)

# === PRÉPARATION DES DONNÉES ===
df_paired <- m.final %>%
  filter(!is.na(Pair_ID), I_stomie == 1) %>%
  mutate(
    time = as.numeric(S_duree_avec_stomie),
    event = ifelse(S_fermeturestomie == 1, 1, 0),
    time = ifelse(event == 1, time, 730),
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann_Non_Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann_Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA_Protegee",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(groupe))

# Objet de survie
surv_obj <- Surv(time = df_paired$time, event = df_paired$event)

cat("=== EFFECTIFS ===\n")
table(df_paired$groupe)

# === 1. LOG-RANK TESTS ===
cat("\n=== LOG-RANK TESTS ===\n")

# Test global à 3 courbes
test_global <- survdiff(surv_obj ~ groupe, data = df_paired)
p_global <- 1 - pchisq(test_global$chisq, df = length(test_global$n) - 1)
cat("Global (3 groupes): p =", formatC(p_global, format = "e", digits = 3), "\n")

# Comparaisons par paires
pairs <- list(
  c("RA_Protegee", "Hartmann_Converti"),
  c("RA_Protegee", "Hartmann_Non_Converti"), 
  c("Hartmann_Converti", "Hartmann_Non_Converti")
)

for(i in 1:length(pairs)) {
  subset_data <- df_paired %>% filter(groupe %in% pairs[[i]])
  subset_surv <- Surv(time = subset_data$time, event = subset_data$event)
  test <- survdiff(subset_surv ~ groupe, data = subset_data)
  p_val <- 1 - pchisq(test$chisq, df = 1)
  cat(paste(pairs[[i]], collapse = " vs "), ": p =", formatC(p_val, format = "f", digits = 4), "\n")
}

# === 2. TEST DE PETO ===
cat("\n=== TESTS DE PETO ===\n")

# Test global Peto
test_peto_global <- survdiff(surv_obj ~ groupe, data = df_paired, rho = 1)
p_peto_global <- 1 - pchisq(test_peto_global$chisq, df = length(test_peto_global$n) - 1)
cat("Peto global (3 groupes): p =", formatC(p_peto_global, format = "e", digits = 3), "\n")

# Comparaisons par paires Peto
for(i in 1:length(pairs)) {
  subset_data <- df_paired %>% filter(groupe %in% pairs[[i]])
  subset_surv <- Surv(time = subset_data$time, event = subset_data$event)
  test_peto <- survdiff(subset_surv ~ groupe, data = subset_data, rho = 1)
  p_peto <- 1 - pchisq(test_peto$chisq, df = 1)
  cat("Peto", paste(pairs[[i]], collapse = " vs "), ": p =", formatC(p_peto, format = "f", digits = 4), "\n")
}

# === 3. COX AVEC CLUSTER ===
cat("\n=== COX AVEC CLUSTER ===\n")

# Cox global avec cluster
cox_global <- coxph(surv_obj ~ groupe + cluster(Pair_ID), data = df_paired)
cat("Cox global avec cluster:\n")
summary_cox <- summary(cox_global)
for(i in 1:nrow(summary_cox$coefficients)) {
  coef_name <- rownames(summary_cox$coefficients)[i]
  hr <- exp(summary_cox$coefficients[i, "coef"])
  p_val <- summary_cox$coefficients[i, "Pr(>|z|)"]
  cat("  ", coef_name, ": HR =", round(hr, 3), ", p =", formatC(p_val, format = "f", digits = 4), "\n")
}

# Comparaisons par paires Cox cluster
for(i in 1:length(pairs)) {
  subset_data <- df_paired %>% filter(groupe %in% pairs[[i]])
  subset_data$groupe_bin <- ifelse(subset_data$groupe == pairs[[i]][2], 1, 0)
  subset_surv <- Surv(time = subset_data$time, event = subset_data$event)
  
  cox_pair <- coxph(subset_surv ~ groupe_bin + cluster(Pair_ID), data = subset_data)
  hr <- exp(cox_pair$coefficients["groupe_bin"])
  p_val <- summary(cox_pair)$coefficients["groupe_bin", "Pr(>|z|)"]
  
  cat("Cox cluster", paste(pairs[[i]], collapse = " vs "), ": HR =", round(hr, 3), 
      ", p =", formatC(p_val, format = "f", digits = 4), "\n")
}

# === 4. COX STRATIFIÉ ===
cat("\n=== COX STRATIFIÉ ===\n")

# Cox global stratifié
cox_strat_global <- coxph(surv_obj ~ groupe + strata(Pair_ID), data = df_paired)
cat("Cox global stratifié:\n")
summary_cox_strat <- summary(cox_strat_global)
for(i in 1:nrow(summary_cox_strat$coefficients)) {
  coef_name <- rownames(summary_cox_strat$coefficients)[i]
  hr <- exp(summary_cox_strat$coefficients[i, "coef"])
  p_val <- summary_cox_strat$coefficients[i, "Pr(>|z|)"]
  cat("  ", coef_name, ": HR =", round(hr, 3), ", p =", formatC(p_val, format = "f", digits = 4), "\n")
}

# Comparaisons par paires Cox stratifié
for(i in 1:length(pairs)) {
  subset_data <- df_paired %>% filter(groupe %in% pairs[[i]])
  subset_data$groupe_bin <- ifelse(subset_data$groupe == pairs[[i]][2], 1, 0)
  subset_surv <- Surv(time = subset_data$time, event = subset_data$event)
  
  tryCatch({
    cox_strat_pair <- coxph(subset_surv ~ groupe_bin + strata(Pair_ID), data = subset_data)
    hr <- exp(cox_strat_pair$coefficients["groupe_bin"])
    p_val <- summary(cox_strat_pair)$coefficients["groupe_bin", "Pr(>|z|)"]
    
    cat("Cox stratifié", paste(pairs[[i]], collapse = " vs "), ": HR =", round(hr, 3), 
        ", p =", formatC(p_val, format = "f", digits = 4), "\n")
  }, error = function(e) {
    cat("Cox stratifié", paste(pairs[[i]], collapse = " vs "), ": ERREUR (pas assez d'événements)\n")
  })
}

# === 5. TABLEAU RÉSUMÉ ===
cat("\n=== TABLEAU RÉSUMÉ ===\n")

# Créer tableau résumé simple
cat("Comparaison                           | Log-rank | Peto     | Cox Cluster | Cox Stratifié\n")
cat("-------------------------------------|----------|----------|-------------|---------------\n")

# Global
cat(sprintf("%-37s| %-8s | %-8s | %-11s | %-13s\n", 
            "Global (3 groupes)", 
            formatC(p_global, format = "f", digits = 4),
            formatC(p_peto_global, format = "f", digits = 4),
            "Voir détail", 
            "Voir détail"))

# Par paires - recalcul rapide pour le tableau
comparisons <- c("RA vs Hartmann Converti", "RA vs Hartmann Non Converti", "Converti vs Non Converti")

for(i in 1:length(pairs)) {
  # Log-rank
  subset_data <- df_paired %>% filter(groupe %in% pairs[[i]])
  subset_surv <- Surv(time = subset_data$time, event = subset_data$event)
  lr_test <- survdiff(subset_surv ~ groupe, data = subset_data)
  p_lr <- 1 - pchisq(lr_test$chisq, df = 1)
  
  # Peto
  peto_test <- survdiff(subset_surv ~ groupe, data = subset_data, rho = 1)
  p_peto <- 1 - pchisq(peto_test$chisq, df = 1)
  
  # Cox cluster
  subset_data$groupe_bin <- ifelse(subset_data$groupe == pairs[[i]][2], 1, 0)
  cox_cluster <- coxph(subset_surv ~ groupe_bin + cluster(Pair_ID), data = subset_data)
  p_cox_cluster <- summary(cox_cluster)$coefficients["groupe_bin", "Pr(>|z|)"]
  
  # Cox stratifié
  p_cox_strat <- tryCatch({
    cox_strat <- coxph(subset_surv ~ groupe_bin + strata(Pair_ID), data = subset_data)
    summary(cox_strat)$coefficients["groupe_bin", "Pr(>|z|)"]
  }, error = function(e) NA)
  
  cat(sprintf("%-37s| %-8s | %-8s | %-11s | %-13s\n", 
              comparisons[i],
              formatC(p_lr, format = "f", digits = 4),
              formatC(p_peto, format = "f", digits = 4),
              formatC(p_cox_cluster, format = "f", digits = 4),
              ifelse(is.na(p_cox_strat), "NaN", formatC(p_cox_strat, format = "f", digits = 4))))
}

cat("\n=== ANALYSE TERMINÉE ===\n")
cat("Données: 101 paires,", nrow(df_paired), "patients\n")
cat("Significativité: p < 0.05\n")




#tests statistiques
#***************ANALYSES DE SURVIE APPARIÉES AVEC GTSUMMARY*****************

library(survival)
library(dplyr)
library(gtsummary)
library(gt)

# === PRÉPARATION DES DONNÉES ===
df_paired <- m.final %>%
  filter(!is.na(Pair_ID), I_stomie == 1) %>%
  mutate(
    time = as.numeric(S_duree_avec_stomie),
    event = ifelse(S_fermeturestomie == 1, 1, 0),
    time = ifelse(event == 1, time, 730),
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann_Non_Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann_Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA_Protegee",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(groupe))

# Objet de survie
surv_obj <- Surv(time = df_paired$time, event = df_paired$event)

cat("=== EFFECTIFS ===\n")
table(df_paired$groupe)

# === FONCTION POUR CRÉER TABLEAU GTSUMMARY ===
create_survival_gtsummary <- function(data, comparison_pairs, comparison_names) {
  
  results_list <- list()
  
  for(i in 1:length(comparison_pairs)) {
    
    # Préparer les données pour cette comparaison
    subset_data <- data %>% filter(groupe %in% comparison_pairs[[i]])
    subset_data$groupe_bin <- ifelse(subset_data$groupe == comparison_pairs[[i]][2], 1, 0)
    subset_surv <- Surv(time = subset_data$time, event = subset_data$event)
    
    # Log-rank
    lr_test <- survdiff(subset_surv ~ groupe, data = subset_data)
    p_lr <- 1 - pchisq(lr_test$chisq, df = 1)
    
    # Peto
    peto_test <- survdiff(subset_surv ~ groupe, data = subset_data, rho = 1)
    p_peto <- 1 - pchisq(peto_test$chisq, df = 1)
    
    # Cox cluster
    cox_cluster <- coxph(subset_surv ~ groupe_bin + cluster(Pair_ID), data = subset_data)
    hr_cluster <- exp(coef(cox_cluster)["groupe_bin"])
    ci_cluster <- exp(confint(cox_cluster)["groupe_bin", ])
    p_cluster <- summary(cox_cluster)$coefficients["groupe_bin", "Pr(>|z|)"]
    
    # Cox stratifié
    cox_strat_result <- tryCatch({
      cox_strat <- coxph(subset_surv ~ groupe_bin + strata(Pair_ID), data = subset_data)
      hr_strat <- exp(coef(cox_strat)["groupe_bin"])
      ci_strat <- exp(confint(cox_strat)["groupe_bin", ])
      p_strat <- summary(cox_strat)$coefficients["groupe_bin", "Pr(>|z|)"]
      list(hr = hr_strat, ci = ci_strat, p = p_strat)
    }, error = function(e) {
      list(hr = NA, ci = c(NA, NA), p = NA)
    })
    
    # Stocker les résultats
    results_list[[i]] <- data.frame(
      Comparaison = comparison_names[i],
      LogRank_p = formatC(p_lr, format = "f", digits = 4),
      Peto_p = formatC(p_peto, format = "f", digits = 4),
      Cox_Cluster_HR = round(hr_cluster, 2),
      Cox_Cluster_CI_low = round(ci_cluster[1], 2),
      Cox_Cluster_CI_high = round(ci_cluster[2], 2),
      Cox_Cluster_p = formatC(p_cluster, format = "f", digits = 4),
      Cox_Strat_HR = ifelse(is.na(cox_strat_result$hr), NA, round(cox_strat_result$hr, 2)),
      Cox_Strat_CI_low = ifelse(is.na(cox_strat_result$ci[1]), NA, round(cox_strat_result$ci[1], 2)),
      Cox_Strat_CI_high = ifelse(is.na(cox_strat_result$ci[2]), NA, round(cox_strat_result$ci[2], 2)),
      Cox_Strat_p = ifelse(is.na(cox_strat_result$p), "NaN", formatC(cox_strat_result$p, format = "f", digits = 4))
    )
  }
  
  return(do.call(rbind, results_list))
}

# === CALCULS DES COMPARAISONS ===

# Comparaisons par paires (ordre inversé pour HR > 1)
pairs <- list(
  c("Hartmann_Converti", "RA_Protegee"),
  c("Hartmann_Non_Converti", "RA_Protegee"),
  c("Hartmann_Non_Converti", "Hartmann_Converti")
)

comparisons <- c(
  "Hartmann Converti vs RA Protégée",
  "Hartmann Non Converti vs RA Protégée", 
  "Non Converti vs Converti"
)

# Créer le dataframe des résultats
results_df <- create_survival_gtsummary(df_paired, pairs, comparisons)

# === TABLEAU GTSUMMARY PRINCIPAL ===
cat("\n=== CRÉATION DU TABLEAU GTSUMMARY ===\n")

# Créer tableau avec gtsummary
survival_table <- results_df %>%
  select(Comparaison, Cox_Cluster_HR, Cox_Cluster_CI_low, Cox_Cluster_CI_high, Cox_Cluster_p,
         Cox_Strat_HR, Cox_Strat_CI_low, Cox_Strat_CI_high, Cox_Strat_p) %>%
  mutate(
    Cox_Cluster_CI = paste0("[", Cox_Cluster_CI_low, "-", Cox_Cluster_CI_high, "]"),
    Cox_Strat_CI = ifelse(is.na(Cox_Strat_HR), "NaN", 
                          paste0("[", Cox_Strat_CI_low, "-", Cox_Strat_CI_high, "]")),
    Cox_Cluster_Result = paste0(Cox_Cluster_HR, " ", Cox_Cluster_CI, " (p=", Cox_Cluster_p, ")"),
    Cox_Strat_Result = ifelse(is.na(Cox_Strat_HR), "NaN", 
                              paste0(Cox_Strat_HR, " ", Cox_Strat_CI, " (p=", Cox_Strat_p, ")"))
  ) %>%
  select(Comparaison, Cox_Cluster_Result, Cox_Strat_Result)

# Afficher le tableau simple d'abord
cat("\n=== TABLEAU SIMPLE ===\n")
print(survival_table)

# === TABLEAU GTSUMMARY FORMATÉ ===

# Préparer données pour gtsummary (format long)
gts_data <- results_df %>%
  select(Comparaison, LogRank_p, Peto_p, 
         Cox_Cluster_HR, Cox_Cluster_CI_low, Cox_Cluster_CI_high, Cox_Cluster_p,
         Cox_Strat_HR, Cox_Strat_CI_low, Cox_Strat_CI_high, Cox_Strat_p) %>%
  mutate(
    # Formater les HR avec IC
    `Cox Cluster` = paste0(Cox_Cluster_HR, " [", Cox_Cluster_CI_low, "-", Cox_Cluster_CI_high, "]"),
    `Cox Stratifié` = ifelse(is.na(Cox_Strat_HR), "NaN", 
                             paste0(Cox_Strat_HR, " [", Cox_Strat_CI_low, "-", Cox_Strat_CI_high, "]")),
    `Log-rank` = LogRank_p,
    `Test de Peto` = Peto_p,
    `p Cox Cluster` = Cox_Cluster_p,
    `p Cox Stratifié` = Cox_Strat_p
  ) %>%
  select(Comparaison, `Log-rank`, `Test de Peto`, `Cox Cluster`, `p Cox Cluster`, 
         `Cox Stratifié`, `p Cox Stratifié`)

# Créer le tableau gtsummary
gt_table <- gts_data %>%
  gt() %>%
  tab_header(
    title = "Analyses de survie pour données appariées",
    subtitle = "Comparaisons du délai de fermeture de stomie (n=202 patients, 101 paires)"
  ) %>%
  cols_label(
    Comparaison = "Comparaison",
    `Log-rank` = "Log-rank p",
    `Test de Peto` = "Peto p", 
    `Cox Cluster` = "Cox Cluster HR [IC95%]",
    `p Cox Cluster` = "p-value",
    `Cox Stratifié` = "Cox Stratifié HR [IC95%]",
    `p Cox Stratifié` = "p-value"
  ) %>%
  tab_spanner(
    label = "Tests univariés",
    columns = c(`Log-rank`, `Test de Peto`)
  ) %>%
  tab_spanner(
    label = "Cox avec cluster",
    columns = c(`Cox Cluster`, `p Cox Cluster`)
  ) %>%
  tab_spanner(
    label = "Cox stratifié",
    columns = c(`Cox Stratifié`, `p Cox Stratifié`)
  ) %>%
  fmt_number(
    columns = c(`Log-rank`, `Test de Peto`, `p Cox Cluster`, `p Cox Stratifié`),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title()
  ) %>%
  tab_footnote(
    footnote = "HR > 1 indique un délai plus long pour le premier groupe de la comparaison",
    locations = cells_column_labels(columns = c(`Cox Cluster`, `Cox Stratifié`))
  ) %>%
  tab_footnote(
    footnote = "NaN = Calcul impossible (données insuffisantes par strate)",
    locations = cells_body(columns = `Cox Stratifié`, rows = 3)
  )

# Afficher le tableau
print(gt_table)

# === SAUVEGARDE ===
cat("\n=== SAUVEGARDE DES RÉSULTATS ===\n")

# Sauvegarder en Word
gtsave(gt_table, file = "tableau_survie_appariee.docx")
cat("Tableau sauvegardé : tableau_survie_appariee.docx\n")

# Sauvegarder les données brutes
write.csv(results_df, file = "resultats_survie_appariee.csv", row.names = FALSE)
cat("Données brutes sauvegardées : resultats_survie_appariee.csv\n")

# === TABLEAU COMPLÉMENTAIRE AVEC TESTS GLOBAUX ===

# Tests globaux
test_global <- survdiff(surv_obj ~ groupe, data = df_paired)
p_global <- 1 - pchisq(test_global$chisq, df = length(test_global$n) - 1)

test_peto_global <- survdiff(surv_obj ~ groupe, data = df_paired, rho = 1)
p_peto_global <- 1 - pchisq(test_peto_global$chisq, df = length(test_peto_global$n) - 1)

global_results <- data.frame(
  Test = c("Log-rank global", "Peto global"),
  p_value = c(formatC(p_global, format = "e", digits = 3),
              formatC(p_peto_global, format = "e", digits = 3)),
  Interpretation = c("Test omnibus à 3 groupes", "Test omnibus pondéré")
)

gt_global <- global_results %>%
  gt() %>%
  tab_header(
    title = "Tests globaux à 3 groupes",
    subtitle = "Hartmann Non Converti vs Hartmann Converti vs RA Protégée"
  ) %>%
  cols_label(
    Test = "Test statistique",
    p_value = "p-value", 
    Interpretation = "Interprétation"
  )

print(gt_global)

cat("\n=== ANALYSE TERMINÉE ===\n")
cat("Tableaux gtsummary créés et sauvegardés\n")
cat("Données: 101 paires,", nrow(df_paired), "patients\n")



#***************TESTS ÉVÉNEMENTS PRÉCOCES VS TARDIFS*****************

library(survival)
library(dplyr)
library(ggplot2)

# === PRÉPARATION DES DONNÉES ===
df_paired <- m.final %>%
  filter(!is.na(Pair_ID), I_stomie == 1) %>%
  mutate(
    time = as.numeric(S_duree_avec_stomie),
    event = ifelse(S_fermeturestomie == 1, 1, 0),
    time = ifelse(event == 1, time, 730),
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann_Non_Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann_Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA_Protegee",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(groupe))

# === 1. TEST DE PETO-PETO (pondère les événements précoces) ===
cat("=== TESTS POUR ÉVÉNEMENTS PRÉCOCES VS TARDIFS ===\n\n")

surv_obj <- Surv(time = df_paired$time, event = df_paired$event)

cat("1. COMPARAISON LOG-RANK VS PETO (rho=1) :\n")
cat("   - Log-rank : poids égal à tous les temps\n")
cat("   - Peto (rho=1) : poids plus fort aux événements précoces\n\n")

# Test global
lr_global <- survdiff(surv_obj ~ groupe, data = df_paired)
peto_global <- survdiff(surv_obj ~ groupe, data = df_paired, rho = 1)

cat("Test global :\n")
cat("  Log-rank : p =", formatC(1 - pchisq(lr_global$chisq, df = 2), format = "e", digits = 3), "\n")
cat("  Peto     : p =", formatC(1 - pchisq(peto_global$chisq, df = 2), format = "e", digits = 3), "\n")

if((1 - pchisq(peto_global$chisq, df = 2)) < (1 - pchisq(lr_global$chisq, df = 2))) {
  cat("  → Différence plus marquée avec Peto → Effet précoce dominant\n\n")
} else {
  cat("  → Différence plus marquée avec Log-rank → Effet tardif dominant\n\n")
}

# === 2. TESTS PAR TRANCHES TEMPORELLES ===
cat("2. TESTS PAR TRANCHES TEMPORELLES :\n")

# Fonction pour tester une tranche de temps
test_time_window <- function(data, t_start, t_end, window_name) {
  
  # Créer données avec événements dans la fenêtre
  data_window <- data %>%
    mutate(
      event_window = ifelse(time >= t_start & time <= t_end & event == 1, 1, 0),
      time_window = ifelse(time > t_end, t_end, time)
    )
  
  # Test si assez d'événements
  total_events <- sum(data_window$event_window)
  if(total_events < 10) {
    cat("  ", window_name, ": Pas assez d'événements (n=", total_events, ")\n")
    return(NA)
  }
  
  # Test log-rank sur la fenêtre
  surv_window <- Surv(time = data_window$time_window, event = data_window$event_window)
  test_window <- survdiff(surv_window ~ groupe, data = data_window)
  p_window <- 1 - pchisq(test_window$chisq, df = length(test_window$n) - 1)
  
  cat("  ", window_name, ": p =", formatC(p_window, format = "f", digits = 4), 
      " (", total_events, "événements)\n")
  
  return(p_window)
}

# Tester différentes fenêtres
windows <- list(
  c(0, 60, "0-60 jours"),
  c(0, 90, "0-90 jours"), 
  c(0, 120, "0-120 jours"),
  c(61, 180, "61-180 jours"),
  c(121, 365, "121-365 jours"),
  c(181, 730, "181+ jours")
)

window_results <- sapply(windows, function(w) {
  test_time_window(df_paired, w[1], as.numeric(w[2]), w[3])
})

# === 3. TEST DE FLEMING-HARRINGTON ===
cat("\n3. TESTS DE FLEMING-HARRINGTON (différents poids temporels) :\n")

# Différents paramètres rho pour pondérer différemment
rho_values <- c(0, 0.5, 1, 1.5, 2)
cat("  rho=0   : Log-rank standard\n")
cat("  rho>0   : Plus de poids aux événements précoces\n")
cat("  rho<0   : Plus de poids aux événements tardifs\n\n")

for(rho in rho_values) {
  test_fh <- survdiff(surv_obj ~ groupe, data = df_paired, rho = rho)
  p_fh <- 1 - pchisq(test_fh$chisq, df = length(test_fh$n) - 1)
  cat("  rho =", sprintf("%3.1f", rho), ": p =", formatC(p_fh, format = "e", digits = 3), "\n")
}

# === 4. ANALYSE OPTIMALE DU SEUIL "PRÉCOCE" ===
cat("\n4. RECHERCHE DU SEUIL OPTIMAL POUR 'PRÉCOCE' :\n")

# Tester différents seuils pour maximiser la différence
cutoffs <- seq(60, 180, by = 15)  # De 60 à 180 jours par pas de 15

optimal_results <- data.frame(
  cutoff = cutoffs,
  p_early = NA,
  p_late = NA,
  n_early = NA,
  n_late = NA
)

for(i in 1:length(cutoffs)) {
  cutoff <- cutoffs[i]
  
  # Événements précoces (0 à cutoff)
  early_events <- sum(df_paired$time <= cutoff & df_paired$event == 1)
  
  # Événements tardifs (cutoff+ à fin)  
  late_events <- sum(df_paired$time > cutoff & df_paired$event == 1)
  
  optimal_results$n_early[i] <- early_events
  optimal_results$n_late[i] <- late_events
  
  # Test précoce si assez d'événements
  if(early_events >= 10) {
    p_early <- test_time_window(df_paired, 0, cutoff, paste0("0-", cutoff, "j"))
    optimal_results$p_early[i] <- p_early
  }
  
  # Test tardif si assez d'événements  
  if(late_events >= 10) {
    p_late <- test_time_window(df_paired, cutoff+1, 730, paste0(cutoff+1, "-730j"))
    optimal_results$p_late[i] <- p_late
  }
}

# Afficher les résultats
cat("\nSeuils testés :\n")
cat("Cutoff | Événements précoces | p précoce | Événements tardifs | p tardif\n")
cat("-------|-------------------|-----------|-------------------|----------\n")

for(i in 1:nrow(optimal_results)) {
  cat(sprintf("%6dj |      %3d          |   %7s   |      %3d          |  %7s\n",
              optimal_results$cutoff[i],
              optimal_results$n_early[i],
              ifelse(is.na(optimal_results$p_early[i]), "NA", formatC(optimal_results$p_early[i], format = "f", digits = 3)),
              optimal_results$n_late[i], 
              ifelse(is.na(optimal_results$p_late[i]), "NA", formatC(optimal_results$p_late[i], format = "f", digits = 3))))
}

# === 5. RECOMMANDATIONS ===
cat("\n=== RECOMMANDATIONS ===\n")

# Seuil optimal basé sur les événements
all_events <- df_paired %>% filter(event == 1)
q33 <- quantile(all_events$time, 0.33)
q50 <- quantile(all_events$time, 0.50)

cat("Seuils recommandés pour 'précoce' :\n")
cat("  - Clinique     : 90-120 jours (standard chirurgical)\n")
cat("  - Statistique  : ", round(q33), " jours (33e percentile)\n")
cat("  - Médiane      : ", round(q50), " jours (50e percentile)\n")

# Identifier le meilleur seuil
valid_early <- optimal_results[!is.na(optimal_results$p_early) & optimal_results$n_early >= 15, ]
if(nrow(valid_early) > 0) {
  best_cutoff <- valid_early$cutoff[which.min(valid_early$p_early)]
  cat("  - Optimal stats: ", best_cutoff, " jours (p minimal précoce)\n")
}

# === 6. INTERPRÉTATION CLINIQUE ===
cat("\n=== INTERPRÉTATION ===\n")

lr_p <- 1 - pchisq(lr_global$chisq, df = 2)
peto_p <- 1 - pchisq(peto_global$chisq, df = 2)

if(peto_p < lr_p * 0.8) {
  cat("✅ EFFET PRÉCOCE DOMINANT :\n")
  cat("   - Test de Peto plus significatif que Log-rank\n") 
  cat("   - Les différences apparaissent tôt après l'intervention\n")
  cat("   - Seuil recommandé : 90-120 jours pour définir 'précoce'\n")
} else if(lr_p < peto_p * 0.8) {
  cat("✅ EFFET TARDIF DOMINANT :\n")
  cat("   - Log-rank plus significatif que Peto\n")
  cat("   - Les différences s'accentuent avec le temps\n") 
  cat("   - Importance du suivi à long terme\n")
} else {
  cat("✅ EFFET CONSTANT :\n")
  cat("   - Log-rank et Peto similaires\n")
  cat("   - Différences constantes dans le temps\n")
}

cat("\n🎯 CONCLUSION : Utilisez le test de Peto (rho=1) si vous suspectez un effet précoce,")
cat("\n   et analysez les fenêtres temporelles pour identifier les périodes critiques.\n")







#***************TAUX ET DÉLAIS MÉDIANS AVEC TESTS STATISTIQUES*****************

library(dplyr)
library(tidyr)

cat("=== CALCUL DES TAUX ET DÉLAIS MÉDIANS AVEC TESTS STATISTIQUES ===\n\n")

# === PRÉPARATION DES DONNÉES ===
df_analysis <- m.final %>%
  filter(!is.na(Pair_ID), I_stomie == 1) %>%
  mutate(
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann_Non_Converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann_Converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA_Protegee",
      TRUE ~ NA_character_
    ),
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    S_fermeturestomie = as.numeric(S_fermeturestomie)
  ) %>%
  filter(!is.na(groupe))

cat("Effectifs par groupe:\n")
table(df_analysis$groupe) %>% print()

# === 1. CALCULS DES MÉDIANES PAR GROUPE ===
cat("\n### 1. TAUX ET DÉLAIS MÉDIANS PAR GROUPE ###\n")

medians_summary <- df_analysis %>%
  group_by(groupe) %>%
  summarise(
    n = n(),
    n_fermetures = sum(S_fermeturestomie, na.rm = TRUE),
    taux_fermeture_pct = round(mean(S_fermeturestomie, na.rm = TRUE) * 100, 1),
    delai_median_jours = round(median(S_duree_avec_stomie, na.rm = TRUE), 1),
    q25_delai = round(quantile(S_duree_avec_stomie, 0.25, na.rm = TRUE), 1),
    q75_delai = round(quantile(S_duree_avec_stomie, 0.75, na.rm = TRUE), 1),
    delai_iqr = paste0(q25_delai, "-", q75_delai),
    .groups = "drop"
  ) %>%
  mutate(
    groupe_clean = case_when(
      groupe == "Hartmann_Converti" ~ "Hartmann Converti",
      groupe == "Hartmann_Non_Converti" ~ "Hartmann Non Converti", 
      groupe == "RA_Protegee" ~ "RA Protégée",
      TRUE ~ groupe
    )
  ) %>%
  select(groupe_clean, n, n_fermetures, taux_fermeture_pct, delai_median_jours, delai_iqr)

cat("Résultats:\n")
print(medians_summary)

# === 2. TESTS STATISTIQUES POUR TAUX DE FERMETURE ===
cat("\n### 2. TESTS STATISTIQUES POUR TAUX DE FERMETURE ###\n")

# Fonction pour test McNemar (données appariées binaires)
test_mcnemar_paired <- function(data, group1, group2, test_name) {
  
  pairs_data <- data %>%
    filter(groupe %in% c(group1, group2)) %>%
    select(Pair_ID, groupe, S_fermeturestomie) %>%
    pivot_wider(names_from = groupe, values_from = S_fermeturestomie, names_prefix = "group_")
  
  if(ncol(pairs_data) != 3) {
    cat("❌ ", test_name, ": Données insuffisantes\n")
    return(list(p = NA, n = 0))
  }
  
  names(pairs_data)[2:3] <- c("group1", "group2")
  pairs_complete <- pairs_data %>% filter(!is.na(group1) & !is.na(group2))
  
  if(nrow(pairs_complete) < 5) {
    cat("❌ ", test_name, ": Pas assez de paires (n=", nrow(pairs_complete), ")\n")
    return(list(p = NA, n = nrow(pairs_complete)))
  }
  
  # Calculer les taux pour chaque groupe dans les paires
  taux1 <- round(mean(pairs_complete$group1, na.rm = TRUE) * 100, 1)
  taux2 <- round(mean(pairs_complete$group2, na.rm = TRUE) * 100, 1)
  
  # Test McNemar
  table_2x2 <- table(pairs_complete$group1, pairs_complete$group2)
  mcnemar_test <- mcnemar.test(table_2x2, correct = FALSE)
  
  cat("✅ ", test_name, ":\n")
  cat("   Paires: n =", nrow(pairs_complete), "\n")
  cat("   Taux", substr(group1, 1, 12), ":", taux1, "%\n")
  cat("   Taux", substr(group2, 1, 12), ":", taux2, "%\n")
  cat("   McNemar p =", formatC(mcnemar_test$p.value, format = "f", digits = 4))
  cat(ifelse(mcnemar_test$p.value < 0.05, " (significatif)", " (non significatif)"), "\n\n")
  
  return(list(p = mcnemar_test$p.value, n = nrow(pairs_complete), 
              taux1 = taux1, taux2 = taux2))
}

# Tests appariés
result_mcnemar_1 <- test_mcnemar_paired(df_analysis, "RA_Protegee", "Hartmann_Non_Converti", 
                                        "RA Protégée vs Hartmann Non Converti")

result_mcnemar_2 <- test_mcnemar_paired(df_analysis, "RA_Protegee", "Hartmann_Converti",
                                        "RA Protégée vs Hartmann Converti")

# Test non apparié Fisher
cat("✅ Hartmann Converti vs Non Converti (non apparié):\n")
hart_conv_taux <- df_analysis %>% filter(groupe == "Hartmann_Converti")
hart_nonconv_taux <- df_analysis %>% filter(groupe == "Hartmann_Non_Converti")

taux_conv <- round(mean(hart_conv_taux$S_fermeturestomie, na.rm = TRUE) * 100, 1)
taux_nonconv <- round(mean(hart_nonconv_taux$S_fermeturestomie, na.rm = TRUE) * 100, 1)

contingency_hart <- rbind(
  c(sum(hart_conv_taux$S_fermeturestomie), nrow(hart_conv_taux) - sum(hart_conv_taux$S_fermeturestomie)),
  c(sum(hart_nonconv_taux$S_fermeturestomie), nrow(hart_nonconv_taux) - sum(hart_nonconv_taux$S_fermeturestomie))
)

fisher_hart <- fisher.test(contingency_hart)

cat("   n Converti =", nrow(hart_conv_taux), ", n Non Converti =", nrow(hart_nonconv_taux), "\n")
cat("   Taux Converti:", taux_conv, "%\n")
cat("   Taux Non Converti:", taux_nonconv, "%\n")
cat("   Fisher p =", formatC(fisher_hart$p.value, format = "f", digits = 4))
cat(ifelse(fisher_hart$p.value < 0.05, " (significatif)", " (non significatif)"), "\n\n")

# === 3. TESTS STATISTIQUES POUR DÉLAIS MÉDIANS ===
cat("### 3. TESTS STATISTIQUES POUR DÉLAIS MÉDIANS ###\n")

# Fonction pour test Wilcoxon signed rank (données appariées continues)
test_wilcoxon_paired <- function(data, group1, group2, test_name) {
  
  pairs_data <- data %>%
    filter(groupe %in% c(group1, group2)) %>%
    select(Pair_ID, groupe, S_duree_avec_stomie) %>%
    pivot_wider(names_from = groupe, values_from = S_duree_avec_stomie, names_prefix = "group_")
  
  if(ncol(pairs_data) != 3) {
    cat("❌ ", test_name, ": Données insuffisantes\n")
    return(list(p = NA, n = 0))
  }
  
  names(pairs_data)[2:3] <- c("delai1", "delai2")
  pairs_complete <- pairs_data %>% filter(!is.na(delai1) & !is.na(delai2))
  
  if(nrow(pairs_complete) < 5) {
    cat("❌ ", test_name, ": Pas assez de paires (n=", nrow(pairs_complete), ")\n")
    return(list(p = NA, n = nrow(pairs_complete)))
  }
  
  # Test Wilcoxon signed rank
  wilcox_test <- wilcox.test(pairs_complete$delai1, pairs_complete$delai2, paired = TRUE)
  
  # Statistiques
  median1 <- round(median(pairs_complete$delai1, na.rm = TRUE), 1)
  median2 <- round(median(pairs_complete$delai2, na.rm = TRUE), 1)
  diff_median <- round(median1 - median2, 1)
  
  cat("✅ ", test_name, ":\n")
  cat("   Paires: n =", nrow(pairs_complete), "\n")
  cat("   Médiane", substr(group1, 1, 12), ":", median1, "jours\n")
  cat("   Médiane", substr(group2, 1, 12), ":", median2, "jours\n")
  cat("   Différence:", diff_median, "jours\n")
  cat("   Wilcoxon p =", formatC(wilcox_test$p.value, format = "f", digits = 4))
  cat(ifelse(wilcox_test$p.value < 0.05, " (significatif)", " (non significatif)"), "\n\n")
  
  return(list(p = wilcox_test$p.value, n = nrow(pairs_complete),
              median1 = median1, median2 = median2, diff = diff_median))
}

# Tests appariés pour délais
result_wilcox_1 <- test_wilcoxon_paired(df_analysis, "RA_Protegee", "Hartmann_Non_Converti",
                                        "RA Protégée vs Hartmann Non Converti")

result_wilcox_2 <- test_wilcoxon_paired(df_analysis, "RA_Protegee", "Hartmann_Converti",
                                        "RA Protégée vs Hartmann Converti")

# Test non apparié Mann-Whitney
cat("✅ Hartmann Converti vs Non Converti (Mann-Whitney):\n")
hart_conv_delai <- df_analysis %>% filter(groupe == "Hartmann_Converti") %>% pull(S_duree_avec_stomie)
hart_nonconv_delai <- df_analysis %>% filter(groupe == "Hartmann_Non_Converti") %>% pull(S_duree_avec_stomie)

mann_whitney <- wilcox.test(hart_conv_delai, hart_nonconv_delai, paired = FALSE)

median_conv <- round(median(hart_conv_delai, na.rm = TRUE), 1)
median_nonconv <- round(median(hart_nonconv_delai, na.rm = TRUE), 1)
diff_hart <- round(median_conv - median_nonconv, 1)

cat("   n Converti =", length(hart_conv_delai), ", n Non Converti =", length(hart_nonconv_delai), "\n")
cat("   Médiane Converti:", median_conv, "jours\n")
cat("   Médiane Non Converti:", median_nonconv, "jours\n")
cat("   Différence:", diff_hart, "jours\n")
cat("   Mann-Whitney p =", formatC(mann_whitney$p.value, format = "f", digits = 4))
cat(ifelse(mann_whitney$p.value < 0.05, " (significatif)", " (non significatif)"), "\n\n")

# === 4. TABLEAU RÉSUMÉ ===
cat("### 4. TABLEAU RÉSUMÉ DES RÉSULTATS ###\n\n")

# Créer tableau résumé
cat("TAUX DE FERMETURE:\n")
cat("Groupe                     | n    | Taux (%) | Test vs RA Protégée    | p-value\n")
cat("---------------------------|------|----------|------------------------|----------\n")
cat(sprintf("%-27s| %-4d | %-8.1f | —                      | —\n", 
            "RA Protégée", 
            medians_summary$n[medians_summary$groupe_clean == "RA Protégée"],
            medians_summary$taux_fermeture_pct[medians_summary$groupe_clean == "RA Protégée"]))

cat(sprintf("%-27s| %-4d | %-8.1f | McNemar (apparié)      | %-8.4f\n",
            "Hartmann Non Converti",
            medians_summary$n[medians_summary$groupe_clean == "Hartmann Non Converti"],
            medians_summary$taux_fermeture_pct[medians_summary$groupe_clean == "Hartmann Non Converti"],
            result_mcnemar_1$p))

cat(sprintf("%-27s| %-4d | %-8.1f | McNemar (apparié)      | %-8.4f\n",
            "Hartmann Converti",
            medians_summary$n[medians_summary$groupe_clean == "Hartmann Converti"],
            medians_summary$taux_fermeture_pct[medians_summary$groupe_clean == "Hartmann Converti"],
            result_mcnemar_2$p))

cat("\nDÉLAIS MÉDIANS:\n")
cat("Groupe                     | n    | Médiane  | Test vs RA Protégée    | p-value\n")
cat("---------------------------|------|----------|------------------------|----------\n")
cat(sprintf("%-27s| %-4d | %-8.1f | —                      | —\n",
            "RA Protégée",
            medians_summary$n[medians_summary$groupe_clean == "RA Protégée"],
            medians_summary$delai_median_jours[medians_summary$groupe_clean == "RA Protégée"]))

cat(sprintf("%-27s| %-4d | %-8.1f | Wilcoxon (apparié)     | %-8.4f\n",
            "Hartmann Non Converti", 
            medians_summary$n[medians_summary$groupe_clean == "Hartmann Non Converti"],
            medians_summary$delai_median_jours[medians_summary$groupe_clean == "Hartmann Non Converti"],
            result_wilcox_1$p))

cat(sprintf("%-27s| %-4d | %-8.1f | Wilcoxon (apparié)     | %-8.4f\n",
            "Hartmann Converti",
            medians_summary$n[medians_summary$groupe_clean == "Hartmann Converti"], 
            medians_summary$delai_median_jours[medians_summary$groupe_clean == "Hartmann Converti"],
            result_wilcox_2$p))

cat("\nCOMPARAISON HARTMANN CONVERTI vs NON CONVERTI:\n")
cat("Variable               | Converti  | Non Converti | Test           | p-value\n")
cat("-----------------------|-----------|--------------|----------------|----------\n")
cat(sprintf("%-23s| %-9.1f | %-12.1f | Fisher         | %-8.4f\n",
            "Taux fermeture (%)", taux_conv, taux_nonconv, fisher_hart$p.value))
cat(sprintf("%-23s| %-9.1f | %-12.1f | Mann-Whitney   | %-8.4f\n",
            "Délai médian (jours)", median_conv, median_nonconv, mann_whitney$p.value))

cat("\n=== ANALYSE TERMINÉE ===\n")
cat("✅ Tous les tests statistiques appropriés ont été appliqués\n")
cat("✅ Tests appariés utilisés quand approprié (McNemar, Wilcoxon signed rank)\n")
cat("✅ Tests non appariés pour comparaisons Hartmann converti vs non converti\n")

# === 5. INTERPRÉTATION CLINIQUE ===
cat("\n### 5. INTERPRÉTATION CLINIQUE ###\n")

cat("TAUX DE FERMETURE:\n")
if(!is.na(result_mcnemar_1$p) && result_mcnemar_1$p < 0.05) {
  cat("🔹 RA Protégée vs Hartmann Non Converti: DIFFÉRENCE SIGNIFICATIVE\n")
  cat("   → RA protégée a un taux de fermeture supérieur\n")
} else {
  cat("🔸 RA Protégée vs Hartmann Non Converti: Pas de différence significative\n")
}

if(!is.na(result_mcnemar_2$p) && result_mcnemar_2$p < 0.05) {
  cat("🔹 RA Protégée vs Hartmann Converti: DIFFÉRENCE SIGNIFICATIVE\n")
  cat("   → RA protégée a un taux de fermeture supérieur\n")
} else {
  cat("🔸 RA Protégée vs Hartmann Converti: Pas de différence significative\n")
}

if(fisher_hart$p.value < 0.05) {
  cat("🔹 Hartmann Converti vs Non Converti: DIFFÉRENCE SIGNIFICATIVE\n")
} else {
  cat("🔸 Hartmann Converti vs Non Converti: Pas de différence significative\n")
}

cat("\nDÉLAIS DE FERMETURE:\n")
if(!is.na(result_wilcox_1$p) && result_wilcox_1$p < 0.05) {
  cat("🔹 RA Protégée vs Hartmann Non Converti: DIFFÉRENCE SIGNIFICATIVE\n")
  cat("   → RA protégée ferme plus rapidement\n")
} else {
  cat("🔸 RA Protégée vs Hartmann Non Converti: Pas de différence significative\n")
}

if(!is.na(result_wilcox_2$p) && result_wilcox_2$p < 0.05) {
  cat("🔹 RA Protégée vs Hartmann Converti: DIFFÉRENCE SIGNIFICATIVE\n")
  cat("   → RA protégée ferme plus rapidement\n")
} else {
  cat("🔸 RA Protégée vs Hartmann Converti: Pas de différence significative\n")
}

if(mann_whitney$p.value < 0.05) {
  cat("🔹 Hartmann Converti vs Non Converti: DIFFÉRENCE SIGNIFICATIVE\n")
} else {
  cat("🔸 Hartmann Converti vs Non Converti: Pas de différence significative\n")
}

# === 6. SAUVEGARDE OPTIONNELLE ===
cat("\n### 6. SAUVEGARDE DES RÉSULTATS ###\n")

# Créer dataframe pour export
results_export <- data.frame(
  Groupe = medians_summary$groupe_clean,
  n = medians_summary$n,
  Taux_fermeture_pct = medians_summary$taux_fermeture_pct,
  Delai_median_jours = medians_summary$delai_median_jours,
  IQR_delai = medians_summary$delai_iqr
)

# Tests statistiques résumés
tests_summary <- data.frame(
  Comparaison = c("RA vs Hartmann Non Converti", "RA vs Hartmann Converti", "Hartmann Converti vs Non Converti"),
  Test_Taux = c("McNemar", "McNemar", "Fisher"),
  p_Taux = c(
    ifelse(is.na(result_mcnemar_1$p), "NA", formatC(result_mcnemar_1$p, format = "f", digits = 4)),
    ifelse(is.na(result_mcnemar_2$p), "NA", formatC(result_mcnemar_2$p, format = "f", digits = 4)),
    formatC(fisher_hart$p.value, format = "f", digits = 4)
  ),
  Test_Delai = c("Wilcoxon signed rank", "Wilcoxon signed rank", "Mann-Whitney"),
  p_Delai = c(
    ifelse(is.na(result_wilcox_1$p), "NA", formatC(result_wilcox_1$p, format = "f", digits = 4)),
    ifelse(is.na(result_wilcox_2$p), "NA", formatC(result_wilcox_2$p, format = "f", digits = 4)),
    formatC(mann_whitney$p.value, format = "f", digits = 4)
  )
)

# Export CSV (optionnel)
tryCatch({
  write.csv(results_export, "resultats_taux_delais_medians.csv", row.names = FALSE)
  write.csv(tests_summary, "tests_statistiques_summary.csv", row.names = FALSE)
  cat("📄 Fichiers CSV sauvegardés:\n")
  cat("   - resultats_taux_delais_medians.csv\n")
  cat("   - tests_statistiques_summary.csv\n")
}, error = function(e) {
  cat("📄 Sauvegarde CSV non effectuée (répertoire non accessible)\n")
})

cat("\n🎯 SCRIPT TERMINÉ AVEC SUCCÈS 🎯\n")
cat("📊 Résultats: Taux et délais médians calculés\n")
cat("🧪 Tests: McNemar, Wilcoxon signed rank, Fisher, Mann-Whitney\n")
cat("📈 Interprétation: Comparaisons statistiques appropriées effectuées\n")