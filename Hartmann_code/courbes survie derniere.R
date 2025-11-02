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

