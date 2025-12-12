# Appariement après score de propension

# %% ---- Définition des fonctions -----
paired_wilcox_gts <- function(data, variable, by, ...) {
  d <- data %>%
    dplyr::select(Pair_ID, !!rlang::sym(by), !!rlang::sym(variable)) %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(by),
      values_from = !!rlang::sym(variable)
    )
  
  res <- wilcox.test(d[[2]], d[[3]], paired = TRUE)
  tibble::tibble(p.value = res$p.value)
}

#Student apparié
paired_ttest_gts <- function(data, variable, by, ...) {
  d <- data %>%
    dplyr::select(Pair_ID, !!rlang::sym(by), !!rlang::sym(variable)) %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(by),
      values_from = !!rlang::sym(variable)
    )
  
  res <- t.test(d[[2]], d[[3]], paired = TRUE)
  tibble::tibble(p.value = res$p.value)
}

#McNemar apparié (pour variable binaire)
mcnemar_safe <- function(data, variable, by, ...) {
  
  d <- data %>%
    dplyr::select(Pair_ID, !!rlang::sym(by), !!rlang::sym(variable)) %>%
    tidyr::pivot_wider(
      names_from  = !!rlang::sym(by),
      values_from = !!rlang::sym(variable)
    )
  
  # Vérifier binaire
  vals <- na.omit(unique(unlist(d[, 2:3])))
  if (length(vals) != 2) {
    return(tibble::tibble(p.value = NA))
  }
  
  tab <- table(d[[2]], d[[3]])
  
  # Vérifier que c'est 2x2 complet
  if (!all(dim(tab) == c(2,2))) {
    return(tibble::tibble(p.value = NA))
  }
  
  res <- mcnemar.test(tab, correct = FALSE)
  tibble::tibble(p.value = res$p.value)
}

# %% ---- Tableaux -----

table1adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table1),
    missing = "no",
    by = I_geste_comparaison_1HartmannCoelio,
    percent = "column",
    type = list(
      H_taille_abces ~ "continuous"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous()  ~ "{median} [{p25}, {p75}]"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ paired_wilcox_gts,
      all_categorical() ~ mcnemar_safe
    )
  )
table1adjusted

# Conversion en flextable
table1adjusted_flex <- table1adjusted %>%
  as_flex_table()

# Export DOCX
doc <- officer::read_docx()
doc <- flextable::body_add_flextable(doc, table1adjusted_flex)
print(doc, target = "table1_adjusted.docx")


table2adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table2),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} [{p25}, {p75}]"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ paired_wilcox_gts,
      all_categorical() ~ mcnemar_safe
    )
  )
table2adjusted


table2adjusted_flex <- table2adjusted %>%
  as_flex_table()

# Export DOCX
doc2 <- officer::read_docx()
doc2 <- flextable::body_add_flextable(doc2, table2adjusted_flex)
print(doc2, target = "table2_adjusted.docx")




table3adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table3),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n}",
      all_continuous() ~ "{median} [{p25}, {p75}]"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ paired_wilcox_gts,
      all_categorical() ~ mcnemar_safe
    )
  )
table3adjusted

table3adjusted_flex <- table3adjusted %>%
  as_flex_table()

# Export DOCX
doc3 <- officer::read_docx()
doc3 <- flextable::body_add_flextable(doc3, table3adjusted_flex)
print(doc3, target = "table3_adjusted.docx")


table4adjusted <- m.final %>%
  tbl_summary(
    include = all_of(cols_to_include_in_table4),
    missing = "ifany",
    by = I_geste_comparaison_1HartmannCoelio,  # Comparaison entre groupes
    percent = "column",
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{median} ({p25} - {p75})"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ paired_wilcox_gts,
      all_categorical() ~ mcnemar_safe
    )
  )
table4adjusted

table4adjusted_flex <- table4adjusted %>%
  as_flex_table()

# Export DOCX
doc4 <- officer::read_docx()
doc4 <- flextable::body_add_flextable(doc4, table4adjusted_flex)
print(doc4, target = "table4_adjusted.docx")



# Filtrer les patients avec stomie
df_survie_apparie <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    fermeture_stomie = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1, S_duree_avec_stomie, 730)
  )

# Objet de survie
surv_object <- Surv(df_survie_apparie$S_duree_avec_stomie, df_survie_apparie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_geste_comparaison_1HartmannCoelio, data = df_survie_apparie)

# p-value log-rank
test_logrank <- survdiff(surv_object ~ I_geste_comparaison_1HartmannCoelio, data = df_survie_apparie)
raw_p_value <- 1 - pchisq(test_logrank$chisq, df = length(test_logrank$n) - 1)
p_value <- ifelse(raw_p_value < 0.001, formatC(raw_p_value, format = "e", digits = 2),
                  formatC(raw_p_value, format = "f", digits = 3))

# Pourcentage de fermeture
taux_final_fermeture <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie) * 100, .groups = "drop")

# Medianes donnees
mediane_fermeture <- data.frame(
  I_geste_comparaison_1HartmannCoelio = c(0, 1),
  mediane_jours = c(143.5, 115),
  y_label = c(-1, -4)
)

# Recuperer les couleurs automatiques
p_temp <- ggsurvplot(km_fit, data = df_survie_apparie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# Recuperer la valeur de survie au jour de mediane dans km_fit
get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data <- summary(km_fit, times = temps)$surv
  surv_percent <- 100 * (1 - surv_data)  # car on affiche 100*(1-survie)
  return(surv_percent[groupe + 1])  # groupe 0 ou 1
}

mediane_fermeture$y_courbe <- sapply(1:nrow(mediane_fermeture), function(i) {
  get_surv_at_time(km_fit, mediane_fermeture$I_geste_comparaison_1HartmannCoelio[i],
                   mediane_fermeture$mediane_jours[i])
})


p <- ggsurvplot(
  km_fit,
  data = df_survie_apparie,
  pval = FALSE,
  risk.table = TRUE,
  break.time.by = 100,
  conf.int = TRUE,
  xlim = c(0, 730),
  add_p = TRUE,
  xlab = "Follow-up (days)",
  ylab = "Stoma free (%)",
  legend = "right",
  legend.title = "Surgical technique",
  legend.labs = c("LHP", "OPA"),
  fun = function(x) 100 * (1 - x),
  theme = theme_classic2(),
)

p

# afficher résultat du log rank 
p$plot <- p$plot +
  annotate("text", x = 500, y = 20,
           label = paste0("Log-rank p = ", p_value),
           size = 5)

p

#calculer un log rank stratifié sur les paires 
library(survival)
stratified_logrank <- function(data, time, status, group, strata) {
  surv_object <- Surv(data[[time]], data[[status]])
  formula <- as.formula(paste("surv_object ~", group, "+ strata(", strata, ")"))
  fit <- survdiff(formula, data = data)
  p_value <- 1 - pchisq(fit$chisq, df = length(fit$n) - 1)
  return(p_value)
}
p_value_stratified <- stratified_logrank(
  data = df_survie_apparie,
  time = "S_duree_avec_stomie",
  status = "fermeture_stomie",
  group = "I_geste_comparaison_1HartmannCoelio",
  strata = "Pair_ID"
)
# afficher résultat du log rank stratifié
p$plot <- p$plot +
  annotate("text", x = 500, y = 10,
           label = paste0("Stratified log-rank p = ",
                          ifelse(p_value_stratified < 0.001,
                                 formatC(p_value_stratified, format = "e", digits = 2),
                                 formatC(p_value_stratified, format = "f", digits = 3))),
           size = 5)
p


# calculer un estimateur de cox robuste avec cluster sur les paires
cox_robust <- function(data, time, status, group, cluster) {
  surv_object <- Surv(data[[time]], data[[status]])
  formula <- as.formula(paste("surv_object ~", group, "+ cluster(", cluster, ")"))
  fit <- coxph(formula, data = data)
  summary_fit <- summary(fit)
  p_value <- summary_fit$coefficients[,"Pr(>|z|)"][1]
  return(p_value)
}

p_value_cox_robust <- cox_robust(
  data = df_survie_apparie,
  time = "S_duree_avec_stomie",
  status = "fermeture_stomie",
  group = "I_geste_comparaison_1HartmannCoelio",
  cluster = "Pair_ID"
)

# afficher résultat du cox robuste dans un tableau gtsummary
p$plot <- p$plot +
  annotate("text", x = 500, y = 5,
           label = paste0("Robust Cox p = ",
                          ifelse(p_value_cox_robust < 0.001,
                                 formatC(p_value_cox_robust, format = "e", digits = 2),
                                 formatC(p_value_cox_robust, format = "f", digits = 3))),
           size = 5)
p

#afficher HR et ConfidenceIntervals et p value dans un tableau gtsummary
library(broom)
library(gtsummary)
cox_model <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID), data = df_survie_apparie)
cox_summary <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
cox_summary_table <- cox_summary %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    HR_CI = paste0(round(estimate, 2), " (", round(conf.low, 2), "-", round(conf.high, 2), ")"),
    p.value = ifelse(p.value < 0.001, formatC(p.value, format = "e", digits = 2),
                     formatC(p.value, format = "f", digits = 3))
  ) %>%
  select(term, HR_CI, p.value)
cox_summary_table_gt <- cox_summary_table %>%
  gt() %>%
  tab_header(
    title = "Cox Proportional Hazards Model with Robust Standard Errors"
  ) %>%
  cols_label(
    term = "Variable",
    HR_CI = "Hazard Ratio (95% CI)",
    p.value = "p-value"
  )
print(cox_summary_table_gt)


#export .svg
ggsave("12_25_stoma_free_survival_main.svg", plot = p$plot, width = 10, height = 5)

