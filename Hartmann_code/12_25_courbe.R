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

#calcul médiane de suivi par groupe avec kaplan meier
library(dplyr)
library(survival)

# Calcul du temps de suivi en jours
df_survie_apparie <- df_survie_apparie %>%
  mutate(
    follow_up_days = as.numeric(
      as.Date(S_dateDDN) - as.Date(I_dateinter)
    )
  )

# Fonction pour médiane de suivi par reverse Kaplan-Meier
median_follow_up <- function(data, time_var) {
  surv_obj <- Surv(time = data[[time_var]], event = rep(1, nrow(data)))
  fit <- survfit(surv_obj ~ 1)
  summary(fit)$table["median"]
}

# Médiane de suivi par groupe
median_follow_up_by_group <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(
    median_follow_up_days = median_follow_up(cur_data(), "follow_up_days"),
    .groups = "drop"
  )

median_follow_up_by_group




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
  palette = c("#bf616a","#5e81ac") 
)

p
# Assurez-vous d'avoir chargé la librairie
library(survminer)


# Export direct de l'objet complet 'p'
ggexport(p, filename = "12_25_stoma_free_survival_main.svg", width = 10, height = 6)



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

#ajouter les pourcentages finaux de fermeture sur le graphique
p$plot <- p$plot +
  geom_text(data = taux_final_fermeture,
            aes(x = 500, y = pourcentage_fermeture,
                label = paste0("Final closure: ", round(pourcentage_fermeture, 1), "%")),
            size = 5)
p


#export .svg
ggsave("12_25_stoma_free_survival_main.svg", plot = p$plot, width = 10, height = 5)










library(survival)
library(gtsummary)
library(flextable)
library(officer)

# Modèle de Cox avec variance robuste
cox_model <- coxph(
  Surv(S_duree_avec_stomie, fermeture_stomie) ~ 
    I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
  data = df_survie_apparie
)

# Tableau gtsummary
table_cox <- tbl_regression(
  cox_model,
  exponentiate = TRUE,
  label = list(
    I_geste_comparaison_1HartmannCoelio ~ 
      "OPA vs LHP"
  )
) %>%
  modify_header(
    label ~ "Variable",
    estimate ~ "Hazard Ratio (95% CI)"
  ) %>%
  modify_caption(
    "**Table X. Cox proportional hazards model for time to stoma closure**"
  ) %>%
  modify_footnote(
    everything() ~
      "Hazard ratios were obtained using a Cox proportional hazards model with robust standard errors clustered on matched pairs."
  )

table_cox

# Exporter en Word
# Conversion en flextable
table_cox_flex <- as_flex_table(table_cox)

# Export Word
read_docx() %>%
  body_add_flextable(table_cox_flex) %>%
  print(target = "Table_Cox_stoma_closure.docx")








##--------------

## %% ----- 3 niveaux ----

library(survival)
library(dplyr)

df_surv <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    time = as.numeric(S_duree_avec_stomie),
    event = as.numeric(S_fermeturestomie),
    time = ifelse(event == 1, time, 730),
    groupe = case_when(
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 0 ~ "Hartmann non converti",
      I_geste_comparaison_1HartmannCoelio == "Hartmann" & I_conversion == 1 ~ "Hartmann converti",
      I_geste_comparaison_1HartmannCoelio == "Résection anastomose protégée" ~ "RA ouverte",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(groupe)) %>%
  mutate(
    groupe = factor(
      groupe,
      levels = c("Hartmann non converti", "Hartmann converti", "RA ouverte")
    )
  )

surv_obj <- Surv(time = df_surv$time, event = df_surv$event)

km_fit_3groupes <- survfit(surv_obj ~ groupe, data = df_surv)

library(survminer)

p <- ggsurvplot(
  km_fit_3groupes,
  data = df_surv,
  fun = function(x) 100 * (1 - x),
  conf.int = FALSE,
  pval = FALSE,
  risk.table = TRUE,
  xlim = c(0, 730),
  break.time.by = 100,
  palette = c("#d08770", "#b48ead", "#5e81ac"),  # rouge, bleu, vert
  legend.title = "Surgical technique",
  legend.labs = c(
    "LHP non converted",
    "LHP converted",
    "OPA"
  ),
  xlab = "Follow-up (days)",
  ylab = "Stoma free (%)",
)

p

# Export .svg avec ggexport()
ggexport(p, filename = "figures/12_25_stoma_free_survival_3groups.svg", width = 10, height = 6)
