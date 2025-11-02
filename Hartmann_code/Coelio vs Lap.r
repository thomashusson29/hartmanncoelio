# etape 1 : Importer toute la base
gs4_deauth()

df <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1PrWmbpaXgLFLBJ8eGCTdcPI0qEJhmxr6mXe4wylyQtY/edit?gid=531779098#gid=531779098",
  sheet = "base_filtrée_péritonites",
)

# Filtrer les patients avec une valeur renseignee pour I_geste_comparaison_Coelio_vs_RA_Laparo == 1
df_Coelio <- df %>%
  filter(I_geste_comparaison_Coelio_vs_RA_Laparo == 1)

table(df_Coelio$I_geste_comparaison_whole)


cols_to_include_in_table1 <- c(
  "Age",
  "Sexe_Male",
  "BMI",
  "A_ASA",
  "A_ASA_sup2",
  "A_ATCD_neuro",
  "A_ATCD_Cardio",
  "A_ATCD_pneumo",
  "A_diabete",
  "A_tabac",
  "A_immunosuppression",
  "A_anticoagulants",
  "A_antiaggregants",
  "A_ATCD_laparo_mediane",
  "H_ATCD_diverticulite",
  "H_n_pousse",
  "H_n_pousse_sup_equal_2",
  "H_ATCD_Divert_complique",
  "H_perforation",
  "H_abces",
  "H_taille_abces",
  "H_drainage_YN",
  "H_drainage_TDM",
  "H_drainage_endoscopie",
  "H_lavage_celio",
  "H_occlusion",
  "H_hemorragie",
  "H_fistule",
  "H_tt_medical",
  "H_antibio_YN",
  "H_antibio_IV",
  "H_antibio_PO",
  "H_antibio_IV_PO",
  "H_duree_antibio",
  "H_colo",
  "I_urgence",
  "I_voie_coelio",
  "I_conversion",
  "I_voie_laparo",
  "I_voie_robot",
  "I_indication_poussee_compliquee",
  "I_indication_immunodepression",
  "I_indication_hemorragie",
  "I_delai_jours_J0",
  "I_delai_jours_24h",
  "I_delai_jours_sup24h",
  "I_localisation",
  "I_localisation_colondroit",
  "I_localisation_sigmoide",
  "I_peritonite",
  "I_scoreHinchey",
  "I_scoreHinchey_3",
  "I_scoreHinchey_4",
  "I_occlusion",
  "I_hemorragie",
  "I_arret_ATC",
  "I_fistule"
)

cols_to_include_in_table2 <- c(
  "I_voie_coelio",
  "I_conversion",
  "I_voie_laparo",
  "I_voie_robot",
  "I_abaissement",
  "I_section_origine_AMI",
  "I_anastomose",
  "I_anastomose_terminoterminale",
  "I_anastomose_laterolaterale",
  "I_stomie",
  "I_drainage",
  "I_duree",
  "I_transfusion",
  "I_conversion",
  "I_culots"
)

# Definition des colonnes à inclure dans le tableau 3
cols_to_include_in_table3 <- c(
  "S_mortalite90d",
  "S_morbidite90d",
  "S_dindo",
  "S_dindo_sup2",
  "S_fistuleanasto",
  "S_abscesprof",
  "S_drainage",
  "S_complication_hemorragique",
  "S_transfusion",
  "S_Culots",
  "S_reoperation",
  "S_date_Reop_delai",
  "S_voie_Reop",
  "S_voie_Reop_coelio",
  "S_voie_Reop_coelio_conversion",
  "S_voie_Reop_laparo",
  "S_voie_Reop_robot",
  "S_geste_Reop",
  "S_geste_Reop_Hartmann",
  "S_geste_Reop_stomie_drainage",
  "S_geste_Reop_drainage",
  "S_geste_Reop_autres",
  "S_rea",
  "S_duree_hospit_postop",
  "S_complicationtardive"
)

# Definition des colonnes à inclure dans le tableau 4
cols_to_include_in_table4 <- c(
  "S_complicationtardive",
  "S_rehospitalisation",
  "S_stenoseanastomotique",
  "S_eventration",
  "S_occlusionpostop",
  "S_recidivediverti",
  "S_fermeturestomie",
  "S_duree_avec_stomie",
  "S_etatDDN_vivant"
)


colors        = c("#db1d88", "#4b6bd6")



#********APPARIE : courbe de survie sans stomie******#
#********MATCHED: stoma-free survival curve******#

library(survival)
library(survminer)
library(patchwork)
library(dplyr)

# 1) Préparation & km_fit … (inchangé)
df_survie_apparie <- m.final %>%
  filter(I_stomie == 1) %>%
  mutate(
    S_duree_avec_stomie = as.numeric(S_duree_avec_stomie),
    fermeture_stomie     = ifelse(S_fermeturestomie == 1, 1, 0),
    S_duree_avec_stomie = ifelse(S_fermeturestomie == 1,
                                 S_duree_avec_stomie, 730)
  )

surv_object <- Surv(df_survie_apparie$S_duree_avec_stomie,
                    df_survie_apparie$fermeture_stomie)
km_fit <- survfit(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                  data = df_survie_apparie)

# 2) calculs préliminaires (p-value, taux finaux) … (inchangé)
test_logrank <- survdiff(surv_object ~ I_geste_comparaison_1HartmannCoelio,
                         data = df_survie_apparie)
raw_p_value <- 1 - pchisq(test_logrank$chisq,
                          df = length(test_logrank$n) - 1)
p_value <- ifelse(
  raw_p_value < 0.001,
  "< 0.001",
  paste0(formatC(raw_p_value, format = "f", digits = 3))
)


taux_final_fermeture <- df_survie_apparie %>%
  group_by(I_geste_comparaison_1HartmannCoelio) %>%
  summarise(pourcentage_fermeture = mean(S_fermeturestomie)*100,
            .groups = "drop")

# 3) médianes + y_courbe (LIGNE AJOUTÉE)
mediane_fermeture <- data.frame(
  I_geste_comparaison_1HartmannCoelio = c(0, 1),
  mediane_jours = c(143.5, 115),
  y_label      = c(-1, -4)
)

get_surv_at_time <- function(km_fit, groupe, temps) {
  surv_data   <- summary(km_fit, times = temps)$surv
  surv_percent<- 100 * (1 - surv_data)
  surv_percent[groupe + 1]
}

mediane_fermeture$y_courbe <- sapply(
  seq_len(nrow(mediane_fermeture)),
  function(i) {
    get_surv_at_time(
      km_fit,
      mediane_fermeture$I_geste_comparaison_1HartmannCoelio[i],
      mediane_fermeture$mediane_jours[i]
    )
  }
)

# 4) récupère les couleurs auto (inchangé)
p_temp      <- ggsurvplot(km_fit, data = df_survie_apparie)
colors_used <- ggplot_build(p_temp$plot)$data[[1]] %>%
  distinct(group, colour) %>%
  arrange(group) %>%
  pull(colour)

# 5) build du ggsurvplot de base (inchangé, pval=FALSE) …
p <- ggsurvplot(
  km_fit, data = df_survie_apparie,
  fun                  = function(x) 100*(1 - x),
  pval                 = FALSE,
  palette = colors,
  conf.int             = TRUE,
  xlim                 = c(0, 730),
  break.time.by        = 100,
  risk.table           = TRUE,
  risk.table.height    = 0.25,
  risk.table.y.text.col= TRUE,
  risk.table.breaks    = seq(0, 730, 100),
  risk.table.xlim      = c(0, 730),
  xlab                 = "Time (days)",
  ylab                 = "Stoma reversal rate (%)",
  legend.title         = "Surgical technique",
  legend.labs          = c("Laparoscopic Hartmann",
                           "Open Diverted anastomosis"),
  ggtheme              = theme_classic()
)

# 6) on customise p$plot avec vos segments, textes, annotate() …
p$plot <- p$plot +
  guides(
    fill     = "none",
    linetype = "none",
    color    = guide_legend(override.aes = list(linetype = 1))
  ) +
  # texte du pourcentage final au bout du plot
  geom_text(
    data    = taux_final_fermeture,
    aes(x = 730, y = pourcentage_fermeture,
        label = paste0(round(pourcentage_fermeture, 1), "%")),
    vjust     = -0.5,
    size      = 5,
    fontface  = "bold",
    color     = "black"
  ) +
  # annotation du p-value
  annotate(
    "text",
    x     = 400,
    y     = 10,
    label = paste0("p ", p_value),
    size  = 5,
    hjust = 0
  )

# 7) re-assemblement patchwork
combined_plot <- p$plot / p$table +
  plot_layout(heights = c(8, 2))

print(combined_plot)



