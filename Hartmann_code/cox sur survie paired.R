# Extraire le taux de fermeture de stomie à 2 ans (730 jours) dans chaque groupe
surv_2_ans <- summary(km_fit, times = 730)$surv
taux_fermeture_730 <- 100 * (1 - surv_2_ans)  # en pourcentage
names(taux_fermeture_730) <- c("RA", "Hartmann")  # selon l'ordre des groupes dans km_fit
print(taux_fermeture_730)



# Modèle de Cox marginal avec robustesse intra-paire
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

summary(cox_robuste)


% # Extraire les coefficients et IC

# Charger les packages necessaires
library(survival)
library(forestplot)
library(dplyr)

# Modèle de Cox marginal (dejà calcule)
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

# Extraire les infos
hr <- exp(coef(cox_robuste))
ci <- exp(confint(cox_robuste))
labels <- c("RA laparo protegee vs Hartmann coelio")

# Preparer le tableau pour le forestplot
table_forest <- data.frame(
  Variable = labels,
  HR = round(hr, 2),
  CI_low = round(ci[, 1], 2),
  CI_high = round(ci[, 2], 2)
)

# Creer un objet texte avec HR et IC95%
tabletext <- cbind(
  c("Technique", table_forest$Variable),
  c("HR [IC95%]", paste0(table_forest$HR, " [", table_forest$CI_low, " – ", table_forest$CI_high, "]"))
)

# Generer le forestplot
forestplot(
  labeltext = tabletext,
  mean = c(NA, table_forest$HR),
  lower = c(NA, table_forest$CI_low),
  upper = c(NA, table_forest$CI_high),
  zero = 1,
  boxsize = 0.2,
  line.margin = 0.2,
  col = fpColors(box = "black", line = "black", summary = "black"),
  xlog = TRUE,
  lwd.zero = 2,
  lwd.ci = 2,
  ci.vertices = TRUE,
  xlab = "Hazard Ratio (log-echelle)",
  title = "Modèle de Cox ajuste à l’appariement"
)



# Packages
library(survival)
library(forestplot)
library(dplyr)

# Modèle Cox robuste
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

# Resume HR + IC
hr <- exp(coef(cox_robuste))
ci <- exp(confint(cox_robuste))
labels <- c("RA laparo protegee vs Hartmann coelio")

# Preparer donnees pour forestplot
table_forest <- data.frame(
  Variable = labels,
  HR = round(hr, 2),
  CI_low = round(ci[, 1], 2),
  CI_high = round(ci[, 2], 2)
)

tabletext <- cbind(
  c("Technique", table_forest$Variable),
  c("HR [IC95%]", paste0(table_forest$HR, " [", table_forest$CI_low, " – ", table_forest$CI_high, "]"))
)







# Chargement des packages
library(survival)
library(dplyr)
library(broom)
library(ggplot2)
install.packages("lmtest")
library(patchwork)
library(sandwich)
library(lmtest)

# Modèle de Cox robuste (avec cluster)
cox_robuste <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
                     data = df_survie_apparie)

# Extraire les coefficients robustes
# On utilise coeftest() avec estimateur sandwich (robust)
robust_test <- coeftest(cox_robuste)

# Extraire HR, IC 95% et p-value
hr <- exp(robust_test[,"Estimate"])
ci_low <- exp(robust_test[,"Estimate"] - 1.96 * robust_test[,"Std. Error"])
ci_high <- exp(robust_test[,"Estimate"] + 1.96 * robust_test[,"Std. Error"])
pval <- robust_test[,"Pr(>|z|)"]

# Creer dataframe pour ggplot
df_forest <- data.frame(
  variable = "RA laparo protegee vs Hartmann coelio",
  HR = hr,
  CI_low = ci_low,
  CI_high = ci_high,
  p_value = pval
)

# Forestplot ggplot
forest_plot <- ggplot(df_forest, aes(x = HR, y = variable)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10(limits = c(0.4, 3), breaks = c(0.5, 1, 2, 3)) +
  xlab("Hazard Ratio (echelle log)") +
  ylab("") +
  ggtitle("Modèle de Cox avec estimateur robuste (cluster sur les paires)") +
  theme_minimal(base_size = 14)

# Flèches directionnelles (en bas du plot)
flèche_gg <- ggplot() +
  annotate("text", x = 0, y = 0, label = "← en faveur du Hartmann coelio", hjust = 0, size = 5) +
  annotate("text", x = 1, y = 0, label = "→ en faveur de la RA laparo protegee", hjust = 1, size = 5) +
  theme_void() +
  xlim(0, 1)

# Combiner les deux avec patchwork
forest_plot / flèche_gg + plot_layout(heights = c(10, 1))



library(survival)

# Modèle de Cox stratifie sur les paires
cox_stratifie <- coxph(Surv(S_duree_avec_stomie, fermeture_stomie) ~ I_geste_comparaison_2HartmannRA + strata(Pair_ID),
                       data = df_survie_apparie)

# Afficher le resume
summary(cox_stratifie)



# Charger les bibliothèques nécessaires
library(survival)
library(ggplot2)
library(dplyr)
library(forestplot)

# Créer un data.frame résumant les deux modèles
df_forest <- tibble::tibble(
  Modèle = c("Cox marginal robuste", "Cox stratifié par paire"),
  HR = c(1.84, 2.28),
  IC_inf = c(1.39, 1.47),
  IC_sup = c(2.44, 3.52),
  p = c(2.34e-05, 0.000223)
)

# Créer une colonne pour affichage textuel de l'IC
df_forest <- df_forest %>%
  mutate(
    HR_IC = sprintf("%.2f [%.2f–%.2f]", HR, IC_inf, IC_sup),
    p_text = ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
  )

# Préparer les lignes pour forestplot
tabletext <- cbind(
  c("Modèle", df_forest$Modèle),
  c("HR [IC 95%]", df_forest$HR_IC),
  c("p", df_forest$p_text)
)

# Tracer le forest plot
forestplot::forestplot(
  labeltext = tabletext,
  mean = c(NA, df_forest$HR),
  lower = c(NA, df_forest$IC_inf),
  upper = c(NA, df_forest$IC_sup),
  zero = 1,
  xlog = TRUE,  # Échelle logarithmique
  col = forestplot::fpColors(box = "black", lines = "black", zero = "gray50"),
  boxsize = 0.2,
  lineheight = unit(1.5, "cm"),
  lwd.ci = 2,
  ci.vertices = TRUE,
  ci.vertices.height = 0.1,
  xticks = c(0.5, 1, 2, 4),
  xlab = "<--- Faveur Hartmann                  Faveur RA protégée --->"
)











# Script Cox simplifié pour analyse de survie avec données appariées
# Analyse du délai jusqu'à fermeture de stomie

library(survival)
library(survminer)
library(forestplot)
library(dplyr)

# ===== 1. ANALYSE PRINCIPALE : Cox avec cluster robuste =====
cox_principal <- coxph(
  Surv(S_duree_avec_stomie, fermeture_stomie) ~ 
    I_geste_comparaison_1HartmannCoelio + cluster(Pair_ID),
  data = df_survie_apparie
)

# Résumé complet
summary(cox_principal)

# ===== 2. ANALYSE DE SENSIBILITÉ : Cox stratifié =====
cox_sensibilite <- coxph(
  Surv(S_duree_avec_stomie, fermeture_stomie) ~ 
    I_geste_comparaison_1HartmannCoelio + strata(Pair_ID),
  data = df_survie_apparie
)

# ===== 3. FOREST PLOT COMPARATIF =====
# Extraire les résultats des deux modèles
resultats <- data.frame(
  Modele = c("Cox avec cluster robuste", "Cox stratifié par paire"),
  HR = c(exp(coef(cox_principal)), exp(coef(cox_sensibilite))),
  IC_inf = c(
    summary(cox_principal)$conf.int[,"lower .95"],
    summary(cox_sensibilite)$conf.int[,"lower .95"]
  ),
  IC_sup = c(
    summary(cox_principal)$conf.int[,"upper .95"],
    summary(cox_sensibilite)$conf.int[,"upper .95"]
  ),
  p_value = c(
    summary(cox_principal)$coefficients[,"Pr(>|z|)"],
    summary(cox_sensibilite)$coefficients[,"Pr(>|z|)"]
  )
)

# Formatter pour l'affichage
resultats$HR_IC <- sprintf("%.2f [%.2f-%.2f]", 
                           resultats$HR, 
                           resultats$IC_inf, 
                           resultats$IC_sup)
resultats$p_text <- ifelse(resultats$p_value < 0.001, 
                           "< 0.001", 
                           sprintf("%.3f", resultats$p_value))

# Créer le forest plot
tabletext <- cbind(
  c("Modèle", resultats$Modele),
  c("HR [IC 95%]", resultats$HR_IC),
  c("p", resultats$p_text)
)

forestplot(
  labeltext = tabletext,
  mean = c(NA, resultats$HR),
  lower = c(NA, resultats$IC_inf),
  upper = c(NA, resultats$IC_sup),
  zero = 1,
  xlog = TRUE,
  col = fpColors(box = "black", lines = "black", zero = "gray50"),
  boxsize = 0.2,
  lwd.ci = 2,
  ci.vertices = TRUE,
  xticks = c(0.5, 1, 2, 4),
  xlab = "← Faveur Hartmann        Faveur RA protégée →",
  title = "Comparaison des modèles de Cox pour données appariées"
)

# ===== 4. VÉRIFICATION DES HYPOTHÈSES =====
# Test de proportionnalité des risques
test_ph <- cox.zph(cox_principal)
print(test_ph)
plot(test_ph)

# ===== 5. RÉSULTATS CLÉS POUR LE RAPPORT =====
cat("\n=== RÉSULTATS POUR LE RAPPORT ===\n")
cat("Modèle principal (Cox avec cluster robuste):\n")
cat("HR =", sprintf("%.2f", exp(coef(cox_principal))), "\n")
cat("IC 95% = [", sprintf("%.2f", summary(cox_principal)$conf.int[,"lower .95"]), 
    "-", sprintf("%.2f", summary(cox_principal)$conf.int[,"upper .95"]), "]\n")
cat("p =", format.pval(summary(cox_principal)$coefficients[,"Pr(>|z|)"], digits = 3), "\n")

# Taux de fermeture à 2 ans (optionnel)
km_fit <- survfit(Surv(S_duree_avec_stomie, fermeture_stomie) ~ 
                    I_geste_comparaison_1HartmannCoelio, 
                  data = df_survie_apparie)
surv_2ans <- summary(km_fit, times = 730)
cat("\nTaux de fermeture à 2 ans:\n")
cat("- Hartmann:", sprintf("%.1f%%", 100*(1-surv_2ans$surv[1])), "\n")
cat("- RA protégée:", sprintf("%.1f%%", 100*(1-surv_2ans$surv[2])), "\n")
