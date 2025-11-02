# ================================================================
# GRAPHIQUES SÉPARÉS — 3 COULEURS PERSONNALISÉES (1 PAR GROUPE)
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# ------------------------------------------------
# Données
# ------------------------------------------------
df_plot <- tribble(
  ~groupe,                       ~n,  ~taux_pct, ~p_taux_vs_RA, ~delai_median, ~iqr_low, ~iqr_high, ~p_delai_vs_RA,
  "RA protégée",                 101, 88.1,      NA,            113,           83,        174,       NA,
  "Hartmann non converti",        52,  63.5,      0.0067,        142,           112,       177,       0.3811,
  "Hartmann converti",            49,  77.6,      0.0389,        148,           105,       196,       0.1257
) %>%
  mutate(
    groupe = factor(groupe, levels = c("RA protégée","Hartmann non converti","Hartmann converti")),
    taux_lab  = sprintf("%.1f%%", taux_pct),
    delai_lab = paste0(delai_median, " j [", iqr_low, "–", iqr_high, "]")
  )

# ------------------------------------------------
# n et p-values (gardés en commentaires, pas affichés)
# ------------------------------------------------
# n :
# - RA protégée : n = 101
# - Hartmann non converti : n = 52
# - Hartmann converti     : n = 49
#
# p-values vs RA :
# - Taux   : HNC p = 0.0067 ; HC p = 0.0389
# - Délai  : HNC p = 0.3811 ; HC p = 0.1257

# ------------------------------------------------
# Palette (1 couleur par groupe)
# 👉 Tu peux modifier ces hex à ta guise
# ------------------------------------------------
pal <- c(
  "RA protégée"            = "#1f4e79",  # bleu foncé
  "Hartmann non converti"  = "#e63946",  # rouge
  "Hartmann converti"      = "#2a9d8f"   # vert turquoise
)

# ------------------------------------------------
# Thème
# ------------------------------------------------
theme_clean <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face="bold", size=16, hjust=0),
    plot.subtitle    = element_text(size=11, colour="#374151"),
    axis.title       = element_text(face="bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(10, 12, 10, 12)
  )

# ------------------------------------------------
# (1) PLOT TAUX — barres verticales + % affichés
# ------------------------------------------------
ymax_taux <- max(df_plot$taux_pct) * 1.15

plot_taux <- ggplot(df_plot, aes(x = groupe, y = taux_pct, fill = groupe)) +
  geom_col(width = 0.62, colour = NA) +
  geom_col(width = 0.62, fill = alpha("white", 0.05)) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(0, ymax_taux), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Taux de rétablissement de continuité",
    subtitle = "Bar plot vertical",
    x = NULL, y = "Taux (%)"
  ) +
  geom_text(aes(label = taux_lab), vjust = -0.6, size = 4.3, fontface = "bold") +
  theme_clean

print(plot_taux)

#export
ggsave("plot_taux_avec_conversion.png", plot_taux, width = 6, height = 3, dpi = 1000)

# ------------------------------------------------
# (2) PLOT DÉLAI — horizontal "lollipop" + médiane[IQR]
# ------------------------------------------------
xmax_delai <- max(df_plot$iqr_high) * 1.18

plot_delai_h <- ggplot(df_plot, aes(y = groupe, x = delai_median, colour = groupe)) +
  geom_linerange(aes(xmin = iqr_low, xmax = iqr_high), size = 6, alpha = 0.18) +
  geom_point(size = 4.6, stroke = 0, alpha = 0.95) +
  geom_text(aes(label = delai_lab), hjust = -0.1, size = 4.2, fontface = "bold") +
  scale_color_manual(values = pal) +
  scale_x_continuous(limits = c(0, xmax_delai), expand = expansion(mult = c(0.05, 0.08))) +
  labs(
    title = "Délai jusqu’au rétablissement",
    subtitle = "Horizontal — médiane (point) et IQR (bande)",
    x = "Jours", y = NULL
  ) +
  theme_clean

print(plot_delai_h)

#export
ggsave("plot_delai_avec_conversion.png", plot_delai_h, width = 6, height = 3, dpi = 1000)
