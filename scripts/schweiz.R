
# SCHWEIZ 

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# ── Daten laden ──────────────────────────────────────────────
daten      <- read_csv("data/schweiz_daten.csv")
zeitreihe  <- read_csv("data/schweiz_zeitreihe.csv")
gdp_kanton <- read_csv("data/schweiz_gdp_kantone.csv")
cat("✓ Alle Daten geladen\n")


# ════════════════════════════════════════════════════════════
# PLOT 1: Karte der Schweiz
# ════════════════════════════════════════════════════════════
europe  <- ne_countries(scale = "medium", returnclass = "sf",
                        continent = "Europe")
schweiz <- ne_countries(scale = "medium", returnclass = "sf",
                        country = "Switzerland")

ggplot() +
  geom_sf(data = europe,
          fill = "#F1EFE8", color = "#CCCCCC", linewidth = 0.2) +
  geom_sf(data = schweiz,
          fill = "#1A4A8A", color = "white", linewidth = 0.6) +
  coord_sf(xlim = c(5.5, 10.8), ylim = c(45.7, 47.9)) +
  labs(
    title    = "Schweiz – Geographische Lage",
    subtitle = "Binnenstaat · kein Meereszugang · Grenze: 1 935 km",
    caption  = "Quelle: Natural Earth / EDA 2024"
  ) +
  theme_void(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"))

ggsave("plots/schweiz/01_karte.png", width = 7, height = 4, dpi = 150)
cat("✓ Plot 1: Karte\n")


# ════════════════════════════════════════════════════════════
# PLOT 2: Landnutzung
# ════════════════════════════════════════════════════════════
landnutzung <- daten %>%
  filter(kategorie == "Landnutzung",
         merkmal != "Wirtschaftlich_nutzbar",
         merkmal != "Produktive_LW_Flaeche",
         merkmal != "Anzahl_LW_Betriebe") %>%
  mutate(merkmal = recode(merkmal,
                          "Landwirtschaft"             = "Landwirtschaft 36%",
                          "Wald"                       = "Wald 30%",
                          "Unproduktiv_Fels_Gletscher" = "Unproduktiv 22%",
                          "Siedlung"                   = "Siedlung 8%",
                          "Gewaesser"                  = "Gewässer 4%"
  ))

ggplot(landnutzung, aes(x = "", y = wert, fill = merkmal)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c(
    "Landwirtschaft 36%"  = "#63993D",
    "Wald 30%"            = "#27500A",
    "Unproduktiv 22%"     = "#B4B2A9",
    "Siedlung 8%"         = "#C0A882",
    "Gewässer 4%"         = "#378ADD"
  )) +
  labs(title    = "Landnutzung der Schweiz",
       subtitle = "Wirtschaftlich nutzbar: ca. 44% (LW + Siedlung)",
       caption  = "Quelle: BFS – Arealstatistik 2024",
       fill     = "") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray50"))

ggsave("plots/schweiz/02_landnutzung.png", width = 6, height = 5, dpi = 150)
cat("✓ Plot 2: Landnutzung\n")


# ════════════════════════════════════════════════════════════
# PLOT 3: Temperatur Zeitreihe
# ════════════════════════════════════════════════════════════
zeitreihe %>%
  filter(merkmal == "Jahrestemperatur") %>%
  ggplot(aes(x = jahr, y = wert)) +
  geom_line(color = "#A32D2D", linewidth = 1.2) +
  geom_point(color = "#A32D2D", size = 3) +
  geom_text(aes(label = paste0(wert, "°C")),
            vjust = -0.8, size = 3.2) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(5.5, 8.5)) +
  labs(title   = "Durchschnittstemperatur Schweiz 2014–2023",
       x = "Jahr", y = "Grad C",
       caption = "Quelle: MeteoSchweiz") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("plots/schweiz/03_temperatur.png", width = 7, height = 4, dpi = 150)
cat("✓ Plot 3: Temperatur\n")


# ════════════════════════════════════════════════════════════
# PLOT 4: Niederschlag Zeitreihe
# ════════════════════════════════════════════════════════════
zeitreihe %>%
  filter(merkmal == "Niederschlag_national") %>%
  ggplot(aes(x = jahr, y = wert)) +
  geom_line(color = "#185FA5", linewidth = 1.2) +
  geom_point(color = "#185FA5", size = 3) +
  geom_text(aes(label = paste0(wert, " mm")),
            vjust = -0.8, size = 3.2) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(900, 1400)) +
  labs(title   = "Jahresniederschlag Schweiz 2014–2023",
       x = "Jahr", y = "mm pro Jahr",
       caption = "Quelle: MeteoSchweiz") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("plots/schweiz/04_niederschlag.png", width = 7, height = 4, dpi = 150)
cat("✓ Plot 4: Niederschlag\n")


# ════════════════════════════════════════════════════════════
# PLOT 5: BIP Schweiz gesamt Zeitreihe
# ════════════════════════════════════════════════════════════
zeitreihe %>%
  filter(merkmal == "BIP_gesamt_Mrd_CHF") %>%
  ggplot(aes(x = jahr, y = wert)) +
  geom_line(color = "#1A4A8A", linewidth = 1.2) +
  geom_point(color = "#1A4A8A", size = 3) +
  geom_text(aes(label = paste0(wert, " Mrd")),
            vjust = -0.8, size = 3.2) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(600, 850)) +
  labs(title   = "BIP der Schweiz 2014–2023",
       x = "Jahr", y = "Mrd CHF",
       caption = "Quelle: BFS") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("plots/schweiz/05_bip_gesamt.png", width = 7, height = 4, dpi = 150)
cat("✓ Plot 5: BIP gesamt\n")


# ════════════════════════════════════════════════════════════
# PLOT 6: BIP Top 5 Kantone Zeitreihe 2014–2023
# ════════════════════════════════════════════════════════════
top5 <- c("Zuerich", "Bern", "Waadt", "Genf", "Aargau")

gdp_kanton %>%
  filter(kanton %in% top5) %>%
  ggplot(aes(x = jahr, y = gdp_mio_chf, color = kanton)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_color_manual(values = c(
    "Zuerich" = "#1A4A8A",
    "Bern"    = "#A32D2D",
    "Waadt"   = "#1D9E75",
    "Genf"    = "#854F0B",
    "Aargau"  = "#534AB7"
  )) +
  labs(title   = "BIP Top 5 Kantone 2014–2023",
       x = "Jahr", y = "Mio CHF",
       color   = "Kanton",
       caption = "Quelle: BFS") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("plots/schweiz/06_bip_top5_zeitreihe.png", width = 8, height = 5, dpi = 150)
cat("✓ Plot 6: BIP Top 5 Zeitreihe\n")


# ════════════════════════════════════════════════════════════
# PLOT 7: BIP pro Kopf Top 5 Kantone Zeitreihe 2014–2023
# ════════════════════════════════════════════════════════════
top5_kopf <- c("Basel_Stadt", "Zug", "Genf", "Neuenburg", "Zuerich")

gdp_kanton %>%
  filter(kanton %in% top5_kopf) %>%
  ggplot(aes(x = jahr, y = gdp_pro_kopf_chf, color = kanton)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_color_manual(values = c(
    "Basel_Stadt" = "#1A4A8A",
    "Zug"         = "#A32D2D",
    "Genf"        = "#1D9E75",
    "Neuenburg"   = "#854F0B",
    "Zuerich"     = "#534AB7"
  )) +
  labs(title   = "BIP pro Kopf – reichste Kantone 2014–2023",
       x = "Jahr", y = "CHF pro Einwohner",
       color   = "Kanton",
       caption = "Quelle: BFS") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("plots/schweiz/07_bip_prokopf_zeitreihe.png", width = 8, height = 5, dpi = 150)
cat("✓ Plot 7: BIP pro Kopf Zeitreihe\n")


# ════════════════════════════════════════════════════════════
# PLOT 8: Alle Kantone BIP 2023 Balken
# ════════════════════════════════════════════════════════════
gdp_kanton %>%
  filter(jahr == 2023) %>%
  ggplot(aes(x = reorder(kanton, gdp_mio_chf), y = gdp_mio_chf)) +
  geom_col(fill = "#1A4A8A", color = "white") +
  geom_text(aes(label = paste0(round(gdp_mio_chf / 1000, 1), " Mrd")),
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 200000)) +
  labs(title   = "BIP pro Kanton 2023",
       x = "", y = "Mio CHF",
       caption = "Quelle: BFS") +
  theme_minimal(base_size = 11) +
  theme(plot.title         = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

ggsave("plots/schweiz/08_bip_alle_kantone_2023.png",
       width = 8, height = 8, dpi = 150)
cat("✓ Plot 8: Alle Kantone BIP 2023\n")


# ════════════════════════════════════════════════════════════
# PLOT 9: Bevölkerung Zeitreihe
# ════════════════════════════════════════════════════════════
zeitreihe %>%
  filter(merkmal == "Einwohner_gesamt") %>%
  ggplot(aes(x = jahr, y = wert / 1000000)) +
  geom_line(color = "#534AB7", linewidth = 1.2) +
  geom_point(color = "#534AB7", size = 3) +
  geom_text(aes(label = paste0(round(wert / 1000000, 2), " Mio")),
            vjust = -0.8, size = 3.2) +
  scale_x_continuous(breaks = 2014:2023) +
  scale_y_continuous(limits = c(8.1, 9.1)) +
  labs(title   = "Bevölkerungsentwicklung Schweiz 2014–2023",
       x = "Jahr", y = "Millionen Einwohner",
       caption = "Quelle: BFS") +
  theme_minimal(base_size = 12) +
  theme(plot.title       = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1))

ggsave("plots/schweiz/09_bevoelkerung.png", width = 7, height = 4, dpi = 150)
cat("✓ Plot 9: Bevölkerung\n")


cat("\n✅ FERTIG – alle 9 Plots in plots/schweiz/ gespeichert!\n")