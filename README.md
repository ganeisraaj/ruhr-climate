# Thema 2 — Comparison of Distributions for the Analysis of Temporal and Spatial Climate Differences in the Ruhr Region
**Vergleich von Verteilungen zur Analyse zeitlicher und räumlicher Klimaunterschiede im Ruhrgebiet**

**Technische Universität Dortmund | Department of Statistics | SoSe 2026**
Supervised by Prof. Dr. Katja Ickstadt, JProf. Dr. Nils Weitzel, Dr. Zeyu Ding
Author: Ganeisraaj Kathiravan | Group partner: Jakub Marczat

---

## Overview / Überblick

This project analyses long-term temperature records from six weather stations in North Rhine-Westphalia (NRW) to address two questions: how much have temperatures changed over time at a representative station (Kahler Asten), and how do these temporal changes compare in magnitude to the spatial differences between stations? The report also assesses whether the adiabatic lapse rate of approximately 0.65°C per 100 m of elevation can explain the observed spatial temperature gradient.

Dieses Projekt analysiert Langzeit-Temperaturreihen von sechs Wetterstationen in NRW. Zwei Fragen stehen im Mittelpunkt: Wie stark haben sich die Temperaturen an Kahler Asten zwischen den Klimanormalperioden 1931–1960 und 1991–2020 verändert? Und wie groß sind diese zeitlichen Veränderungen im Vergleich zu den räumlichen Unterschieden zwischen den Stationen? Zusätzlich wird geprüft, ob die adiabatische Abkühlungsrate von ca. 0,65°C pro 100 m Höhe die räumlichen Temperaturunterschiede erklärt.

---

## Data / Daten

| | |
|---|---|
| **Source** | European Climate Assessment & Dataset (ECA&D) |
| **Stations** | Duisburg, Dortmund, Essen, Arnsberg, Brilon, Kahler Asten |
| **Elevation range** | 31 m (Duisburg) to 839 m (Kahler Asten) |
| **Period** | 1887–2025 (varies by station) |
| **Variables** | Annual, winter (DJF), and summer (JJA) mean temperatures (°C) |

---

## Methods / Methoden

- Descriptive statistics and outlier detection (1.5 × IQR rule)
- Missing value handling: pairwise deletion justified by gap location outside comparison windows
- Normality assessment: histograms, Q-Q plots, Shapiro-Wilk test
- Variance homogeneity: F-test
- **Temporal comparison (Kahler Asten, 1931–1960 vs. 1991–2020):** Welch *t*-test, Wilcoxon rank-sum test (DJF robustness check), Kolmogorov-Smirnov test
- **Spatial comparison (six stations, 1961–1990):** Kruskal-Wallis test, pairwise Wilcoxon comparisons with Bonferroni correction
- **Lapse rate assessment:** elevation-adjusted temperatures, re-run Kruskal-Wallis
- **Effect sizes:** Cohen's *d*, ε² (Kruskal-Wallis), raw mean difference (°C)
- **Multiple testing correction:** Bonferroni, Holm, Benjamini-Hochberg

---

## Key Findings / Zentrale Ergebnisse

- Annual mean temperature at Kahler Asten rose by **+0.76°C** between 1931–1960 and 1991–2020 (*d* = 0.955).
- The spatial temperature range across stations (~6°C over 800 m) is approximately **eight times** the temporal warming signal.
- Elevation accounts for the large majority of spatial differences; Arnsberg shows residual cooling (~0.7°C) consistent with cold air pooling, and Duisburg shows slight residual warmth consistent with an urban heat island effect.

---

## Software

R 4.5.x · `tidyverse` · `ggplot2` · `patchwork` · `readr`
