# R-codes-for-EBM
##DCA curves.
# --- ANÁLISIS DE CURVA DE DECISIÓN (DCA) v3 ---
#
# 1. Corrige el 'Modelo Completo' para evitar redundancia.
# 2. Pone todas las etiquetas y títulos en inglés.
#

# --- 1. Carga de Librerías (ya deberían estar cargadas) ---
library(dcurves)
library(readr)
library(dplyr)
library(ggplot2)

# --- 2. Carga y Preparación de Datos (sin cambios) ---
file_path <- "PANGEAS_R.csv" 
df <- read_csv2(file_path)

df_clean <- df %>%
  select(
    Def_sepsis, 
    Procalcitonin, 
    C_reactive_protein, 
    Septiscore
  ) %>%
  na.omit() # Solo seleccionamos los predictores que usaremos

cat(sprintf("Analysis will be performed on %d complete cases.\n", nrow(df_clean)))

# --- 3. Creación de Modelos Predictivos (Versión Corregida) ---

# Modelo 1: Solo Procalcitonin
mod_pct <- glm(
  Def_sepsis ~ Procalcitonin, 
  data = df_clean, 
  family = binomial(link = "logit")
)

# Modelo 2: Solo Septiscore
mod_score <- glm(
  Def_sepsis ~ Septiscore, 
  data = df_clean, 
  family = binomial(link = "logit")
)

# Modelo 3: Solo C-reactive protein (PCR)
mod_pcr <- glm(
  Def_sepsis ~ C_reactive_protein, 
  data = df_clean, 
  family = binomial(link = "logit")
)

# **** ¡MODELO CORREGIDO! ****
# Modelo 4: Modelo "Completo" (sin redundancia)
mod_full <- glm(
  Def_sepsis ~ Procalcitonin + C_reactive_protein + Septiscore, 
  data = df_clean, 
  family = binomial(link = "logit")
)

# --- 4. Añadir Probabilidades al Dataframe (Versión Corregida) ---
df_clean <- df_clean %>%
  mutate(
    prob_pct = predict(mod_pct, type = "response"),
    prob_score = predict(mod_score, type = "response"),
    prob_pcr = predict(mod_pcr, type = "response"), 
    prob_full = predict(mod_full, type = "response")
  )

# --- 5. Ejecución del DCA (Etiquetas en Inglés) ---
dca_analysis <- dca(
  data = df_clean,
  formula = Def_sepsis ~ prob_pct + prob_score + prob_pcr + prob_full,
  label = list( 
    prob_pct = "Procalcitonin Model",
    prob_score = "Septiscore Model",
    prob_pcr = "CRP Model",
    prob_full = "Combined Model"
  )
)

# --- 6. Generación de Gráfico (Etiquetas en Inglés) ---
dca_plot_english <- plot(
  dca_analysis, 
  smooth = TRUE 
) +
  labs(
    title = "Decision Curve Analysis",
    subtitle = "Comparison of models to predict Sepsis",
    x = "Threshold Probability",
    y = "Net Benefit"
  ) +
  theme_minimal(base_size = 14) 

# Mostrar el gráfico en RStudio
print(dca_plot_english)

# Guardar el gráfico final en inglés
ggsave("decision_curve_analysis_final_ENG.png", plot = dca_plot_english, width = 10, height = 7, dpi = 300)

cat("\nAnalysis complete! Final plot saved as 'decision_curve_analysis_final_ENG.png'\n")
