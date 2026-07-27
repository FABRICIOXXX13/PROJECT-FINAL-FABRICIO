# ============================================================
# ANALISIS: Tipo de Cambio vs Demanda Interna
# ============================================================

# LIMPIAR ENTORNO
rm(list = ls())
cat("\n--- INICIO DEL ANALISIS ---\n")

#  CARGAR LIBRERIAS
library(tidyverse)
library(patchwork)

#  SELECCIONAR EL PRIMER ARCHIVO

tc <- read.csv(file.choose(),
               skip = 2,
               header = FALSE,
               col.names = c("anio", "tipo_cambio"),
               stringsAsFactors = FALSE)


# SELECCIONAR EL SEGUNDO ARCHIVO

demanda <- read.csv(file.choose(),
                    skip = 2,
                    header = FALSE,
                    col.names = c("anio", "demanda_interna"),
                    stringsAsFactors = FALSE)


#  FILTRAR DATOS
tc <- tc %>%
  filter(anio >= 2002 & anio <= 2025) %>%
  mutate(tipo_cambio = as.numeric(tipo_cambio))

demanda <- demanda %>%
  filter(anio >= 2002 & anio <= 2025) %>%
  mutate(demanda_interna = as.numeric(demanda_interna))

#  COMBINAR DATOS
datos <- tc %>%
  inner_join(demanda, by = "anio") %>%
  arrange(anio) %>%
  rename(año = anio)

# CALCULAR VARIACION
datos <- datos %>%
  mutate(
    variacion = (tipo_cambio / lag(tipo_cambio) - 1) * 100,
    color_variacion = ifelse(variacion > 0, "positive", "negative")
  ) %>%
  filter(!is.na(variacion))

print(head(datos))

#  ESTADISTICAS

print(summary(datos$tipo_cambio))
print(summary(datos$demanda_interna))

# 9. CORRELACION
correlacion <- cor(datos$tipo_cambio, datos$demanda_interna)



# Gráfico 1: Área
g1 <- ggplot(datos, aes(x = año, y = tipo_cambio)) +
  geom_area(fill = "#3498db", alpha = 0.5) +
  geom_line(color = "#2c3e50", size = 1.2) +
  geom_point(color = "#2c3e50", size = 2) +
  labs(
    title = "Evolución del Tipo de Cambio en Perú (2003-2025)",
    subtitle = "El área sombreada representa la magnitud del tipo de cambio",
    x = "Año",
    y = "Tipo de Cambio (S/ por US$)",
    caption = "Fuente: BCRP | Elaboración: Propia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7f8c8d"),
    axis.title = element_text(size = 12, face = "bold")
  )

ggsave("Figures/01_evolucion_area.png", g1, width = 10, height = 6, dpi = 300)

# Gráfico 2: Relación con color
g2 <- ggplot(datos, aes(x = demanda_interna, y = tipo_cambio)) +
  geom_point(aes(size = año, color = año), alpha = 0.8) +
  geom_smooth(method = "lm", color = "#e74c3c", se = TRUE, fill = "#f5b7b1", alpha = 0.3) +
  geom_text(aes(label = año), vjust = -1.2, size = 3, alpha = 0.6) +
  scale_color_gradient(low = "#3498db", high = "#2c3e50") +
  scale_size_continuous(range = c(2, 7), guide = "none") +
  labs(
    title = "Relación entre Demanda Interna y Tipo de Cambio",
    subtitle = "Cada punto representa un año. El color indica la evolución temporal",
    x = "Demanda Interna (millones S/)",
    y = "Tipo de Cambio (S/ por US$)",
    caption = "Fuente: BCRP | Elaboración: Propia"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7f8c8d"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

ggsave("Figures/02_relacion_color.png", g2, width = 10, height = 6, dpi = 300)

# Gráfico 3: Barras con línea
g3 <- ggplot(datos, aes(x = año)) +
  geom_bar(aes(y = variacion, fill = color_variacion), stat = "identity", width = 0.6) +
  geom_line(aes(y = variacion), color = "#2c3e50", size = 0.8, linetype = "dashed") +
  geom_point(aes(y = variacion), color = "#2c3e50", size = 2) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  geom_text(aes(y = variacion, label = paste0(round(variacion, 1), "%")), 
            vjust = ifelse(datos$variacion > 0, -0.5, 1.5), 
            size = 3, fontface = "bold") +
  scale_fill_manual(values = c("positive" = "#2ecc71", "negative" = "#e74c3c")) +
  labs(
    title = "Variación Anual del Tipo de Cambio (2003-2025)",
    subtitle = "Barras verdes: depreciación | Barras rojas: apreciación",
    x = "Año",
    y = "Variación Anual (%)",
    caption = "Fuente: BCRP | Elaboración: Propia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7f8c8d"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )

ggsave("Figures/03_variacion_barras_tendencia.png", g3, width = 10, height = 6, dpi = 300)

# Collage
collage_nuevo <- (g1 / (g2 | g3)) +
  plot_annotation(
    title = "Análisis del Tipo de Cambio en Perú (2003-2025)",
    subtitle = "Evolución, relación con demanda interna y variación anual",
    caption = "Fuente: BCRP | Elaboración: Propia"
  ) &
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#7f8c8d"),
    plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  )

ggsave("Figures/collage_nuevo.png", collage_nuevo, width = 14, height = 12, dpi = 300)

#  COLLAGE
collage <- (g1 / (g2 | g3)) +
  plot_annotation(title = "Analisis del Tipo de Cambio en Peru (2003-2025)", caption = "Fuente: BCRP")

ggsave("Figures/collage_tc_demanda.png", collage, width = 14, height = 10, dpi = 300)

# GUARDAR DATOS
if(!dir.exists("Data")) dir.create("Data")
write.csv(datos, "Data/datos_combinados_2002-2025.csv", row.names = FALSE)

