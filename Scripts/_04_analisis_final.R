# ============================================================
# ANALISIS FINAL 
# Tipo de Cambio vs Demanda Interna (2003-2025)
# ============================================================

# 1. LIMPIAR ENTORNO
rm(list = ls())

# 2. CARGAR LIBRERIAS
library(tidyverse)

# 3. SELECCIONAR EL ARCHIVO COMBINADO
cat("\n--- SELECCIONA EL ARCHIVO COMBINADO ---\n")
cat("Archivo: datos_combinados_2002-2025.csv\n")

datos <- read.csv(file.choose(), stringsAsFactors = FALSE)

# 4. VERIFICAR DATOS
cat("\n--- DATOS CARGADOS ---\n")
print(head(datos))

# 5. CORRELACION
correlacion <- cor(datos$tipo_cambio, datos$demanda_interna)
cat("\n--- CORRELACION ---\n")
cat("Coeficiente de Pearson:", round(correlacion, 4), "\n")

# 6. TABLA RESUMEN
tabla_resumen <- datos %>%
  select(año, tipo_cambio, demanda_interna) %>%
  mutate(
    tipo_cambio = round(tipo_cambio, 2),
    demanda_interna = round(demanda_interna, 0)
  )

cat("\n--- TABLA RESUMEN ---\n")
print(tabla_resumen)

write.csv(tabla_resumen, "tabla_resumen_final.csv", row.names = FALSE)

# 7. GRAFICO PARA PUBLICACION
g_final <- ggplot(datos, aes(x = año, y = tipo_cambio)) +
  geom_line(color = "#2c3e50", size = 1.5) +
  geom_point(color = "#2c3e50", size = 4) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE, linetype = "dashed", size = 1) +
  labs(
    title = "Evolución del Tipo de Cambio en Perú (2003-2025)",
    subtitle = "El tipo de cambio muestra una tendencia decreciente en el largo plazo",
    x = "Año",
    y = "Tipo de Cambio (S/ por US$)",
    caption = "Fuente: BCRP | Elaboración: Propia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7f8c8d"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

ggsave("grafico_final_linkedin.png", g_final, width = 12, height = 7, dpi = 300)

# 8. MENSAJE FINAL
cat("\n✅ ANALISIS FINAL COMPLETADO\n")
cat("=================================\n")
cat("\nGRAFICO GENERADO:\n")
cat("   grafico_final_linkedin.png\n")
cat("\nTABLA RESUMEN GUARDADA:\n")
cat("   tabla_resumen_final.csv\n")
cat("\nCORRELACION:", round(correlacion, 4), "\n")