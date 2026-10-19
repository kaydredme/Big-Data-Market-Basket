# ==============================================================================
# TAREA: REGLAS DE ASOCIACIÓN - ANÁLISIS DE CANASTA DE COMPRA (ESTILO WALMART)
# Dataset: Groceries (9,835 transacciones de supermercado)
# ==============================================================================

graphics.off()
rm(list = ls())

library(arules)
library(arulesViz)
library(ggplot2)

data("Groceries")
datos_super <- Groceries

# Gráfica 1: Los 10 productos más vendidos
png("img1_top_productos.png", width = 900, height = 600, res = 150)
itemFrequencyPlot(datos_super, topN = 10, type = "absolute",
                  col = "coral3", horiz = TRUE,
                  main = "Top 10 Productos Más Vendidos en el Supermercado",
                  xlab = "Cantidad de veces comprado")
dev.off()

# 4. Algoritmo Apriori (Buscando patrones de compra)
# Usaremos parámetros diferentes a los de tu amigo para que sea único:
# Soporte de 0.005 (al menos 50 compras conjuntas) y Confianza de 0.6 (60% de seguridad)
reglas_super <- apriori(datos_super, 
                        parameter = list(supp = 0.005, conf = 0.6, minlen = 2))

quality(reglas_super) <- cbind(
  quality(reglas_super),
  interestMeasure(reglas_super, 
                  measure = c("leverage", "conviction", "chisquared"), 
                  transactions = datos_super)
)
metricas <- quality(reglas_super)

#  Gráfica 2: Histogramas de calidad
png("img2_histogramas.png", width = 1200, height = 400, res = 150)
par(mfrow = c(1, 3))
hist(metricas$support,    col = "dodgerblue4", main = "Soporte",   xlab = "Support")
hist(metricas$confidence, col = "firebrick4",  main = "Confianza", xlab = "Confidence")
hist(metricas$lift,       col = "forestgreen", main = "Lift (Elevación)", xlab = "Lift")
par(mfrow = c(1, 1))
dev.off()

#  Gráfica 3: Matriz de relaciones
png("img3_matriz.png", width = 800, height = 700, res = 150)
pairs(metricas[, c("support", "confidence", "lift", "leverage")],
      main = "Relación entre las métricas de las reglas",
      pch = 19, col = rgb(0.8, 0.2, 0.2, 0.3))
dev.off()

# Gráfica 4: Gráfico avanzado con ggplot2 
grafico_gg <- ggplot(as.data.frame(metricas), aes(x = support, y = confidence, color = lift)) +
  geom_point(alpha = 0.8, size = 2) +
  scale_color_viridis_c(option = "magma") +
  labs(title = "Análisis de Canasta: Soporte vs Confianza",
       x = "Soporte (Frecuencia)", y = "Confianza (Seguridad)", color = "Lift") +
  theme_dark() +
  theme(plot.title = element_text(face = "bold", size = 14))

png("img4_ggplot.png", width = 900, height = 600, res = 150)
print(grafico_gg)
dev.off()

#  Gráficas 5 y 6 con arulesViz 
png("img5_scatter.png", width = 850, height = 600, res = 150)
print(plot(reglas_super, measure = c("support", "confidence"), shading = "lift", engine = "ggplot2"))
dev.off()

png("img6_twokey.png", width = 850, height = 600, res = 150)
print(plot(reglas_super, method = "two-key plot", engine = "ggplot2"))
dev.off()

#  Gráfica 7: El top 10 de las mejores reglas (Grafo)
top_10_reglas <- head(sort(reglas_super, by = "lift"), 10)

png("img7_grafo.png", width = 900, height = 700, res = 150)
print(plot(top_10_reglas, method = "graph", engine = "igraph"))
dev.off()