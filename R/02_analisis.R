# =============================================================================
# 02_analisis.R
# Análisis estadístico de un servicio técnico de reparaciones informáticas:
#   1. Carga y preparación de datos
#   2. Estadística descriptiva y ajuste de distribuciones (tiempos y costes)
#   3. Correlación y regresión
#   4. Fiabilidad de discos: análisis de supervivencia (Kaplan-Meier y Weibull)
#   5. Optimización: ANOVA, objetivos por tipo de equipo, ahorro y sensibilidad
#
# Usa los datos simulados de la carpeta datos/ (créalos antes con
# R/01_generar_datos.R). Ejecútalo desde la carpeta raíz del proyecto.
#
# - En VS Code, línea a línea (Ctrl/Cmd + Enter): los gráficos salen en pantalla.
# - Con "Rscript R/02_analisis.R": los gráficos se guardan en
#   resultados/graficos.pdf y las tablas en resultados/.
# =============================================================================

# --- Paquetes (instálalos una vez con R/00_instalar_paquetes.R) ---
library(readr)         # leer CSV
library(ggplot2)       # gráficos
library(fitdistrplus)  # ajuste de distribuciones
library(actuar)        # distribuciones Pareto y Burr
library(corrplot)      # matrices de correlación
library(GGally)        # matriz de dispersión (extensión de ggplot2)
library(scatterplot3d) # gráfico 3D del modelo de regresión
library(survival)      # análisis de supervivencia
library(survminer)     # gráficos de supervivencia
# dplyr se carga el último: fitdistrplus carga MASS, que tiene otra función
# select() que taparía la de dplyr si se cargara después
library(dplyr)         # manipular datos

# --- Rutas ---
ruta_reparaciones <- "datos/reparaciones_simuladas.csv"
ruta_fiabilidad   <- "datos/fiabilidad_simulada.csv"

if (!file.exists(ruta_reparaciones) || !file.exists(ruta_fiabilidad)) {
  stop("No encuentro los datos. Ejecuta primero R/01_generar_datos.R ",
       "desde la carpeta raíz del proyecto.")
}
dir.create("resultados", showWarnings = FALSE)

# Comprueba que un CSV tiene las columnas que necesita el análisis y que las
# numéricas se han leído como números. Así, si se usa un archivo con otro
# formato, el script se para con un mensaje claro en vez de fallar a mitad.
comprobar_columnas <- function(df, necesarias, numericas, archivo) {
  faltan <- setdiff(necesarias, names(df))
  if (length(faltan) > 0) {
    stop("En ", archivo, " faltan estas columnas: ", paste(faltan, collapse = ", "),
         ". Revisa el diccionario de datos del README.", call. = FALSE)
  }
  no_numericas <- numericas[!vapply(df[numericas], is.numeric, logical(1))]
  if (length(no_numericas) > 0) {
    stop("En ", archivo, " estas columnas deberían ser numéricas y no lo son: ",
         paste(no_numericas, collapse = ", "),
         ". ¿El archivo usa ';' como separador y ',' como decimal?", call. = FALSE)
  }
}

# --- Informe con la interpretación de los resultados ---
# Cada llamada a escribir() muestra el texto en la consola y lo guarda para
# crear al final resultados/informe.md (Markdown, que GitHub muestra con formato).
informe <- character()
escribir <- function(...) {
  texto <- paste0(...)
  informe <<- c(informe, texto)
  cat(texto, "\n", sep = "")
}
# Formato de números en español: 1.234,5
num  <- function(x, decimales = 1) {
  format(round(x, decimales), nsmall = decimales, big.mark = ".", decimal.mark = ",")
}
porc <- function(x, decimales = 1) paste0(num(100 * x, decimales), " %")
fuerza <- function(r) {
  if (abs(r) >= 0.7) "fuerte" else if (abs(r) >= 0.4) "moderada" else "débil"
}
texto_p <- function(p) if (p < 0.001) "p < 0,001" else paste("p =", num(p, 3))
horas_a_anios <- function(h) h / (8 * 365)  # suponiendo 8 horas de uso al día

# Si el script se ejecuta entero con Rscript, todos los gráficos van a un PDF
if (!interactive()) {
  pdf("resultados/graficos.pdf", width = 10, height = 7)
}


# =============================================================================
# 1. CARGA Y PREPARACIÓN
# =============================================================================

# read_csv2: separador ";" y coma decimal (formato europeo).
# La columna Fecha (formato AAAA-MM-DD) se reconoce como fecha automáticamente.
datos <- read_csv2(ruta_reparaciones, show_col_types = FALSE)
comprobar_columnas(
  datos,
  necesarias = c("ID_Incidencia", "Cliente_Tipo", "Tipo_Dispositivo", "Equipo",
                 "Tipo_Intervencion", "Tiempo_Horas", "Coste_Material", "Coste_Servicio"),
  numericas  = c("Tiempo_Horas", "Coste_Material", "Coste_Servicio"),
  archivo    = ruta_reparaciones
)

# Variable nueva: coste total = material + mano de obra
datos <- datos %>%
  mutate(Coste_Total = Coste_Material + Coste_Servicio)

cat("Registros cargados:", nrow(datos), "\n")
print(head(datos))

escribir("# Informe del análisis de reparaciones\n")
escribir("Generado el ", format(Sys.Date(), "%d/%m/%Y"), " a partir de `", ruta_reparaciones,
         "` (", nrow(datos), " reparaciones) y `", ruta_fiabilidad, "`. ",
         "Los gráficos están en `graficos.pdf` y la tabla de objetivos en `tabla_objetivos.csv`.\n")


# =============================================================================
# 2. ESTADÍSTICA DESCRIPTIVA
# =============================================================================

resumen_global <- datos %>%
  summarise(
    Facturacion_Total      = sum(Coste_Total),
    Coste_Medio_Reparacion = mean(Coste_Total),
    Tiempo_Medio           = mean(Tiempo_Horas),
    Maximo_Coste           = max(Coste_Total)
  )
print(resumen_global)

variables_numericas <- datos %>%
  select(Tiempo_Horas, Coste_Material, Coste_Servicio, Coste_Total)

# R no trae una función para la moda estadística, así que la definimos
calcular_moda <- function(x) {
  u <- unique(x)                    # valores distintos
  u[which.max(tabulate(match(x, u)))] # el que más veces aparece
}

# Coeficiente de variación (%): dispersión relativa a la media
calcular_cv <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
}

# sapply aplica todas las medidas a cada columna numérica
tabla_estadistica <- sapply(variables_numericas, function(x) {
  c(
    Media              = mean(x, na.rm = TRUE),
    Mediana            = median(x, na.rm = TRUE),
    Moda               = calcular_moda(x),
    Media_Recortada_5  = mean(x, trim = 0.05, na.rm = TRUE), # sin el 5 % más extremo
    Minimo             = min(x, na.rm = TRUE),
    Maximo             = max(x, na.rm = TRUE),
    Rango              = diff(range(x, na.rm = TRUE)),
    Varianza           = var(x, na.rm = TRUE),
    Desviacion_Tipica  = sd(x, na.rm = TRUE),
    Coef_Variacion_Pct = calcular_cv(x),
    Q1                 = unname(quantile(x, 0.25, na.rm = TRUE)),
    Q3                 = unname(quantile(x, 0.75, na.rm = TRUE)),
    Rango_Intercuartil = IQR(x, na.rm = TRUE)
  )
})
tabla_estadistica <- round(tabla_estadistica, 2)
print(tabla_estadistica)

# Frecuencias de las variables cualitativas (la "moda" de los textos)
print(datos %>% count(Tipo_Intervencion, sort = TRUE) %>% head(5))
print(datos %>% count(Cliente_Tipo, sort = TRUE))

# Interpretación
t_media <- mean(datos$Tiempo_Horas);  t_mediana <- median(datos$Tiempo_Horas)
c_media <- mean(datos$Coste_Total);   c_mediana <- median(datos$Coste_Total)
cv_coste <- calcular_cv(datos$Coste_Total)
tipos <- sort(table(datos$Tipo_Dispositivo), decreasing = TRUE)

escribir("## 1. Cómo es una reparación típica\n")
escribir("- La reparación mediana dura **", num(t_mediana), " h** y cuesta **", num(c_mediana, 0),
         " €**. La media es de ", num(t_media), " h y ", num(c_media, 0), " €.")
if (c_media > 1.1 * c_mediana) {
  escribir("- Que la media esté claramente por encima de la mediana indica que unas pocas ",
           "reparaciones muy largas o caras tiran de ella. Las distribuciones tienen cola a la ",
           "derecha, así que la mediana representa mejor el caso típico.")
}
escribir("- El coeficiente de variación del coste es del ", num(cv_coste, 0), " %",
         if (cv_coste > 100) ": la dispersión es muy alta, hay trabajos de coste muy distinto."
         else ": la dispersión es moderada.")
escribir("- En el ", porc(mean(datos$Coste_Material == 0), 0), " de las reparaciones no se usó ",
         "material. El tipo de equipo más reparado es **", names(tipos)[1], "** (",
         porc(tipos[[1]] / nrow(datos), 0), " del total).\n")


# --- 2.1 Ajuste de distribuciones ---
# Weibull, log-normal y gamma solo admiten valores positivos
datos_tiempo <- datos$Tiempo_Horas[datos$Tiempo_Horas > 0]
datos_coste  <- datos$Coste_Total[datos$Coste_Total > 0]

# Histograma y función de distribución empírica de cada variable
plotdist(datos_tiempo, histo = TRUE, demp = TRUE, las = 1, col = "lightblue")
plotdist(datos_coste,  histo = TRUE, demp = TRUE, las = 1, col = "orange")
# Las dos son muy asimétricas, con cola a la derecha: descartamos la normal.

# Gráfico de Cullen y Frey (asimetría frente a curtosis) para ver qué
# distribuciones son candidatas. El bootstrap da robustez a la conclusión.
par(mfrow = c(1, 1))
descdist(datos_tiempo, boot = 1000)
descdist(datos_coste,  boot = 1000)

# Función auxiliar: compara varios ajustes con los cuatro gráficos de
# fitdistrplus (densidad, CDF, QQ y PP) y devuelve la tabla de bondad de ajuste.
# Si algún ajuste no ha convergido (NULL), se deja fuera de la comparación.
comparar_ajustes <- function(ajustes, nombres, variable) {
  validos <- !vapply(ajustes, is.null, logical(1))
  ajustes <- ajustes[validos]
  nombres <- nombres[validos]
  denscomp(ajustes, legendtext = nombres, main = paste("Histograma y densidades -", variable))
  cdfcomp(ajustes,  legendtext = nombres, main = paste("CDF -", variable))
  qqcomp(ajustes,   legendtext = nombres, main = paste("QQ-plot -", variable))
  ppcomp(ajustes,   legendtext = nombres, main = paste("PP-plot -", variable))
  bondad <- gofstat(ajustes, fitnames = nombres)
  names(bondad$aic) <- nombres  # para poder saber después cuál gana
  bondad
}
# Nombre de la distribución con menor AIC
mejor_ajuste <- function(bondad) names(bondad$aic)[which.min(bondad$aic)]

# Tiempo: log-normal, Weibull y gamma
fit_t_lnorm   <- fitdist(datos_tiempo, "lnorm")
fit_t_weibull <- fitdist(datos_tiempo, "weibull")
fit_t_gamma   <- fitdist(datos_tiempo, "gamma")
gof_t <- comparar_ajustes(list(fit_t_lnorm, fit_t_weibull, fit_t_gamma),
                          c("Log-Normal", "Weibull", "Gamma"), "Tiempo")
print(gof_t$aic)  # menor AIC = mejor ajuste

# Coste: las mismas tres candidatas
fit_c_lnorm   <- fitdist(datos_coste, "lnorm")
fit_c_weibull <- fitdist(datos_coste, "weibull")
fit_c_gamma   <- fitdist(datos_coste, "gamma")
gof_c <- comparar_ajustes(list(fit_c_lnorm, fit_c_weibull, fit_c_gamma),
                          c("Log-Normal", "Weibull", "Gamma"), "Coste")
print(gof_c$aic)

# Probamos dos distribuciones de cola pesada (Pareto y Burr) frente a la
# log-normal, porque hay reparaciones con costes y tiempos muy extremos
# Estas dos necesitan valores iniciales y a veces no convergen: si falla una,
# avisamos y seguimos con el resto.
ajuste_seguro <- function(x, distr, start) {
  tryCatch(fitdist(x, distr, start = start),
           error = function(e) {
             message("No se pudo ajustar ", distr, ": ", conditionMessage(e))
             NULL
           })
}

fit_c_pareto <- ajuste_seguro(datos_coste, "pareto",
                              start = list(shape = 1, scale = 200))
fit_c_burr   <- ajuste_seguro(datos_coste, "burr",
                              start = list(shape1 = 1, shape2 = 1, scale = 200))
gof_c2 <- comparar_ajustes(list(fit_c_lnorm, fit_c_pareto, fit_c_burr),
                           c("Log-Normal", "Pareto", "Burr"), "Coste (cola pesada)")
print(gof_c2$aic)

fit_t_pareto <- ajuste_seguro(datos_tiempo, "pareto",
                              start = list(shape = 1, scale = 2))
fit_t_burr   <- ajuste_seguro(datos_tiempo, "burr",
                              start = list(shape1 = 1, shape2 = 1, scale = 2))
gof_t2 <- comparar_ajustes(list(fit_t_lnorm, fit_t_pareto, fit_t_burr),
                           c("Log-Normal", "Pareto", "Burr"), "Tiempo (cola pesada)")
print(gof_t2$aic)


# --- 2.2 Detección de outliers ---
par(mfrow = c(1, 2))
boxplot(datos$Tiempo_Horas, main = "Outliers en tiempo", col = "lightblue",
        ylab = "Horas", las = 1)
boxplot(datos$Coste_Total, main = "Outliers en coste", col = "orange",
        ylab = "Euros", las = 1)
par(mfrow = c(1, 1))

# Criterio del boxplot: es outlier lo que supera Q3 + 1,5 * IQR
umbral_outlier <- function(x) quantile(x, 0.75) + 1.5 * IQR(x)

outliers_c <- datos %>% filter(Coste_Total > umbral_outlier(Coste_Total))
outliers_t <- datos %>% filter(Tiempo_Horas > umbral_outlier(Tiempo_Horas))
cat("Outliers de coste:", nrow(outliers_c), "| Outliers de tiempo:", nrow(outliers_t), "\n")

# Las 3 reparaciones más caras y las 3 más largas
print(outliers_c %>% arrange(desc(Coste_Total)) %>%
        select(ID_Incidencia, Tipo_Dispositivo, Equipo, Coste_Total) %>% head(3))
print(outliers_t %>% arrange(desc(Tiempo_Horas)) %>%
        select(ID_Incidencia, Tipo_Dispositivo, Equipo, Tiempo_Horas) %>% head(3))

# Interpretación
mas_cara <- datos[which.max(datos$Coste_Total), ]
cola_pesada <- c("Pareto", "Burr")

escribir("## 2. Qué distribución siguen los tiempos y los costes\n")
escribir("- Entre log-normal, Weibull y gamma, la que mejor se ajusta según el AIC es la **",
         mejor_ajuste(gof_t), "** para el tiempo y la **", mejor_ajuste(gof_c), "** para el coste.")
escribir("- Al añadir distribuciones de cola pesada, gana la **", mejor_ajuste(gof_t2),
         "** para el tiempo y la **", mejor_ajuste(gof_c2), "** para el coste.",
         if (mejor_ajuste(gof_t2) %in% cola_pesada || mejor_ajuste(gof_c2) %in% cola_pesada)
           paste(" Hay más valores extremos de los que predice una log-normal. Para presupuestar",
                 "importa: las reparaciones caras no son tan raras como parecen.")
         else " La log-normal sigue siendo suficiente para describir los extremos.")
escribir("- Con el criterio del boxplot hay ", nrow(outliers_t), " reparaciones atípicas por tiempo (",
         porc(nrow(outliers_t) / nrow(datos)), ") y ", nrow(outliers_c), " por coste (",
         porc(nrow(outliers_c) / nrow(datos)), "). La más cara costó ", num(mas_cara$Coste_Total, 0),
         " € (", mas_cara$Equipo, ", ", tolower(mas_cara$Tipo_Intervencion), ").\n")


# =============================================================================
# 3. CORRELACIÓN Y REGRESIÓN
# =============================================================================

# Pearson mide relación lineal y es muy sensible a los outliers.
# Spearman usa rangos: capta relaciones monótonas y es robusta ante outliers,
# así que es la adecuada con estas distribuciones tan asimétricas.
matriz_pearson  <- cor(variables_numericas, use = "complete.obs", method = "pearson")
matriz_spearman <- cor(variables_numericas, use = "complete.obs", method = "spearman")
print(round(matriz_pearson, 3))
print(round(matriz_spearman, 3))

corrplot.mixed(matriz_spearman,
               lower = "number", upper = "ellipse", tl.col = "black",
               title = "Correlación de Spearman", mar = c(0, 0, 2, 0))

p <- ggpairs(variables_numericas,
             title = "Matriz de dispersión y correlación",
             upper = list(continuous = wrap("cor", method = "spearman")),
             lower = list(continuous = "smooth"),
             diag  = list(continuous = "barDiag"),
             axisLabels = "none")
# Zoom en el panel Coste_Material frente a Tiempo_Horas, para que los
# outliers no aplasten la nube de puntos
p[2, 1] <- p[2, 1] + coord_cartesian(xlim = c(0, 10), ylim = c(0, 500))
print(p)

# Regresión lineal del coste total. Quitamos Coste_Servicio como predictor
# porque es casi proporcional al tiempo (horas x tarifa): colinealidad.
modelo_coste <- lm(Coste_Total ~ Tiempo_Horas + Coste_Material, data = variables_numericas)
print(summary(modelo_coste))   # coeficientes y R cuadrado
print(confint(modelo_coste))   # intervalos de confianza al 95 %

prediccion <- data.frame(Real = variables_numericas$Coste_Total,
                         Predicho = predict(modelo_coste))
print(
  ggplot(prediccion, aes(x = Real, y = Predicho)) +
    geom_point(alpha = 0.5, colour = "steelblue") +
    geom_abline(slope = 1, intercept = 0, colour = "red", linewidth = 1) +
    labs(title = "Coste real frente a coste predicho",
         subtitle = paste("R² =", round(summary(modelo_coste)$r.squared, 4)),
         x = "Coste real (€)", y = "Coste predicho (€)") +
    theme_minimal()
)

s3d <- scatterplot3d(
  x = variables_numericas$Tiempo_Horas,
  y = variables_numericas$Coste_Material,
  z = variables_numericas$Coste_Total,
  pch = 16, color = "blue", grid = TRUE, box = FALSE, angle = 55,
  main = "Modelo de costes en 3D",
  xlab = "Tiempo (horas)", ylab = "Material (€)", zlab = "Coste total (€)"
)
s3d$plane3d(modelo_coste, col = "red", lty = "dotted")  # plano de regresión

# Interpretación
rho_s <- matriz_spearman["Tiempo_Horas", "Coste_Total"]
rho_p <- matriz_pearson["Tiempo_Horas", "Coste_Total"]
coefs <- coef(modelo_coste)
r2    <- summary(modelo_coste)$r.squared

escribir("## 3. Qué explica el coste\n")
escribir("- La correlación de Spearman entre horas y coste total es de ", num(rho_s, 2),
         " (relación ", fuerza(rho_s), "). Pearson da ", num(rho_p, 2),
         ". Pearson se ve mucho más afectada por los valores extremos, por eso se usa Spearman.")
escribir("- La regresión explica el ", porc(r2), " de la variabilidad del coste total. ",
         "Cada hora de trabajo añade de media ", num(coefs[["Tiempo_Horas"]], 1),
         " € y cada euro de material se traslada al coste en ", num(coefs[["Coste_Material"]], 2), " €.")
escribir("- Como el coste total es material más horas por tarifa, un R² alto es lo esperable. ",
         "Lo útil del modelo es cuantificar la tarifa efectiva por hora y comprobar que el ",
         "material se repercute casi íntegro al cliente.\n")


# =============================================================================
# 4. FIABILIDAD DE DISCOS: ANÁLISIS DE SUPERVIVENCIA
# =============================================================================

# stringsAsFactors = TRUE para que R pueda agrupar por marca
discos <- read.csv2(ruta_fiabilidad, stringsAsFactors = TRUE)
comprobar_columnas(discos,
                   necesarias = c("Marca", "Horas_Uso", "Fallo"),
                   numericas  = "Horas_Uso",
                   archivo    = ruta_fiabilidad)
if (!all(discos$Fallo %in% c("SI", "NO"))) {
  stop("La columna Fallo de ", ruta_fiabilidad, " solo puede contener SI o NO.", call. = FALSE)
}
# Evento: 1 si el disco ha fallado, 0 si sigue funcionando (dato censurado)
discos$Evento <- ifelse(discos$Fallo == "SI", 1, 0)

# Kaplan-Meier por marca. Surv() combina tiempo y evento: un disco con
# 5000 horas que no ha fallado cuenta como "5000+" (sabemos que sobrevivió
# al menos hasta ahí)
km_marcas <- survfit(Surv(Horas_Uso, Evento) ~ Marca, data = discos)

grafico_km <- ggsurvplot(
  km_marcas, data = discos,
  size = 1.2, censor.size = 5, palette = "Dark2",
  conf.int = FALSE,          # con 6 marcas las bandas de confianza tapan las curvas
  pval = TRUE,               # p-valor del test log-rank
  pval.coord = c(1000, 0.1),
  risk.table = TRUE, risk.table.col = "strata",
  title = "Fiabilidad por fabricante (Kaplan-Meier)",
  xlab = "Horas de funcionamiento", ylab = "Probabilidad de supervivencia",
  legend.title = "Fabricante", ggtheme = theme_bw()
)
# La curva y la tabla de riesgo se imprimen por separado para que quepan
print(grafico_km$plot + theme(legend.position = "bottom"))
print(grafico_km$table + labs(title = "Discos en riesgo por fabricante"))

# Modelos paramétricos: survreg da una fórmula continua en lugar de la
# escalera de Kaplan-Meier. "~ 1" = un solo modelo para todos los discos.
modelo_weibull   <- survreg(Surv(Horas_Uso, Evento) ~ 1, data = discos, dist = "weibull")
modelo_exp       <- survreg(Surv(Horas_Uso, Evento) ~ 1, data = discos, dist = "exponential")
modelo_lognormal <- survreg(Surv(Horas_Uso, Evento) ~ 1, data = discos, dist = "lognormal")

aic_supervivencia <- c(Weibull     = AIC(modelo_weibull),
                       Exponencial = AIC(modelo_exp),
                       LogNormal   = AIC(modelo_lognormal))
print(aic_supervivencia)  # menor AIC = mejor modelo

# Comparamos los tres modelos con la curva de Kaplan-Meier global
pct <- 1:99 / 100   # percentiles 1 %, 2 %, ..., 99 %
cuantiles <- function(modelo) {
  predict(modelo, type = "quantile", p = pct, newdata = data.frame(1))
}
plot(survfit(Surv(Horas_Uso, Evento) ~ 1, data = discos), conf.int = FALSE,
     main = "Ajuste paramétrico frente a Kaplan-Meier",
     xlab = "Horas", ylab = "Probabilidad de supervivencia", col = "gray", lwd = 2)
lines(cuantiles(modelo_weibull),   1 - pct, col = "red",   lwd = 2, lty = 1)
lines(cuantiles(modelo_exp),       1 - pct, col = "blue",  lwd = 2, lty = 2)
lines(cuantiles(modelo_lognormal), 1 - pct, col = "darkgreen", lwd = 2, lty = 3)
legend("bottomleft",
       legend = c("Kaplan-Meier (datos)", "Weibull", "Exponencial", "Log-normal"),
       col = c("gray", "red", "blue", "darkgreen"), lwd = 2, lty = c(1, 1, 2, 3), bty = "n")

# Parámetros Weibull. survreg usa otra parametrización, así que convertimos:
beta <- 1 / modelo_weibull$scale        # forma: >1 indica desgaste con el uso
eta  <- exp(coef(modelo_weibull)[[1]])  # escala: a las eta horas ha fallado el 63,2 %
mttf <- eta * gamma(1 + 1 / beta)       # tiempo medio hasta el fallo
b10  <- eta * (-log(0.90))^(1 / beta)   # vida B10: horas a las que ha fallado el 10 %

cat(sprintf("Weibull: beta = %.2f | eta = %.0f h | MTTF = %.0f h | B10 = %.0f h\n",
            beta, eta, mttf, b10))

# Funciones de fiabilidad del modelo Weibull
tasa_fallo   <- function(t) (beta / eta) * (t / eta)^(beta - 1)  # h(t)
supervivencia <- function(t) exp(-(t / eta)^beta)                # S(t)
fallo_acum   <- function(t) 1 - supervivencia(t)                 # F(t)

t_max <- 35000
curve(tasa_fallo(x), from = 1, to = t_max, col = "red", lwd = 3,
      main = "Tasa de fallo instantánea h(t) - Weibull",
      xlab = "Horas de funcionamiento", ylab = "h(t)")
grid()

curve(supervivencia(x), from = 0, to = t_max, col = "blue", lwd = 3, ylim = c(0, 1),
      main = "Función de supervivencia S(t) - Weibull",
      xlab = "Horas de funcionamiento", ylab = "S(t)")
grid()
abline(h = c(0.5, 0.9), col = "orange", lty = 2)
abline(v = b10, col = "orange", lty = 3)
text(0.1 * t_max, c(0.53, 0.93), c("50 % de supervivencia", "90 % (vida B10)"),
     col = "orange", cex = 0.8)

curve(fallo_acum(x), from = 0, to = t_max, col = "darkgreen", lwd = 3, ylim = c(0, 1),
      main = "Probabilidad acumulada de fallo F(t) - Weibull",
      xlab = "Horas de funcionamiento", ylab = "F(t)")
grid()

# Tabla de fiabilidad suponiendo un uso de 8 horas al día
anios <- c(1, 2, 3, 5, 7, 10)
horas <- anios * 8 * 365
tabla_weibull <- data.frame(
  Anios               = anios,
  Horas_Uso           = horas,
  Supervivencia_St    = paste0(round(supervivencia(horas) * 100, 1), "%"),
  Fallo_Acumulado_Ft  = paste0(round(fallo_acum(horas) * 100, 1), "%"),
  Tasa_Fallo_ht       = signif(tasa_fallo(horas), 3)
)
print(tabla_weibull)

# Interpretación
# Test log-rank: ¿son distintas las curvas de supervivencia de las marcas?
logrank   <- survdiff(Surv(Horas_Uso, Evento) ~ Marca, data = discos)
p_logrank <- pchisq(logrank$chisq, df = length(logrank$n) - 1, lower.tail = FALSE)

# Supervivencia de cada marca a las 10.000 horas (Kaplan-Meier)
km_10000 <- summary(km_marcas, times = 10000, extend = TRUE)
sup_10000 <- setNames(km_10000$surv, sub("Marca=", "", as.character(km_10000$strata)))
marca_mejor <- names(sup_10000)[which.max(sup_10000)]
marca_peor  <- names(sup_10000)[which.min(sup_10000)]

escribir("## 4. Fiabilidad de los discos\n")
escribir("- Se analizaron ", nrow(discos), " discos y ", sum(discos$Evento), " habían fallado (",
         porc(mean(discos$Evento), 0), "). El resto seguía funcionando al revisarlo: son datos ",
         "censurados, y por eso se usa análisis de supervivencia en lugar de una media.")
escribir("- A las 10.000 horas de uso, la marca más fiable es **", marca_mejor, "** (",
         porc(sup_10000[[marca_mejor]], 0), " de discos sin fallo) y la menos fiable **", marca_peor,
         "** (", porc(sup_10000[[marca_peor]], 0), "). El test log-rank da ", texto_p(p_logrank),
         if (p_logrank < 0.05) ", así que las diferencias entre marcas son estadísticamente significativas."
         else ", así que no hay evidencia suficiente de que las marcas se comporten distinto.")
escribir("- El modelo paramétrico con menor AIC es el **", names(aic_supervivencia)[which.min(aic_supervivencia)],
         "**.")
escribir("- En el modelo Weibull, β = ", num(beta, 2), ". ",
         if (beta > 1) paste("Al ser mayor que 1, el riesgo de fallo aumenta con el uso: los discos se",
                             "desgastan, así que tiene sentido sustituirlos de forma preventiva.")
         else paste("Al no ser mayor que 1, el riesgo no aumenta con el uso: predominan los fallos",
                    "tempranos o aleatorios, y la sustitución preventiva no aporta mucho."))
escribir("- La vida B10 es de ", num(b10, 0), " h (unos ", num(horas_a_anios(b10)),
         " años con 8 h de uso al día): en ese momento ya ha fallado 1 de cada 10 discos. ",
         "El tiempo medio hasta el fallo es de ", num(mttf, 0), " h (", num(horas_a_anios(mttf)),
         " años), y a los 3 años sigue funcionando el ", porc(supervivencia(3 * 8 * 365), 0),
         " de los discos.\n")


# =============================================================================
# 5. OPTIMIZACIÓN Y MEJORA DE PROCESOS
# =============================================================================

# --- 5.1 Clasificar cada intervención en una categoría de trabajo ---
# grepl busca palabras clave en el texto. case_when evalúa en orden, así que
# una intervención se queda con la primera categoría que encaje.
patron_software <- paste(
  "Virus|Malware|Ransomware|Configura|Instala|Windows|Office|Datos|Copia|Software",
  "Format|Actualiza|Driver|VPN|Certificado|Licencia|Nube|Correo|Web|Firmware|Outlook",
  "Bios|Logs|Password|Clave|Reset|Error|Bloqueo|Lento|Lentitud|Formación|Phishing",
  "Dominio|Red|Conecta|VLAN|Wifi|Señal|Imprime|Spooler|Reconstrucc|Calibraci|Renovaci",
  "Kernel|Bitlocker|iCloud|Contactos|Reinicio", sep = "|")
patron_mantenimiento <- paste(
  "Limpieza|Mantenimiento|Pasta|Revisi|Auditoría|Puesta a punto|Optimiza|Recupera",
  "Polvo|Engrasado|Test|Temperatura|Calor|Calentamiento|Alerta", sep = "|")
patron_hardware <- paste(
  "Pantalla|Disco|SSD|NVMe|Bater|Teclado|Fuente|Placa|Ventilador|Bisagra|Conector",
  "Pieza|Componente|Montaje|Ampliaci|Carcasa|Derrame|Físico|Hardware|Lámpara|Fusor",
  "Rodillo|Tambor|Memoria|RAM|Gráfica|Cajón|Puerto|Switch|Router|Cable|Táctil|Botón",
  "Cargador|Antena|Atasco|Papel|Cuchilla|Boquilla|Obstruida|Sensor|Filamento|Correa",
  "Fusible|Controlador|Sonda|Alimentaci|Refrigeraci|Goma|Lente|Chip|Lector|Lápiz",
  "Enciende|HDMI|Tecla|Touchpad|Trackpad|Ratón|Ratones|Cartucho|Venta|Parque",
  "Impresora|Ticket", sep = "|")

datos <- datos %>%
  mutate(
    Categoria_Trabajo = case_when(
      grepl(patron_software,      Tipo_Intervencion, ignore.case = TRUE) ~ "Software/Config",
      grepl(patron_mantenimiento, Tipo_Intervencion, ignore.case = TRUE) ~ "Mantenimiento",
      grepl(patron_hardware,      Tipo_Intervencion, ignore.case = TRUE) ~ "Hardware/Piezas",
      TRUE ~ "Otros/General"
    ),
    Categoria_Trabajo = as.factor(Categoria_Trabajo),
    Tipo_Dispositivo  = as.factor(Tipo_Dispositivo),
    Cliente_Tipo      = as.factor(Cliente_Tipo)
  )
print(datos %>% count(Categoria_Trabajo))

# --- 5.2 ANOVA: ¿qué factores influyen en el tiempo y en el coste? ---
# anova() hace un ANOVA secuencial (tipo I): el orden de los factores importa.
modelo_tiempo_cual <- lm(Tiempo_Horas ~ Tipo_Dispositivo + Cliente_Tipo + Categoria_Trabajo,
                         data = datos)
modelo_coste_cual  <- lm(Coste_Total ~ Tipo_Dispositivo + Cliente_Tipo + Categoria_Trabajo,
                         data = datos)
anova_tiempo <- anova(modelo_tiempo_cual)
anova_coste  <- anova(modelo_coste_cual)
print(anova_tiempo)
print(anova_coste)

# Suma de cuadrados de cada factor: cuánta variabilidad explica
par(mfrow = c(1, 2))
barplot(anova_tiempo$`Sum Sq`[1:3], names.arg = c("Dispositivo", "Cliente", "Categoría"),
        main = "Tiempo", col = "lightblue", ylab = "Suma de cuadrados",
        ylim = c(0, max(anova_tiempo$`Sum Sq`[1:3]) * 1.2))
barplot(anova_coste$`Sum Sq`[1:3], names.arg = c("Dispositivo", "Cliente", "Categoría"),
        main = "Coste", col = "orange", ylab = "Suma de cuadrados",
        ylim = c(0, max(anova_coste$`Sum Sq`[1:3]) * 1.2))
par(mfrow = c(1, 1))

# --- 5.3 Objetivos de eficiencia por tipo de dispositivo ---
# Situación actual = mediana (robusta ante días malos).
# Objetivo = primer cuartil, es decir, lo que ya consigue el 25 % más eficiente.
tabla_objetivos <- datos %>%
  group_by(Tipo_Dispositivo) %>%
  summarise(
    Tiempo_Actual_Mediana  = median(Tiempo_Horas, na.rm = TRUE),
    Tiempo_Objetivo_Q1     = quantile(Tiempo_Horas, 0.25, na.rm = TRUE),
    Coste_Actual_Mediana   = median(Coste_Total, na.rm = TRUE),
    Coste_Objetivo_Q1      = quantile(Coste_Total, 0.25, na.rm = TRUE),
    Volumen_Reparaciones   = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Ahorro_Tiempo_Potencial = Tiempo_Actual_Mediana - Tiempo_Objetivo_Q1,
    Ahorro_Coste_Potencial  = Coste_Actual_Mediana - Coste_Objetivo_Q1
  )
print(tabla_objetivos)
write.csv2(tabla_objetivos, "resultados/tabla_objetivos.csv",
           row.names = FALSE, fileEncoding = "UTF-8")

# Gráficos de situación actual frente a objetivo (formato largo con rbind)
grafico_brecha <- function(col_actual, col_objetivo, unidad, color, titulo) {
  largo <- rbind(
    data.frame(Tipo = tabla_objetivos$Tipo_Dispositivo,
               Valor = tabla_objetivos[[col_actual]], Metrica = "Actual (mediana)"),
    data.frame(Tipo = tabla_objetivos$Tipo_Dispositivo,
               Valor = tabla_objetivos[[col_objetivo]], Metrica = "Objetivo (Q1)")
  )
  ggplot(largo, aes(x = reorder(Tipo, Valor), y = Valor, fill = Metrica)) +
    geom_col(position = "dodge") +   # barras una al lado de la otra
    coord_flip() +
    scale_fill_manual(values = c("gray", color)) +
    labs(title = titulo, subtitle = "Brecha de eficiencia por tipo de equipo",
         x = "", y = unidad, fill = "") +
    theme_minimal()
}
print(grafico_brecha("Tiempo_Actual_Mediana", "Tiempo_Objetivo_Q1", "Horas",
                     "lightblue", "Tiempo: situación actual frente a objetivo"))
print(grafico_brecha("Coste_Actual_Mediana", "Coste_Objetivo_Q1", "Coste total (€)",
                     "orange", "Coste: situación actual frente a objetivo"))

# --- 5.4 Simulación de ahorro según el grado de mejora ---
datos_simulacion <- datos %>%
  left_join(tabla_objetivos %>%
              select(Tipo_Dispositivo, Tiempo_Objetivo_Q1, Coste_Objetivo_Q1),
            by = "Tipo_Dispositivo")

# Si se recupera un porcentaje (tasa_mejora) del exceso sobre el objetivo,
# ¿cuántas horas y euros se ahorran en total?
calcular_ahorro <- function(tasa_mejora) {
  datos_simulacion %>%
    mutate(
      Exceso_Tiempo = pmax(0, Tiempo_Horas - Tiempo_Objetivo_Q1),
      Exceso_Coste  = pmax(0, Coste_Total - Coste_Objetivo_Q1)
    ) %>%
    summarise(
      Tasa_Mejora        = tasa_mejora,
      Ahorro_Total_Horas = sum(Exceso_Tiempo * tasa_mejora, na.rm = TRUE),
      Ahorro_Total_Euros = sum(Exceso_Coste * tasa_mejora, na.rm = TRUE)
    )
}
tabla_simulacion <- bind_rows(lapply(seq(0, 1, by = 0.1), calcular_ahorro))
print(tabla_simulacion)

print(
  ggplot(tabla_simulacion, aes(x = Tasa_Mejora * 100, y = Ahorro_Total_Euros)) +
    geom_area(alpha = 0.2, fill = "orange") +
    geom_line(colour = "orange", linewidth = 1.5) +
    geom_point(colour = "orange", size = 3) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    labs(title = "Ahorro potencial acumulado",
         subtitle = "Según el porcentaje del exceso sobre el objetivo que se consiga reducir",
         x = "Grado de mejora (%)", y = "Ahorro total estimado (€)") +
    theme_minimal()
)

# --- 5.5 Análisis de sensibilidad ---
# ¿Qué pesa más en el presupuesto: una subida del 20 % en material o en mano de obra?
coste_base <- sum(datos$Coste_Total, na.rm = TRUE)
impacto_material <- sum(datos$Coste_Servicio + 1.20 * datos$Coste_Material) - coste_base
impacto_servicio <- sum(1.20 * datos$Coste_Servicio + datos$Coste_Material) - coste_base

datos_impacto <- data.frame(
  Factor        = c("Material +20 %", "Mano de obra +20 %"),
  Impacto_Euros = c(impacto_material, impacto_servicio)
)
print(datos_impacto)

print(
  ggplot(datos_impacto, aes(x = reorder(Factor, Impacto_Euros), y = Impacto_Euros, fill = Factor)) +
    geom_col(width = 0.5) +
    coord_flip() +
    scale_fill_manual(values = c("#E67E22", "#8E44AD")) +
    labs(title = "Análisis de sensibilidad",
         subtitle = "Aumento del coste total ante una subida del 20 %",
         x = "", y = "Incremento del coste total (€)") +
    theme_minimal() +
    theme(legend.position = "none")
)

# Interpretación
factores <- c("el tipo de dispositivo", "el tipo de cliente", "la categoría de trabajo")
significativos <- function(tabla_anova) {
  sig <- factores[tabla_anova$`Pr(>F)`[1:3] < 0.05]
  if (length(sig) == 0) "ninguno de los factores estudiados" else paste(sig, collapse = ", ")
}
i_max <- which.max(tabla_objetivos$Ahorro_Coste_Potencial * tabla_objetivos$Volumen_Reparaciones)
fila_max <- tabla_objetivos[i_max, ]
fila_50 <- tabla_simulacion[which.min(abs(tabla_simulacion$Tasa_Mejora - 0.5)), ]

escribir("## 5. Dónde se puede mejorar\n")
escribir("- Según el ANOVA (p < 0,05), en el tiempo de reparación influye ", significativos(anova_tiempo),
         ", y en el coste influye ", significativos(anova_coste), ".")
escribir("- El tipo de equipo con más margen de ahorro es **", as.character(fila_max$Tipo_Dispositivo),
         "**: su coste mediano es de ", num(fila_max$Coste_Actual_Mediana, 0), " € frente a un objetivo de ",
         num(fila_max$Coste_Objetivo_Q1, 0), " € (lo que ya consigue el 25 % más eficiente), y suma ",
         fila_max$Volumen_Reparaciones, " reparaciones.")
escribir("- Si se redujera a la mitad el exceso sobre el objetivo en cada reparación, se ahorrarían unas ",
         num(fila_50$Ahorro_Total_Horas, 0), " horas y ", num(fila_50$Ahorro_Total_Euros, 0),
         " € en el periodo analizado (", porc(fila_50$Ahorro_Total_Euros / coste_base),
         " de la facturación).")
escribir("- Una subida del 20 % en la mano de obra aumentaría el coste total en ", num(impacto_servicio, 0),
         " €, frente a ", num(impacto_material, 0), " € si la subida fuera en el material. ",
         if (impacto_servicio > impacto_material)
           paste("El coste depende más del tiempo de los técnicos que de las piezas, así que mejorar",
                 "la eficiencia del taller es la palanca principal.")
         else paste("El coste depende más de las piezas que del tiempo de los técnicos, así que",
                    "negociar con proveedores es la palanca principal."))


# --- Cierre ---
con <- file("resultados/informe.md", encoding = "UTF-8")
writeLines(informe, con)
close(con)

if (!interactive()) {
  dev.off()
  cat("Gráficos guardados en resultados/graficos.pdf\n")
}
cat("Informe guardado en resultados/informe.md\n")
cat("Análisis terminado. Tablas en la carpeta resultados/\n")
