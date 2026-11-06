# =============================================================================
# 01_generar_datos.R
# Genera los dos conjuntos de datos SIMULADOS que usa 02_analisis.R:
#   - datos/reparaciones_simuladas.csv : registro de reparaciones de un servicio
#     técnico informático (fecha, equipo, avería, horas y costes).
#   - datos/fiabilidad_simulada.csv    : diagnósticos S.M.A.R.T. de discos
#     (horas de uso y si han fallado), para el análisis de supervivencia.
#
# Los datos originales de las prácticas son confidenciales. Este script crea
# datos con la misma estructura y un comportamiento parecido (colas largas,
# diferencias entre tipos de equipo, fallos tipo Weibull), pero todos los
# valores y parámetros son inventados.
#
# Solo usa R base, así que no necesita instalar ningún paquete.
# Ejecútalo desde la carpeta raíz del proyecto:  Rscript R/01_generar_datos.R
# =============================================================================

set.seed(2025)  # misma semilla = mismos datos cada vez
dir.create("datos", showWarnings = FALSE)


# -----------------------------------------------------------------------------
# 1. REGISTRO DE REPARACIONES
# -----------------------------------------------------------------------------
n_rep <- 400

# Tipos de dispositivo: probabilidad de aparecer y "multiplicador" de horas
# (un servidor tarda más en repararse que un periférico).
dispositivos <- data.frame(
  tipo        = c("Portátil", "Sobremesa", "Infraestructura/Servidor",
                  "Redes/Conectividad", "Impresora/Periférico", "Otros Dispositivos"),
  prob        = c(0.30, 0.18, 0.12, 0.14, 0.10, 0.16),
  mult_tiempo = c(1.0, 1.2, 1.8, 1.1, 0.7, 0.8),
  stringsAsFactors = FALSE
)

# Catálogo de equipos y marcas por tipo de dispositivo
catalogo <- list(
  "Portátil" = data.frame(
    marca  = c("Lenovo", "HP", "Asus", "Apple", "Dell"),
    equipo = c("Portátil Lenovo ThinkPad", "Portátil HP ProBook", "Portátil Asus VivoBook",
               "MacBook Air", "Portátil Dell Latitude")),
  "Sobremesa" = data.frame(
    marca  = c("HP", "Dell", "Genérico", "Apple"),
    equipo = c("Sobremesa HP EliteDesk", "Sobremesa Dell OptiPlex", "Sobremesa clónico", "iMac")),
  "Infraestructura/Servidor" = data.frame(
    marca  = c("HP", "Dell", "Synology"),
    equipo = c("Servidor HP ProLiant", "Servidor Dell PowerEdge", "NAS Synology")),
  "Redes/Conectividad" = data.frame(
    marca  = c("Genérico", "TP-Link", "Ubiquiti"),
    equipo = c("Router de oficina", "Switch gestionable", "Punto de acceso wifi")),
  "Impresora/Periférico" = data.frame(
    marca  = c("HP", "Brother", "Genérico"),
    equipo = c("Impresora láser HP", "Impresora multifunción Brother", "Impresora de tickets")),
  "Otros Dispositivos" = data.frame(
    marca  = c("Genérico", "Samsung", "Apple"),
    equipo = c("TPV de comercio", "Tablet Samsung Galaxy Tab", "iPad"))
)

# Categorías de trabajo: probabilidad, probabilidad de usar material y
# coste típico (mediana) del material cuando lo hay.
categorias <- data.frame(
  categoria       = c("Hardware/Piezas", "Software/Config", "Mantenimiento"),
  prob            = c(0.55, 0.30, 0.15),
  prob_material   = c(0.80, 0.25, 0.35),
  mediana_mat     = c(90, 60, 15),
  stringsAsFactors = FALSE
)

# Intervenciones posibles según el tipo de dispositivo y la categoría, para que
# cada equipo solo tenga averías que le correspondan (un router no lleva SSD).
# Los nombres están elegidos para que la clasificación por palabras clave de
# 02_analisis.R los asigne a la categoría correcta.
intervenciones <- list(
  "Portátil" = list(
    "Hardware/Piezas" = c("Cambio de pantalla", "Sustitución de disco SSD", "Cambio de batería",
                          "Ampliación de RAM", "Cambio de teclado", "Reparación de bisagra",
                          "Sustitución de cargador", "Cambio de ventilador"),
    "Software/Config" = c("Eliminación de virus", "Instalación de Office", "Actualización de Windows",
                          "Recuperación de datos", "Configuración VPN",
                          "Formateo e instalación de sistema"),
    "Mantenimiento"   = c("Limpieza interna y pasta térmica", "Puesta a punto", "Limpieza de polvo")),
  "Sobremesa" = list(
    "Hardware/Piezas" = c("Sustitución de disco SSD", "Ampliación de RAM",
                          "Sustitución de fuente de alimentación", "Sustitución de placa base",
                          "Cambio de ventilador", "Cambio de tarjeta gráfica"),
    "Software/Config" = c("Eliminación de virus", "Instalación de Office", "Actualización de Windows",
                          "Recuperación de datos", "Formateo e instalación de sistema"),
    "Mantenimiento"   = c("Limpieza interna y pasta térmica", "Mantenimiento preventivo",
                          "Limpieza de polvo", "Test de temperatura")),
  "Infraestructura/Servidor" = list(
    "Hardware/Piezas" = c("Sustitución de disco del RAID", "Sustitución de fuente de alimentación",
                          "Ampliación de RAM", "Cambio de ventilador"),
    "Software/Config" = c("Configuración de copias de seguridad", "Actualización de firmware",
                          "Renovación de certificado digital", "Configuración de usuarios del dominio"),
    "Mantenimiento"   = c("Mantenimiento preventivo", "Revisión general del equipo",
                          "Test de temperatura")),
  "Redes/Conectividad" = list(
    "Hardware/Piezas" = c("Sustitución de fuente de alimentación", "Cambio de antena",
                          "Sustitución de puerto dañado", "Sustitución de cableado"),
    "Software/Config" = c("Configuración VPN", "Configuración Wifi", "Actualización de firmware",
                          "Configuración de VLAN"),
    "Mantenimiento"   = c("Mantenimiento preventivo", "Revisión general del equipo")),
  "Impresora/Periférico" = list(
    "Hardware/Piezas" = c("Atasco de papel en impresora", "Cambio de tambor de impresora",
                          "Sustitución de fusor", "Cambio de rodillo"),
    "Software/Config" = c("Instalación de driver", "Configuración de escaneo a correo"),
    "Mantenimiento"   = c("Limpieza de cabezales", "Mantenimiento preventivo")),
  "Otros Dispositivos" = list(
    "Hardware/Piezas" = c("Cambio de pantalla táctil", "Sustitución de cargador",
                          "Sustitución de conector de carga", "Cambio de batería"),
    "Software/Config" = c("Configuración de correo", "Actualización de software",
                          "Recuperación de datos", "Configuración Wifi"),
    "Mantenimiento"   = c("Revisión general del equipo", "Limpieza de polvo"))
)

clientes <- c("Empresa", "Pyme", "Autonomo", "Particular", "Institucion")
prob_clientes <- c(0.27, 0.24, 0.17, 0.19, 0.13)

# --- Variables cualitativas ---
fechas <- sort(sample(seq(as.Date("2025-01-07"), as.Date("2025-12-19"), by = "day"),
                      n_rep, replace = TRUE))
tipo   <- sample(dispositivos$tipo, n_rep, replace = TRUE, prob = dispositivos$prob)
cliente <- sample(clientes, n_rep, replace = TRUE, prob = prob_clientes)
categoria <- sample(categorias$categoria, n_rep, replace = TRUE, prob = categorias$prob)

marca  <- character(n_rep)
equipo <- character(n_rep)
intervencion <- character(n_rep)
for (i in seq_len(n_rep)) {
  cat_i <- catalogo[[tipo[i]]]
  fila  <- sample(nrow(cat_i), 1)
  marca[i]  <- cat_i$marca[fila]
  equipo[i] <- cat_i$equipo[fila]
  opciones <- intervenciones[[tipo[i]]][[categoria[i]]]
  intervencion[i] <- opciones[sample(length(opciones), 1)]
}

# --- Tiempo de reparación (horas) ---
# Log-normal (asimétrica, con cola a la derecha) y un 3 % de casos complicados
# que tardan mucho más, para que haya outliers como en la realidad.
mult <- dispositivos$mult_tiempo[match(tipo, dispositivos$tipo)]
tiempo <- rlnorm(n_rep, meanlog = log(1.3 * mult), sdlog = 0.55)
complicados <- runif(n_rep) < 0.03
tiempo[complicados] <- tiempo[complicados] * runif(sum(complicados), 5, 15)
tiempo <- pmax(0.3, round(tiempo, 1))

# --- Costes (euros) ---
# Mano de obra: horas x tarifa por hora (la tarifa varía un poco por trabajo).
tarifa <- sample(c(35, 40, 45), n_rep, replace = TRUE, prob = c(0.2, 0.6, 0.2))
coste_servicio <- round(tiempo * tarifa)

# Material: muchas reparaciones no usan piezas (coste 0); cuando las usan,
# el coste es log-normal con cola larga (alguna pieza de servidor muy cara).
idx_cat <- match(categoria, categorias$categoria)
usa_material <- runif(n_rep) < categorias$prob_material[idx_cat]
coste_material <- ifelse(usa_material,
                         rlnorm(n_rep, meanlog = log(categorias$mediana_mat[idx_cat]), sdlog = 0.9),
                         0)
coste_material <- round(coste_material / 5) * 5  # redondeo a 5 euros

reparaciones <- data.frame(
  ID_Incidencia     = 1001:(1000 + n_rep),
  Fecha             = fechas,
  Cliente_Tipo      = cliente,
  Tipo_Dispositivo  = tipo,
  Marca             = marca,
  Equipo            = equipo,
  Tipo_Intervencion = intervencion,
  Tiempo_Horas      = tiempo,
  Coste_Material    = coste_material,
  Coste_Servicio    = coste_servicio,
  Facturable        = sample(c("SI", "NO"), n_rep, replace = TRUE, prob = c(0.9, 0.1))
)
# Nota: no se guarda la categoría de trabajo. El análisis la reconstruye a
# partir del texto de Tipo_Intervencion, igual que en el proyecto original.

# write.csv2 usa ";" como separador y "," como decimal (formato europeo)
write.csv2(reparaciones, "datos/reparaciones_simuladas.csv",
           row.names = FALSE, fileEncoding = "UTF-8")


# -----------------------------------------------------------------------------
# 2. DIAGNÓSTICOS S.M.A.R.T. DE DISCOS (FIABILIDAD)
# -----------------------------------------------------------------------------
n_disc <- 250

# Cada marca tiene una vida característica distinta (parámetro de escala
# Weibull, en horas). La forma común beta = 1.5 > 1 significa desgaste: la
# probabilidad de fallo aumenta con el uso.
marcas_disco <- data.frame(
  marca = c("WD", "Samsung", "Kingston", "Crucial", "Seagate", "Toshiba"),
  prob  = c(0.27, 0.22, 0.17, 0.15, 0.12, 0.07),
  eta   = c(30000, 42000, 17000, 24000, 21000, 27000),
  stringsAsFactors = FALSE
)
modelos_disco <- list(
  WD       = c("Blue SN570", "Black SN850", "Green SN350", "Red SA500"),
  Samsung  = c("970 EVO Plus", "980 Pro", "990 Pro"),
  Kingston = c("A400", "NV2", "KC3000"),
  Crucial  = c("P3", "P5 Plus", "MX500"),
  Seagate  = c("BarraCuda Q5", "FireCuda 530"),
  Toshiba  = c("Kioxia Exceria")
)
capacidades <- c("240GB", "500GB", "1TB", "2TB", "4TB")

marca_d <- sample(marcas_disco$marca, n_disc, replace = TRUE, prob = marcas_disco$prob)
modelo_d <- vapply(marca_d, function(m) sample(modelos_disco[[m]], 1), character(1))
capacidad_d <- sample(capacidades, n_disc, replace = TRUE, prob = c(0.1, 0.3, 0.35, 0.2, 0.05))

# Tiempo hasta el fallo: Weibull según la marca, más un 6 % de fallos
# prematuros (defectos de fábrica en las primeras horas).
eta_d <- marcas_disco$eta[match(marca_d, marcas_disco$marca)]
t_fallo <- rweibull(n_disc, shape = 1.5, scale = eta_d)
prematuro <- runif(n_disc) < 0.06
t_fallo[prematuro] <- runif(sum(prematuro), 50, 1500)

# Censura: el disco llega al taller tras un tiempo de uso aleatorio. Si aún no
# ha fallado, solo sabemos que ha sobrevivido hasta ese momento (dato censurado).
t_revision <- runif(n_disc, 200, 33000)
horas_uso <- round(pmin(t_fallo, t_revision))
fallo <- ifelse(t_fallo <= t_revision, "SI", "NO")

# Estado S.M.A.R.T.: muy relacionado con el fallo, pero no perfecto.
estado <- ifelse(
  fallo == "SI",
  sample(c("Critical", "Warning", "OK"), n_disc, replace = TRUE, prob = c(0.75, 0.18, 0.07)),
  sample(c("OK", "Warning"), n_disc, replace = TRUE, prob = c(0.82, 0.18))
)

fiabilidad <- data.frame(
  ID_Disco     = seq_len(n_disc),
  Marca        = marca_d,
  Modelo       = unname(modelo_d),
  Capacidad    = capacidad_d,
  Horas_Uso    = horas_uso,
  Estado_Smart = estado,
  Fallo        = fallo
)
write.csv2(fiabilidad, "datos/fiabilidad_simulada.csv",
           row.names = FALSE, fileEncoding = "UTF-8")


cat("Datos generados en la carpeta 'datos/':\n")
cat(" - reparaciones_simuladas.csv:", nrow(reparaciones), "reparaciones\n")
cat(" - fiabilidad_simulada.csv:   ", nrow(fiabilidad), "discos,",
    sum(fiabilidad$Fallo == "SI"), "con fallo\n")
