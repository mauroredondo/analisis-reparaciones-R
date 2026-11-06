# Análisis estadístico de un servicio técnico de reparaciones (R)

Durante mis prácticas (450 h) en una empresa de servicios informáticos analicé su histórico de reparaciones para responder a cuatro preguntas de negocio:

1. ¿Cuánto tarda y cuánto cuesta una reparación típica, y cómo se distribuyen esos valores?
2. ¿Qué factores explican que una reparación sea más larga o más cara?
3. ¿Cuánto duran los discos que llegan al taller y qué marcas fallan antes?
4. ¿Cuánto se podría ahorrar si los trabajos menos eficientes se acercaran a los mejores?

Este repositorio contiene el análisis completo en R, reorganizado y documentado para que cualquiera pueda ejecutarlo.

> **Sobre los datos.** Los datos reales de la empresa son confidenciales y no están aquí. El script `R/01_generar_datos.R` crea datos **simulados** con la misma estructura y un comportamiento parecido: distribuciones asimétricas, reparaciones atípicas muy largas o caras, diferencias entre tipos de equipo y fallos de disco tipo Weibull con censura. Por eso las cifras que produce este repositorio son ilustrativas y no corresponden a la empresa.

---

## Contenido

```
R/
  00_instalar_paquetes.R   instala los paquetes que falten (solo la primera vez)
  01_generar_datos.R       crea los datos simulados en datos/
  02_analisis.R            el análisis completo, en cinco bloques
datos/
  reparaciones_simuladas.csv
  fiabilidad_simulada.csv
resultados/                se crea al ejecutar el análisis (no se sube a GitHub)
  informe.md               interpretación de los resultados, en texto
  graficos.pdf             todos los gráficos
  tabla_objetivos.csv      objetivos de eficiencia por tipo de equipo
```

---

## Qué hace el análisis

### 1. Estadística descriptiva

Calcula media, mediana, moda, media recortada, varianza, desviación típica, coeficiente de variación y cuartiles de las horas y los costes. R no trae funciones para la moda ni para el coeficiente de variación, así que están implementadas a mano.

La comparación entre media y mediana ya muestra lo importante: las distribuciones tienen una cola larga a la derecha. Unas pocas reparaciones muy caras o muy largas tiran de la media hacia arriba, por lo que la mediana describe mejor una reparación típica.

### 2. Ajuste de distribuciones y outliers

Al ser datos asimétricos, la normal queda descartada. Para elegir candidatas se usa el gráfico de Cullen y Frey (asimetría frente a curtosis) con bootstrap. Después se ajustan por máxima verosimilitud una log-normal, una Weibull y una gamma, y en una segunda ronda dos distribuciones de cola pesada, Pareto y Burr.

Cada ajuste se compara de dos formas: visualmente, con cuatro gráficos (densidad, función de distribución, QQ-plot y PP-plot), y numéricamente, con el AIC.

Los outliers se detectan con el criterio del boxplot: es atípico todo lo que supera Q3 + 1,5·IQR.

### 3. Correlación y regresión

Se calculan las correlaciones de Pearson y de Spearman entre horas y costes. Con estas distribuciones Spearman es la adecuada, porque trabaja con rangos y no se deja arrastrar por los valores extremos.

Después se ajusta una regresión lineal del coste total en función de las horas y del coste del material. El coste de mano de obra se deja fuera porque es casi proporcional a las horas (horas por tarifa) y meterlo introduciría colinealidad. El script muestra los coeficientes, el R², los intervalos de confianza y un gráfico de coste real frente a predicho.

### 4. Fiabilidad de discos (análisis de supervivencia)

En el taller se hace un diagnóstico S.M.A.R.T. a los discos de los equipos que llegan. Muchos discos siguen funcionando al revisarlos: de ellos solo sabemos que han aguantado *al menos* esas horas. Son datos censurados, y por eso hace falta análisis de supervivencia en lugar de una media normal.

- **Kaplan-Meier por fabricante**, con el test log-rank para comprobar si las diferencias entre marcas son significativas.
- **Modelos paramétricos** (Weibull, exponencial y log-normal), comparados con el AIC y superpuestos a la curva de Kaplan-Meier.
- **Indicadores del modelo Weibull**: parámetro de forma β (si es mayor que 1, el riesgo de fallo crece con el uso), vida característica η, tiempo medio hasta el fallo (MTTF) y vida B10 (horas a las que ha fallado el 10 % de los discos).
- **Funciones de fiabilidad**: tasa de fallo h(t), supervivencia S(t) y probabilidad acumulada de fallo F(t), más una tabla para 1, 2, 3, 5, 7 y 10 años de uso (suponiendo 8 horas al día).

### 5. Optimización de procesos

- **Clasificación de las intervenciones.** El texto libre de cada reparación se clasifica por palabras clave en Hardware, Software o Mantenimiento.
- **ANOVA.** Mide cuánto influyen el tipo de dispositivo, el tipo de cliente y la categoría de trabajo en las horas y en el coste.
- **Objetivos de eficiencia.** Para cada tipo de equipo se compara la situación actual (mediana) con un objetivo realista: el primer cuartil, es decir, lo que ya consigue el 25 % de reparaciones más eficientes.
- **Simulación de ahorro.** Calcula cuántas horas y euros se ahorrarían si se redujera un 10 %, un 20 %… hasta un 100 % del exceso sobre ese objetivo.
- **Análisis de sensibilidad.** Compara qué afecta más al coste total: una subida del 20 % en el material o en la mano de obra.

---

## Diccionario de datos

El análisis está pensado para estos dos archivos. Ambos usan `;` como separador y `,` como separador decimal (formato europeo). Si falta alguna columna obligatoria o una numérica no se lee como número, `02_analisis.R` se para y dice cuál.

### `reparaciones_simuladas.csv`: una fila por reparación

| Columna | Tipo | Obligatoria | Descripción |
|---|---|---|---|
| `ID_Incidencia` | entero | sí | Identificador de la reparación |
| `Fecha` | fecha (AAAA-MM-DD) | no | Día de entrada del equipo |
| `Cliente_Tipo` | texto | sí | Empresa, Pyme, Autonomo, Particular o Institucion |
| `Tipo_Dispositivo` | texto | sí | Portátil, Sobremesa, Infraestructura/Servidor, Redes/Conectividad, Impresora/Periférico u Otros Dispositivos |
| `Marca` | texto | no | Fabricante del equipo |
| `Equipo` | texto | sí | Modelo o descripción del equipo |
| `Tipo_Intervencion` | texto | sí | Descripción libre del trabajo (por ejemplo, "Cambio de pantalla"). De aquí se deduce la categoría |
| `Tiempo_Horas` | número | sí | Horas de trabajo del técnico |
| `Coste_Material` | número (€) | sí | Coste de las piezas (0 si no se usaron) |
| `Coste_Servicio` | número (€) | sí | Coste de la mano de obra |
| `Facturable` | SI / NO | no | Si se cobró al cliente |

El script calcula `Coste_Total = Coste_Material + Coste_Servicio`.

### `fiabilidad_simulada.csv`: una fila por disco diagnosticado

| Columna | Tipo | Obligatoria | Descripción |
|---|---|---|---|
| `ID_Disco` | entero | no | Identificador del disco |
| `Marca` | texto | sí | Fabricante (WD, Samsung, Kingston…) |
| `Modelo` | texto | no | Modelo del disco |
| `Capacidad` | texto | no | Capacidad (500GB, 1TB…) |
| `Horas_Uso` | número | sí | Horas de encendido acumuladas según S.M.A.R.T. |
| `Estado_Smart` | texto | no | OK, Warning o Critical |
| `Fallo` | SI / NO | sí | SI si el disco ha fallado; NO si sigue funcionando (dato censurado) |

---

## Cómo ejecutarlo

Necesitas [R](https://cran.r-project.org/) 4.1 o superior. Abre una terminal en la carpeta del proyecto y ejecuta:

```bash
Rscript R/00_instalar_paquetes.R   # solo la primera vez
Rscript R/01_generar_datos.R       # crea los CSV en datos/
Rscript R/02_analisis.R            # genera resultados/
```

Además de los gráficos y las tablas, el script escribe una interpretación de cada bloque: qué distribución ajusta mejor, qué marcas de disco son más fiables, dónde está el mayor margen de ahorro… Ese texto sale en la consola y se guarda en `resultados/informe.md`. Se calcula a partir de los resultados, así que cambia si cambian los datos.

**Ver el informe con formato:**

- **VS Code:** si abres la carpeta del proyecto, `informe.md` se abre directamente en vista previa (lo configura `.vscode/settings.json`). Si no, ábrelo y pulsa **Cmd+Shift+V** en macOS o **Ctrl+Shift+V** en Windows y Linux. Para verlo al lado del código: **Cmd+K** y luego **V** (macOS) o **Ctrl+K** y luego **V** (Windows y Linux).
- **RStudio:** abre el archivo y pulsa **Visual**, en la barra de arriba del editor.
- **Cualquier editor de texto:** se lee igual, solo que sin formato.

Con `Rscript` los gráficos se guardan en `resultados/graficos.pdf`. Si abres `02_analisis.R` en VS Code (con la extensión de R) o en RStudio y lo ejecutas línea a línea, los gráficos salen en pantalla.

El generador usa una semilla fija, así que siempre produce los mismos datos. Para probar otro escenario, cambia `set.seed(2025)` o los parámetros del principio de `01_generar_datos.R`.

---

## Limitaciones

- Los datos son simulados. El análisis sirve para mostrar el método, no para sacar conclusiones sobre una empresa real.
- El ANOVA es secuencial (tipo I), así que el orden de los factores en el modelo influye en el resultado. Con datos desequilibrados convendría un ANOVA de tipo II.
- La clasificación de intervenciones por palabras clave depende de cómo estén redactadas. Con textos muy distintos habría que revisar las listas de palabras.
- El análisis está hecho para el caso concreto de un servicio técnico. No es una herramienta genérica para cualquier CSV.

---

## Herramientas

R · readr · dplyr · ggplot2 · fitdistrplus · actuar · survival · survminer · corrplot · GGally · scatterplot3d

## Autor

**Mauro Redondo Beltrán** · Grado en Matemáticas (Universidad de Salamanca) · Máster en Inteligencia Artificial (La Salle - URL)
