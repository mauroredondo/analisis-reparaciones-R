# Statistical analysis of an IT repair service (R)

🇬🇧 English | 🇪🇸 [Español](README.es.md)

During my 450-hour internship at an IT services company, I analysed its repair history to answer four business questions:

1. How long does a typical repair take, how much does it cost, and how are those values distributed?
2. Which factors make a repair longer or more expensive?
3. How long do the disks that come into the workshop last, and which brands fail first?
4. How much could be saved if the least efficient jobs got closer to the best ones?

This repository contains the full analysis in R, reorganised and documented so that anyone can run it.

> **About the data.** The company's real data is confidential and is not included. The script `R/01_generar_datos.R` generates **simulated** data with the same structure and similar behaviour: skewed distributions, occasional very long or expensive repairs, differences between device types, and Weibull-type disk failures with censoring, plus a small share of premature failures (factory defects). The figures this repository produces are therefore illustrative and do not describe the company.

---

## Contents

```
R/
  00_instalar_paquetes.R   installs any missing packages (first run only)
  01_generar_datos.R       creates the simulated data in datos/
  02_analisis.R            the full analysis, in five blocks
datos/
  reparaciones_simuladas.csv
  fiabilidad_simulada.csv
resultados/                created when the analysis runs (not uploaded to GitHub)
  informe.md               written interpretation of the results (English)
  informe.es.md            written interpretation of the results (Spanish)
  graficos.pdf             all charts
  tabla_objetivos.csv      efficiency targets by device type
  tabla_weibull.csv        Weibull reliability at 1, 2, 3, 5, 7 and 10 years
  tabla_fiabilidad_marcas.csv   characteristic life, MTTF and B10 by brand
```

The code, comments and chart titles are in Spanish; the data dictionary below explains every column.

---

## What the analysis does

### 1. Descriptive statistics

Computes the mean, median, mode, trimmed mean, variance, standard deviation, coefficient of variation and quartiles of repair hours and costs. Base R has no functions for the mode or the coefficient of variation, so both are implemented by hand.

Comparing the mean with the median already shows the key point: the distributions have a long right tail. A few very expensive or very long repairs pull the mean up, so the median is a better description of a typical repair.

### 2. Distribution fitting and outliers

Because the data is skewed, the normal distribution is ruled out. Candidate distributions are chosen with a Cullen and Frey graph (skewness against kurtosis) with bootstrap. Log-normal, Weibull and gamma distributions are then fitted by maximum likelihood, followed by a second round with two heavy-tailed distributions, Pareto and Burr.

Each fit is compared in two ways: visually, with four plots (density, CDF, QQ-plot and PP-plot), and numerically, with the AIC.

Outliers are detected with the boxplot rule: anything above Q3 + 1.5·IQR.

### 3. Correlation and regression

Pearson and Spearman correlations are computed between hours and costs. With these distributions Spearman is the right choice, because it works with ranks and is not dragged by extreme values.

A linear regression then models total cost as a function of hours and parts cost. Labour cost is left out because it is almost proportional to hours (hours times rate), and including it would introduce collinearity. The script reports the coefficients, R², confidence intervals and a plot of actual against predicted cost.

### 4. Disk reliability (survival analysis)

The workshop runs a S.M.A.R.T. diagnostic on the disks of every device it receives. Many disks are still working at that point: all we know is that they have lasted *at least* that many hours. These are censored observations, which is why survival analysis is needed instead of a simple average.

- **Kaplan-Meier curves by manufacturer**, with a log-rank test to check whether the differences between brands are significant.
- **Parametric models** (Weibull, exponential and log-normal), compared by AIC and overlaid on the Kaplan-Meier curve.
- **Weibull by brand**: a model with a different characteristic life for each manufacturer, compared by AIC with the single Weibull. It produces a table of η, MTTF and B10 per brand.
- **Weibull indicators**: shape parameter β (clearly above 1 means the failure risk grows with use; close to 1 means it is roughly constant), characteristic life η, mean time to failure (MTTF) and B10 life (hours by which 10% of disks have failed).
- **Reliability functions**: hazard rate h(t), survival S(t) and cumulative failure probability F(t), plus a table for 1, 2, 3, 5, 7 and 10 years of use (assuming 8 hours a day).

### 5. Process optimisation

- **Job classification.** The free-text description of each repair is classified by keywords into Hardware, Software or Maintenance.
- **ANOVA.** Measures how much device type, customer type and job category influence hours and cost. It is run on log-transformed values: on the raw values, a few extreme repairs inflate the residual variance and hide real differences (for example, that servers take longer).
- **Efficiency targets.** For each device type, the current situation (median) is compared with a realistic target: the first quartile, i.e. what the most efficient 25% of repairs already achieve.
- **Savings simulation.** Calculates how many hours and euros would be saved by cutting 10%, 20%… up to 100% of the excess over that target.
- **Sensitivity analysis.** Compares which has a bigger impact on total cost: a 20% rise in parts or in labour.

---

## Data dictionary

The analysis expects these two files. Both use `;` as the field separator and `,` as the decimal mark (European format). If a required column is missing or a numeric column is not read as a number, `02_analisis.R` stops and says which one.

### `reparaciones_simuladas.csv`: one row per repair

| Column | Type | Required | Description |
|---|---|---|---|
| `ID_Incidencia` | integer | yes | Repair ID |
| `Fecha` | date (YYYY-MM-DD) | no | Date the device came in |
| `Cliente_Tipo` | text | yes | Customer type: Empresa (company), Pyme (SME), Autonomo (self-employed), Particular (individual) or Institucion (public body) |
| `Tipo_Dispositivo` | text | yes | Device type: Portátil (laptop), Sobremesa (desktop), Infraestructura/Servidor (server), Redes/Conectividad (networking), Impresora/Periférico (printer) or Otros Dispositivos (other) |
| `Marca` | text | no | Device manufacturer |
| `Equipo` | text | yes | Device model or description |
| `Tipo_Intervencion` | text | yes | Free-text description of the job (e.g. "Cambio de pantalla", screen replacement). The job category is derived from it |
| `Tiempo_Horas` | number | yes | Technician hours |
| `Coste_Material` | number (€) | yes | Cost of parts (0 if none were used) |
| `Coste_Servicio` | number (€) | yes | Labour cost |
| `Facturable` | SI / NO | no | Whether the customer was charged |

The script computes `Coste_Total = Coste_Material + Coste_Servicio`.

### `fiabilidad_simulada.csv`: one row per diagnosed disk

| Column | Type | Required | Description |
|---|---|---|---|
| `ID_Disco` | integer | no | Disk ID |
| `Marca` | text | yes | Manufacturer (WD, Samsung, Kingston…) |
| `Modelo` | text | no | Disk model |
| `Capacidad` | text | no | Capacity (500GB, 1TB…) |
| `Horas_Uso` | number | yes | Accumulated power-on hours according to S.M.A.R.T. |
| `Estado_Smart` | text | no | OK, Warning or Critical |
| `Fallo` | SI / NO | yes | SI if the disk has failed; NO if it is still working (censored observation) |

---

## How to run it

You need [R](https://cran.r-project.org/) 4.1 or later. Open a terminal in the project folder and run:

```bash
Rscript R/00_instalar_paquetes.R   # first run only
Rscript R/01_generar_datos.R       # creates the CSV files in datos/
Rscript R/02_analisis.R            # generates resultados/
```

Besides the charts and tables, the script writes an interpretation of each block: which distribution fits best, which disk brands are most reliable, where the biggest savings are… The text is printed to the console in Spanish and saved in two files: `resultados/informe.md` (English) and `resultados/informe.es.md` (Spanish). It is computed from the results, so it changes if the data changes.

**Viewing the report with formatting:**

- **VS Code:** if you open the project folder, the reports open directly in preview mode (configured in `.vscode/settings.json`). Otherwise, open the file and press **Cmd+Shift+V** on macOS or **Ctrl+Shift+V** on Windows and Linux. To see it next to the code: **Cmd+K** then **V** (macOS) or **Ctrl+K** then **V** (Windows and Linux).
- **RStudio:** open the file and click **Visual** in the editor toolbar.
- **Any text editor:** it reads fine as plain text, just without formatting.

When run with `Rscript`, charts are saved to `resultados/graficos.pdf`. If you open `02_analisis.R` in VS Code (with the R extension) or RStudio and run it line by line, charts are shown on screen.

The generator uses a fixed seed, so it always produces the same data. To try a different scenario, change `set.seed(2025)` or the parameters at the top of `01_generar_datos.R`.

---

## Limitations

- The data is simulated. The analysis shows the method; it cannot be used to draw conclusions about a real company.
- The ANOVA is sequential (type I), so the order of the factors in the model affects the result. With unbalanced data a type II ANOVA would be more appropriate.
- In the simulated data, disks of each brand wear out with use (Weibull with β = 1.5), but a small share fail in the first hours. Pooled, those premature failures offset the wear and the estimated β comes out close to 1 (almost constant risk). This is a limitation of fitting a single Weibull when there are two failure mechanisms; a mixture model would separate them.
- The savings simulation is optimistic: the target is the first quartile of each device type, without separating by job category. Also, total cost is what the customer pays, so the saving is also revenue the workshop would stop earning.
- Keyword-based job classification depends on how the descriptions are written. Very different wording would require revising the keyword lists.
- The analysis is built for the specific case of a repair service. It is not a generic tool for any CSV file.

---

## Tools

R · readr · dplyr · ggplot2 · fitdistrplus · actuar · survival · survminer · corrplot · GGally · scatterplot3d

## Author

**Mauro Redondo Beltrán** · BSc in Mathematics (University of Salamanca) · MSc in Artificial Intelligence (La Salle - URL)
