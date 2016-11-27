################################################################################
# Script para generar análisis por agrupamiento en fondos de Renta Variable Mexicana
################################################################################
# descargar los paquetes de CRAN
#install.packages("cluster")
#install.packages("ifultools")
#install.packages("MASS")
#install.packages("pdc")
#install.packages("splus2R")
#install.packages("wmtsa")
#install.packages("TSclust")
# cargar paquetes a utilizar
require(TSclust)
# Configurar nombre para archivo con resultados
f_ana_time <- format(Sys.time(), "%Y%m%d%H%M%S")

# cargar archivos con series de precios e índices para comparación
df_founds <- read.csv("RVMexico.prices_normalized.csv")
df_i_bmv <- read.csv("RVMexico.market_normalized_BMV.csv")
# obtener datos de la lista de precios de fondos
l_founds <- levels(df_founds[, 1])
n_founds <- length(l_founds)

# Matriz para alamacenar precios normalizados de fondos para el análisis CORT
m_tsclust_founds <- rbind()

# Lista de fondos con número incompleto de registros
m_wrong_founds <- rbind()

# Limitar el número de lineas a imprimir en pantalla
mpl <- options()$max.print
options(max.print = 4)

### --- Cálculo de CORT --- ###
# Agregar lista de precios a matriz para análisis por agrupamiento
m_tsclust_founds <- rbind(m_tsclust_founds, df_i_bmv[, "c_price"])
# Para cada fondo en el listado...
for (i_found in 1:n_founds) {
    # Separar precios del fondo a analizar
    df_found <- df_founds[df_founds$a_found == l_founds[i_found],]
    # Verificar que la cantidad de registros de precios coincida con el índice de la BMV
    if (length(df_found[, "b_date"]) == length(df_i_bmv[, "b_date"])) {
        # Agregar lista de precios a matriz para análisis por agrupamiento
        m_tsclust_founds <- rbind(m_tsclust_founds, df_found[, "c_price"])
    } # if
    else {
        m_wrong_founds <- rbind(m_wrong_founds, i_found)
    } # else
} # for

## Eliminar fondos con número incompleto de registros
n_wrong_founds <- length(m_wrong_founds[, 1])
l_wrong_founds <- integer(n_wrong_founds)
if (n_wrong_founds >= 1) {
    for (i_wrong_found in 1:n_wrong_founds) {
        l_wrong_founds[i_wrong_found] <- m_wrong_founds[i_wrong_found, 1]
    } # for
    l_founds <- l_founds[-c(l_wrong_founds)]
} # if

# Restaurar límite de lineas a imprimir en pantalla
options(max.print = mpl)

### - diss - ###
#   - Cómputo de Disimilaridad TSclust
# Descripción:
#       Computa la matríz de disimilaridad de la matríz, lista, data.frame u objeto mts numérico dado utilizando el método de disimilitud TSclust seleccionado.
# Uso:
#       diss(SERIES, METHOD, ...)
# - Argumentos:
#   SERIES:  Matríz, lista, data.frame u objeto mts numérico. las matrices numéricas son interpretadas por renglón (una serie por fila), mientras que los objetos data.frame y mts son interpretados por columna.
#   METHOD:  La medida de disimilitud a utilizarse. Debe ser una de: "ACF", "AR.LPC.CEPS", "AR.MAH", "AR.PIC", "CDM", "CID", "COR", "CORT", "DTWARP", "DWT", "EUCL", "FRECHET", INT.PER", "NCD", "PACF", "PDC", PER", "PRED", "MINDIST.SAX", "SPEC.LLR", "SPEC.GLK" or "SPEC.ISD". Cualquier subcadena sin ambigüedad puede ser proporcionada. Ver los detalles para el uso particular.
#   ...:     Argumentos adicionales para el método seleccionado.
### ~ Método seleccionado: diss.CORT ~ ###
#   - Índice de disimilaridad combinando correlación temporal y comportamiento de valores brutos.
# Uso:
#       diss.CORT(x, y, k = 2, deltamethod="Euclid")
# - Argumentos
#   x:           Vector numérico que contiene la primera de las dos series de tiempo.
#   y:           Vector numérico que contiene la segunda de las dos series de tiempo.
#   k:           Parámetro que controla el peso de la disimilaridad entre los comportamientos dinámicos.
#   deltamethod: Define el método para la discrepancia de los valores brutos. Puede ser "Euclid" (default), "Frechet" ó "DTW".

# Análisis CORT de los precios normalizados del índice BMV y de los fondos de renta variable
## CORT con: k=2 y deltamethod="euclid"
od_cort <- diss(m_tsclust_founds, METHOD="CORT", k=2, deltamethod="Euclid")
print(od_cort)

# Crea matriz con resultados CORT en un data.frame para crear reporte
df_tsclust_results <- data.frame(
      as.matrix(od_cort)
    , row.names=c("BMV", l_founds), stringsAsFactors=FALSE)
names(df_tsclust_results) <- c("BMV", l_founds)

# Escribe y muestra resultados de análisis CORT
f_ana_result_cort <- paste("RVMexico.tsclust_analysis_CORT_", f_ana_time, ".csv", sep="")
write.csv(df_tsclust_results, f_ana_result_cort)
print(df_tsclust_results)

### --- Cálculo de Algoritmo de Agrupamiento basado en valores-p --- ###
# Descripción:
#       Algoritmo de agrupamiento basado en valores-p. Cada grupo en la solución de agrupamiento está formado por series con valores-p asociados mayores que un nivel de significancia preespecificado.
# Uso:
#       pvalues.clust(pvalues, significance)
# - Argumentos
#   pvalues:      Un objeto dist que contiene los valores-p de una prueba de la calidad de cada par de series de tiempo bajo estudio.
#   significance: El nivel de significancia.
l_pclust <- pvalues.clust(od_cort, 0.05)
l_pclust_010 <- pvalues.clust(od_cort, 0.10)
l_pclust_015 <- pvalues.clust(od_cort, 0.15)
l_pclust_025 <- pvalues.clust(od_cort, 0.25)
l_pclust_035 <- pvalues.clust(od_cort, 0.35)
l_pclust_050 <- pvalues.clust(od_cort, 0.50)
l_pclust_065 <- pvalues.clust(od_cort, 0.65)
l_pclust_075 <- pvalues.clust(od_cort, 0.75)
l_pclust_085 <- pvalues.clust(od_cort, 0.85)
l_pclust_095 <- pvalues.clust(od_cort, 0.95)
l_pclust_100 <- pvalues.clust(od_cort, 1.00)

# Crea matriz con resultados de agrupamiento (clustering) en un data.frame para crear reporte
df_tsclust_pclust <- data.frame(
      l_pclust
    , l_pclust_010
    , l_pclust_015
    , l_pclust_025
    , l_pclust_035
    , l_pclust_050
    , l_pclust_065
    , l_pclust_075
    , l_pclust_085
    , l_pclust_095
    , l_pclust_100
    , row.names=c("BMV", l_founds), stringsAsFactors=FALSE)
names(df_tsclust_pclust) <- c(
      "pvalues.clust w/sig.=0.05"
    , "pvalues.clust w/sig.=0.10"
    , "pvalues.clust w/sig.=0.15"
    , "pvalues.clust w/sig.=0.25"
    , "pvalues.clust w/sig.=0.35"
    , "pvalues.clust w/sig.=0.50"
    , "pvalues.clust w/sig.=0.65"
    , "pvalues.clust w/sig.=0.75"
    , "pvalues.clust w/sig.=0.85"
    , "pvalues.clust w/sig.=0.95"
    , "pvalues.clust w/sig.=1.00"
    )

# Escribe y muestra resultados de agrupamiento de series
f_ana_result_pclust <- paste("RVMexico.tsclust_analysis_CORT_pclust_", f_ana_time, ".csv", sep="")
write.csv(df_tsclust_pclust, f_ana_result_pclust)
print(df_tsclust_pclust)


### ~ Método de disimilaridad: diss.AR.MAH ~ ###
# Descripción:
#       Disimilaridad basada en modelos, propuesta por Maharaj (1996, 2000). Contiene el resultado del cómputo de la disimilaridad entre dos series de tiempo mediante la prueba de si ambas series son o no generadas por el mismo modelo ARMA
# Uso:
#       diss.AR.MAH(x, y, dependence=FALSE, permissive=TRUE)
# - Argumentos:
#   x:          Vector numérico que contiene la primera de las dos series de tiempo.
#   y:          Vector numérico que contiene la segunda de las dos series de tiempo.
#   dependence: Boleano para considerar la dependencia entre las observaciones de la serie en el mismo punto en el tiempo.
#   permissive: Boleano para continuar o no con el método incluso cuando no se tenga un orden válido seleccionado por AIC.
od_pvalues <- diss(m_tsclust_founds, "AR.MAH")$p_value

# Crea matriz con valores-p en un data.frame para crear reporte
df_tsclust_pvalues <- data.frame(
      as.matrix(od_pvalues)
    , row.names=c("BMV", l_founds), stringsAsFactors=FALSE)
names(df_tsclust_pvalues) <- c("BMV", l_founds)

# Escribe y muestra resultados de valores-p obtenidos con el método de distancias de Maharaj
f_ana_result_pvalues <- paste("RVMexico.tsclust_analysis_pvalues_AR.MAH_", f_ana_time, ".csv", sep="")
write.csv(df_tsclust_pvalues, f_ana_result_pvalues)
print(df_tsclust_pvalues)

### Cálculo de Algoritmo de Agrupamiento basado en p-values: Con algoritmo AR.MAH ###
l_pclust <- pvalues.clust(od_pvalues, 0.05)
l_pclust_010 <- pvalues.clust(od_pvalues, 0.10)
l_pclust_015 <- pvalues.clust(od_pvalues, 0.15)
l_pclust_025 <- pvalues.clust(od_pvalues, 0.25)
l_pclust_035 <- pvalues.clust(od_pvalues, 0.35)
l_pclust_050 <- pvalues.clust(od_pvalues, 0.50)
l_pclust_065 <- pvalues.clust(od_pvalues, 0.65)
l_pclust_075 <- pvalues.clust(od_pvalues, 0.75)
l_pclust_085 <- pvalues.clust(od_pvalues, 0.85)
l_pclust_095 <- pvalues.clust(od_pvalues, 0.95)
l_pclust_100 <- pvalues.clust(od_pvalues, 1.00)

# Crea matriz con resultados de agrupamiento (clustering) en un data.frame para crear reporte con algoritmo AR.MAH
df_tsclust_pclust <- data.frame(
      l_pclust
    , l_pclust_010
    , l_pclust_015
    , l_pclust_025
    , l_pclust_035
    , l_pclust_050
    , l_pclust_065
    , l_pclust_075
    , l_pclust_085
    , l_pclust_095
    , l_pclust_100
    , row.names=c("BMV", l_founds), stringsAsFactors=FALSE)
names(df_tsclust_pclust) <- c(
      "pvalues.clust w/sig.=0.05"
    , "pvalues.clust w/sig.=0.10"
    , "pvalues.clust w/sig.=0.15"
    , "pvalues.clust w/sig.=0.25"
    , "pvalues.clust w/sig.=0.35"
    , "pvalues.clust w/sig.=0.50"
    , "pvalues.clust w/sig.=0.65"
    , "pvalues.clust w/sig.=0.75"
    , "pvalues.clust w/sig.=0.85"
    , "pvalues.clust w/sig.=0.95"
    , "pvalues.clust w/sig.=1.00"
    )

# Escribe y muestra resultados de agrupamiento de series
f_ana_result_pclust <- paste("RVMexico.tsclust_analysis_pvalues_AR.MAH_pclust_", f_ana_time, ".csv", sep="")
write.csv(df_tsclust_pclust, f_ana_result_pclust)
print(df_tsclust_pclust)
