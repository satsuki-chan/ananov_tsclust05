################################################################################
# Script to generate the clustering analysis for Mexican equity funds.
################################################################################
# Download and install of needed packages from CRAN Package archive website:
#install.packages("cluster")     # Version >=2.0.3
#install.packages("ifultools")   # Version >=2.0-1
#install.packages("MASS")        # Version >=7.3-45
#install.packages("pdc")         # Version >=1.0.3
#install.packages("splus2R")     # Version >=1.2-0
#install.packages("wmtsa")       # Version >=2.0-0
#install.packages("TSclust")     # Version ==1.2.3


# Load required package, besides base packages from R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
require(TSclust)
# Timestamp for results files
f_ana_time <- format(Sys.time(), "%Y%m%d%H%M%S")

# Load files with prices and index data series for analysis
df_founds <- read.csv("RVMexico.prices_normalized.csv")
df_i_bmv <- read.csv("RVMexico.market_normalized_BMV.csv")

# Funds' prices lists data
l_founds <- levels(df_founds[, 1])
n_founds <- length(l_founds)

# Data matrix to store funds' normalized prices for the CORT clustering analysis
m_tsclust_founds <- rbind()

# List of funds with incomplete number of price registries
m_wrong_founds <- rbind()

# Limit the number of lines to print on screen
mpl <- options()$max.print
options(max.print = 4)

### --- CORT calculation --- ###
# Add list of pricess to matrix for clustering analysis
m_tsclust_founds <- rbind(m_tsclust_founds, df_i_bmv[, "c_price"])
# For each fund in the main list...
for (i_found in 1:n_founds) {
    # Separate pricess from fund to analyze
    df_found <- df_founds[df_founds$a_found == l_founds[i_found],]
    # Check that the number of price registries matches the number of BMV index's
    if (length(df_found[, "b_date"]) == length(df_i_bmv[, "b_date"])) {
        # Add liist of pricess to matrix
        m_tsclust_founds <- rbind(m_tsclust_founds, df_found[, "c_price"])
    } # if
    else {
        m_wrong_founds <- rbind(m_wrong_founds, i_found)
    } # else
} # for

## Remove funds with incomplete number of price registries
n_wrong_founds <- length(m_wrong_founds[, 1])
l_wrong_founds <- integer(n_wrong_founds)
if (n_wrong_founds >= 1) {
    for (i_wrong_found in 1:n_wrong_founds) {
        l_wrong_founds[i_wrong_found] <- m_wrong_founds[i_wrong_found, 1]
    } # for
    l_founds <- l_founds[-c(l_wrong_founds)]
} # if

# Restore limit of lines to print on screen
options(max.print = mpl)

### - diss - ###
#   - TSclust Dissimilarity Computation
# Description:
#       Computes the dissimilarity matrix of the given numeric matrix, list, data.frame or mts object using the selected TSclust dissimilarity method..
# Usage:
#       diss(SERIES, METHOD, ...)
# - Arguments:
#   SERIES:  Numeric matrix, list, data.frame or mts object. Numeric matrices are interpreted row-wise (one series per row) meanwhile data.frame and mts objects are interpredted column-wise.
#   METHOD:  the dissimilarity measure to be used. This must be one of "ACF", "AR.LPC.CEPS", "AR.MAH", "AR.PIC", "CDM", "CID", "COR", "CORT", "DTWARP", "DWT", "EUCL", "FRECHET", INT.PER", "NCD", "PACF", "PDC", PER", "PRED", "MINDIST.SAX", "SPEC.LLR", "SPEC.GLK" or "SPEC.ISD". Any unambiguous substring can be given. See details for individual usage.
#   ...:     Additional arguments for the selected method.
### ~ Selected metod: diss.CORT ~ ###
#   - Dissimilarity Index Combining Temporal Correlation and Raw Values Behaviors.
# Usage:
#       diss.CORT(x, y, k = 2, deltamethod="Euclid")
# - Arguments
#   x:           Numeric vector containing the first of the two time series.
#   y:           Numeric vector containing the second of the two time series.
#   k:           Parameter controlling the weight of the dissimilarity between dynamic behaviors (See Details).
#   deltamethod: Defines the method for the raw data discrepancy. Either "Euclid", "Frechet" or "DTW".

# CORT analysis of the normalized prices of the BMV index and the funds'
## CORT with: k=2 and deltamethod="euclid"
od_cort <- diss(m_tsclust_founds, METHOD="CORT", k=2, deltamethod="Euclid")
print(od_cort)

# Matrix with CORT results in a data.frame to create the results report
df_tsclust_results <- data.frame(
      as.matrix(od_cort)
    , row.names=c("BMV", l_founds), stringsAsFactors=FALSE)
names(df_tsclust_results) <- c("BMV", l_founds)

# Write and display the CORT analysis results
f_ana_result_cort <- paste("RVMexico.tsclust_analysis_CORT_", f_ana_time, ".csv", sep="")
write.csv(df_tsclust_results, f_ana_result_cort)
print(df_tsclust_results)

### --- Clustering Algorithm Based on p-values. --- ###
# Description:
#       Clustering algorithm based on p-values. Each group in the cluster solution is formed by series with associated p-values greater than a pre-specified level of significance.
# Usage:
#       pvalues.clust(pvalues, significance)
# - Arguments
#   pvalues:      A dist object containing the p-values from testing the equality of each pair of time series under study.
#   significance: The significance level.
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

# Matrix with clustering results in a data.frame to create the report
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

# Write and display series clustering results
f_ana_result_pclust <- paste("RVMexico.tsclust_analysis_CORT_pclust_", f_ana_time, ".csv", sep="")
write.csv(df_tsclust_pclust, f_ana_result_pclust)
print(df_tsclust_pclust)
