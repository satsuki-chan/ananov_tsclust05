# Novel machine learning analysis of selected Mexican equity funds with clustering analysis

## R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
Platform: x86_64-suse-linux-gnu (64-bit)
>**R Core Team (2015)**. *R: A language and environment for statistical computing*. Vienna, Austria: R Foundation for Statistical Computing.

>https://www.R-project.org/

##Aditional R packages:
###TSclust - Version >= 1.2.3
>**Pablo Montero and Jos{\'e} A. Vilar (2014)**. *TSclust: An R Package for Time Series Clustering*. R package version 1.2.3. Journal of Statistical Software, 62(1), 1-43.

>http://www.jstatsoft.org/v62/i01/


##Execution instructions
####Script file:
* `ananov_tsclust.r`

####Input files:
* `RVMexico.prices_normalized.csv`
* `RVMexico.market_normalized_BMV.csv`

####Output files:
* `RVMexico.tsclust_analysis_CORT_<YYYYmmddHHMMSS>.csv`
* `RVMexico.tsclust_analysis_CORT_pclust_<YYYYmmddHHMMSS>.csv`

####To execute the script:

1. Verify that you have installed R, version 3.2.3 or above
2. Check that all the files are in the same directory and that you have reading and writing permissions
3. Open a terminal window in the directory and execute:
  * `R -f ananov_tsclust.r`
