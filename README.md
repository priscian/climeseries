# climeseries
Download, aggregate, process, and display monthly climatological data.

## I don't care about the stupid package&mdash;where's the latest data?!
Okay! It's [here](inst/extdata/latest/climate-series_20200803.zip?raw=true).

## Preliminaries
The *climeseries* R package is fairly easy to set up. In an R session:
```
install.packages("remotes") # If necessary.
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true") # https://github.com/r-lib/remotes#environment-variables
remotes::install_github("priscian/climeseries")
library(climeseries)

## Once the package has been installed as described above, all you need to use it is:
library(climeseries)
```

## Using *climeseries*
*climeseries* will by default store downloaded data sets in the current working directory (i.e. `getwd()`) of your R session, and will also load existing data sets from that directory. If you want to change *climeseries*'s default directory, set the following option (with a directory of your choice) before you use *climeseries*:
```
options(climeseries_data_dir = "C:/common/data/climate/climeseries")
```
Now you're ready to go:
```
## Download a current climatological data set from the Internet.
inst <- get_climate_data(download = TRUE)

## Try loading this most recent data set from the default directory.
inst <- get_climate_data(download = FALSE, baseline = TRUE)

## Description of the data set returned by 'get_climate_data()'.
?get_climate_data
```
Note that `get_climate_data()` saves the current climatological data set, in the default directory, as two different file types: `.RData` and `.csv`; the `.csv` file is the most portable type and can be imported into other data-management software such as Microsoft Excel for plotting or further processing.

### Making plots
*climeseries* has a pair of functions, `plot_climate_data()` and `plot_models_and_climate_data()`, to simplify plotting climate time series. Some examples follow.
```
########################################
## Plot several global instrumental temperature series.
########################################

airs_series <- "AIRS Surface Skin Global"
new_airs <- interpolate_baseline(airs_series, baseline = 1981:2010)
inst <- get_climate_data(download = FALSE, baseline = TRUE)
inst[[airs_series]] <- new_airs[[airs_series]]
series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global",
  "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5", "UAH TLT 6.0 Global",
  "JRA-55 Surface Air Global", "ERA5 Surface Air Global", "NCEP/NCAR R1 Surface Air Global", airs_series)
## N.B. Don't rebaseline here!
plot_climate_data(inst, series = series, 1880, yearly = TRUE, lwd = 1, ylim = c(-1.0, 1.0), save_png = FALSE)
```

![Some major monthly global average temperature time series.](inst/images/monthly-temp-series_1880.1-recent_yearly_baseline1981-2010.png)

```
########################################
## Plot global instrumental temperature series with 95% confidence intervals.
########################################

inst <- get_climate_data(download = FALSE, baseline = TRUE)
series <- c("Cowtan & Way Krig. Global", "HadCRUT4 Global")
plot_climate_data(inst, series = series, 1850, yearly = TRUE, lwd = 2, conf_int = TRUE, col = c("red", "blue"),
  alpha = 0.2, ci_alpha = 0.1, save_png = FALSE)
```

![Cowtan & Way hybrid global average temperature series w/ 95% confidence intervals.](inst/images/cw14.ci-hadcrut4.ci_1850.1-recent_yearly_baseline1981-2010.png)

```
########################################
## Plot all CMIP5 scenario realizations, no instrumental temperature series.
########################################

inst <- get_climate_data(download = FALSE, baseline = TRUE)
cmip5 <- get_models_data(ensemble = "cmip5")
plot_models_and_climate_data(inst, cmip5, series = NULL, scenario = NULL, start = 1950, end = 2100.99, ma = 12,
  baseline = 1981:2010, center_fun = "mean", smooth_envelope = TRUE, col_m_mean = "red", ylim = c(-1, 5),
  save_png = FALSE)
```

![CMIP5 scenario realizations.](inst/images/cmip5-realizations_1950.1-2100.1_ma12_baseline1981-2010.png)

```
########################################
## CMIP5 RCP 8.5 TAS + TOS scenario realizations compared to the primary land+SST series.
## Cf. Fig. 4(a) of Cowtan et al. 2015, dx.doi.org/10.1002/2015GL064888
########################################

inst <- get_climate_data(download = FALSE, baseline = TRUE)
cmip5 <- get_models_data(ensemble = "cmip5", subdir = "tas + tos")
series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global",
  "BEST Global (Air Ice Temp.)", "JMA Global")
plot_models_and_climate_data(inst, cmip5, series = series, scenario = "RCP 8.5", start = 1880, end = 2025.99,
  yearly = TRUE, ma = 12, baseline = 1970:2000, scenario_text = "Scenario TAS + TOS Realizations",
  center_fun = "mean", smooth_envelope = FALSE, envelope_type = "range", envelope_text = "range",
  ylim = c(-1.0, 1.5), conf_int_i = FALSE, save_png = FALSE)
```

![CMIP5 RCP 8.5 TAS + TOS scenario realizations compared to the major land+SST series.](inst/images/cmip5-tas+tos-rcp85-realizations.range+land-sst_1880.1-2020.1_yearly_baseline1970-2000.png)

```
########################################
## Remove influence of exogenous factors characterizing ENSO, volcanic activity, and solar.
## Cf. Foster & Rahmstorf 2011, dx.doi.org/10.1088/1748-9326/6/4/044022
########################################

series <- c("GISTEMP v4 Global", "NCEI Global", "HadCRUT4 Global", "Cowtan & Way Krig. Global",
  "BEST Global (Air Ice Temp.)", "JMA Global", "RSS TLT 4.0 -70.0/82.5", "UAH TLT 6.0 Global",
  "JRA-55 Surface Air Global", "ERA5 Surface Air Global", "NCEP/NCAR R1 Surface Air Global")
start <- 1970; end <- NULL
g <- remove_exogenous_influences(series = series, start = start, end = end, max_lag = 12)
series_adj <- paste(series, "(adj.)")
main <- "Adjusted for ENSO, Volcanic, and Solar Influences"
plot_climate_data(g, series_adj, yearly = TRUE, main = main, type = "o", pch = 19, baseline = TRUE,
  save_png = FALSE)
```

![Remove influence of exogenous factors characterizing ENSO, volcanic activity, and solar.](inst/images/major-monthly-inst-series-adj_1970.1-recent_yearly_baseline1981-2010.png)

```
########################################
## Estimate optimal number and location of significant changepoints in piecewise regression of climate series.
## Cf. Figure 1 of Cahill et al. 2015, dx.doi.org/10.1088/1748-9326/10/8/084002
########################################

inst <- get_climate_data(download = FALSE, baseline = TRUE)
series <- c("HadCRUT4 Global", "NCEI Global", "GISTEMP v4 Global", "Cowtan & Way Krig. Global")
plot_climate_data(inst, series, yearly = TRUE, col = c("red", "purple", "blue", "green"), lwd = 1, segmented = TRUE,
  save_png = FALSE)
```

![Estimate optimal number and location of significant changepoints in piecewise regression of climate series.](inst/images/hadcrut4+ncei+gistemp+cw14_1850.1-recent_yearly_baseline1981-2010_seg.png)

```
########################################
## Has past sea-level rise accelerated?
## V. Church & White 2011, dx.doi.org/10.1007/s10712-011-9119-1.
########################################

inst <- get_climate_data(download = FALSE, baseline = FALSE)
series <- c("CSIRO Reconstructed Global Mean Sea Level")
g <- remove_periodic_cycle(inst, series, fit_unc = FALSE)
series_adj <- series %_% " (anomalies)"
ylab <- "Global Mean Sea Level (mm)"
main <- "Reconstructed GMSL"
plot_climate_data(g, series_adj, yearly = TRUE, ylab = ylab, main = main, col = "blue", conf_int = TRUE,
  segmented = TRUE, mark_segments = "lines", vline... = list(text... = list(y = 125)),
  segmented... = list(yearly = FALSE, breakpoints... = list(h = 360, breaks = NULL)),
  plot.segmented... = list(col = "red"), save_png = FALSE)
```

![Has past sea-level rise accelerated?](inst/images/csiro-reconstructed-gmsl-anomalies_1880.1-recent_yearly_seg.png)

```
########################################
## Has recent sea-level rise accelerated?
## V. https://tamino.wordpress.com/2017/10/24/what-is-sea-level-up-to-lately
########################################

inst <- get_climate_data(download = FALSE, baseline = FALSE)
series <- c("NOAA Global Mean Sea Level")
g <- remove_periodic_cycle(subset(inst, inst$year >= 1993), series)
series_adj <- series %_% " (anomalies)"
ylab <- "Global Mean Sea Level (mm)"
main <- "GMSL from TOPEX/Poseidon, Jason-1, & Jason-2 Satellite Altimetry"
plot_climate_data(g, series_adj, ylab = ylab, main = main, col = "blue", segmented = TRUE, mark_segments = "lines",
  segmented... = list(yearly = FALSE, breakpoints... = list(h = 120, breaks = NULL)),
  plot.segmented... = list(col = "red"), save_png = FALSE)
```

![Has recent sea-level rise accelerated?](inst/images/noaa-gmsl-anomalies_1993.1-recent_ma0_seg.png)

### More information
*climeseries* is presented here as a working beta. For more information on what the package offers, check out
```
library(help = climeseries)
```
from the R command line.

## Data sets
The latest data sets downloaded by me (where "latest" means whenever I've gotten around to updating them) can be found here: [Current "climeseries" data](inst/extdata/latest/climate-series_20200803.zip?raw=true). Older data sets are listed [here](inst/extdata/latest), too.

### Latest column names
The current column names&mdash;the names of the monthly climatological data sets&mdash;are given below. You will eventually find more information on each data set from the R command line via:
```
?get_climate_data
```

1. year
1. met_year
1. yr_part
1. month
1. 20th C. Reanalysis V3 Surface Air Global
1. 20th C. Reanalysis V3 Surface Air Global Land
1. 20th C. Reanalysis V3 Surface Air Global Ocean
1. 20th C. Reanalysis V3 Surface Air NH
1. 20th C. Reanalysis V3 Surface Air NH Land
1. 20th C. Reanalysis V3 Surface Air NH Ocean
1. 20th C. Reanalysis V3 Surface Air NH Polar
1. 20th C. Reanalysis V3 Surface Air NH Polar Land
1. 20th C. Reanalysis V3 Surface Air NH Polar Ocean
1. 20th C. Reanalysis V3 Surface Air SH
1. 20th C. Reanalysis V3 Surface Air SH Land
1. 20th C. Reanalysis V3 Surface Air SH Ocean
1. 20th C. Reanalysis V3 Surface Air SH Polar
1. 20th C. Reanalysis V3 Surface Air SH Polar Land
1. 20th C. Reanalysis V3 Surface Air SH Polar Ocean
1. 20th C. Reanalysis V3 Surface Air Tropics
1. 20th C. Reanalysis V3 Surface Air Tropics Land
1. 20th C. Reanalysis V3 Surface Air Tropics Ocean
1. 20th C. Reanalysis V3 Surface Air USA 48
1. 20th C. Reanalysis V3 Surface Air USA 48 Land
1. 20th C. Reanalysis V3 Surface Air USA 48 Ocean
1. AIRS Surface Skin Global
1. Antarctica Land Ice Mass Variation
1. Antarctica Land Ice Mass Variation_uncertainty
1. BEST Global (Air Ice Temp.)
1. BEST Global (Water Ice Temp.)
1. BEST Global (Air Ice Temp.)_uncertainty
1. BEST Global (Water Ice Temp.)_uncertainty
1. BEST Global Land
1. BEST Global Land_uncertainty
1. BEST Greenland
1. BEST Greenland_uncertainty
1. BEST NH Land
1. BEST NH Land_uncertainty
1. BEST SH Land
1. BEST SH Land_uncertainty
1. BEST US
1. BEST US_uncertainty
1. CO2 Cape Grim
1. CO2 Cape Grim_uncertainty
1. CO2 Mauna Loa
1. CO2 NOAA ESRL
1. Cowtan & Way Krig. Global
1. Cowtan & Way Krig. Global_uncertainty
1. Cowtan & Way Krig. Global Land
1. CRUTEM4 Global
1. CRUTEM4 NH
1. CRUTEM4 SH
1. CRUTEM4v Global
1. CRUTEM4v NH
1. CRUTEM4v SH
1. CSIRO Global Mean Sea Level
1. CSIRO Reconstructed Global Mean Sea Level
1. CSIRO Reconstructed Global Mean Sea Level_uncertainty
1. ERA5 2m Global
1. ERA5 2m European
1. ERA5 Surface Air Global
1. ERA5 Surface Air Global Land
1. ERA5 Surface Air Global Ocean
1. ERA5 Surface Air NH
1. ERA5 Surface Air NH Land
1. ERA5 Surface Air NH Ocean
1. ERA5 Surface Air NH Polar
1. ERA5 Surface Air NH Polar Land
1. ERA5 Surface Air NH Polar Ocean
1. ERA5 Surface Air SH
1. ERA5 Surface Air SH Land
1. ERA5 Surface Air SH Ocean
1. ERA5 Surface Air SH Polar
1. ERA5 Surface Air SH Polar Land
1. ERA5 Surface Air SH Polar Ocean
1. ERA5 Surface Air Tropics
1. ERA5 Surface Air Tropics Land
1. ERA5 Surface Air Tropics Ocean
1. ERA5 Surface Air USA 48
1. ERA5 Surface Air USA 48 Land
1. ERA5 Surface Air USA 48 Ocean
1. ESRL AMO
1. Extended Multivariate ENSO Index
1. GISS Stratospheric Aerosol Optical Depth (550 nm) Global
1. GISS Stratospheric Aerosol Optical Depth (550 nm) NH
1. GISS Stratospheric Aerosol Optical Depth (550 nm) SH
1. GISTEMP v3 Global
1. GISTEMP v3 Global Land
1. GISTEMP v3 NH
1. GISTEMP v3 NH Land
1. GISTEMP v3 SH
1. GISTEMP v3 SH Land
1. GISTEMP v3 Zonal Glob
1. GISTEMP v3 Zonal NHem
1. GISTEMP v3 Zonal SHem
1. GISTEMP v3 Zonal 24N-90N
1. GISTEMP v3 Zonal 24S-24N
1. GISTEMP v3 Zonal 90S-24S
1. GISTEMP v3 Zonal 64N-90N
1. GISTEMP v3 Zonal 44N-64N
1. GISTEMP v3 Zonal 24N-44N
1. GISTEMP v3 Zonal EQU-24N
1. GISTEMP v3 Zonal 24S-EQU
1. GISTEMP v3 Zonal 44S-24S
1. GISTEMP v3 Zonal 64S-44S
1. GISTEMP v3 Zonal 90S-64S
1. GISTEMP v3 Zonal Land Glob
1. GISTEMP v3 Zonal Land NHem
1. GISTEMP v3 Zonal Land SHem
1. GISTEMP v3 Zonal Land 24N-90N
1. GISTEMP v3 Zonal Land 24S-24N
1. GISTEMP v3 Zonal Land 90S-24S
1. GISTEMP v3 Zonal Land 64N-90N
1. GISTEMP v3 Zonal Land 44N-64N
1. GISTEMP v3 Zonal Land 24N-44N
1. GISTEMP v3 Zonal Land EQU-24N
1. GISTEMP v3 Zonal Land 24S-EQU
1. GISTEMP v3 Zonal Land 44S-24S
1. GISTEMP v3 Zonal Land 64S-44S
1. GISTEMP v3 Zonal Land 90S-64S
1. GISTEMP v4 Global
1. GISTEMP v4 Global Land
1. GISTEMP v4 NH
1. GISTEMP v4 NH Land
1. GISTEMP v4 SH
1. GISTEMP v4 SH Land
1. GISTEMP v4 Zonal Glob
1. GISTEMP v4 Zonal NHem
1. GISTEMP v4 Zonal SHem
1. GISTEMP v4 Zonal 24N-90N
1. GISTEMP v4 Zonal 24S-24N
1. GISTEMP v4 Zonal 90S-24S
1. GISTEMP v4 Zonal 64N-90N
1. GISTEMP v4 Zonal 44N-64N
1. GISTEMP v4 Zonal 24N-44N
1. GISTEMP v4 Zonal EQU-24N
1. GISTEMP v4 Zonal 24S-EQU
1. GISTEMP v4 Zonal 44S-24S
1. GISTEMP v4 Zonal 64S-44S
1. GISTEMP v4 Zonal 90S-64S
1. GISTEMP v4 Zonal Land Glob
1. GISTEMP v4 Zonal Land NHem
1. GISTEMP v4 Zonal Land SHem
1. GISTEMP v4 Zonal Land 24N-90N
1. GISTEMP v4 Zonal Land 24S-24N
1. GISTEMP v4 Zonal Land 90S-24S
1. GISTEMP v4 Zonal Land 64N-90N
1. GISTEMP v4 Zonal Land 44N-64N
1. GISTEMP v4 Zonal Land 24N-44N
1. GISTEMP v4 Zonal Land EQU-24N
1. GISTEMP v4 Zonal Land 24S-EQU
1. GISTEMP v4 Zonal Land 44S-24S
1. GISTEMP v4 Zonal Land 64S-44S
1. GISTEMP v4 Zonal Land 90S-64S
1. GRACE-FO Antarctic Ice Mass AIS [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_301 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_302 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_303 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_304 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_305 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_306 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_307 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_308 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_309 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_310 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_311 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_312 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_313 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_314 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_315 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_316 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_317 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_318 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_319 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_320 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_321 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_322 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_323 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_324 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS_325 [Gt]
1. GRACE-FO Antarctic Ice Mass AIS [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_301 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_302 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_303 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_304 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_305 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_306 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_307 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_308 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_309 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_310 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_311 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_312 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_313 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_314 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_315 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_316 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_317 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_318 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_319 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_320 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_321 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_322 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_323 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_324 [Gt]_uncertainty
1. GRACE-FO Antarctic Ice Mass AIS_325 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS [Gt]
1. GRACE-FO Greenland Ice Mass GIS_301 [Gt]
1. GRACE-FO Greenland Ice Mass GIS_302 [Gt]
1. GRACE-FO Greenland Ice Mass GIS_303 [Gt]
1. GRACE-FO Greenland Ice Mass GIS_304 [Gt]
1. GRACE-FO Greenland Ice Mass GIS_305 [Gt]
1. GRACE-FO Greenland Ice Mass GIS_306 [Gt]
1. GRACE-FO Greenland Ice Mass GIS_307 [Gt]
1. GRACE-FO Greenland Ice Mass GIS [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_301 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_302 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_303 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_304 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_305 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_306 [Gt]_uncertainty
1. GRACE-FO Greenland Ice Mass GIS_307 [Gt]_uncertainty
1. Greenland Land Ice Mass Variation
1. Greenland Land Ice Mass Variation_uncertainty
1. HadCET
1. HadCRUT4 Global
1. HadCRUT4 Global_uncertainty
1. HadCRUT4 NH
1. HadCRUT4 NH_uncertainty
1. HadCRUT4 SH
1. HadCRUT4 SH_uncertainty
1. HadCRUT4 Tropics
1. HadCRUT4 Tropics_uncertainty
1. HadSST3 Global
1. HadSST3 Global_uncertainty
1. HadSST3 NH
1. HadSST3 NH_uncertainty
1. HadSST3 SH
1. HadSST3 SH_uncertainty
1. HadSST3 Tropics
1. HadSST3 Tropics_uncertainty
1. HadSST4 Global
1. HadSST4 Global_uncertainty
1. HadSST4 NH
1. HadSST4 NH_uncertainty
1. HadSST4 SH
1. HadSST4 SH_uncertainty
1. HadSST4 Tropics
1. HadSST4 Tropics_uncertainty
1. JMA Global
1. JRA-55 Surface Air Global
1. JRA-55 Surface Air Global Land
1. JRA-55 Surface Air Global Ocean
1. JRA-55 Surface Air NH
1. JRA-55 Surface Air NH Land
1. JRA-55 Surface Air NH Ocean
1. JRA-55 Surface Air NH Polar
1. JRA-55 Surface Air NH Polar Land
1. JRA-55 Surface Air NH Polar Ocean
1. JRA-55 Surface Air SH
1. JRA-55 Surface Air SH Land
1. JRA-55 Surface Air SH Ocean
1. JRA-55 Surface Air SH Polar
1. JRA-55 Surface Air SH Polar Land
1. JRA-55 Surface Air SH Polar Ocean
1. JRA-55 Surface Air Tropics
1. JRA-55 Surface Air Tropics Land
1. JRA-55 Surface Air Tropics Ocean
1. JRA-55 Surface Air USA 48
1. JRA-55 Surface Air USA 48 Land
1. JRA-55 Surface Air USA 48 Ocean
1. MERRA-2 Surface Air Global
1. MERRA-2 Surface Air Global Land
1. MERRA-2 Surface Air Global Ocean
1. MERRA-2 Surface Air NH
1. MERRA-2 Surface Air NH Land
1. MERRA-2 Surface Air NH Ocean
1. MERRA-2 Surface Air NH Polar
1. MERRA-2 Surface Air NH Polar Land
1. MERRA-2 Surface Air NH Polar Ocean
1. MERRA-2 Surface Air SH
1. MERRA-2 Surface Air SH Land
1. MERRA-2 Surface Air SH Ocean
1. MERRA-2 Surface Air SH Polar
1. MERRA-2 Surface Air SH Polar Land
1. MERRA-2 Surface Air SH Polar Ocean
1. MERRA-2 Surface Air Tropics
1. MERRA-2 Surface Air Tropics Land
1. MERRA-2 Surface Air Tropics Ocean
1. MERRA-2 Surface Air USA 48
1. MERRA-2 Surface Air USA 48 Land
1. MERRA-2 Surface Air USA 48 Ocean
1. Multivariate ENSO Index
1. NCEI Global
1. NCEI Global Land
1. NCEI Global Ocean
1. NCEI NH
1. NCEI NH Land
1. NCEI NH Ocean
1. NCEI Atlantic Ocean Heat Content 0-2000m
1. NCEI Atlantic Ocean Heat Content 0-2000m NH
1. NCEI Atlantic Ocean Heat Content 0-2000m SH
1. NCEI Atlantic Ocean Heat Content 0-700m
1. NCEI Atlantic Ocean Heat Content 0-700m NH
1. NCEI Atlantic Ocean Heat Content 0-700m SH
1. NCEI Global Ocean Heat Content 0-2000m
1. NCEI Global Ocean Heat Content 0-2000m NH
1. NCEI Global Ocean Heat Content 0-2000m SH
1. NCEI Global Ocean Heat Content 0-700m
1. NCEI Global Ocean Heat Content 0-700m NH
1. NCEI Global Ocean Heat Content 0-700m SH
1. NCEI Indian Ocean Heat Content 0-2000m
1. NCEI Indian Ocean Heat Content 0-2000m NH
1. NCEI Indian Ocean Heat Content 0-2000m SH
1. NCEI Indian Ocean Heat Content 0-700m
1. NCEI Indian Ocean Heat Content 0-700m NH
1. NCEI Indian Ocean Heat Content 0-700m SH
1. NCEI Pacific Ocean Heat Content 0-2000m
1. NCEI Pacific Ocean Heat Content 0-2000m NH
1. NCEI Pacific Ocean Heat Content 0-2000m SH
1. NCEI Pacific Ocean Heat Content 0-700m
1. NCEI Pacific Ocean Heat Content 0-700m NH
1. NCEI Pacific Ocean Heat Content 0-700m SH
1. NCEI Atlantic Ocean Heat Content 0-2000m (Pentadal)
1. NCEI Atlantic Ocean Heat Content 0-2000m NH (Pentadal)
1. NCEI Atlantic Ocean Heat Content 0-2000m SH (Pentadal)
1. NCEI Atlantic Ocean Heat Content 0-700m (Pentadal)
1. NCEI Atlantic Ocean Heat Content 0-700m NH (Pentadal)
1. NCEI Atlantic Ocean Heat Content 0-700m SH (Pentadal)
1. NCEI Global Ocean Heat Content 0-2000m (Pentadal)
1. NCEI Global Ocean Heat Content 0-2000m NH (Pentadal)
1. NCEI Global Ocean Heat Content 0-2000m SH (Pentadal)
1. NCEI Global Ocean Heat Content 0-700m (Pentadal)
1. NCEI Global Ocean Heat Content 0-700m NH (Pentadal)
1. NCEI Global Ocean Heat Content 0-700m SH (Pentadal)
1. NCEI Indian Ocean Heat Content 0-2000m (Pentadal)
1. NCEI Indian Ocean Heat Content 0-2000m NH (Pentadal)
1. NCEI Indian Ocean Heat Content 0-2000m SH (Pentadal)
1. NCEI Indian Ocean Heat Content 0-700m (Pentadal)
1. NCEI Indian Ocean Heat Content 0-700m NH (Pentadal)
1. NCEI Indian Ocean Heat Content 0-700m SH (Pentadal)
1. NCEI Pacific Ocean Heat Content 0-2000m (Pentadal)
1. NCEI Pacific Ocean Heat Content 0-2000m NH (Pentadal)
1. NCEI Pacific Ocean Heat Content 0-2000m SH (Pentadal)
1. NCEI Pacific Ocean Heat Content 0-700m (Pentadal)
1. NCEI Pacific Ocean Heat Content 0-700m NH (Pentadal)
1. NCEI Pacific Ocean Heat Content 0-700m SH (Pentadal)
1. NCEI Atlantic Ocean Heat Content 0-2000m_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-2000m NH_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-2000m SH_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-700m_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-700m NH_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-700m SH_uncertainty
1. NCEI Global Ocean Heat Content 0-2000m_uncertainty
1. NCEI Global Ocean Heat Content 0-2000m NH_uncertainty
1. NCEI Global Ocean Heat Content 0-2000m SH_uncertainty
1. NCEI Global Ocean Heat Content 0-700m_uncertainty
1. NCEI Global Ocean Heat Content 0-700m NH_uncertainty
1. NCEI Global Ocean Heat Content 0-700m SH_uncertainty
1. NCEI Indian Ocean Heat Content 0-2000m_uncertainty
1. NCEI Indian Ocean Heat Content 0-2000m NH_uncertainty
1. NCEI Indian Ocean Heat Content 0-2000m SH_uncertainty
1. NCEI Indian Ocean Heat Content 0-700m_uncertainty
1. NCEI Indian Ocean Heat Content 0-700m NH_uncertainty
1. NCEI Indian Ocean Heat Content 0-700m SH_uncertainty
1. NCEI Pacific Ocean Heat Content 0-2000m_uncertainty
1. NCEI Pacific Ocean Heat Content 0-2000m NH_uncertainty
1. NCEI Pacific Ocean Heat Content 0-2000m SH_uncertainty
1. NCEI Pacific Ocean Heat Content 0-700m_uncertainty
1. NCEI Pacific Ocean Heat Content 0-700m NH_uncertainty
1. NCEI Pacific Ocean Heat Content 0-700m SH_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-2000m (Pentadal)_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-2000m NH (Pentadal)_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-2000m SH (Pentadal)_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-700m (Pentadal)_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-700m NH (Pentadal)_uncertainty
1. NCEI Atlantic Ocean Heat Content 0-700m SH (Pentadal)_uncertainty
1. NCEI Global Ocean Heat Content 0-2000m (Pentadal)_uncertainty
1. NCEI Global Ocean Heat Content 0-2000m NH (Pentadal)_uncertainty
1. NCEI Global Ocean Heat Content 0-2000m SH (Pentadal)_uncertainty
1. NCEI Global Ocean Heat Content 0-700m (Pentadal)_uncertainty
1. NCEI Global Ocean Heat Content 0-700m NH (Pentadal)_uncertainty
1. NCEI Global Ocean Heat Content 0-700m SH (Pentadal)_uncertainty
1. NCEI Indian Ocean Heat Content 0-2000m (Pentadal)_uncertainty
1. NCEI Indian Ocean Heat Content 0-2000m NH (Pentadal)_uncertainty
1. NCEI Indian Ocean Heat Content 0-2000m SH (Pentadal)_uncertainty
1. NCEI Indian Ocean Heat Content 0-700m (Pentadal)_uncertainty
1. NCEI Indian Ocean Heat Content 0-700m NH (Pentadal)_uncertainty
1. NCEI Indian Ocean Heat Content 0-700m SH (Pentadal)_uncertainty
1. NCEI Pacific Ocean Heat Content 0-2000m (Pentadal)_uncertainty
1. NCEI Pacific Ocean Heat Content 0-2000m NH (Pentadal)_uncertainty
1. NCEI Pacific Ocean Heat Content 0-2000m SH (Pentadal)_uncertainty
1. NCEI Pacific Ocean Heat Content 0-700m (Pentadal)_uncertainty
1. NCEI Pacific Ocean Heat Content 0-700m NH (Pentadal)_uncertainty
1. NCEI Pacific Ocean Heat Content 0-700m SH (Pentadal)_uncertainty
1. NCEI SH
1. NCEI SH Land
1. NCEI SH Ocean
1. NCEI US Avg. Temp.
1. NCEI US Max. Temp.
1. NCEI US Min. Temp.
1. NCEI US Palmer Z-Index
1. NCEI US PDSI
1. NCEI US PHDI
1. NCEI US PMDI
1. NCEI US Precip.
1. NCEI v5 Land 60S-30S
1. NCEI v5 Land 60N-90N
1. NCEI v5 Land 60S-60N
1. NCEI v5 Land 90S-00N
1. NCEI v5 Land 90S-20S
1. NCEI v5 Land 90S-60S
1. NCEI v5 Land 90S-90N
1. NCEI v5 Land + Ocean 00N-90N
1. NCEI v5 Land + Ocean 00N-30N
1. NCEI v5 Land + Ocean 20N-90N
1. NCEI v5 Land + Ocean 20S-20N
1. NCEI v5 Land + Ocean 30N-60N
1. NCEI v5 Land + Ocean 30S-00N
1. NCEI v5 Land 00N-30N
1. NCEI v5 Land 00N-90N
1. NCEI v5 Land 20N-90N
1. NCEI v5 Land 20S-20N
1. NCEI v5 Land 30N-60N
1. NCEI v5 Land + Ocean 60N-90N
1. NCEI v5 Land + Ocean 60S-30S
1. NCEI v5 Land + Ocean 60S-60N
1. NCEI v5 Land + Ocean 90S-00N
1. NCEI v5 Land + Ocean 90S-20S
1. NCEI v5 Land 30S-00N
1. NCEI v5 Land + Ocean 90S-60S
1. NCEI v5 Land + Ocean 90S-90N
1. NCEI v5 Ocean 00N-30N
1. NCEI v5 Ocean 00N-90N
1. NCEI v5 Ocean 20N-90N
1. NCEI v5 Ocean 20S-20N
1. NCEI v5 Ocean 30N-60N
1. NCEI v5 Ocean 30S-00N
1. NCEI v5 Ocean 60N-90N
1. NCEI v5 Ocean 60S-30S
1. NCEI v5 Ocean 60S-60N
1. NCEI v5 Ocean 90S-00N
1. NCEI v5 Ocean 90S-20S
1. NCEI v5 Ocean 90S-60S
1. NCEI v5 Ocean 90S-90N
1. NCEI v5 Land 60S-30S_uncertainty
1. NCEI v5 Land 60N-90N_uncertainty
1. NCEI v5 Land 60S-60N_uncertainty
1. NCEI v5 Land 90S-00N_uncertainty
1. NCEI v5 Land 90S-20S_uncertainty
1. NCEI v5 Land 90S-60S_uncertainty
1. NCEI v5 Land 90S-90N_uncertainty
1. NCEI v5 Land + Ocean 00N-90N_uncertainty
1. NCEI v5 Land + Ocean 00N-30N_uncertainty
1. NCEI v5 Land + Ocean 20N-90N_uncertainty
1. NCEI v5 Land + Ocean 20S-20N_uncertainty
1. NCEI v5 Land + Ocean 30N-60N_uncertainty
1. NCEI v5 Land + Ocean 30S-00N_uncertainty
1. NCEI v5 Land 00N-30N_uncertainty
1. NCEI v5 Land 00N-90N_uncertainty
1. NCEI v5 Land 20N-90N_uncertainty
1. NCEI v5 Land 20S-20N_uncertainty
1. NCEI v5 Land 30N-60N_uncertainty
1. NCEI v5 Land + Ocean 60N-90N_uncertainty
1. NCEI v5 Land + Ocean 60S-30S_uncertainty
1. NCEI v5 Land + Ocean 60S-60N_uncertainty
1. NCEI v5 Land + Ocean 90S-00N_uncertainty
1. NCEI v5 Land + Ocean 90S-20S_uncertainty
1. NCEI v5 Land 30S-00N_uncertainty
1. NCEI v5 Land + Ocean 90S-60S_uncertainty
1. NCEI v5 Land + Ocean 90S-90N_uncertainty
1. NCEI v5 Ocean 00N-30N_uncertainty
1. NCEI v5 Ocean 00N-90N_uncertainty
1. NCEI v5 Ocean 20N-90N_uncertainty
1. NCEI v5 Ocean 20S-20N_uncertainty
1. NCEI v5 Ocean 30N-60N_uncertainty
1. NCEI v5 Ocean 30S-00N_uncertainty
1. NCEI v5 Ocean 60N-90N_uncertainty
1. NCEI v5 Ocean 60S-30S_uncertainty
1. NCEI v5 Ocean 60S-60N_uncertainty
1. NCEI v5 Ocean 90S-00N_uncertainty
1. NCEI v5 Ocean 90S-20S_uncertainty
1. NCEI v5 Ocean 90S-60S_uncertainty
1. NCEI v5 Ocean 90S-90N_uncertainty
1. NCEP/CSFR Surface Air Global
1. NCEP/CSFR Surface Air Global Land
1. NCEP/CSFR Surface Air Global Ocean
1. NCEP/CSFR Surface Air NH
1. NCEP/CSFR Surface Air NH Land
1. NCEP/CSFR Surface Air NH Ocean
1. NCEP/CSFR Surface Air NH Polar
1. NCEP/CSFR Surface Air NH Polar Land
1. NCEP/CSFR Surface Air NH Polar Ocean
1. NCEP/CSFR Surface Air SH
1. NCEP/CSFR Surface Air SH Land
1. NCEP/CSFR Surface Air SH Ocean
1. NCEP/CSFR Surface Air SH Polar
1. NCEP/CSFR Surface Air SH Polar Land
1. NCEP/CSFR Surface Air SH Polar Ocean
1. NCEP/CSFR Surface Air Tropics
1. NCEP/CSFR Surface Air Tropics Land
1. NCEP/CSFR Surface Air Tropics Ocean
1. NCEP/CSFR Surface Air USA 48
1. NCEP/CSFR Surface Air USA 48 Land
1. NCEP/CSFR Surface Air USA 48 Ocean
1. NCEP/DOE R2 Surface Air Global
1. NCEP/DOE R2 Surface Air Global Land
1. NCEP/DOE R2 Surface Air Global Ocean
1. NCEP/DOE R2 Surface Air NH
1. NCEP/DOE R2 Surface Air NH Land
1. NCEP/DOE R2 Surface Air NH Ocean
1. NCEP/DOE R2 Surface Air NH Polar
1. NCEP/DOE R2 Surface Air NH Polar Land
1. NCEP/DOE R2 Surface Air NH Polar Ocean
1. NCEP/DOE R2 Surface Air SH
1. NCEP/DOE R2 Surface Air SH Land
1. NCEP/DOE R2 Surface Air SH Ocean
1. NCEP/DOE R2 Surface Air SH Polar
1. NCEP/DOE R2 Surface Air SH Polar Land
1. NCEP/DOE R2 Surface Air SH Polar Ocean
1. NCEP/DOE R2 Surface Air Tropics
1. NCEP/DOE R2 Surface Air Tropics Land
1. NCEP/DOE R2 Surface Air Tropics Ocean
1. NCEP/DOE R2 Surface Air USA 48
1. NCEP/DOE R2 Surface Air USA 48 Land
1. NCEP/DOE R2 Surface Air USA 48 Ocean
1. NCEP/NCAR R1 Surface Air Global
1. NCEP/NCAR R1 Surface Air Global Land
1. NCEP/NCAR R1 Surface Air Global Ocean
1. NCEP/NCAR R1 Surface Air NH
1. NCEP/NCAR R1 Surface Air NH Land
1. NCEP/NCAR R1 Surface Air NH Ocean
1. NCEP/NCAR R1 Surface Air NH Polar
1. NCEP/NCAR R1 Surface Air NH Polar Land
1. NCEP/NCAR R1 Surface Air NH Polar Ocean
1. NCEP/NCAR R1 Surface Air SH
1. NCEP/NCAR R1 Surface Air SH Land
1. NCEP/NCAR R1 Surface Air SH Ocean
1. NCEP/NCAR R1 Surface Air SH Polar
1. NCEP/NCAR R1 Surface Air SH Polar Land
1. NCEP/NCAR R1 Surface Air SH Polar Ocean
1. NCEP/NCAR R1 Surface Air Tropics
1. NCEP/NCAR R1 Surface Air Tropics Land
1. NCEP/NCAR R1 Surface Air Tropics Ocean
1. NCEP/NCAR R1 Surface Air USA 48
1. NCEP/NCAR R1 Surface Air USA 48 Land
1. NCEP/NCAR R1 Surface Air USA 48 Ocean
1. NOAA Global Mean Sea Level
1. NOAA Sunspot No.
1. NSIDC Sea Ice NH Extent
1. NSIDC Sea Ice NH Area
1. NSIDC Sea Ice SH Extent
1. NSIDC Sea Ice SH Area
1. NSIDC Sea Ice Global Extent
1. NSIDC Sea Ice Global Area
1. Ocean Mass Variation
1. Ocean Mass Variation_uncertainty
1. OSIRIS Stratospheric Aerosol Optical Depth (550 nm) Global
1. OSIRIS Stratospheric Aerosol Optical Depth (550 nm) NH
1. OSIRIS Stratospheric Aerosol Optical Depth (550 nm) SH
1. PIOMAS Arctic Sea Ice Volume
1. PMOD TSI (new VIRGO)
1. PMOD TSI (orig. VIRGO)
1. RATPAC-A Surface NH
1. RATPAC-A 850 mb NH
1. RATPAC-A 700 mb NH
1. RATPAC-A 500 mb NH
1. RATPAC-A 400 mb NH
1. RATPAC-A 300 mb NH
1. RATPAC-A 250 mb NH
1. RATPAC-A 200 mb NH
1. RATPAC-A 150 mb NH
1. RATPAC-A 100 mb NH
1. RATPAC-A 70 mb NH
1. RATPAC-A 50 mb NH
1. RATPAC-A 30 mb NH
1. RATPAC-A Surface SH
1. RATPAC-A 850 mb SH
1. RATPAC-A 700 mb SH
1. RATPAC-A 500 mb SH
1. RATPAC-A 400 mb SH
1. RATPAC-A 300 mb SH
1. RATPAC-A 250 mb SH
1. RATPAC-A 200 mb SH
1. RATPAC-A 150 mb SH
1. RATPAC-A 100 mb SH
1. RATPAC-A 70 mb SH
1. RATPAC-A 50 mb SH
1. RATPAC-A 30 mb SH
1. RATPAC-A Surface GLOBE
1. RATPAC-A 850 mb GLOBE
1. RATPAC-A 700 mb GLOBE
1. RATPAC-A 500 mb GLOBE
1. RATPAC-A 400 mb GLOBE
1. RATPAC-A 300 mb GLOBE
1. RATPAC-A 250 mb GLOBE
1. RATPAC-A 200 mb GLOBE
1. RATPAC-A 150 mb GLOBE
1. RATPAC-A 100 mb GLOBE
1. RATPAC-A 70 mb GLOBE
1. RATPAC-A 50 mb GLOBE
1. RATPAC-A 30 mb GLOBE
1. RATPAC-A Surface TROPICS (30S-30N)
1. RATPAC-A 850 mb TROPICS (30S-30N)
1. RATPAC-A 700 mb TROPICS (30S-30N)
1. RATPAC-A 500 mb TROPICS (30S-30N)
1. RATPAC-A 400 mb TROPICS (30S-30N)
1. RATPAC-A 300 mb TROPICS (30S-30N)
1. RATPAC-A 250 mb TROPICS (30S-30N)
1. RATPAC-A 200 mb TROPICS (30S-30N)
1. RATPAC-A 150 mb TROPICS (30S-30N)
1. RATPAC-A 100 mb TROPICS (30S-30N)
1. RATPAC-A 70 mb TROPICS (30S-30N)
1. RATPAC-A 50 mb TROPICS (30S-30N)
1. RATPAC-A 30 mb TROPICS (30S-30N)
1. RATPAC-A Surface NH Extratropics
1. RATPAC-A 850 mb NH Extratropics
1. RATPAC-A 700 mb NH Extratropics
1. RATPAC-A 500 mb NH Extratropics
1. RATPAC-A 400 mb NH Extratropics
1. RATPAC-A 300 mb NH Extratropics
1. RATPAC-A 250 mb NH Extratropics
1. RATPAC-A 200 mb NH Extratropics
1. RATPAC-A 150 mb NH Extratropics
1. RATPAC-A 100 mb NH Extratropics
1. RATPAC-A 70 mb NH Extratropics
1. RATPAC-A 50 mb NH Extratropics
1. RATPAC-A 30 mb NH Extratropics
1. RATPAC-A Surface SH Extratropics
1. RATPAC-A 850 mb SH Extratropics
1. RATPAC-A 700 mb SH Extratropics
1. RATPAC-A 500 mb SH Extratropics
1. RATPAC-A 400 mb SH Extratropics
1. RATPAC-A 300 mb SH Extratropics
1. RATPAC-A 250 mb SH Extratropics
1. RATPAC-A 200 mb SH Extratropics
1. RATPAC-A 150 mb SH Extratropics
1. RATPAC-A 100 mb SH Extratropics
1. RATPAC-A 70 mb SH Extratropics
1. RATPAC-A 50 mb SH Extratropics
1. RATPAC-A 30 mb SH Extratropics
1. RATPAC-A Surface TROPICS (20S-20N)
1. RATPAC-A 850 mb TROPICS (20S-20N)
1. RATPAC-A 700 mb TROPICS (20S-20N)
1. RATPAC-A 500 mb TROPICS (20S-20N)
1. RATPAC-A 400 mb TROPICS (20S-20N)
1. RATPAC-A 300 mb TROPICS (20S-20N)
1. RATPAC-A 250 mb TROPICS (20S-20N)
1. RATPAC-A 200 mb TROPICS (20S-20N)
1. RATPAC-A 150 mb TROPICS (20S-20N)
1. RATPAC-A 100 mb TROPICS (20S-20N)
1. RATPAC-A 70 mb TROPICS (20S-20N)
1. RATPAC-A 50 mb TROPICS (20S-20N)
1. RATPAC-A 30 mb TROPICS (20S-20N)
1. RATPAC-A 850-300 mb NH
1. RATPAC-A 850-300 mb SH
1. RATPAC-A 850-300 mb Global
1. RATPAC-A 850-300 mb Tropics
1. RATPAC-A 850-300 mb NH Extratropics
1. RATPAC-A 850-300 mb SH Extratropics
1. RATPAC-A 850-300 mb 20N-S
1. RATPAC-A 300-100 mb NH
1. RATPAC-A 300-100 mb SH
1. RATPAC-A 300-100 mb Global
1. RATPAC-A 300-100 mb Tropics
1. RATPAC-A 300-100 mb NH Extratropics
1. RATPAC-A 300-100 mb SH Extratropics
1. RATPAC-A 300-100 mb 20N-S
1. RATPAC-A 100-50 mb NH
1. RATPAC-A 100-50 mb SH
1. RATPAC-A 100-50 mb Global
1. RATPAC-A 100-50 mb Tropics
1. RATPAC-A 100-50 mb NH Extratropics
1. RATPAC-A 100-50 mb SH Extratropics
1. RATPAC-A 100-50 mb 20N-S
1. RSS TLS 3.3 -82.5/82.5
1. RSS TLS 3.3 -20.0/20.0
1. RSS TLS 3.3 20.0/82.5
1. RSS TLS 3.3 -82.5/-20.0
1. RSS TLS 3.3 60.0/82.5
1. RSS TLS 3.3 -82.5/-60.0
1. RSS TLS 3.3 Cont. USA
1. RSS TLS 3.3 0.0/82.5
1. RSS TLS 3.3 -82.5/0.0
1. RSS TLS 3.3 Land -82.5/82.5
1. RSS TLS 3.3 Land -20.0/20.0
1. RSS TLS 3.3 Land 20.0/82.5
1. RSS TLS 3.3 Land -82.5/-20.0
1. RSS TLS 3.3 Land 60.0/82.5
1. RSS TLS 3.3 Land -82.5/-60.0
1. RSS TLS 3.3 Ocean -82.5/82.5
1. RSS TLS 3.3 Ocean -20.0/20.0
1. RSS TLS 3.3 Ocean 20.0/82.5
1. RSS TLS 3.3 Ocean -82.5/-20.0
1. RSS TLS 3.3 Ocean 60.0/82.5
1. RSS TLS 3.3 Ocean -82.5/-60.0
1. RSS TLT 3.3 -70.0/82.5
1. RSS TLT 3.3 -20.0/20.0
1. RSS TLT 3.3 20.0/82.5
1. RSS TLT 3.3 -70.0/-20.0
1. RSS TLT 3.3 60.0/82.5
1. RSS TLT 3.3 -70.0/-60.0
1. RSS TLT 3.3 Cont. USA
1. RSS TLT 3.3 0.0/82.5
1. RSS TLT 3.3 -70.0/0.0
1. RSS TLT 3.3 Land -70.0/82.5
1. RSS TLT 3.3 Land -20.0/20.0
1. RSS TLT 3.3 Land 20.0/82.5
1. RSS TLT 3.3 Land -70.0/-20.0
1. RSS TLT 3.3 Land 60.0/82.5
1. RSS TLT 3.3 Land -70.0/-60.0
1. RSS TLT 3.3 Ocean -70.0/82.5
1. RSS TLT 3.3 Ocean -20.0/20.0
1. RSS TLT 3.3 Ocean 20.0/82.5
1. RSS TLT 3.3 Ocean -70.0/-20.0
1. RSS TLT 3.3 Ocean 60.0/82.5
1. RSS TLT 3.3 Ocean -70.0/-60.0
1. RSS TLT 4.0 -70.0/82.5
1. RSS TLT 4.0 -20.0/20.0
1. RSS TLT 4.0 20.0/82.5
1. RSS TLT 4.0 -70.0/-20.0
1. RSS TLT 4.0 60.0/82.5
1. RSS TLT 4.0 -70.0/-60.0
1. RSS TLT 4.0 Cont. USA
1. RSS TLT 4.0 0.0/82.5
1. RSS TLT 4.0 -70.0/0.0
1. RSS TLT 4.0 Land -70.0/82.5
1. RSS TLT 4.0 Land -20.0/20.0
1. RSS TLT 4.0 Land 20.0/82.5
1. RSS TLT 4.0 Land -70.0/-20.0
1. RSS TLT 4.0 Land 60.0/82.5
1. RSS TLT 4.0 Land -70.0/-60.0
1. RSS TLT 4.0 Ocean -70.0/82.5
1. RSS TLT 4.0 Ocean -20.0/20.0
1. RSS TLT 4.0 Ocean 20.0/82.5
1. RSS TLT 4.0 Ocean -70.0/-20.0
1. RSS TLT 4.0 Ocean 60.0/82.5
1. RSS TLT 4.0 Ocean -70.0/-60.0
1. RSS TMT 3.3 -82.5/82.5
1. RSS TMT 3.3 -20.0/20.0
1. RSS TMT 3.3 20.0/82.5
1. RSS TMT 3.3 -82.5/-20.0
1. RSS TMT 3.3 60.0/82.5
1. RSS TMT 3.3 -82.5/-60.0
1. RSS TMT 3.3 Cont. USA
1. RSS TMT 3.3 0.0/82.5
1. RSS TMT 3.3 -82.5/0.0
1. RSS TMT 3.3 Land -82.5/82.5
1. RSS TMT 3.3 Land -20.0/20.0
1. RSS TMT 3.3 Land 20.0/82.5
1. RSS TMT 3.3 Land -82.5/-20.0
1. RSS TMT 3.3 Land 60.0/82.5
1. RSS TMT 3.3 Land -82.5/-60.0
1. RSS TMT 3.3 Ocean -82.5/82.5
1. RSS TMT 3.3 Ocean -20.0/20.0
1. RSS TMT 3.3 Ocean 20.0/82.5
1. RSS TMT 3.3 Ocean -82.5/-20.0
1. RSS TMT 3.3 Ocean 60.0/82.5
1. RSS TMT 3.3 Ocean -82.5/-60.0
1. RSS TMT 4.0 -82.5/82.5
1. RSS TMT 4.0 -20.0/20.0
1. RSS TMT 4.0 20.0/82.5
1. RSS TMT 4.0 -82.5/-20.0
1. RSS TMT 4.0 60.0/82.5
1. RSS TMT 4.0 -82.5/-60.0
1. RSS TMT 4.0 Cont. USA
1. RSS TMT 4.0 0.0/82.5
1. RSS TMT 4.0 -82.5/0.0
1. RSS TMT 4.0 Land -82.5/82.5
1. RSS TMT 4.0 Land -20.0/20.0
1. RSS TMT 4.0 Land 20.0/82.5
1. RSS TMT 4.0 Land -82.5/-20.0
1. RSS TMT 4.0 Land 60.0/82.5
1. RSS TMT 4.0 Land -82.5/-60.0
1. RSS TMT 4.0 Ocean -82.5/82.5
1. RSS TMT 4.0 Ocean -20.0/20.0
1. RSS TMT 4.0 Ocean 20.0/82.5
1. RSS TMT 4.0 Ocean -82.5/-20.0
1. RSS TMT 4.0 Ocean 60.0/82.5
1. RSS TMT 4.0 Ocean -82.5/-60.0
1. RSS TTS 3.3 -82.5/82.5
1. RSS TTS 3.3 -20.0/20.0
1. RSS TTS 3.3 20.0/82.5
1. RSS TTS 3.3 -82.5/-20.0
1. RSS TTS 3.3 60.0/82.5
1. RSS TTS 3.3 -82.5/-60.0
1. RSS TTS 3.3 Cont. USA
1. RSS TTS 3.3 0.0/82.5
1. RSS TTS 3.3 -82.5/0.0
1. RSS TTS 3.3 Land -82.5/82.5
1. RSS TTS 3.3 Land -20.0/20.0
1. RSS TTS 3.3 Land 20.0/82.5
1. RSS TTS 3.3 Land -82.5/-20.0
1. RSS TTS 3.3 Land 60.0/82.5
1. RSS TTS 3.3 Land -82.5/-60.0
1. RSS TTS 3.3 Ocean -82.5/82.5
1. RSS TTS 3.3 Ocean -20.0/20.0
1. RSS TTS 3.3 Ocean 20.0/82.5
1. RSS TTS 3.3 Ocean -82.5/-20.0
1. RSS TTS 3.3 Ocean 60.0/82.5
1. RSS TTS 3.3 Ocean -82.5/-60.0
1. RSS TTT 3.3 -82.5/82.5
1. RSS TTT 3.3 -20.0/20.0
1. RSS TTT 3.3 20.0/82.5
1. RSS TTT 3.3 -82.5/-20.0
1. RSS TTT 3.3 60.0/82.5
1. RSS TTT 3.3 -82.5/-60.0
1. RSS TTT 3.3 Cont. USA
1. RSS TTT 3.3 0.0/82.5
1. RSS TTT 3.3 -82.5/0.0
1. RSS TTT 3.3 Land -82.5/82.5
1. RSS TTT 3.3 Land -20.0/20.0
1. RSS TTT 3.3 Land 20.0/82.5
1. RSS TTT 3.3 Land -82.5/-20.0
1. RSS TTT 3.3 Land 60.0/82.5
1. RSS TTT 3.3 Land -82.5/-60.0
1. RSS TTT 3.3 Ocean -82.5/82.5
1. RSS TTT 3.3 Ocean -20.0/20.0
1. RSS TTT 3.3 Ocean 20.0/82.5
1. RSS TTT 3.3 Ocean -82.5/-20.0
1. RSS TTT 3.3 Ocean 60.0/82.5
1. RSS TTT 3.3 Ocean -82.5/-60.0
1. RSS TTT 4.0 -82.5/82.5
1. RSS TTT 4.0 -20.0/20.0
1. RSS TTT 4.0 20.0/82.5
1. RSS TTT 4.0 -82.5/-20.0
1. RSS TTT 4.0 60.0/82.5
1. RSS TTT 4.0 -82.5/-60.0
1. RSS TTT 4.0 Cont. USA
1. RSS TTT 4.0 0.0/82.5
1. RSS TTT 4.0 -82.5/0.0
1. RSS TTT 4.0 Land -82.5/82.5
1. RSS TTT 4.0 Land -20.0/20.0
1. RSS TTT 4.0 Land 20.0/82.5
1. RSS TTT 4.0 Land -82.5/-20.0
1. RSS TTT 4.0 Land 60.0/82.5
1. RSS TTT 4.0 Land -82.5/-60.0
1. RSS TTT 4.0 Ocean -82.5/82.5
1. RSS TTT 4.0 Ocean -20.0/20.0
1. RSS TTT 4.0 Ocean 20.0/82.5
1. RSS TTT 4.0 Ocean -82.5/-20.0
1. RSS TTT 4.0 Ocean 60.0/82.5
1. RSS TTT 4.0 Ocean -82.5/-60.0
1. Rutgers Eurasia Snow Cover
1. Rutgers N. America (No Greenland) Snow Cover
1. Rutgers N. America Snow Cover
1. Rutgers NH Snow Cover
1. SORCE TSI
1. SORCE TSI_uncertainty
1. TSI Reconstructed
1. UAH TLS 5.6 Global
1. UAH TLS 5.6 Global Land
1. UAH TLS 5.6 Global Ocean
1. UAH TLS 5.6 NH
1. UAH TLS 5.6 NH Land
1. UAH TLS 5.6 NH Ocean
1. UAH TLS 5.6 SH
1. UAH TLS 5.6 SH Land
1. UAH TLS 5.6 SH Ocean
1. UAH TLS 5.6 Tropics
1. UAH TLS 5.6 Tropics Land
1. UAH TLS 5.6 Tropics Ocean
1. UAH TLS 5.6 NH Extratropics
1. UAH TLS 5.6 NH Extratropics Land
1. UAH TLS 5.6 NH Extratropics Ocean
1. UAH TLS 5.6 SH Extratropics
1. UAH TLS 5.6 SH Extratropics Land
1. UAH TLS 5.6 SH Extratropics Ocean
1. UAH TLS 5.6 NH Polar
1. UAH TLS 5.6 NH Polar Land
1. UAH TLS 5.6 NH Polar Ocean
1. UAH TLS 5.6 SH Polar
1. UAH TLS 5.6 SH Polar Land
1. UAH TLS 5.6 SH Polar Ocean
1. UAH TLS 5.6 USA 48
1. UAH TLS 5.6 USA 48 + Alaska
1. UAH TLS 5.6 Australia
1. UAH TLS 6.0 Global
1. UAH TLS 6.0 Global Land
1. UAH TLS 6.0 Global Ocean
1. UAH TLS 6.0 NH
1. UAH TLS 6.0 NH Land
1. UAH TLS 6.0 NH Ocean
1. UAH TLS 6.0 SH
1. UAH TLS 6.0 SH Land
1. UAH TLS 6.0 SH Ocean
1. UAH TLS 6.0 Tropics
1. UAH TLS 6.0 Tropics Land
1. UAH TLS 6.0 Tropics Ocean
1. UAH TLS 6.0 NH Extratropics
1. UAH TLS 6.0 NH Extratropics Land
1. UAH TLS 6.0 NH Extratropics Ocean
1. UAH TLS 6.0 SH Extratropics
1. UAH TLS 6.0 SH Extratropics Land
1. UAH TLS 6.0 SH Extratropics Ocean
1. UAH TLS 6.0 NH Polar
1. UAH TLS 6.0 NH Polar Land
1. UAH TLS 6.0 NH Polar Ocean
1. UAH TLS 6.0 SH Polar
1. UAH TLS 6.0 SH Polar Land
1. UAH TLS 6.0 SH Polar Ocean
1. UAH TLS 6.0 USA 48
1. UAH TLS 6.0 USA 48 + Alaska
1. UAH TLS 6.0 Australia
1. UAH TLT 5.6 Global
1. UAH TLT 5.6 Global Land
1. UAH TLT 5.6 Global Ocean
1. UAH TLT 5.6 NH
1. UAH TLT 5.6 NH Land
1. UAH TLT 5.6 NH Ocean
1. UAH TLT 5.6 SH
1. UAH TLT 5.6 SH Land
1. UAH TLT 5.6 SH Ocean
1. UAH TLT 5.6 Tropics
1. UAH TLT 5.6 Tropics Land
1. UAH TLT 5.6 Tropics Ocean
1. UAH TLT 5.6 NH Extratropics
1. UAH TLT 5.6 NH Extratropics Land
1. UAH TLT 5.6 NH Extratropics Ocean
1. UAH TLT 5.6 SH Extratropics
1. UAH TLT 5.6 SH Extratropics Land
1. UAH TLT 5.6 SH Extratropics Ocean
1. UAH TLT 5.6 NH Polar
1. UAH TLT 5.6 NH Polar Land
1. UAH TLT 5.6 NH Polar Ocean
1. UAH TLT 5.6 SH Polar
1. UAH TLT 5.6 SH Polar Land
1. UAH TLT 5.6 SH Polar Ocean
1. UAH TLT 5.6 USA 48
1. UAH TLT 5.6 USA 48 + Alaska
1. UAH TLT 5.6 Australia
1. UAH TLT 6.0 Global
1. UAH TLT 6.0 Global Land
1. UAH TLT 6.0 Global Ocean
1. UAH TLT 6.0 NH
1. UAH TLT 6.0 NH Land
1. UAH TLT 6.0 NH Ocean
1. UAH TLT 6.0 SH
1. UAH TLT 6.0 SH Land
1. UAH TLT 6.0 SH Ocean
1. UAH TLT 6.0 Tropics
1. UAH TLT 6.0 Tropics Land
1. UAH TLT 6.0 Tropics Ocean
1. UAH TLT 6.0 NH Extratropics
1. UAH TLT 6.0 NH Extratropics Land
1. UAH TLT 6.0 NH Extratropics Ocean
1. UAH TLT 6.0 SH Extratropics
1. UAH TLT 6.0 SH Extratropics Land
1. UAH TLT 6.0 SH Extratropics Ocean
1. UAH TLT 6.0 NH Polar
1. UAH TLT 6.0 NH Polar Land
1. UAH TLT 6.0 NH Polar Ocean
1. UAH TLT 6.0 SH Polar
1. UAH TLT 6.0 SH Polar Land
1. UAH TLT 6.0 SH Polar Ocean
1. UAH TLT 6.0 USA 48
1. UAH TLT 6.0 USA 48 + Alaska
1. UAH TLT 6.0 Australia
1. UAH TMT 5.6 Global
1. UAH TMT 5.6 Global Land
1. UAH TMT 5.6 Global Ocean
1. UAH TMT 5.6 NH
1. UAH TMT 5.6 NH Land
1. UAH TMT 5.6 NH Ocean
1. UAH TMT 5.6 SH
1. UAH TMT 5.6 SH Land
1. UAH TMT 5.6 SH Ocean
1. UAH TMT 5.6 Tropics
1. UAH TMT 5.6 Tropics Land
1. UAH TMT 5.6 Tropics Ocean
1. UAH TMT 5.6 NH Extratropics
1. UAH TMT 5.6 NH Extratropics Land
1. UAH TMT 5.6 NH Extratropics Ocean
1. UAH TMT 5.6 SH Extratropics
1. UAH TMT 5.6 SH Extratropics Land
1. UAH TMT 5.6 SH Extratropics Ocean
1. UAH TMT 5.6 NH Polar
1. UAH TMT 5.6 NH Polar Land
1. UAH TMT 5.6 NH Polar Ocean
1. UAH TMT 5.6 SH Polar
1. UAH TMT 5.6 SH Polar Land
1. UAH TMT 5.6 SH Polar Ocean
1. UAH TMT 5.6 USA 48
1. UAH TMT 5.6 USA 48 + Alaska
1. UAH TMT 5.6 Australia
1. UAH TMT 6.0 Global
1. UAH TMT 6.0 Global Land
1. UAH TMT 6.0 Global Ocean
1. UAH TMT 6.0 NH
1. UAH TMT 6.0 NH Land
1. UAH TMT 6.0 NH Ocean
1. UAH TMT 6.0 SH
1. UAH TMT 6.0 SH Land
1. UAH TMT 6.0 SH Ocean
1. UAH TMT 6.0 Tropics
1. UAH TMT 6.0 Tropics Land
1. UAH TMT 6.0 Tropics Ocean
1. UAH TMT 6.0 NH Extratropics
1. UAH TMT 6.0 NH Extratropics Land
1. UAH TMT 6.0 NH Extratropics Ocean
1. UAH TMT 6.0 SH Extratropics
1. UAH TMT 6.0 SH Extratropics Land
1. UAH TMT 6.0 SH Extratropics Ocean
1. UAH TMT 6.0 NH Polar
1. UAH TMT 6.0 NH Polar Land
1. UAH TMT 6.0 NH Polar Ocean
1. UAH TMT 6.0 SH Polar
1. UAH TMT 6.0 SH Polar Land
1. UAH TMT 6.0 SH Polar Ocean
1. UAH TMT 6.0 USA 48
1. UAH TMT 6.0 USA 48 + Alaska
1. UAH TMT 6.0 Australia
1. UAH TTP 6.0 Global
1. UAH TTP 6.0 Global Land
1. UAH TTP 6.0 Global Ocean
1. UAH TTP 6.0 NH
1. UAH TTP 6.0 NH Land
1. UAH TTP 6.0 NH Ocean
1. UAH TTP 6.0 SH
1. UAH TTP 6.0 SH Land
1. UAH TTP 6.0 SH Ocean
1. UAH TTP 6.0 Tropics
1. UAH TTP 6.0 Tropics Land
1. UAH TTP 6.0 Tropics Ocean
1. UAH TTP 6.0 NH Extratropics
1. UAH TTP 6.0 NH Extratropics Land
1. UAH TTP 6.0 NH Extratropics Ocean
1. UAH TTP 6.0 SH Extratropics
1. UAH TTP 6.0 SH Extratropics Land
1. UAH TTP 6.0 SH Extratropics Ocean
1. UAH TTP 6.0 NH Polar
1. UAH TTP 6.0 NH Polar Land
1. UAH TTP 6.0 NH Polar Ocean
1. UAH TTP 6.0 SH Polar
1. UAH TTP 6.0 SH Polar Land
1. UAH TTP 6.0 SH Polar Ocean
1. UAH TTP 6.0 USA 48
1. UAH TTP 6.0 USA 48 + Alaska
1. UAH TTP 6.0 Australia
