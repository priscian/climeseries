#' Concatenate Strings Easily
#'
#' Allows quick chaining together of character strings.
#'
#' @param a R object to be converted to a character vector.
#' @param b R object to be converted to a character vector.
#' @param sep A character string to separate the terms; passed to \code{\link[base]{paste}()}.
#'
#' @return The concatenation of \code{a} and \code{b}.
#'
#' @seealso \code{\link[base]{paste}}
#'
#' @examples
#' who <- "world"
#' "Hello " %_% who %_% "!"
#'
#' @export
`%_%` <- function(a, b, sep='') paste(a, b, sep = sep)

#' Package Constants
#'
#' @name constants
#' @format Various.
NULL

## http://rrubyperlundich.blogspot.com/2011/07/r-generate-vector-with-names-of-months.html
#' @rdname constants
#' @export
MOS <- format(ISOdatetime(2000, 1:12, 1, 0, 0, 0), "%b")

#' @rdname constants
#' @export
MONTHS <- month.name

#' @rdname constants
#' @export
current_month <- as.integer(format(Sys.Date(), "%m"))
current_month_lagged <- current_month

#' @rdname constants
#' @export
current_year <- as.integer(format(Sys.Date(), "%Y"))
current_year_lagged <- current_year
if (current_month == 1) {
  current_year_lagged <- current_year - 1
  current_month_lagged <- 12
}
#current_year_lagged <- 2024

dataDir <- "."
filenameBase <- "climate-series_"

defaultBaseline <- 1981:2010

## Some climatological time-series base URLs.
gistempBaseV3 <- "https://data.giss.nasa.gov/gistemp/tabledata_v3/"
gistempBaseV4 <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/"
nceiBase <- "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/"
nceiGlobalMonthly <- sprintf("/1/0/1850-%s/data.csv", current_year_lagged) # N.B. Change this back in Feb 2024!!
nceiUsMonthly <- sprintf("/1/0/1895-%s.csv?base_prd=true&begbaseyear=1901&endbaseyear=2000", current_year_lagged) # N.B. Change this back in Feb 2024!!
crutemBase <- "https://crudata.uea.ac.uk/cru/data/temperature/"
hadcrutBase <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/"
hadsstBaseV3 <- "http://www.metoffice.gov.uk/hadobs/hadsst3/data/HadSST.3.1.1.0/diagnostics/"
hadsstBaseV4 <- "https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/"
crutem5Base <- "https://www.metoffice.gov.uk/hadobs/crutem5/data/CRUTEM.5.0.2.0/diagnostics/"
hadcrut5Base <- "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/diagnostics/"
hadcrut5NonInfilledBase <- "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/non-infilled/diagnostics/"
cowtanWayBase <- "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/"
#bestBase <- "http://berkeleyearth.lbl.gov/auto/"
bestBase <- "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/"
bestBase2 <- "https://berkeleyearth.lbl.gov:4443/auto/"
rssBase <- "http://data.remss.com/msu/monthly_time_series/"
#rssBase <- "ftp://priscian%40gmail.com:priscian%40gmail.com@ftp.remss.com/msu/monthly_time_series/"
rssChannel <- "RSS_Monthly_MSU_AMSU_Channel_@@CHANNEL@@_Anomalies_"
rssTls <- sub("@@CHANNEL@@", "TLS", rssChannel)
rssTlt <- sub("@@CHANNEL@@", "TLT", rssChannel)
rssTmt <- sub("@@CHANNEL@@", "TMT", rssChannel)
rssTts <- sub("@@CHANNEL@@", "TTS", rssChannel)
rssTtt <- sub("@@CHANNEL@@", "TTT", rssChannel)
uahBase <- "http://www.nsstc.uah.edu/data/msu/"
esrlBase <- "https://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype = 1&level=2000&iseas=0&mon1=0&mon2=11&iarea=1&typeout=1&Submit=Create+Timeseries&lat1=@@LAT1@@&lat2=@@LAT2@@&lon1=@@LON1@@&lon2=@@LON2@@&var=@@VAR@@"
esrlLatOnlyBase <- sub("@@LON1@@", "-180", sub("@@LON2@@", "180", esrlBase))
## Start here: http://www.esrl.noaa.gov/psd/data/timeseries/
rutgerssnowBase <- "http://climate.rutgers.edu/snowcover/files/moncov."
modisAodBase <- "http://giovanni.gsfc.nasa.gov/giovanni/daac-bin/service_manager.pl?session=@@SESSIONID@@&service=ArAvTs&starttime=2000-03-01T00:00:00Z&endtime=@@DATE@@T23:59:59Z&data=MOD08_M3_6_Aerosol_Optical_Depth_Land_Ocean_Mean_Mean&portal=GIOVANNI&format=json"
## ERA-Interim 2m temperature
## https://climate.copernicus.eu/surface-air-temperature-maps
## https://confluence.ecmwf.int/display/CKB/How+to+download+ERA-Interim+data+from+the+ECMWF+data+archive
#eraInterim2mTempBase <- "https://climate.copernicus.eu/sites/default/files/ftp-data/temperature/"
eraInterim2mTempBase <- "https://climate.copernicus.eu/sites/default/files/"
noaaOhcBase <- "https://data.nodc.noaa.gov/woa/DATA_ANALYSIS/3M_HEAT_CONTENT/DATA/basin/"
#nasaLandIceMassBase <- "https://podaac-tools.jpl.nasa.gov/drive/files/allData/tellus/L4/ice_mass/RL06.1/v03/mascon_CRI/"
nasaLandIceMassBase <- "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/"
#nasaOceanMassBase <- "https://podaac-tools.jpl.nasa.gov/drive/files/allData/tellus/L4/ocean_mass/RL06.1/v03/mascon_CRI/"
nasaOceanMassBase <- "https://archive.podaac.earthdata.nasa.gov/podaac-ops-cumulus-protected/"
#graceFoBase <- "http://gravis.gfz-potsdam.de/csvdata/"
graceFoBase <- "http://gravis.gfz-potsdam.de/zipcsvdata/"


## Reanalyses:
make_reanalysis_urls <- function()
{
  ## What's available: https://psl.noaa.gov/cgi-bin/data/atmoswrit/timeseries.pl
  ## 3 Jan 2022: Needed to add '&level=1000mb&level2=1000mb' to meet server requirements to retrieve data (should be irrelevant for 2-m air, though):
  writBase <- sprintf("https://psl.noaa.gov/cgi-bin/data/atmoswrit/timeseries.proc.pl?dataset1=@@SERIES@@&var=@@VAR@@&fyear=1840&fyear2=%s&fmonth=0&fmonth2=11&xlat1=@@LAT1@@&xlat2=@@LAT2@@&xlon1=@@LON1@@&xlon2=@@LON2@@&maskx=@@MASK@@&level=1000mb&level2=1000mb", current_year_lagged) # N.B. Change this back in Feb 2024!!
  reanalyses <- list(
    `NCEP/CFSR` = sub("@@SERIES@@", "NCEP%2FCFSR", writBase),
    `JRA-55` = sub("@@SERIES@@", "JRA-55", writBase),
    `JRA-3Q` = sub("@@SERIES@@", "JRA-3Q", writBase),
    `ERA5` = sub("@@SERIES@@", "ERA5", writBase),
    `NCEP/NCAR R1` = sub("@@SERIES@@", "NCEP%2FNCAR+R1", writBase),
    `NCEP/DOE R2` = sub("@@SERIES@@", "NCEP%2FDOE+R2", writBase),
    `MERRA-2` = sub("@@SERIES@@", "MERRA-2", writBase),
    `20th C. Reanalysis V3` = sub("@@SERIES@@", "20th+Century+Reanalysis+V3", writBase)
    #`20th C. Reanalysis V3` = sub("@@SERIES@@", "20th+Century+Reanalysis+V2c", writBase) # Temporary
    #`ERA-20C` = sub("@@SERIES@@", "ERA20C", writBase),
    #`COBE-SST 2` = = sub("@@SERIES@@", "COBE-SST+2", writBase),
    #`JMA` = sub("@@SERIES@@", "JMA+Temperature", writBase)
  )

  reanalysisSeriesSuffixes <- c(
    "Surface Air SH",
    "Surface Air SH Polar",
    "Surface Air NH",
    "Surface Air NH Polar",
    "Surface Air Global",
    "Surface Air Tropics",
    "Surface Air USA 48",
    "Sea Surface SH",
    "Sea Surface NH",
    "Sea Surface Global"
  )

  reanalysisMaskSuffixes <- c("All" = 0, "Land" = 1, "Ocean" = 2)

  r <- sapply(names(reanalyses),
    function(a)
    {
      uri <- reanalyses[[a]]

      r <- list(
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-180", "180", "0", "-90")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-180", "180", "-60", "-90")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-180", "180", "90", "0")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-180", "180", "90", "60")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-180", "180", "90", "-90")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-180", "180", "20", "-20")),
        ## For the US: https://www.quora.com/What-is-the-longitude-and-latitude-of-a-bounding-box-around-the-continental-United-States
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("2m+Air+Temperature", "-125", "-70", "50", "25")),
        ## SSTs
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("Sea+Surface%2FSkin+Temp", "-180", "180", "0", "-90")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("Sea+Surface%2FSkin+Temp", "-180", "180", "90", "0")),
        mgsub::mgsub(uri, c("@@VAR@@", "@@LON1@@", "@@LON2@@", "@@LAT1@@", "@@LAT2@@"), c("Sea+Surface%2FSkin+Temp", "-180", "180", "90", "-90"))
      )
      names(r) <- paste(a, reanalysisSeriesSuffixes)

      r
    }, simplify = FALSE)

  ## Flatten list 'r' to a single level.
  ## These don't quite work: rr <- lapply(r, rapply, f = c); rr <- rlist::list.flatten(r)
  rr <- purrr::flatten(r)

  ## Now apply different masks:
  rrr <- sapply(names(rr),
    function(a) {
      r <- sapply(names(reanalysisMaskSuffixes),
        function(b) {
          mask <- reanalysisMaskSuffixes[b]
          mgsub::mgsub(rr[[a]], "@@MASK@@", mask)
        }, simplify = FALSE)

      names(r) <- paste0(a, c("", " Land", " Ocean"))

      #browser()
      r
    }, simplify = FALSE)

  rv <- purrr::flatten(rrr)
  ## Remove the unecessary/paradoxical SST series
  rv <- rv[names(rv) %>% stringr::str_detect("Sea Surface.+?(Land|Ocean)$", negate = TRUE)]
  rv <- rv[names(rv) %>% stringr::str_detect("^(JRA-55|JRA-3Q|ERA5|NCEP/CFSR|MERRA-2).*?Sea Surface", negate = TRUE)]

  rv
}
reanalysis_urls <- make_reanalysis_urls()
## Make list of switch strings from these URLs:
# cat(backtick(names(reanalysis_urls)) %_% " =,", sep = "\n")

#' @rdname constants
#' @export
#data_urls <- c(reanalysis_urls, list( # To test or add the reanalysis series first
data_urls <- c(list(
  ## N.B. Need to read directory to get latest file name!
  #`PMOD TSI` = list(path = "ftp://ftp.pmodwrc.ch/pub/data/irradiance/virgo/TSI/VIRGO_TSI_Daily_V8_20230411.zip", type = "solar"), # 1996– (daily)
  ## AIRS
  `AIRS Zonal` = gistempBaseV4 %_% "T_AIRS/ZonAnn.Ts+dSST.csv",
  `AIRS Global` = gistempBaseV4 %_% "T_AIRS/GLB.Ts+dSST.csv",
  `AIRS NH` = gistempBaseV4 %_% "T_AIRS/NH.Ts+dSST.csv",
  `AIRS SH` = gistempBaseV4 %_% "T_AIRS/SH.Ts+dSST.csv",
  ## http://gravis.gfz-potsdam.de/antarctica (also new COST-G series)
  `GRACE-FO Antarctic Ice Mass` = list(path = graceFoBase %_% "AIS/GFZOP/", type = "land ice"),
  ## http://gravis.gfz-potsdam.de/greenland (also new COST-G series)
  `GRACE-FO Greenland Ice Mass` = list(path = graceFoBase %_% "GIS/GFZOP/", type = "land ice"),
  `HadCET` = "https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt",
  `NCEI Ocean Heat Content` = list(path = noaaOhcBase, type = "OHC"),
  ## On failure check here: https://climate.copernicus.eu/surface-air-temperature-maps
  #`ERA5 2m` = eraInterim2mTempBase %_% "@@YEARNUM_LASTMONTH@@/@@MONTHNUM_LASTMONTH@@/ERA5_1991-2020/ts_1month_anomaly_Global_ERA5_2t_@@YEARNUM_LASTMONTH@@@@MONTHNUM_LASTMONTH@@_1991-2020_v01.1.csv",
  `ERA5 2m Global` = eraInterim2mTempBase %_% "@@YEARNUM@@-@@MONTHNUM@@/C3S_Bulletin_temp_@@YEARNUM_LASTMONTH@@@@MONTHNUM_LASTMONTH@@_Fig1b_timeseries_anomalies_ref1991-2020_global_allmonths_data.csv",
  `ERA5 2m Europe` = eraInterim2mTempBase %_% "@@YEARNUM@@-@@MONTHNUM@@/C3S_Bulletin_temp_@@YEARNUM_LASTMONTH@@@@MONTHNUM_LASTMONTH@@_Fig4b_timeseries_anomalies_ref1991-2020_Europe_allmonths_data.csv",
  #`ERA-Interim 2m Global` = "http://climexp.knmi.nl/data/ierai_t2m_0-360E_-90-90N_n_su.dat",
  #`ERA5 2m Global` = "http://climexp.knmi.nl/data/iera5_t2m_0-360E_-90-90N_n_su.dat",
  ## Check here in case of failure of ERA5 sea ice: https://climate.copernicus.eu/sea-ice-cover-march-2020 etc.
  #`ERA5 Sea Ice Extent` = list(path = eraInterim2mTempBase %_% "@@YEARNUM@@-@@MONTHNUM@@/ts_1month_anomaly_polar_ea_CIA_@@YEARNUM_LASTMONTH@@@@MONTHNUM_LASTMONTH@@_v01.csv", type = "sea ice"),
  `ESRL AMO` = list(path = "https://www.esrl.noaa.gov/psd/data/correlation/amon.us.long.data", type = "AMO"),
  #`MODIS Aerosol Optical Thickness (550 nm)` = list(path = modisAodBase, type = "AOD"),
  `OSIRIS Stratospheric Aerosol Optical Depth (550 nm)` = list(path = "ftp://osirislevel2user:hugin@odin-osiris.usask.ca/Level2/daily/", type = "SAOD"),
  `Multivariate ENSO Index` = list(path = "https://www.esrl.noaa.gov/psd/enso/mei/data/meiv2.data", type = "ENSO"),
  `Extended Multivariate ENSO Index` = list(path = "http://www.esrl.noaa.gov/psd/enso/mei.ext/table.ext.html", type = "ENSO"),
  ## Land Ice Mass (v. https://climate.nasa.gov/vital-signs/land-ice/)
  `Antarctica Land Ice Mass Variation` = list(path = nasaLandIceMassBase %_% "ANTARCTICA_MASS_TELLUS_MASCON_CRI_TIME_SERIES_RL06.3_V4/antarctica_mass_200204_202412.txt", type = "land ice"),
  `Greenland Land Ice Mass Variation` = list(path = nasaLandIceMassBase %_% "GREENLAND_MASS_TELLUS_MASCON_CRI_TIME_SERIES_RL06.3_V4/greenland_mass_200204_202412.txt", type = "land ice"),
  `Ocean Mass Variation` = list(path = nasaOceanMassBase %_% "OCEAN_MASS_TELLUS_MASCON_CRI_TIME_SERIES_RL06.3_V4/ocean_mass_200204_202412.txt", type = "ocean mass"),
  ## GISTEMP v3
  `GISTEMP v3 Global` = gistempBaseV3 %_% "GLB.Ts+dSST.csv",
  `GISTEMP v3 SH` = gistempBaseV3 %_% "SH.Ts+dSST.csv",
  `GISTEMP v3 NH` = gistempBaseV3 %_% "NH.Ts+dSST.csv",
  `GISTEMP v3 Global Land` = gistempBaseV3 %_% "GLB.Ts.csv",
  `GISTEMP v3 SH Land` = gistempBaseV3 %_% "SH.Ts.csv",
  `GISTEMP v3 NH Land` = gistempBaseV3 %_% "NH.Ts.csv",
  `GISTEMP v3 Zonal` = gistempBaseV3 %_% "ZonAnn.Ts+dSST.csv",
  `GISTEMP v3 Zonal Land` = gistempBaseV3 %_% "ZonAnn.Ts.csv",
  ## GISTEMP v4
  `GISTEMP v4 Global` = gistempBaseV4 %_% "GLB.Ts+dSST.csv",
  `GISTEMP v4 SH` = gistempBaseV4 %_% "SH.Ts+dSST.csv",
  `GISTEMP v4 NH` = gistempBaseV4 %_% "NH.Ts+dSST.csv",
  `GISTEMP v4 Global Land` = gistempBaseV4 %_% "GLB.Ts.csv",
  `GISTEMP v4 SH Land` = gistempBaseV4 %_% "SH.Ts.csv",
  `GISTEMP v4 NH Land` = gistempBaseV4 %_% "NH.Ts.csv",
  `GISTEMP v4 Zonal` = gistempBaseV4 %_% "ZonAnn.Ts+dSST.csv",
  `GISTEMP v4 Zonal Land` = gistempBaseV4 %_% "ZonAnn.Ts.csv",
  ## NCEI (N.B. All the NCEI URLs need attention!)
  `NCEI Global` = nceiBase %_% "global/time-series/globe/tavg/land_ocean" %_% nceiGlobalMonthly,
  `NCEI SH` = nceiBase %_% "global/time-series/shem/tavg/land_ocean" %_% nceiGlobalMonthly,
  `NCEI NH` = nceiBase %_% "global/time-series/nhem/tavg/land_ocean" %_% nceiGlobalMonthly,
  `NCEI Global Land` = nceiBase %_% "global/time-series/globe/tavg/land" %_% nceiGlobalMonthly,
  `NCEI SH Land` = nceiBase %_% "global/time-series/shem/tavg/land" %_% nceiGlobalMonthly,
  `NCEI NH Land` = nceiBase %_% "global/time-series/nhem/tavg/land" %_% nceiGlobalMonthly,
  `NCEI Global Ocean` = nceiBase %_% "global/time-series/globe/tavg/ocean" %_% nceiGlobalMonthly,
  `NCEI SH Ocean` = nceiBase %_% "global/time-series/shem/tavg/ocean" %_% nceiGlobalMonthly,
  `NCEI NH Ocean` = nceiBase %_% "global/time-series/nhem/tavg/ocean" %_% nceiGlobalMonthly,
  `NCEI US Avg. Temp.` = nceiBase %_% "national/time-series/110/tavg" %_% nceiUsMonthly, # Schema "110/00" appears to be "region/division".
  `NCEI US Max. Temp.` = nceiBase %_% "national/time-series/110/tmax" %_% nceiUsMonthly,
  `NCEI US Min. Temp.` = nceiBase %_% "national/time-series/110/tmin" %_% nceiUsMonthly,
  `NCEI US Precip.` = list(path = nceiBase %_% "national/time-series/110/pcp" %_% nceiUsMonthly, type = "precipitation"),
  `NCEI US PDSI` = list(path = nceiBase %_% "national/time-series/110/pdsi" %_% nceiUsMonthly, type = "drought"),
  `NCEI US PHDI` = list(path = nceiBase %_% "national/time-series/110/phdi" %_% nceiUsMonthly, type = "drought"),
  `NCEI US PMDI` = list(path = nceiBase %_% "national/time-series/110/pmdi" %_% nceiUsMonthly, type = "drought"),
  `NCEI US Palmer Z-Index` = list(path = nceiBase %_% "national/time-series/110/zndx" %_% nceiUsMonthly, type = "drought"),
  ## USCRN (individual sites)
  ## https://www1.ncdc.noaa.gov/pub/data/uscrn/products/monthly01/
  ## ERSSTv4
  #ERSSTv4 = "ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/timeseries/",
  ## ERSSTv5
  #`NCEI v4` = "ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/timeseries/",
  `NCEI v4` = "https://www.ncei.noaa.gov/data/noaa-global-surface-temperature/v6/access/timeseries/",
  ## https://www.ncdc.noaa.gov/data-access/marineocean-data/extended-reconstructed-sea-surface-temperature-ersst-v5
  ## Single file: ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst.v5/sst.mnmean.nc
  ## ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/v5/beta/
  ## Hadley
  `CRUTEM4 Global` = crutemBase %_% "CRUTEM4-gl.dat",
  `CRUTEM4 NH` = crutemBase %_% "CRUTEM4-nh.dat",
  `CRUTEM4 SH` = crutemBase %_% "CRUTEM4-sh.dat",
  `CRUTEM4v Global` = crutemBase %_% "CRUTEM4v-gl.dat",
  `CRUTEM4v NH` = crutemBase %_% "CRUTEM4v-nh.dat",
  `CRUTEM4v SH` = crutemBase %_% "CRUTEM4v-sh.dat",
  `HadCRUT4 Global` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_ns_avg.txt",
  `HadCRUT4 SH` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_sh.txt",
  `HadCRUT4 NH` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_nh.txt",
  `HadCRUT4 Tropics` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_30S_30N.txt",
  `HadSST3 Global` = hadsstBaseV3 %_% "HadSST.3.1.1.0_monthly_globe_ts.txt",
  `HadSST3 SH` = hadsstBaseV3 %_% "HadSST.3.1.1.0_monthly_sh_ts.txt",
  `HadSST3 NH` = hadsstBaseV3 %_% "HadSST.3.1.1.0_monthly_nh_ts.txt",
  `HadSST3 Tropics` = hadsstBaseV3 %_% "HadSST.3.1.1.0_monthly_tropics_ts.txt",
  `HadSST4 Global` = hadsstBaseV4 %_% "HadSST.4.1.0.0_monthly_GLOBE.csv",
  `HadSST4 SH` = hadsstBaseV4 %_% "HadSST.4.1.0.0_monthly_SHEM.csv",
  `HadSST4 NH` = hadsstBaseV4 %_% "HadSST.4.1.0.0_monthly_NHEM.csv",
  `HadSST4 Tropics` = hadsstBaseV4 %_% "HadSST.4.1.0.0_monthly_TROP.csv",
  ## https://crudata.uea.ac.uk/cru/data/temperature/
  ## Hadley v5
  `CRUTEM5 Global` = crutem5Base %_% "CRUTEM.5.0.2.0.summary_series.global.monthly.nc",
  `CRUTEM5 NH` = crutem5Base %_% "CRUTEM.5.0.2.0.summary_series.northern_hemisphere.monthly.nc",
  `CRUTEM5 SH` = crutem5Base %_% "CRUTEM.5.0.2.0.summary_series.southern_hemisphere.monthly.nc",
  `HadCRUT5 Global` = hadcrut5Base %_% "HadCRUT.5.0.2.0.analysis.summary_series.global.monthly.nc",
  `HadCRUT5 SH` = hadcrut5Base %_% "HadCRUT.5.0.2.0.analysis.summary_series.southern_hemisphere.monthly.nc",
  `HadCRUT5 NH` = hadcrut5Base %_% "HadCRUT.5.0.2.0.analysis.summary_series.northern_hemisphere.monthly.nc",
  `HadCRUT5 Global (not infilled)` = hadcrut5NonInfilledBase %_% "HadCRUT.5.0.2.0.summary_series.global.monthly.nc",
  `HadCRUT5 SH (not infilled)` = hadcrut5NonInfilledBase %_% "HadCRUT.5.0.2.0.summary_series.southern_hemisphere.monthly.nc",
  `HadCRUT5 NH (not infilled)` = hadcrut5NonInfilledBase %_% "HadCRUT.5.0.2.0.summary_series.northern_hemisphere.monthly.nc",
  ## Cowtan & Way
  `Cowtan & Way Krig. Global` = cowtanWayBase %_% "had4_krig_v2_0_0.txt",
  `Cowtan & Way Krig. Global Land` = cowtanWayBase %_% "cru4_krig_v2_0_0.txt",
  ## BEST
  `BEST Global` = bestBase %_% "Global/Land_and_Ocean_complete.txt",
  `BEST Global Land` = bestBase %_% "Global/Complete_TAVG_complete.txt",
  `BEST SH Land` = bestBase2 %_% "Regional/TAVG/Text/southern-hemisphere-TAVG-Trend.txt",
  `BEST NH Land` = bestBase2 %_% "Regional/TAVG/Text/northern-hemisphere-TAVG-Trend.txt",
  `BEST US` = bestBase2 %_% "Regional/TAVG/Text/contiguous-united-states-TAVG-Trend.txt",
  `BEST Antarctica` = bestBase2 %_% "Regional/TAVG/Text/antarctica-TAVG-Trend.txt",
  `BEST Greenland` = bestBase2 %_% "Regional/TAVG/Text/greenland-TAVG-Trend.txt",
  ## Berkeley Earth land+SST Gridded (NH, SH)
  ## N.B. Before full update, run 'create_zonal_data(x = NULL, what = "be", use_local = FALSE) -> dev_null'
  `BEST Gridded` = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Gridded/Land_and_Ocean_LatLong1.nc",
  # http://berkeleyearth.org/data/
  `JMA Global` = "http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/list/csv/mon_wld.csv",
  #`JMA Global (gridded)` = "http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/map/grid/gst_mon_1891_last.gz",
  ## COBE SST? https://www.data.jma.go.jp/gmd/kaiyou/english/long_term_sst_global/glb_warm_e.html
  ## NOAA STAR? https://www.star.nesdis.noaa.gov/smcd/emb/mscat/products.php
  #`STAR v5.0` = "ftp://ftp.star.nesdis.noaa.gov/pub/smcd/emb/mscat/data/MSU_AMSU_v5.0/Monthly_Atmospheric_Layer_Mean_Temperature/Global_Mean_Anomaly_Time_Series/",
  `STAR v5.0` = "https://www.star.nesdis.noaa.gov/data/mscat/MSU_AMSU_v5.0/Monthly_Atmospheric_Layer_Mean_Temperature/Global_Mean_Anomaly_Time_Series/",
  ## RSS
  `RSS TLS 3.3` = rssBase %_% rssTls %_% "Land_and_Ocean_v03_3.txt",
  `RSS TLS 3.3 Land` = rssBase %_% rssTls %_% "Land_v03_3.txt",
  `RSS TLS 3.3 Ocean` = rssBase %_% rssTls %_% "Ocean_v03_3.txt",
  `RSS TLS 4.0` = rssBase %_% rssTls %_% "Land_and_Ocean_v04_0.txt",
  `RSS TLS 4.0 Land` = rssBase %_% rssTls %_% "Land_v04_0.txt",
  `RSS TLS 4.0 Ocean` = rssBase %_% rssTls %_% "Ocean_v04_0.txt",
  `RSS TLT 3.3` = rssBase %_% rssTlt %_% "Land_and_Ocean_v03_3.txt",
  `RSS TLT 3.3 Land` = rssBase %_% rssTlt %_% "Land_v03_3.txt",
  `RSS TLT 3.3 Ocean` = rssBase %_% rssTlt %_% "Ocean_v03_3.txt",
  `RSS TLT 4.0` = rssBase %_% rssTlt %_% "Land_and_Ocean_v04_0.txt",
  `RSS TLT 4.0 Land` = rssBase %_% rssTlt %_% "Land_v04_0.txt",
  `RSS TLT 4.0 Ocean` = rssBase %_% rssTlt %_% "Ocean_v04_0.txt",
  `RSS TMT 3.3` = rssBase %_% rssTmt %_% "Land_and_Ocean_v03_3.txt",
  `RSS TMT 3.3 Land` = rssBase %_% rssTmt %_% "Land_v03_3.txt",
  `RSS TMT 3.3 Ocean` = rssBase %_% rssTmt %_% "Ocean_v03_3.txt",
  `RSS TMT 4.0` = rssBase %_% rssTmt %_% "Land_and_Ocean_v04_0.txt",
  `RSS TMT 4.0 Land` = rssBase %_% rssTmt %_% "Land_v04_0.txt",
  `RSS TMT 4.0 Ocean` = rssBase %_% rssTmt %_% "Ocean_v04_0.txt",
  `RSS TTS 3.3` = rssBase %_% rssTts %_% "Land_and_Ocean_v03_3.txt",
  `RSS TTS 3.3 Land` = rssBase %_% rssTts %_% "Land_v03_3.txt",
  `RSS TTS 3.3 Ocean` = rssBase %_% rssTts %_% "Ocean_v03_3.txt",
  `RSS TTS 4.0` = rssBase %_% rssTts %_% "Land_and_Ocean_v04_0.txt",
  `RSS TTS 4.0 Land` = rssBase %_% rssTts %_% "Land_v04_0.txt",
  `RSS TTS 4.0 Ocean` = rssBase %_% rssTts %_% "Ocean_v04_0.txt",
  `RSS TTT 3.3` = rssBase %_% rssTtt %_% "Land_and_Ocean_v03_3.txt",
  `RSS TTT 3.3 Land` = rssBase %_% rssTtt %_% "Land_v03_3.txt",
  `RSS TTT 3.3 Ocean` = rssBase %_% rssTtt %_% "Ocean_v03_3.txt",
  `RSS TTT 4.0` = rssBase %_% rssTtt %_% "Land_and_Ocean_v04_0.txt",
  `RSS TTT 4.0 Land` = rssBase %_% rssTtt %_% "Land_v04_0.txt",
  `RSS TTT 4.0 Ocean` = rssBase %_% rssTtt %_% "Ocean_v04_0.txt",
  ## UAH
  `UAH TLS 5.6` = uahBase %_% "t4/uahncdc_ls_5.6.txt",
  `UAH TLT 5.6` = uahBase %_% "t2lt/uahncdc_lt_5.6.txt",
  `UAH TMT 5.6` = uahBase %_% "t2/uahncdc_mt_5.6.txt",
  `UAH TLS 6.0` = uahBase %_% "v6.0/tls/uahncdc_ls_6.0.txt",
  `UAH TLT 6.0` = uahBase %_% "v6.0/tlt/uahncdc_lt_6.0.txt",
  `UAH TMT 6.0` = uahBase %_% "v6.0/tmt/uahncdc_mt_6.0.txt",
  `UAH TTP 6.0` = uahBase %_% "v6.0/ttp/uahncdc_tp_6.0.txt",
  ## RATPAC
  `RATPAC-A Seasonal Layers` = "https://www1.ncdc.noaa.gov/pub/data/ratpac/ratpac-a/RATPAC-A-seasonal-layers.txt.zip", # Version 2; Version 1 is now deprecated (6 Sep. 2016).
  `RATPAC-A Annual Levels` = "https://www1.ncdc.noaa.gov/pub/data/ratpac/ratpac-a/RATPAC-A-annual-levels.txt.zip",
  ## NCEP
  ## N.B. These appear to be NCEP/NCAR R1 1000 mb temps, so I'll leave them out. I can get them from WRIT (above) if necessasry.
  # `NCEP/NCAR Surface Air SH` = sub("@@LAT1@@", "0", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  # `NCEP/NCAR Surface Air SH Polar` = sub("@@LAT1@@", "-60", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  # `NCEP/NCAR Surface Air NH` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "0", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  # `NCEP/NCAR Surface Air NH Polar` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "60", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  # `NCEP/NCAR Surface Air Global` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  # `NCEP/NCAR Surface Air Tropics` = sub("@@LAT1@@", "20", sub("@@LAT2@@", "-20", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  # ## For the US: https://www.quora.com/What-is-the-longitude-and-latitude-of-a-bounding-box-around-the-continental-United-States
  # `NCEP/NCAR Surface Air USA 48` = sub("@@LON1@@", "-125", sub("@@LON2@@", "-70", sub("@@LAT1@@", "50", sub("@@LAT2@@", "25", sub("@@VAR@@", "Air+Temperature", esrlBase))))),
  ## China: CMA-LSAT, dx.doi.org/10.1007/s00382-017-3755-1
  ## CO2
  `CO2 Mauna Loa` = list(path = "https://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/in_situ_co2/monthly/monthly_in_situ_co2_mlo.csv", type = "CO2"), # Mauna Loa CO2 series.
  `CO2 NOAA ESRL` = list(path = "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt", type = "CO2"),
  `CO2 Cape Grim` = list(path = "https://capegrim.csiro.au/GreenhouseGas/data/CapeGrim_CO2_data_download.csv", type = "CO2"),
  ## TODO: Add Cape Grim CH4 and N2O data (should be easy).
  `NSIDC Sea Ice` = list(path = "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135", type = "sea ice"),
  `OSI Sea Ice` = list(path = "ftp://osisaf.met.no/prod_test/ice/index/v2p2", type = "sea ice"),
  `PIOMAS Arctic Sea Ice Volume` = list(path = "http://psc.apl.uw.edu/wordpress/wp-content/uploads/schweiger/ice_volume/PIOMAS.2sst.monthly.Current.v2.1.txt", type = "sea ice"),
  #`PMOD TSI` = list(path = "ftp://ftp.pmodwrc.ch/pub/data/irradiance/composite/DataPlots/ext_composite_42_65_1605.dat", type = "solar"), # 1976–2016 (daily)
  `PMOD TSI` = list(path = "ftp://ftp.pmodwrc.ch/pub/data/irradiance/virgo/TSI/VIRGO_TSI_Daily_V8_20241227.txt", type = "solar"), # 1996– (daily)
  `TSI Reconstructed` = list(path = "https://spot.colorado.edu/~koppg/TSI/Historical_TSI_Reconstruction.txt", type = "solar"), # 1610–2018 (yearly)
  # `TSIS/TIM TSI` = list(path = "https://lasp.colorado.edu/data/tsis/tsi_data/tsis_tsi_L3_c24h_latest.txt", type = "solar") # Replaces SORCE; 2018– (daily)
  ## Also see: https://www.pmodwrc.ch/en/research-development/solar-physics/tsi-composite/
  ##   ftp://ftp.pmodwrc.ch/pub/data/irradiance/virgo/TSI/TSI_composite/MergedPMOD_NobaselineScaleCycle23_January2023.txt # 1980–
  #`SORCE TSI` = list(path = "http://lasp.colorado.edu/data/sorce/tsi_data/daily/sorce_tsi_L3_c24h_latest.txt", type = "solar"), # 2003–2020 (daily)
  ## N.B. Is this complete from 1610–present?
  #   https://www.ncei.noaa.gov/products/climate-data-records/total-solar-irradiance
  `Rutgers NH Snow Cover` = list(path = rutgerssnowBase %_% "nhland.txt", type = "snow"),
  `Rutgers Eurasia Snow Cover` = list(path = rutgerssnowBase %_% "eurasia.txt", type = "snow"),
  `Rutgers N. America Snow Cover` = list(path = rutgerssnowBase %_% "namgnld.txt", type = "snow"),
  `Rutgers N. America (No Greenland) Snow Cover` = list(path = rutgerssnowBase %_% "nam.txt", type = "snow"),
  `NOAA Sunspot No.` = list(path = "http://www.sidc.be/silso/DATA/SN_m_tot_V2.0.txt", type = "solar"),
  # http://www.sidc.be/silso/datafiles
  # https://www.ngdc.noaa.gov/stp/solar/ssndata.html
  `CSIRO Global Mean Sea Level` = list(path = "ftp://ftp.csiro.au/legresy/gmsl_files/CSIRO_Alt.csv", type = "sea level"), # Not updated monthly!
  #`NOAA Global Mean Sea Level` = list(path = "https://www.star.nesdis.noaa.gov/sod/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_txj1j2_90.csv", type = "sea level"),
  `NOAA Global Mean Sea Level` = list(path = "https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_ref_90.csv", type = "sea level"),
  `CSIRO Reconstructed Global Mean Sea Level` = list(path = "http://www.cmar.csiro.au/sealevel/downloads/church_white_gmsl_2011_up.zip", type = "sea level"),
  ## https://data.aviso.altimetry.fr/aviso-gateway/data/indicators/msl/
  #`AVISO Global Mean Sea Level` = list(path = "ftp://ftp.aviso.altimetry.fr/pub/oceano/AVISO/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_Adjust_Filter2m.txt", type = "sea level"),
  #`AVISO Global Mean Sea Level` = list(path = "https://data.aviso.altimetry.fr/aviso-gateway/data/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_NoAdjust_Filter2m_NRT.txt", type = "sea level"),
  `AVISO Global Mean Sea Level` = list(path = "ftp://ftp.aviso.altimetry.fr/pub/oceano/AVISO/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_NoAdjust_Filter2m_NRT.txt", type = "sea level"),
  `AVISO Global Mean Sea Level (nonseasonal)` = list(path = "ftp://ftp.aviso.altimetry.fr/pub/oceano/AVISO/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_Adjust_Filter2m_NRT.txt", type = "sea level"),
  ## Global average CO2 series:
  # https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_annmean_gl.txt
  # ftp://podaac.jpl.nasa.gov/allData/merged_alt/L2/TP_J1_OSTM/global_mean_sea_level/GMSL_TPJAOS_V4_199209_201704.txt
  # ftp://ftp.aviso.altimetry.fr/pub/oceano/AVISO/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_Adjust_Filter2m.txt
  # https://www.star.nesdis.noaa.gov/sod/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_all_66.csv
  # http://www.psmsl.org/products/reconstructions/jevrejevaetal2008.php
  `GISS Stratospheric Aerosol Optical Depth (550 nm)` = list(path = "http://data.giss.nasa.gov/modelforce/strataer/tau.line_2012.12.txt", type = "SAOD")
  #`AIRS Surface Skin Global` = "https://acdisc.gesdisc.eosdis.nasa.gov/data/Aqua_AIRS_Level3/AIRS3STM.006/"
  ## TODO: Ocean heat content. More snow and ice?
  # https://climatedataguide.ucar.edu/climate-data/ocean-temperature-analysis-and-heat-content-estimate-institute-atmospheric-physics
  # https://www.ncdc.noaa.gov/snow-and-ice/
  # https://www.ncdc.noaa.gov/teleconnections/pdo/data.csv
  # http://research.jisao.washington.edu/pdo/PDO.latest.txt / ftp://ftp.atmos.washington.edu/mantua/pnw_impacts/INDICES/PDO.latest
  # NINO indices: http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices
  # CONUS tornadoes: http://www.spc.noaa.gov/wcm/data/1950-2016_all_tornadoes.csv
  # Methane: https://www.esrl.noaa.gov/gmd/ccgg/trends_ch4/
  # Latest TSI: http://lasp.colorado.edu/data/sorce/tsi_data/daily/sorce_tsi_L3_c24h_latest.txt
  # NOAA STAR: ftp://ftp.star.nesdis.noaa.gov/pub/smcd/emb/mscat/data/MSU_AMSU_v4.0/Monthly_Atmospheric_Layer_Mean_Temperature/
  # MASIE sea ice: ftp://sidads.colorado.edu/DATASETS/NOAA/G02186/masie_4km_allyears_extent_sqkm.csv
  # Australia: http://www.bom.gov.au/web01/ncc/www/cli_chg/timeseries/tmean/allmonths/aus/latest.txt
  # JRA-55 reanalysis: https://jra.kishou.go.jp/JRA-55/index_en.html
  #   https://s-rip.ees.hokudai.ac.jp/resources/links.html
  #   ftp://ds.data.jma.go.jp/JRA-55/Hist/Monthly/anl_surf125
  #   https://www.esrl.noaa.gov/psd/cgi-bin/data/testdap/timeseries.pl # This is probably the easiest if it works!
  # ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/
  # GRACE-FO:
  #   http://gravis.gfz-potsdam.de/csvdata/GIS/dm/ # Greenland Ice Mass
  #   http://gravis.gfz-potsdam.de/csvdata/AIS/dm/ # Antarctica Ice Mass
  #   ftp://isdcftp.gfz-potsdam.de/grace/GravIS/GFZ/Level-3/ICE/
  # Historical sea ice:
  #   https://nsidc.org/cryosphere/sotc/sea_ice.html
  # AVISO SLR: ftp://ftp.aviso.altimetry.fr/pub/oceano/AVISO/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_Adjust_Filter2m.txt
  # Ocean acidification in the global ocean: https://www.data.jma.go.jp/gmd/kaiyou/english/co2_flux/co2_flux_data_en.html
), reanalysis_urls)

## Omit by default some series whose downloading or processing takes a very long time.
omitUrlNames <- c(
  "OSIRIS Stratospheric Aerosol Optical Depth (550 nm)"
  #"AIRS Surface Skin Global"
  #"MODIS Aerosol Optical Thickness (550 nm)"
)

#' @rdname constants
#' @export
common_columns <- c("year", "met_year", "yr_part", "month")
