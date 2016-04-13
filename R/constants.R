#' @export
`%_%` <- function(a, b, sep='') paste(a, b, sep=sep)


## http://rrubyperlundich.blogspot.com/2011/07/r-generate-vector-with-names-of-months.html
#' @export
MOS <- format(ISOdatetime(2000, 1:12, 1, 0, 0, 0), "%b")
#' @export
MONTHS <- month.name

#' @export
currentMonth <- as.integer(format(Sys.Date(), "%m"))
#' @export
currentYear <- as.integer(format(Sys.Date(), "%Y"))

#' @export
dataDir <- "./data"
#' @export
filenameBase <- "climate-series_"

#' @export
defaultBaseline <- 1981:2010

## Some climatological time-series base URLs.
gistempBase <- "http://data.giss.nasa.gov/gistemp/tabledata_v3/"
hadcrutBase <- "http://www.cru.uea.ac.uk/cru/data/temperature/"
rssBase <- "ftp://ftp.remss.com/msu/monthly_time_series/"
esrlBase <- "http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&level=2000&lon1=-180&lon2=180&iseas=0&mon1=0&mon2=11&iarea=1&typeout=1&Submit=Create+Timeseries&lat1=@@LAT1@@&lat2=@@LAT2@@&var=@@VAR@@"

#' @export
instrumentalUrls <- list( # Last updated 24 Feb. 2016.
  GISTEMP = gistempBase %_% "GLB.Ts+dSST.txt",
  `GISTEMP SH` = gistempBase %_% "SH.Ts+dSST.txt",
  `GISTEMP NH` = gistempBase %_% "NH.Ts+dSST.txt",
  NCEI = "http://www.ncdc.noaa.gov/cag/time-series/global/globe/land_ocean/p12/12/1880-2100.csv",
  # But...? ftp://ftp.ncdc.noaa.gov/pub/data/mlost/operational/products/aravg.mon.land_ocean.90S.90N.v3.5.4.201504.asc
  HadCRUT3 = hadcrutBase %_% "HadCRUT3-gl.dat",
  HadCRUT3v = hadcrutBase %_% "HadCRUT3v-gl.dat",
  ## TODO: Change HadCRUT4 URL and processing, and also include uncertainties:
  ## http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.4.0.0.monthly_ns_avg.txt
  HadCRUT4 = hadcrutBase %_% "HadCRUT4-gl.dat",
  `Cowtan & Way Hybrid` = "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/had4_krig_v2_0_0.txt",
  BEST = "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt",
  JMA = "http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/map/grid/gst_mon_1891_last.gz",
  `RSS TLT 3.3` = rssBase %_% "RSS_Monthly_MSU_AMSU_Channel_TLT_Anomalies_Land_and_Ocean_v03_3.txt",
  `RSS TMT 3.3` = rssBase %_% "RSS_Monthly_MSU_AMSU_Channel_TMT_Anomalies_Land_and_Ocean_v03_3.txt",
  `RSS TMT 4.0` = rssBase %_% "RSS_Monthly_MSU_AMSU_Channel_TMT_Anomalies_Land_and_Ocean_v04_0.txt",
  `UAH TLT 5.6` = "http://www.nsstc.uah.edu/data/msu/t2lt/tltglhmam_5.6.txt",
  `UAH TLT 6.0` = "http://vortex.nsstc.uah.edu/data/msu/v6.0beta/tlt/tltglhmam_6.0beta5", # Updated 8 February 2016 from "beta4".
  `RATPAC-A 850-300 mb` = "http://www1.ncdc.noaa.gov/pub/data/ratpac/ratpac-a/RATPAC-A-seasonal-layers.txt",
  `NCEP Surface Air SH` = sub("@@LAT1@@", "0", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlBase))),
  `NCEP Surface Air NH` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "0", sub("@@VAR@@", "Air+Temperature", esrlBase))),
  `NCEP Surface Air` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlBase))),
  Keeling = list(path="http://scrippsco2.ucsd.edu/sites/default/files/data/in_situ_co2/monthly_mlo.csv", type="CO2") # Mauna Loa CO2 series.
)

#' @export
commonColumns <- c("year", "met_year", "yr_part", "month")
