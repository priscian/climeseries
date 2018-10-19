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
`%_%` <- function(a, b, sep='') paste(a, b, sep=sep)

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

#' @rdname constants
#' @export
current_year <- as.integer(format(Sys.Date(), "%Y"))

dataDir <- "."
filenameBase <- "climate-series_"

defaultBaseline <- 1981:2010

## Some climatological time-series base URLs.
gistempBase <- "https://data.giss.nasa.gov/gistemp/tabledata_v3/"
nceiBase <- "https://www.ncdc.noaa.gov/cag/time-series/"
nceiGlobalMonthly <- "/p12/12/1880-2100.csv"; nceiUsMonthly <- "/p12/12/1895-2100.csv?base_prd=true&begbaseyear=1901&endbaseyear=2000"
hadcrutBase <- "http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/"
hadsstBase <- "http://www.metoffice.gov.uk/hadobs/hadsst3/data/HadSST.3.1.1.0/diagnostics/"
bestBase <- "http://berkeleyearth.lbl.gov/auto/"
rssBase <- "http://data.remss.com/msu/monthly_time_series/"
#rssBase <- "ftp://priscian%40gmail.com:priscian%40gmail.com@ftp.remss.com/msu/monthly_time_series/"
rssChannel <- "RSS_Monthly_MSU_AMSU_Channel_@@CHANNEL@@_Anomalies_"
rssTls <- sub("@@CHANNEL@@", "TLS", rssChannel)
rssTlt <- sub("@@CHANNEL@@", "TLT", rssChannel)
rssTmt <- sub("@@CHANNEL@@", "TMT", rssChannel)
rssTts <- sub("@@CHANNEL@@", "TTS", rssChannel)
rssTtt <- sub("@@CHANNEL@@", "TTT", rssChannel)
uahBase <- "http://www.nsstc.uah.edu/data/msu/"
esrlBase <- "https://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=1&level=2000&iseas=0&mon1=0&mon2=11&iarea=1&typeout=1&Submit=Create+Timeseries&lat1=@@LAT1@@&lat2=@@LAT2@@&lon1=@@LON1@@&lon2=@@LON2@@&var=@@VAR@@"
esrlLatOnlyBase <- sub("@@LON1@@", "-180", sub("@@LON2@@", "180", esrlBase))
## Start here: http://www.esrl.noaa.gov/psd/data/timeseries/
rutgerssnowBase <- "http://climate.rutgers.edu/snowcover/files/moncov."
modisAodBase <- "http://giovanni.gsfc.nasa.gov/giovanni/daac-bin/service_manager.pl?session=@@SESSIONID@@&service=ArAvTs&starttime=2000-03-01T00:00:00Z&endtime=@@DATE@@T23:59:59Z&data=MOD08_M3_6_Aerosol_Optical_Depth_Land_Ocean_Mean_Mean&portal=GIOVANNI&format=json"
## ERA-Interim 2m temperature
## https://climate.copernicus.eu/surface-air-temperature-maps
## https://confluence.ecmwf.int/display/CKB/How+to+download+ERA-Interim+data+from+the+ECMWF+data+archive
eraInterim2mTempBase <- "https://climate.copernicus.eu/sites/default/files/"
noaaOhcBase <- "http://data.nodc.noaa.gov/woa/DATA_ANALYSIS/3M_HEAT_CONTENT/DATA/basin/"
nasaLandIceMassBase <- "ftp://podaac-ftp.jpl.nasa.gov/allData/tellus/L3/mascon/RL05/JPL/CRI/mass_variability_time_series/"

#' @rdname constants
#' @export
data_urls <- list(
  `HadCET` = "https://www.metoffice.gov.uk/hadobs/hadcet/cetml1659on.dat",
  `NCEI Ocean Heat Content` = list(path=noaaOhcBase, type="OHC"),
  `ERA-Interim 2m` = eraInterim2mTempBase %_% "@@YEARNUM@@-@@MONTHNUM@@/ts_1month_anomaly_Global_ei_2T_@@YEARNUM_LASTMONTH@@@@MONTHNUM_LASTMONTH@@.csv",
  `ESRL AMO` = list(path="https://www.esrl.noaa.gov/psd/data/correlation/amon.us.long.data", type="AMO"),
  #`MODIS Aerosol Optical Thickness (550 nm)` = list(path=modisAodBase, type="AOD"),
  `OSIRIS Stratospheric Aerosol Optical Depth (550 nm)` = list(path="ftp://osirislevel2user:hugin@odin-osiris.usask.ca/Level2/daily/", type="SAOD"),
  `Multivariate ENSO Index` = list(path="http://www.esrl.noaa.gov/psd/enso/mei/table.html", type="ENSO"),
  `Extended Multivariate ENSO Index` = list(path="http://www.esrl.noaa.gov/psd/enso/mei.ext/table.ext.html", type="ENSO"),
  ## Land Ice Mass (v. https://climate.nasa.gov/vital-signs/land-ice/)
  `Antarctica Land Ice Mass Variation` = list(path=nasaLandIceMassBase %_% "antarctica_mass_", type="land ice"),
  `Greenland Land Ice Mass Variation` = list(path=nasaLandIceMassBase %_% "greenland_mass_", type="land ice"),
  `Ocean Mass Variation` = list(path=nasaLandIceMassBase %_% "ocean_mass_", type="ocean mass"),
  ## GISTEMP
  `GISTEMP Global` = gistempBase %_% "GLB.Ts+dSST.csv",
  `GISTEMP SH` = gistempBase %_% "SH.Ts+dSST.csv",
  `GISTEMP NH` = gistempBase %_% "NH.Ts+dSST.csv",
  `GISTEMP Global Land` = gistempBase %_% "GLB.Ts.csv",
  `GISTEMP SH Land` = gistempBase %_% "SH.Ts.csv",
  `GISTEMP NH Land` = gistempBase %_% "NH.Ts.csv",
  `GISTEMP Zonal` = gistempBase %_% "ZonAnn.Ts+dSST.csv",
  `GISTEMP Zonal Land` = gistempBase %_% "ZonAnn.Ts.csv",
  ## NCEI
  `NCEI Global` = nceiBase %_% "global/globe/land_ocean" %_% nceiGlobalMonthly,
  `NCEI SH` = nceiBase %_% "global/shem/land_ocean" %_% nceiGlobalMonthly,
  `NCEI NH` = nceiBase %_% "global/nhem/land_ocean" %_% nceiGlobalMonthly,
  `NCEI Global Land` = nceiBase %_% "global/globe/land" %_% nceiGlobalMonthly,
  `NCEI SH Land` = nceiBase %_% "global/shem/land" %_% nceiGlobalMonthly,
  `NCEI NH Land` = nceiBase %_% "global/nhem/land" %_% nceiGlobalMonthly,
  `NCEI Global Ocean` = nceiBase %_% "global/globe/ocean" %_% nceiGlobalMonthly,
  `NCEI SH Ocean` = nceiBase %_% "global/shem/ocean" %_% nceiGlobalMonthly,
  `NCEI NH Ocean` = nceiBase %_% "global/nhem/ocean" %_% nceiGlobalMonthly,
  `NCEI US Avg. Temp.` = nceiBase %_% "us/110/00/tavg" %_% nceiUsMonthly, # Schema "110/00" appears to be "region/division".
  `NCEI US Max. Temp.` = nceiBase %_% "us/110/00/tmax" %_% nceiUsMonthly,
  `NCEI US Min. Temp.` = nceiBase %_% "us/110/00/tmin" %_% nceiUsMonthly,
  `NCEI US Precip.` = list(path=nceiBase %_% "us/110/00/pcp" %_% nceiUsMonthly, type="precipitation"),
  `NCEI US PDSI` = list(path=nceiBase %_% "us/110/00/pdsi" %_% nceiUsMonthly, type="drought"),
  `NCEI US PHDI` = list(path=nceiBase %_% "us/110/00/phdi" %_% nceiUsMonthly, type="drought"),
  `NCEI US PMDI` = list(path=nceiBase %_% "us/110/00/pmdi" %_% nceiUsMonthly, type="drought"),
  `NCEI US Palmer Z-Index` = list(path=nceiBase %_% "us/110/00/zndx" %_% nceiUsMonthly, type="drought"),
  ## USCRN (individual sites)
  ## https://www1.ncdc.noaa.gov/pub/data/uscrn/products/monthly01/
  ## ERSSTv4
  ERSSTv4 = "ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/operational/timeseries/",
  ## ERSSTv5
  ## https://www.ncdc.noaa.gov/data-access/marineocean-data/extended-reconstructed-sea-surface-temperature-ersst-v5
  ## Single file: ftp://ftp.cdc.noaa.gov/Datasets/noaa.ersst.v5/sst.mnmean.nc
  ## ftp://ftp.ncdc.noaa.gov/pub/data/noaaglobaltemp/v5/beta/
  ## Hadley
  `HadCRUT4 Global` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_ns_avg.txt",
  `HadCRUT4 SH` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_sh.txt",
  `HadCRUT4 NH` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_nh.txt",
  `HadCRUT4 Tropics` = hadcrutBase %_% "HadCRUT.4.6.0.0.monthly_30S_30N.txt",
  `HadSST3 Global` = hadsstBase %_% "HadSST.3.1.1.0_monthly_globe_ts.txt",
  `HadSST3 SH` = hadsstBase %_% "HadSST.3.1.1.0_monthly_sh_ts.txt",
  `HadSST3 NH` = hadsstBase %_% "HadSST.3.1.1.0_monthly_nh_ts.txt",
  `HadSST3 Tropics` = hadsstBase %_% "HadSST.3.1.1.0_monthly_tropics_ts.txt",
  ## https://crudata.uea.ac.uk/cru/data/temperature/
  ## Cowtan & Way
  `Cowtan & Way Krig. Global` = "http://www-users.york.ac.uk/~kdc3/papers/coverage2013/had4_krig_v2_0_0.txt",
  ## BEST
  `BEST Global` = bestBase %_% "Global/Land_and_Ocean_complete.txt",
  `BEST Global Land` = bestBase %_% "Global/Complete_TAVG_complete.txt",
  `BEST SH Land` = bestBase %_% "Regional/TAVG/Text/southern-hemisphere-TAVG-Trend.txt",
  `BEST NH Land` = bestBase %_% "Regional/TAVG/Text/northern-hemisphere-TAVG-Trend.txt",
  `BEST US` = bestBase %_% "Regional/TAVG/Text/contiguous-united-states-TAVG-Trend.txt",
  #`BEST Antarctica` = bestBase %_% "Regional/TAVG/Text/antarctica-TAVG-Trend.txt", # Currently has all missing values for monthly anomalies.
  `BEST Greenland` = bestBase %_% "Regional/TAVG/Text/greenland-TAVG-Trend.txt",
  # http://berkeleyearth.org/data/
  `JMA Global` = "http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/list/csv/mon_wld.csv",
  #`JMA Global (gridded)` = "http://ds.data.jma.go.jp/tcc/tcc/products/gwp/temp/map/grid/gst_mon_1891_last.gz",
  ## NOAA STAR? https://www.star.nesdis.noaa.gov/smcd/emb/mscat/products.php
  ## RSS
  `RSS TLS 3.3` = rssBase %_% rssTls %_% "Land_and_Ocean_v03_3.txt",
  `RSS TLS 3.3 Land` = rssBase %_% rssTls %_% "Land_v03_3.txt",
  `RSS TLS 3.3 Ocean` = rssBase %_% rssTls %_% "Ocean_v03_3.txt",
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
  `RATPAC-A Seasonal Layers` = "http://www1.ncdc.noaa.gov/pub/data/ratpac/ratpac-a/RATPAC-A-seasonal-layers.txt.zip", # Version 2; Version 1 is now deprecated (6 Sep. 2016).
  `RATPAC-A Annual Levels` = "http://www1.ncdc.noaa.gov/pub/data/ratpac/ratpac-a/RATPAC-A-annual-levels.txt.zip",
  ## NCEP
  `NCEP Surface Air SH` = sub("@@LAT1@@", "0", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  `NCEP Surface Air SH Polar` = sub("@@LAT1@@", "-60", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  `NCEP Surface Air NH` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "0", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  `NCEP Surface Air NH Polar` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "60", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  `NCEP Surface Air Global` = sub("@@LAT1@@", "90", sub("@@LAT2@@", "-90", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  `NCEP Surface Air Tropics` = sub("@@LAT1@@", "20", sub("@@LAT2@@", "-20", sub("@@VAR@@", "Air+Temperature", esrlLatOnlyBase))),
  ## For the US: https://www.quora.com/What-is-the-longitude-and-latitude-of-a-bounding-box-around-the-continental-United-States
  `NCEP Surface Air USA 48` = sub("@@LON1", "-125", sub("@@LON2@@", "-70", sub("@@LAT1@@", "50", sub("@@LAT2@@", "25", sub("@@VAR@@", "Air+Temperature", esrlBase))))),
  ## CO2
  `CO2 Mauna Loa` = list(path="http://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/in_situ_co2/monthly/monthly_in_situ_co2_mlo.csv", type="CO2"), # Mauna Loa CO2 series.
  `CO2 NOAA ESRL` = list(path="ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt", type="CO2"),
  `CO2 Cape Grim` = list(path="http://capegrim.csiro.au/GreenhouseGas/data/CapeGrim_CO2_data_download.csv", type="CO2"),
  ## TODO: Add Cape Grim CH4 and N2O data (should be easy).
  `NSIDC Sea Ice` = list(path="ftp://sidads.colorado.edu/DATASETS/NOAA/G02135", type="sea ice"),
  `PIOMAS Arctic Sea Ice Volume` = list(path="http://psc.apl.uw.edu/wordpress/wp-content/uploads/schweiger/ice_volume/PIOMAS.2sst.monthly.Current.v2.1.txt", type="sea ice"),
  `PMOD TSI` = list(path="ftp://ftp.pmodwrc.ch/pub/data/irradiance/composite/DataPlots/ext_composite_42_65_1605.dat", type="solar"),
  `TSI Reconstructed` = list(path="http://spot.colorado.edu/~koppg/TSI/TIM_TSI_Reconstruction.txt", type="solar"),
  `SORCE TSI` = list(path="http://lasp.colorado.edu/data/sorce/tsi_data/daily/sorce_tsi_L3_c24h_latest.txt", type="solar"),
  `Rutgers NH Snow Cover` = list(path=rutgerssnowBase %_% "nhland.txt", type="snow"),
  `Rutgers Eurasia Snow Cover` = list(path=rutgerssnowBase %_% "eurasia.txt", type="snow"),
  `Rutgers N. America Snow Cover` = list(path=rutgerssnowBase %_% "namgnld.txt", type="snow"),
  `Rutgers N. America (No Greenland) Snow Cover` = list(path=rutgerssnowBase %_% "nam.txt", type="snow"),
  #`NOAA Sunspot No.` = list(path="http://solarscience.msfc.nasa.gov/greenwch/SN_m_tot_V2.0.txt", type="solar"),
  `NOAA Sunspot No.` = list(path="http://www.sidc.be/silso/DATA/SN_m_tot_V2.0.txt", type="solar"),
  # http://www.sidc.be/silso/datafiles
  # https://www.ngdc.noaa.gov/stp/solar/ssndata.html
  `CSIRO Global Mean Sea Level` = list(path="ftp://ftp.marine.csiro.au/pub/legresy/gmsl_files/CSIRO_Alt.csv", type="sea level"), # Not updated monthly!
  #`CSIRO Global Mean Sea Level` = list(path="http://www.hpc.csiro.au/users/326141/Sea_Level_data/ftp.marine.csiro.au/pub/legresy/gmsl_files/CSIRO_Alt.csv", type="sea level"),
  `NOAA Global Mean Sea Level` = list(path="https://www.star.nesdis.noaa.gov/sod/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_txj1j2_90.csv", type="sea level"),
  `CSIRO Reconstructed Global Mean Sea Level` = list(path="http://www.cmar.csiro.au/sealevel/downloads/church_white_gmsl_2011_up.zip", type="sea level"),
  # ftp://podaac.jpl.nasa.gov/allData/merged_alt/L2/TP_J1_OSTM/global_mean_sea_level/GMSL_TPJAOS_V4_199209_201704.txt
  # ftp://ftp.aviso.altimetry.fr/pub/oceano/AVISO/indicators/msl/MSL_Serie_MERGED_Global_AVISO_GIA_Adjust_Filter2m.txt
  # https://www.star.nesdis.noaa.gov/sod/lsa/SeaLevelRise/slr/slr_sla_gbl_keep_all_66.csv
  # http://www.psmsl.org/products/reconstructions/jevrejevaetal2008.php
  `GISS Stratospheric Aerosol Optical Depth (550 nm)` = list(path="http://data.giss.nasa.gov/modelforce/strataer/tau.line_2012.12.txt", type="SAOD")
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
)

## Omit by default some series whose downloading or processing takes a very long time.
omitUrlNames <- c(
  "OSIRIS Stratospheric Aerosol Optical Depth (550 nm)"
  #"MODIS Aerosol Optical Thickness (550 nm)"
)

#' @rdname constants
#' @export
common_columns <- c("year", "met_year", "yr_part", "month")
