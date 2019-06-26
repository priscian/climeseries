## Disclaimer:  As much as I'd like to take credit for writing this,
##              I can't, because I didn't ;)
##              Credit goes to Kevin C at skepticalscience.com
##   (http://www.skepticalscience.com/watts_new_paper_critique.html)
##
## You will need Python to run this script. Python is available for
## free at www.python.org.  Install Python 2.7 (Python 3.X might not work
## with this script).
##
## Here's how to get the data and run the code
##
## (First, if you don't have python installed on your system,
##  go to www.python.org and install it.)
##
## 1) Download GHCN V3 data from ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v3/
##
##    The file ghcnm.tavg.latest.qca.tar.gz contains "homogenized" temperature data
##    The file ghcnm.tavg.latest.qcu.tar.gz contains raw temperature data.
##
##
## 2) Grab this file from the NASA/GISS web-site.
##
##     http://data.giss.nasa.gov/gistemp/graphs_v3/Fig.A.txt
##
##     This file contains the official results that NASA has computed
##     from the GHCN V3 data.  The second column in the file contains
##     the annual-average results.  These are the results you will want
##     to use as a baseline to compare your own results with.
##    
##
## 3) Unpack the files to extract the temperature data and metadata files 
##  
##   Unix/Linux: tar zxvf ghcnm.tavg.latest.qca.tar.gz
##               tar zxvf ghcnm.tavg.latest.gcu.tar.gz
##
##   Windows/Mac -- use your file-manager to launch the appropriate
##                  file unpacking utility.
##
##   A folder with a name of the form "ghcnm.v3.1.0.YYYYMMDD will be
##   created -- YYYYMMDD is the date that the data snapshot was released.
##
##   Inside that folder, you will find two files with the following filename format:
##
##                      ghcnm.tavg.v3.1.0.YYYYMMDD.qcu.inv
##                      ghcnm.tavg.v3.1.0.YYYYMMDD.qcu.dat
##
##    The YYYYMMDD part represents the release date of the particular data file.
##
##    Note: qcu.inv and qcu.dat files correspond to raw temperature data.
##          qca.inv and qca.dat files correspond to homogenized/adjusted temperature data.
##
##    The .dat files contain the actual temperature data.
##    The .inv files contain "metadata" (station latitude/longitudes, rural/urban
##    status, etc).
##
##
## 4) To run the python script with no command-line arguments, you will need
##    to rename the temperature metadata file to "v3.inv" and the data file
##    to "v3.mean".
##
##    i.e. for raw data, use your file manager to rename 
##                      ghcnm.tavg.v3.1.0.YYYYMMDD.qcu.inv to v3.inv
##                                  and
##                      ghcnm.tavg.v3.1.0.YYYYMMDD.qcu.dat to v3.mean
##
##    For adjusted/homogenized data, do the same, except that you
##    will be replacing "qcu" with "qca" in the above filenames.
##
## 5) If you rename the file ghchm.tavg.v3.1.0.YYYYMMDD.qcu.dat to v3.mean and
##    rename the file ghcnm.tavg.v3.1.0.YYYYMMDD.gcu.inv to v3.inv per step 4 
##    above, you can run the script with the following command (be sure that 
##    the script is in the same directory aka folder as your .inv and .dat files).
##
##    Open a terminal window -- i.e. for Windows, open a DOS command window.
##    Then cd to the folder where you have your
##    .dat and .inv files, and run the following command:
##
##    python ghcn-simple.py
##
##    The script will crunch on the data for a minute or two, depending on
##    how fast your computer is. Maybe less time, if you have a powerful
##    pc/laptop.
##    
##    The results will be dumped out to the terminal screen.
##
##    To put the results into a file that you can plot, do this:
##
##    python ghcn-simple.py > results.csv
##
##    The results will be saved in CSV format (for easy plotting with Excel
##    or OpenOffice).
##
##    The program crunches all (rural+suburban+urban) data by default.
##
##
## 6) You can "customize" your runs a bit with command-line arguments.
##
##    To crunch just rural data, do this:
##
##    python ghcn-simple.py v3.inv v3.mean R > rural.csv
##
##
##    To crunch just urban data, do this:
##
##    python ghcn-simple.py v3.inv v3.mean U > urban.csv
##
##    To crunch suburban data, do this:
##
##    python ghcn-simple.py v3.inv v3.mean S > suburban.csv
##
##
## 7) Try separate runs for different combos: raw, adjusted, raw and adjusted rural, 
##    raw and adjusted urban data.
##
##    Plot up your results and compare with the official NASA results that you
##    downloaded in step(2).
##
##

import sys, math

# constants
years  = range(1880,2012)
months = range(1,13)

# grid sampling in degrees
# The grid size is set to 30x30 degrees
# so we don't have to worry about interpolating
# to empty grid cells.
# The standard grid-cell size of 5x5 leaves enough
# empty grid cells in the Southern Hemisphere that the
# Northern Hemisphere gets overweighted in the average.
# This causes the results to show too much warming.
# Increasing the grid-cell size to 30x30 produces results
# much more in line with the NASA/GISS "meteorological
# stations" index.
grid   = 30  


# optional command line arguments: inventory-file data-file population-code
fileinv = "v3.inv" if len(sys.argv) <= 1 else sys.argv[1]
filedat = "v3.mean" if len(sys.argv) <= 2 else sys.argv[2]
popcls  = "RSU"     if len(sys.argv) <= 3 else sys.argv[3]

# station data, indexed by station id
station_locn = {}  # latitude, longitude stored by station id
station_data = {}  # temperature data stored by station id, year, month

print >> sys.stderr, " "
print >> sys.stderr, "Grid size: ",grid,"x",grid," degrees..."
print >> sys.stderr, " "

# read and store the station data
for line in open(fileinv):
  id = line[0:11]
  if line[73] in popcls:  # check rural/suburban/urban
    station_locn[id] = ( float(line[12:20]), float(line[21:30]) )  # location
    station_data[id] = {}  # initialise temperature data

print >> sys.stderr, "Just read in the station metadata..."
print >> sys.stderr, " "
print >> sys.stderr, "Now read in the station data (could take some time)..."
print >> sys.stderr, " "

# read and store the temperature data
for line in open(filedat):
  id = line[0:11]                                 # get station id
  if id in station_data:
    year = int( line[11:15] )                     # get year
    station_data[id][year] = {}
    for month in months:
      temp = int( line[11+8*month:16+8*month] )
      flag = line[16+8*month:19+8*month]
      if temp != -9999 and flag[0:2] == "  ":     # check for error flags
        station_data[id][year][month] = 0.01*temp  # if ok, store data
print >> sys.stderr, "Just read in the temperature data..."
print >> sys.stderr, "#Stations and #station locations below..."
print >> sys.stderr, len(station_locn), len(station_data)
print >> sys.stderr, " "

# Now calculate a baseline for each station
# This will be used to convert the temperature readings into anomalies.
# Without this step, missing months will add warming or cooling noise
# depending on the season and the introduction of new stations will
# add warming or cooling noise depending on whether the new stations
# are in hotter of colder regions.
print >> sys.stderr, "Now start crunching the baselines..."
print >> sys.stderr, " "

baselines = {}
baselineCount=0
baselineValid=0
for id in station_data:
  # calculate mean temp by month over the years 1951-1980
  mtemps = {}
  for month in months:
    mtemps[month] = []
  for year in station_data[id]:
    if year >= 1951 and year <= 1980:
      for month in station_data[id][year]:
        mtemps[month].append( station_data[id][year][month] )
  # calculate averages,
  # but only include data if we have a robust baseline for this id/month
  baselines[id] = {}
  baselineValid=0
  for month in months:
    if len(mtemps[month]) >= 15:
      baselines[id][month] = sum( mtemps[month] ) / len( mtemps[month] )
      baselineValid=1
  if baselineValid:
    baselineCount+=1

print >> sys.stderr, "Just finished calculating baselines..."
print >> sys.stderr, "#stations with valid baseline data = ", baselineCount
print >> sys.stderr, " "
print >> sys.stderr, "Now start crunching the anomalies (could take some time)..."
print >> sys.stderr, " "

# Calculate monthly averages over all grid cells and stations
monthly = {}
for year in years:
  print>>sys.stderr, "Crunching temperature data for year", year
  monthly[year] = {}
  for month in months:
    # make a map of grid cells with a list of anomalies in each cell
    map = [[ [] for i in range(360/grid)] for j in range(180/grid)]
    # fill in all the anomalies for this month in the map
    for id in station_data:
      # store a list of all the station anomalies in every cell in the map
      if year in station_data[id]:
        if month in station_data[id][year]:
          # only include data if we have a baseline
          if month in baselines[id]:
            # anomaly = temperature - baseline
            anom = station_data[id][year][month] - baselines[id][month]
            # add the anomaly to the list for the given map cell
            lati = int((station_locn[id][0]+90.0)/grid)
            lngi = int((station_locn[id][1]+180.0)/grid)
            map[lati][lngi].append(anom)
    # average over the cells
    stemp = 0.0
    swght = 0.0
    for lati in range(len(map)):
      w = math.cos(((lati+0.5)*grid-90.0)*3.1416/180.0)   # weight by cell area
      for lngi in range(len(map[lati])):
        if len( map[lati][lngi] ) > 0:
          t = sum( map[lati][lngi] ) / len( map[lati][lngi] )
          stemp += w*t
          swght += w
    stemp /= max( swght, 1.0e-20 )
    monthly[year][month] = stemp

print >> sys.stderr, " "
print >> sys.stderr, "All finished!!"
print >> sys.stderr, " "

print "year",",","temp"
for year in years:
  yearavg=0
  for month in months:
    yearavg=yearavg+monthly[year][month]
  yearavg=yearavg/len(months)
  print year,",", yearavg

