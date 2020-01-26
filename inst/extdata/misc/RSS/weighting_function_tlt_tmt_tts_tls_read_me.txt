Read me file for files


std_atmosphere_wt_function_chan_TLT_land.txt
std_atmosphere_wt_function_chan_TLT_ocean.txt
std_atmosphere_wt_function_chan_TMT_land.txt
std_atmosphere_wt_function_chan_TMT_ocean.txt
std_atmosphere_wt_function_chan_TTS.txt
std_atmosphere_wt_function_chan_TLS.txt


These are text files containing weighting functions for MSU/AMSU products for
channels TLT, TMT, TTS, and TLS.  We have assumed the US standard atmosphere, a
surface relative humidity of 70%, and a PV scale height of 1500m.

For channels TLT and TMT, the surface makes a significant contribution, and 
must be treated differently for land and ocean scenes.

We have assumed a surface emissivity of 0.90 for land, and have
used a comprehensive sea surface model to calculate the surface
properties for ocean, assuming a wind speed of 8 m/s.  The low
emissivity for the nadir and near nadir views for ocean scenes
decreases the surface weight and moves the peak of the weighting
function several hundred meters lower.

The first several rows contain header information, and the surface
weight.

Each file contains 6 columns

Column 1               Level Number
Column 2               Level Height, in meters
Column 3               Level Temperature, in degrees Kelvin
Column 4               Level Pressure, in Pascals
Column 5               Level Vapor Pressure, in Pascals
Column 6               Weighting function, in km-1


The weighting functions are given as instantaneous values at the given height.  To
calculate the total weight over a layer, one needs to calculate the average over that
layer and multiply by the layer thickness.

total_wt_between_level_minus_one_and_level_one = 

0.5*(wt_function(level) + wt_function(level-1))*(h(level)-h(level-1))

where h is in kilometers.


