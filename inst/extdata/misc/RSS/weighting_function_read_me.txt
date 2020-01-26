Read me file for files

std_atmosphere_wt_function_chan_2_ocean.txt
std_atmosphere_wt_function_chan_2_land.txt
std_atmosphere_wt_function_chan_3.txt
std_atmosphere_wt_function_chan_4.txt

These are text files containing weighting functions for MSU
channels 2,3,4.  We have assumed the US standard atmosphere, a
surface relative humidity of 70%, and a PV scale height of 1500m.

We do not provide weighting functions for channel 1 because
   1. We do not yet provide a channel 1 product
   2. The surface contribution and water vapor crosstalk are so large 
      that the "standard" weighting function concept becomes fatally 
      flawed.

For channel 2, the surface makes a significant contribution, and 
must be treated differently for land and ocean scenes.

We have assumed a surface emissivity of 0.90 for land, and have
used a comprehensive sea surface model to calculate the surface
properties for ocean, assuming a wind speed of 8 m/s.  The low
emissivity for the nadir and near nadir views for ocean scenes
decreases the surface weight and moves the peak of the weighting
function several hundred meters lower.

Each file contains 11 columns

Column 1               Level Number
Column 2               Level Height, in meters
Column 3               Level Temperature, in degrees Kelvin
Column 4               Level Pressure, in Pascals
Column 5               Level Vapor Pressure, in Pascals
Column 6               Weighting function, in km-1, for the nadir view, view 6
Column 7               Weighting function, in km-1, for views 5 and 7
Column 8               Weighting function, in km-1, for views 4 and 8
Column 9               Weighting function, in km-1, for views 3 and 9
Column 10              Weighting function, in km-1, for views 2 and 10
Column 11              Weighting function, in km-1, for the limb views, views 1 and 11

The last 3 rows contain the surface weight, and the computed brightness temperature using 
our full radiative transfer model, and computed brightness temeperatures using the provided
weighting functions.  The small differences between these two brightness temeperatures are
due to the approximation used to define the weighting function in the presence of a reflective
surface, and to differences in the integration scheme.

The weighting functions are given as instantaneous values at the given height.  To
calculate the total weight over a layer, one needs to calculate the average over that
layer and multiply by the layer thickness.

total_wt_between_level_minus_one_and_level = 

0.5*(wt_function(level) + wt_function(level-1))*(h(level)-h(level-1))

where h is in kilometers.


