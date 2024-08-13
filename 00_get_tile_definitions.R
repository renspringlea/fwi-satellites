library(terra)
library(ggplot2)
library(tidyterra)

tiles <- vect("~/Downloads/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00.kml")

entry <- which(tiles$Name=="44QND")
tiles$description[entry]

17.184811575 81 
17.182620081 81.940348288 
16.278763497 81.935926453 
16.280833323 81
