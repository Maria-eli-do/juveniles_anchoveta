# Maria Diaz Ortiz 
rm(list = ls()); gc(reset = T)

# Library ----------------------------------------------------------------------
library(devtools)
library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)

# Data Callao 2009--------------------------------------------------------------
dirCallao <- "rawdat/Muestreos biome por puertos/CALLAO/"
ndir <- dir(dirCallao)


for( t in seq_along(ndir)){
  filep <- file.path(dirCallao, ndir[t])
  nfiles <- dir(filep)
  for( i in seq_along(nfiles)){
    filep2 <- file.path(filep, nfiles[i])
    data   <- read_xls(path = filep2, sheet = 1, skip = 2)
    sub(".*?(\\d{2}\\.\\d{2}\\.\\d{4}).*", "\\1", nfiles[i])
    
  }
  
  
}


file1 <- "Callao01.05.2009.xls"
file2 <- "Callao02.05.2009.xls"
file3 <- "Callao03.05.2009.xls"
file4 <- "Callao04.05.2009.xls"
file5 <- "Callao05.05.2009.xls"
file6 <- "Callao06.05.2009.xls"
file7 <- "Callao07.05.2009.xls"
file8 <- "Callao08.05.2009.xls"
file9 <- "Callao09.05.2009.xls"
file10 <- "Pond(1). callao 17.05.2009.xls"
file11 <- "Pond[1]. callao 18.05.2009.xls"
file12 <- "Pond[1]. callao 19.05.2009.xls"
file13 <- "Pond(1). callao 20.05.2009.xls"
file14 <- "Pond[1]. callao 21.05.2009.xls"
file15 <- "Pond[1]. callao 01.06.2009.xls"
file16 <- "Pond callao 05[1].06.2009.xls"
file17 <- "Pon[1].o callao 06.06.2009.xls"
file18 <- "Pond(1).callao07.06.2009.xls"
file19 <- "Pond(1). callao 09.06.2009.xls"
file20 <- "Pond(1). callao 16.06.2009.xls"
file21 <- "Pond(1). 19.06.2009.xls"

data1 <- read_xls(path = file1, sheet = 1, skip = 2)
data2 <- read_xls(path = file2, sheet = 2, skip = 2)
data3 <- read_xls(path = file3, sheet = 2, skip = 2)
data4 <- read_xls(path = file4, sheet = 2, skip = 2)
data5 <- read_xls(path = file5, sheet = 2, skip = 2)
data6 <- read_xls(path = file6, sheet = 2, skip = 2)
data7 <- read_xls(path = file7, sheet = 2, skip = 2)
data8 <- read_xls(path = file8, sheet = 2, skip = 2)
data9 <- read_xls(path = file9, sheet = 2, skip = 2)
data10 <- read_xls(path = file10, sheet =2 , skip = 2)
data11 <- read_xls(path = file11, sheet =2 , skip = 2)
data12 <- read_xls(path = file12, sheet =2 , skip = 2)
data13 <- read_xls(path = file13, sheet =2 , skip = 2)
data14 <- read_xls(path = file14, sheet =2 , skip = 2)
data15 <- read_xls(path = file15, sheet =2 , skip = 2)
data16 <- read_xls(path = file16, sheet =2 , skip = 2)
data17 <- read_xls(path = file17, sheet =2 , skip = 2)
data18 <- read_xls(path = file18, sheet =2 , skip = 2)
data19 <- read_xls(path = file19, sheet =2 , skip = 2)
data20 <- read_xls(path = file20, sheet =2 , skip = 2)
data21 <- read_xls(path = file21, sheet =2 , skip = 2)

data <- rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9,
              data10, data11, data12, data13, data14, data15, data16, data17,
              data18, data19, data20, data21)
names(data)

