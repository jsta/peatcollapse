# Cleaning and Assembly of Peat Collapse Data

The `peatcollapse` package contains functions to clean and assemble the various Peat Collapse project datasets into consistent summary files. These datasets include:

1. Conductivity, salinity, and temperature data collected `onsite` via YSI sonde 

2. Soil redox-potential readings collected `onsite`

3. Water quality chemistry samples processed in the SFWMD `lab`

4. Phosphorus samples processed in the SFWMD Everglades `lab`

5. Sulfide samples processed at the FBSIC `lab`

The ultimate goal is to create seperate "database" summary files for the Field Study and Key Largo Mesocosm Experiments. These summary files should be stand-alone and saved in an open non-proprietary file format such as csv. 


## Installing the `peatcollapse` package

The `peatcollapse` `R` package is distributed via a .tar.gz (analagous to .zip) package archive file. This package contains the source code for package functions. In RStudio, it can be installed by navigating to Tools -> Install Packages... -> Install from: -> Package Archive File. Computers running the Windows operating system can only install binary .zip package archive files unless they have additional compiler software installed. The `peatcollapse` binary package can be installed by running the following commands from the R console:

```{r eval=FALSE}
install.packages(c("RSQLite", "readxl", "reshape2", "DBI"))
install.packages("peatcollapse_0.1-1.zip", type = "win.binary", repos = NULL)
```