# Cleaning and Assembly of Peat Collapse Data

The `peatcollapse` package contains functions to clean and assemble the various Peat Collapse project datasets into consistent summary files. These datasets include:

1. Conductivity, salinity, and temperature data collected `onsite` via YSI sonde 

2. Soil redox-potential readings collected `onsite`

3. Water quality chemistry samples processed in the SFWMD `lab`

4. Phosphorus samples processed in the SFWMD Everglades `lab`

5. Sulfide samples processed at the FBSIC `lab`

The ultimate goal is to create seperate "database" summary files for the Field Study and Key Largo Mesocosm Experiments. These summary files should be stand-alone and saved in an open non-proprietary file format such as csv. 


## Installing the `peatcollapse` package

```{r eval=FALSE}
install.packages(c("RSQLite", "readxl", "reshape2", "DBI"))
```

`devtools::install_git("http://gitlab.com/jsta/peatcollapse.git",`
`  credentials = git2r::cred_user_pass("<username>",`             `  getPass::getPass()))`

## Assembling Databases

From within the peat-collapse data directory:

```{r eval=FALSE}
field <- assemble_field(tofile = FALSE)

meso <- assemble_meso(tofile = FALSE)
```
