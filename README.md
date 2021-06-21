This repository contains files and codes used for the paper titled "Seasonal Prediction of Indian Summer Monsoon Onset with Echo State Networks"
by Mitsui and Niklas (2021).

- NCEP_Temperature.R
1. Make a directory 'NCEP_Temperature'
2. Download NCEP/NCAR reanalysis daily temerature data from air.1948.nc to air.2020.nc and put them into the directory 'NCEP_Temperature'
3. Running NCEP_Temperature.R makes a text file 'dTT_latest.txt', which contains the daily dTT index (below)

- dTT_latest.txt
  - Columns are dTT, year, day of year, T_N, T_S in this order

- onset_date.R
  - Running this script generates Fig. 1 and 'onset_date.txt'

- onset_date.txt
  - Columns are Year, onset date (day of year) in this order

- ESN_monsoon_demo.R
  - Demo script for the Echo State Network. Running this R-script generates Fig. 2.

- Fig3.R
  - generates Fig. 3 using 69.dat, which is made from the following out.R. 69 means that the prediction is taken placed on t_2=(1+69)=70th date counted from January 1st. 

- out.R
  - Do "Rscript out.R 69" on your terminal if you want to predict the onset date at t_2=(1+69)=70th date of the year. This makes 69.dat which is used to make Fig.3. Then run Fig3.R to produce Fig.3.
  - With out.R, the data in Figure 4(a) and 4(b) are made by changing "69" by other values.
  - With out.R, the data in Figure 4(c) and 4(d) are made by changing the value of xx in "lean<-c(ton[xx]:ton[33])"

- scatter.R
  - Running this generates Fig. S3, scatter.png

- MOKobjective.xlsx
  - Dates of the Monsoon onset over Kerala (MOK) reported objectively defined by the defintion of Indian Meteorological Department. References are shown in the Excel file. This is used for Table S3 and Figs. S6 and S9.
