# To understand trends in APMC, price &amp; quantity arrival data for different commodities in Maharashtra.
## A case study for socialcops

### Objective:

1. Test and filter outliers. 
2. Understand price fluctuations accounting the seasonal effect
    1. Detect seasonality type (multiplicative or additive) for each cluster of APMC and commodities
    2. De-seasonalise prices for each commodity and APMC according to the detected seasonality type
3. Compare prices in APMC/Mandi with MSP(Minimum Support Price)- raw and deseasonalised
4. Flag set of APMC/mandis and commodities with highest price fluctuation across different commodities in each relevant season, and year.

### Data: 
https://drive.google.com/drive/u/0/folders/0B-zoMsiXW40gZlNtNnlINEszRTg

### Variable description: 

* msprice- Minimum Support Price
* arrivals_in_qtl- Quantity arrival in market (in quintal)
* min_price- Minimum price charged per quintal
* max_price- Maximum price charged per quintal
* modal_price- Mode (Average) price charged per quintal


### Files description

* boxplot2014.jpeg - Boxplot showing outliers for year 2014
* boxplot2015.jpeg - Boxplot showing outliers for year 2015.
* boxplot2016.jpeg - Boxplot showing outliers for year 2016.
* CMO_MSP_Mandi.csv - Data containing MSP for various commodities
* Monthly_data_cmo.csv - Data containing monthly information about various APMC and Commodities.
* list of outliers.csv - CSV file containing the list of observations that were flagged as outliers.
* seasonality type.csv - CSV file denoting the seasonality type for the combination of APMC and Commodity.
* decomposed modal prices.csv - CSV file containing the decomposition of modal prices.
* msp comparision - raw and deseasonalised.csv - A CSV file comparing the raw and deseasonalised modal prices with MSP.
* maximum fluctuation.csv - CSV file showing the combination of APMC and commodities with maximum fluctuation for a given month and year.
* Socialcops.Rmd  - An R markdown document.
* r code.R - An R file containing the code.
