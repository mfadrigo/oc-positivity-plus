# COVID-19 Predictors of Mortality and Positivity in Orange County, California: comparison of predictors of test positivity and mortality

## Authors
*Daniel M. Parker<sup>1</sup>; <sup>φ</sup>Tim Bruckner<sup>1</sup>; Verónica M. Vieira<sup>1</sup>; Catalina Medina<sup>1</sup>; Vladimir Minin<sup>1</sup>; Philip L. Felgner<sup>1</sup>; Allisa Dratch<sup>2</sup>; Matthew Zahn<sup>2</sup>; Scott M. Bartell<sup>1</sup>; <sup>φ</sup>Bernadette Boden-Albala<sup>1</sup>
1. University of California, Irvine, California, U.S.A.
2. Orange County Health Care Agency, Santa Ana, California, U.S.A.

*Corresponding author  
<sup>φ</sup>TB and BBA were PIs for the serological survey. BBA oversaw the data exchange between UCI
and the OCHCA as well.

Daniel M. Parker: dparker1@hs.uci.edu  
Tim Bruckner: tim.bruckner@uci.edu  
Verónica M. Vieira: vvieira@uci.edu  
Catalina Medina: catalmm1@uci.edu  
Vladimir minin: vminin@uci.edu  
Philip L. Felgner: pfelgner@hs.uci.edu  
Allisa Dratch: ADratch@ochca.com  
Matthew Zahn: MZahn@ochca.com  
Scott M. Bartell: sbartell@uci.edu  
Bernadette Boden-Albala: bbodenal@hs.uci.edu

## Extended Project Authors  
Christie Yang: christy9@uci.edu  
Micah Fadrigo: mfadrigo@uci.edu  
Roni Asatourian: rasatour@uci.edu   

## Content  

The model equations, graphs, and tables for the analyses performed can be found:  

- [Factors associated with testing positive for SARS-CoV2 in Orange County](analysis/factors-associated-with-testing-positive-oc-analysis-.pdf)  
- [Factors associated with mortality among those who tested positive for SARS-CoV2 in Orange County](analysis/factors-associated-with-mortality-oc-analysis-.pdf)  
- [Factors associated with seroprevelance of SARS-CoV2 in Orange County](analysis/factors-associated-with-seroprevelance-oc-analysis-.pdf)  

Code can be found in the corresponding .Rmd files in the analysis folder.  

## Data

- The data for these investigations came from our professor and are private information not meant for the public. 
- Estimates for zip code level variables such as median income are available under the "data/zip-code-data" folder. 
- Hospital bed data is available under the "data" folder labeled "covid19hospitalbycounty.csv"

## Run Models  
Once given access to the two positivity and mortality data sets, you can run the models build.  
In order to run the "positivity_model.Rmd", "mortality_model.Rmd", and "MLprediction.jpynb", you first need to run the "clean-covid-data.R" file found in the analysis folder.  
This will load the cleaned data that was merged with the hospital and zip code data. After this the model files should be allowed to run properly and illustrate the visualizations created. 

