# sedatedOAEs

This repository will house the code for the paper "The Effect of Sedation on Swept Otoacoustic Emissions in Chinchillas"

## Packages
A few packages are required to run this code. 
library(tidyverse)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)
library(ggpubr)


## Data Analysis Code

Scripts that take the raw measurement and convert to the individual values studied. 


## Statistics

The script `stats\_and\_plots.R` contains all code needed to regenerate the figures and statistical results in the paper. 


It requires four spreadsheets (csv): dpoae\_data, sfoae\_data ,Q443\_Qerb\_TimePoints, Q443\_TimePoints. 

