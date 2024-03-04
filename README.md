## Summary:
This repository contains code for conducting regression analysis as per Hodel et al. (in review).


### Replication Instructions 

- Download all data from the zenodo repository and put them in a folder called data/
- unzip the inference_adam_lr5_ens5.zip into a folder data/inference_adam_lr5_ens5/
- Follow instructions in the preprocessing files 01-10

Execute 
`code/03_reg_analysis.R`
for regression analysis. Results will be generated in /results.


Run 
`figures/coef_plot.R` 
for generating coefficient plots. Output files will be saved in /figures.


Check out the code for the deep learning part:
[leoniehodel/deepCattleCount](https://github.com/leoniehodel/deepCattleCount)

