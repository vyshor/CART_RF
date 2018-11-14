# CART RF

> Data analytics project that inspects various aspects of employment to decide if an employee should be promoted or not through the use of CART and RandomForest models

###### [Click here for Dataset Used](https://www.kaggle.com/dgokeeffe/ibm-hr-wmore-rows)

### Tl;dr

1. Data Cleaning -> Initial Run of CART -> Optimised CART hyperparameters -> Visualise important variables -> Feature Engineering of relevant variables -> Re-run CART with recoded variables -> Initial Run of RandomForest (with non-recoded data) -> Optimised RandomForest hyperparameters -> Re-run RandomForest with recoded variables
2. Using Rstudio for implementation of CART and RandomForest

> Order of running the R-Script & Information of each script:
> 1. clean_data_2.R | Cleaning of the data
> 2. CART_3.R | Initial Run of CART, plus creating of new label
> 3. CARTmultiCV_4.1.R | Multi-runs of CART to determine best k hyperparameter for cross-validation
> 4. CARTmultiseed_4.2.R | Multi-runs of CART with different seeds to determine best accuracy
> 5. visualise_data_5.2.R | Visualisation of data based on the relevant features pointed out by CART
> 6. refine_data_5.3.R | Feature engineering and attempts to recode some variables in order to represent the data better and get better accuracy
> 7. RF_6.R | Initial Run of RandomForest, with pre-recoded data
> 8. RFmultitree_6.3.R | Multi-runs of RF to determine best number of trees
> 9. RFmultiCV_6.4.R | Multi-runs of RF to determine best k hyperparameter for cross-validation
> 10. RFmultiseed_6.5.R | Multi-runs of RF with different seeds to determine best accuracy (with recoded data)