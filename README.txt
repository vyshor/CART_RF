Information on how to run the RScript

-------------------------------
TAKE NOTE:
(a) You have to install the libraries at the top of each script file before running the script file
(b) There are requirements at the top of the file that requires you to run other scripts before running that script, to allow it to work such as:
# --------------------------
# Requirement:
# Run clean_data.R 
# Run CART.R
# There is preshuffle_data.2 in your object variables
# --------------------------
(c) Comments are added to explain each function and the direction and methodology of the data analytics
(d) If the code is taking too long to run, for example, the randomForest, please refer to the results in csv files located in the "tables" folder
------------------------------------
(The Scripts are number-coded to tell you how they are related and in which order you should run them)

Order of running the R-Script & Information of each script:

1. clean_data_2.R | Cleaning of the data
2. CART_3.R | Initial Run of CART, plus creating of new label
3. CARTmultiCV_4.1.R | Multi-runs of CART to determine best k hyperparameter for cross-validation
4. CARTmultiseed_4.2.R | Multi-runs of CART with different seeds to determine best accuracy
5. visualise_data_5.2.R | Visualisation of data based on the relevant features pointed out by CART
6. refine_data_5.3.R | Feature engineering and attempts to recode some variables in order to represent the data better and get better accuracy
7. RF_6.R | Initial Run of RandomForest, with pre-recoded data
8. RFmultitree_6.3.R | Multi-runs of RF to determine best number of trees
9. RFmultiCV_6.4.R | Multi-runs of RF to determine best k hyperparameter for cross-validation
10. RFmultiseed_6.5.R | Multi-runs of RF with different seeds to determine best accuracy (with recoded data)