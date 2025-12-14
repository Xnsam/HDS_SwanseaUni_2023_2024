#1 REQUIRED LIBRARY  ========================================|
tidyverse
naniar
tidytext
mice
broom
ggplot2
rpart
stats
caret
pROC
yardstick
car
epitools
finalfit
survival
crayon
utf8
stringr
here
# ============================================================|



#2 FILE STRUCTURE  ===========================================|

-- code
   |- common.R                         # contain common functions used across the code base
   |- data_modelling.R                 # functions in sequence for performing data modelling in Logistic Regression and Decision Tree
   |- demograph.R                      # functions to display demographic information on the screen
   |- posthoc_analysis.R               # function in sequence to run posthoc analysis 
   |- preprocessing.R                  # function in sequence to perform data cleaning, transformation, encoding, imputation
   |- secondary_analysis.R             # functions to perform secondary analysis
   |- survival_analysis.R              # functions to perform survival analysis
-- data
   |- demograph_data.xlsx                                     # created flat file
   |- wales_data
      |- Born in Wales Data June 2022.csv                     # received flat file
      |- Born in Wales Follow-up Data June 2022.csv           # received 
      |- column_name_mapping.csv                              # created via code  
      |- dtree_model_rules1.csv                               # created manually
      |- imputed_file.csv                                     # created via code
      |- preprocessed_file.csv                                # created via code
      |- processed_born_in_wales_data_june_2022_survival.csv  # created manually
      |- processed_born_in_wales_data_june_2022.csv           # created manually
      |- processed_imputed_file.csv                           # created via code
      |- survival_data.csv                                    # created manually
-- docs
      |- NewReport.docx              # raw report of the analysis
      |- ReportSummary.docx          # raw report summary 
      |- ResearchReport.pdf          # report submission draft
      |- ReportSummary.pdf           # summary submission draft 
-- images
      |- diagnosticPlots.jpeg            # plot used for model diagnostics
      |- forestplot.jpeg                 # plot used for cox regression coefficient analysis
      |- kaplanmeier.jpeg                # plot used for kaplan meier survival estimate analysis
      |- missing_data_survival.jpeg      # missing data plot representing survival data
      |- probPlot.jpeg                   # logistic regression predicted probability plot
      |- Rplot_dtree.jpeg                # decision tree rules plot
      |- missing_data_data_encoded.jpeg  # missing data plot representing born in wales data after preprocessing and data encoding
      |- missing_data_firstlook.jpeg     # missing data plot representing born in wales raw data
      |- marginal_plots.jpeg             # model diagnostic plot to understand the model fit over the variable
-- tests
      |- test_app.R                 # main test_app executor file
      |- test_files.R               # test functions to test files in code base
-- app.R                            # main app executor file to run the data analysis
-- README.txt                       
-- PMIM202.Rproj                    # r project file
# ============================================================|


#3 EXECUTION FLOW   ==========================================|


### TO RUN ANALYSIS ###########################

Step 1:
*** open the source file app.R and click run Source, this will execute 
    the code in the following flow

   --> app.R 
     --> run_preprocessing()
     --> get_demograph_data()
     --> run_initial_modelling()
     --> run_survival_analysis()
     --> run_post_hoc_analysis()

Step 2:
*** open the secondary_analysis.R, this will perform the secondary
    analysis on the processed dataset. Due to R's pkg technical issues
    the functions written in the file could be not executed as a 
    part of the sequence of the flow in Step 1

###########################

### TO RUN TESTS ###########################

Step 1:
*** open the source file tests/test_app.R and click run Source, this will 
    execute the code in the following flow

--> test_app.R
    --> test_files.R


# ============================================================|

