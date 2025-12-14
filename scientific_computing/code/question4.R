# Header ------------------------------------------------------------------
# File name: question4.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-26
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing question 4 from assignment
# ------------------------------------------------------------------------------



# Source ------------------------------------------------------------------
source('code/custom_utils.R')    # Custom utility functions



# Functions ---------------------------------------------------------------

get_hb_gp_spend_data <- function(config){
  #' @description
  #' A function to get the data w.r.t HB and GP
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' # get_hb_gp_spend_data(config)
  #' 
  #' 
  #' @retun df Data Frame consisting the data
  
  display_logs("Fetching data for average spend HB and GP ...", "process")
  display_logs(
    "
    NoTE: limit is not applied to achieve consistency in statistical tests
    ",
    "warning")
  query <-
    "
    SELECT    spend_tbl.hb,
              spend_tbl.practiceid,
    	        spend_tbl.period,
    	        ROUND(CAST(AVG(spend_tbl.actcost) AS NUMERIC), 4) AS avg_spend
    FROM      (SELECT    *
               FROM      (SELECT    gdut.hb,
                                    gdut.actcost,
                                    gdut.practiceid,
                                    gdut.period
                          FROM      gp_data_up_to_2015 AS gdut
    			    				   ) AS gp2015_sample
    			     ) AS spend_tbl
    GROUP BY  spend_tbl.hb, spend_tbl.practiceid, spend_tbl.period;
    "
  
  df <- execute_query(config, query)
  return (df)
}


draw_boxplot_for_hb <- function(data, plot_root_path){
  #' @description
  #' A function to draw histogram for all the HB
  #' 
  #' 
  #' @param data loaded data from database
  #' @param plot_root_path path for saving the plot
  #' 
  #' 
  #' @example
  #' # draw_boxplot_for_hb(data, plot_root_path)
  #' 
  #' 
  #' @retun none
  
  display_logs("Plotting box plot ... ", "process")
  
  # whole data on box plot
  # box plot is collapsed due to presence of large outliers
  g <- ggplot(data=data, aes(x=hb, y=avg_spend)) +
    geom_boxplot() +
    labs(
      title="Box plot with whole data",
      x="Health Board",
      y="Average Spend"
    ) +
    theme(
      plot.title = element_text(hjust=0.5, size=14)
    )
  
  # Save the plot
  file_name <- paste(plot_root_path,"/box_plot_all.png")
  ggsave(filename = file_name, plot=g)
  
  # Filter average spend < 5000 as box plot is collapsed due to presence of
  # large outliers
  sub_df <- data %>%
    filter(avg_spend < 5000) 
  
  g <- ggplot(data=sub_df, aes(x=hb, y=avg_spend)) +
    geom_boxplot() +
    labs(
      title="Box plot with Average Spend < 5000",
      x="Health Board",
      y="Average Spend"
    ) +
    theme(
      plot.title = element_text(hjust=0.5, size=14)
    )
  
  file_name <- paste(plot_root_path,"/box_plot_lt_5000.png")
  ggsave(filename = file_name, plot=g)
  
  
  # filter average spend < 100 box plot is clear and shows almost normal 
  # distribution
  sub_df <- data %>%
    filter(avg_spend < 100)
  
  g <- ggplot(data=sub_df, aes(x=hb, y=avg_spend)) +
    geom_boxplot() +
    labs(
      title="Box plot with Average Spend < 100",
      x="Health Board",
      y="Average Spend"
    ) +
    theme(
      plot.title = element_text(hjust=0.5, size=14)
    )
  
  file_name <- paste(plot_root_path,"/box_plot_lt_100.png")
  ggsave(filename = file_name, plot=g)
  
  display_txt <- paste("images stored in ", plot_root_path)
  display_logs(display_txt, "info")
  
}


display_mean_median <- function(data){
  #' @description
  #' A function to calculate and display mean median table
  #' 
  #' 
  #' @param data loaded data from database
  #' @param plot_root_path path for saving the plot
  #' 
  #' @example
  #' # display_mean_median(data)
  #' 
  #' 
  #' @retun none
  
  # get unique hb list
  hb_list <- unique(data$hb)
  
  # Create a new table
  mean_lt_5000 <- c()
  mean_lt_100 <- c()
  mean_all <- c()
  
  median_lt_5000 <- c()
  median_lt_100 <- c()
  median_all <- c()
  
  for (i in 1:length(hb_list)){
    hb_name <- hb_list[[i]]
    
    # overall
    sub_df <- data %>%
      filter(hb == hb_name)
      
    mean_all <- c(mean_all, mean(sub_df$avg_spend))
    median_all <- c(median_all, median(sub_df$avg_spend))
    
    # lte 5000
    sub_df <- data %>%
      filter(hb == hb_name & avg_spend < 5000)
      
    mean_lt_5000 <- c(mean_lt_5000, mean(sub_df$avg_spend))
    median_lt_5000 <- c(median_lt_5000, median(sub_df$avg_spend))
    
    # lte 100
    sub_df <- data %>%
      filter(hb == hb_name & avg_spend < 100)
    
    mean_lt_100 <- c(mean_lt_100, mean(sub_df$avg_spend))
    median_lt_100 <- c(median_lt_100, median(sub_df$avg_spend))
    
  }
  
  mean_median_df <- data.frame(
    hb=hb_list,
    mean_all=mean_all,
    mean_lt_5000=mean_lt_5000,
    mean_lt_100=mean_lt_100,
    median_all=median_all,
    median_lt_5000=median_lt_5000,
    median_lt_100=median_lt_100
  )
  
  print("Mean - Median table will be opened in view ")
  View(mean_median_df)
  
}


display_explanation <- function(){
  #' @description
  #' A function to display explanation for selecting testing strategy
  #' 
  #' @example
  #' # display_explanation()
  #' 
  #' 
  #' @retun none
  
  display_txt <- "
  
  Observation #1:
  
    1. From boxplot its evident that majority ( ~ 93 % ) of the data for 
       average spend is < 100. Rest minority ( ~ 7 % ) is outlier.
        
    2. From the table its evident that, mean difference is higher in comparison 
       to median difference when calculated inclusive and exclusive of outliers
       respectively.
        
  Conclusion #1:
  
    1. Outliers increases variability in the data which decreases statistical 
       power. Outliers could have negative affect on statistical 
       tests.
    
    2. It is possible to remove the outliers and run statistical tests on them, 
       but outliers could be important data points, and given the limitation 
       on dataset, and scope of investigation, removing the outliers is not 
       a good idea.
       
    3. The mean-median table represents that median is a more stable parameter
        for conducting statistical test. Given, the presence of outliers,
        following nonnormal distribution, and stable median parameter, it
        makes more sense to choose a non-parameteric test.
  
  
  "
  
  display_logs(display_txt)
}


perform_non_param_test <- function(data){
  #' @description
  #' A function to perform non parameteric test and display conclusion
  #' 
  #' @param data average spend data loaded from db w.r.t HB
  #' 
  #' 
  #' @example
  #' # perform_non_param_test(data)
  #' 
  #' @retun none
  
  display_txt <- "
      Thus, the choice for, `Kruskal-Wallis rank sum test` for significance 
      testing. Is a Non-parameteric counterpart to single factor analysis of 
      variance.
      
      Assumptions of `Kruskal-Wallis rank sum test`:
      1. Each HB (Independent Variable) could be considered as a group 
        (7 groups).
      2. Average Spend (Dependent Variable) is of `interval` data type.
      3. Observations are independent.
      4. Considers, Null hypothesis that, medians are from the same population
         and Alternative hypothesis that medians are not from the same 
         population.
  "
  display_logs(display_txt, "info")
  
  
  test_results <- kruskal.test(avg_spend ~ hb, data=data)
  
  print("-------------------- TEST Results -----------------------")
  print(test_results)
  print("-------------------- ------------ -----------------------")
  display_txt <- "
  
  
  Observation #2:
  
  1. The p-value obtained from Krusal-wallis test is < 0.05, thus the Null
     hypothesis is rejected and alternative hypothesis is accepted.
  
  2. From the mean - median table it is observed that median across the HB is 
     not too far from each other.
  
  Conclusion #2: 
  
  1. The statistical tests states that the median of the population is not from
     the same population, i.e. accepting the alternative hypothesis. From, the
     mean-median table, it is evident that the median (median_all) values are
     not to far from each other. 
  
  2. Logically, it would not make sense that similar medication across HB in
     the same region would have different prices. To confirm such an event can
     occur detailed investigation is required considering the demographics and
     socio-economic factors and causes leading to price differences.
     
  3. As non parameteric tests are less efficient to parameteric test and
     it is unclear if the results are accurate or not.
  
  "
  
  display_logs(display_txt)
  
  
}


# Handler -----------------------------------------------------------------

#. timer
run_question_4 <- function(config){
  #' @description
  #' A function to implement the Question 4
  #' Function execution time: 3-5 secs
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' # run_question_4(config)
  #' 
  #' 
  #' @return None
  
  display_logs(" -------- Running Question 4", "info")
  display_logs("Estimated function execution time: 3-5 secs", "info")
  
  plot_root_path <- "output/question4"
  
  # get spend data from db
  tryCatch(
   expr = {
     data <- get_hb_gp_spend_data(config)
   },
   error = function(e){
     display_txt <- "failed to get hb spend data"
     display_logs(display_txt, "debug")
     stop()
   }
  )
  
  # draw box plot to determine the distribution
  tryCatch(
    expr = {
      draw_boxplot_for_hb(data, plot_root_path) 
    }, 
    error = function(e) {
      display_txt <- "failed to draw box plot to determing the distribution"
      display_logs(display_txt, "debug")
      stop()
    }
  )
  
  
  # display mean and median for each HB after and before outlier removal
  tryCatch(
    expr = {
      display_mean_median(data)
    },
    error = function(e){
      display_txt <- "Failed to display mean median table for each HB"
      display_logs(display_txt, "debug")
      stop()
    }
  )
  
  
  # display explanation
  display_explanation()
  
  
  # perform non parametric test
  tryCatch(
    expr = {
      perform_non_param_test(data)
    },
    error = function(e){
      display_txt <- "failed to perform non parameteric test"
      display_logs(display_txt, "debug")
      stop()
    }
  )
  
  display_logs("Question 4 complete", "success")
  print("-----------------------------------------------------------")
}
