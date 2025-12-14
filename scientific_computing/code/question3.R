
# Header ------------------------------------------------------------------
# File name: question3.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-26
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing question 3 from assignment
# ------------------------------------------------------------------------------



# Source ------------------------------------------------------------------
source('code/custom_utils.R')    # Custom utility functions


# Functions ---------------------------------------------------------------


get_top5_prescriptions <- function(hb_name, gp_id, config){
  #' @description
  #' A function to get the data from top 5 prescriptions
  #' 
  #' @param hb_name name of the HB
  #' @param gp_id  ID of the gp
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' # get_top5_prescriptions(hb_name, gp_id, config)
  #' 
  #' 
  #' @return None
  
  query <- sprintf("
  SELECT   SUM(bnf_category_tbl.items) AS num_prescriptions,
           ROUND(CAST(SUM(bnf_category_tbl.actcost) AS NUMERIC), 4)  
                AS spend_prescriptions,
           bnf_category_tbl.subsectiondesc,
           bnf_category_tbl.chapterdesc
  FROM     (SELECT   LEFT(gdut.bnfcode, 9) AS bnfchemical,
                     gdut.bnfname,
                     gdut.items,
                     gdut.actcost,
                     bnf_tbl.bnfsection,
                     bnf_tbl.subsectiondesc,
                     bnf_tbl.chapterdesc 
            FROM     gp_data_up_to_2015 AS gdut
            INNER JOIN 
                     bnf AS bnf_tbl
            ON       LEFT(gdut.bnfcode, 9) = bnf_tbl.bnfchemical
            WHERE    hb='%s' AND practiceid='%s'
            ) as bnf_category_tbl
  GROUP BY  bnf_category_tbl.chapterdesc, bnf_category_tbl.subsectiondesc 
  ORDER BY  num_prescriptions desc
  LIMIT 5;
  ", hb_name, gp_id)
  df <- execute_query(config, query)
  return (df)
}


display_data <- function(data, plot_root_path, gp_id){
  #' @description
  #' A function to display the data from
  #' 
  #' @param data top 5 prescription data
  #' @param plot_root_path path to store the plot
  #' @param gp_id id of the gp
  #' 
  #' @example
  #'#  display_data(data, plot_root_path, gp_id)
  #' 
  #' 
  #' @return None
  
  
  # transformed the data in-order to make 
  transformed_df <- data %>%
    pivot_longer(names_to = "type", values_to = "val", 
                 num_prescriptions:spend_prescriptions) %>%
    arrange("type") %>%
    mutate(
      alphayr = ifelse(type=="num_prescriptions", 0.3, 0.9),
      text_align = val + 0.0001 * val
    )
  
  transformed_df$subsectiondesc <- trimws(transformed_df$subsectiondesc)
  
  bar_chart_plot <- ggplot(data=transformed_df, aes(x=subsectiondesc)) +
    geom_bar(
      aes(y=val, fill=type, group=type, alpha = alphayr),
      stat="identity", position="dodge",
      color="black"
    ) +
    scale_alpha_identity() +
    scale_y_continuous(
      name = "Number of Prescriptions",
      labels = scales::comma,
      sec.axis = sec_axis(
        ~.*1, name="Spend of Prescriptions", labels=scales::comma)
    )+
    theme(
      legend.position = 'top',
      plot.title = element_text(color='black',
                                face='bold',hjust=0.5),
      axis.title.y = element_text(color = "blue", size=13),
      axis.text.x = element_text(
        angle = 90, hjust=1, face="bold")) +
    ggtitle('Top 5 prescriptions by') +
    xlab("Names of BNF subsection description")
  # TODO need to figure out how to group by chapterdesc! ####
    # geom_text(position = position_identity(), 
    #           aes(y=text_align, label=chapterdesc, hjust=1), angle=0)
  
  file_name <- paste(plot_root_path,"/bar_chart_",gp_id,".png", sep="")
  ggsave(file=file_name,  bar_chart_plot, width=9, height=16)
  
  # display the data
  display_logs("Data Table will be opened in view...", 'info')
  view(data)
  
  display_logs(paste("plots saved in ", plot_root_path), "success")
}

# Handler -----------------------------------------------------------------


#. timer
run_question_3 <- function(config){
  #' @description
  #' A function to implement the Question 3
  #' Function execution time: 2-4 secs on default limit
  #' 
  #' @param config loaded config yaml object
  #' 
  #' 
  #' @example
  #' # run_question_3(config)
  #' 
  #' @return None
  
  display_logs(" -------- Running Question 3", "info")
  display_logs(
    "Estimated Function execution time: 2-4 secs on default limit", "info")
  
  user_continue_input <- "Y"
  plot_root_path <- "output/question3"
  
  while(user_continue_input == "Y"){
    # get the HB and GP id as user input
    display_txt <- "
    \n \tEnter the Health Board Name (case sensitive)
    
    Example : 7A3-W98031
    "

    input_pattern <- '7A[1-7]-W[0-9][0-9][0-9][0-9][0-9]'
    user_input <- get_user_input(
      display_txt, return_type="str", pattern=input_pattern)
    
    split_str <- strsplit(user_input, split="-")
    
    hb_name <- split_str[[1]][1]
    gp_id <- split_str[[1]][2]
    
    # create a HB folder
    custom_create_dir(file_path=file.path(plot_root_path, hb_name))
    plot_root_path_hb <- paste(plot_root_path,hb_name,sep="/")
    
    display_logs('Processing data ...', 'process')
    # fetch top 5 prescriptions using number of prescriptions
    tryCatch(
      expr = {
        data <- get_top5_prescriptions(hb_name, gp_id, config)
      },
      error = function(e){
        display_txt <- "
        failed to fetch top 5 prescriptions using number of prescriptions
        "
        display_logs(display_txt, "debug")
        stop()
      }
    )
    display_logs('Drawing plots ...', 'process')
    # display the type and spend or number of prescriptions
    tryCatch(
      expr = {
        display_data(data, plot_root_path_hb, gp_id)
      },
      error = function(e){
        display_txt <- "
        failed to display the type and spend or number of prescriptions
        "
        display_logs(display_txt, "debug")
        stop()
      }
    )
    
    # check if the user wants to check for another HB
    display_txt <- "Do you want to check for another HB / GP : (Y/n)"
    user_continue_input <- get_user_input(
      display_txt, return_type="str", pattern='[Y|n]')
  }
  
  display_logs("Question 3 complete", "success")
  print("-----------------------------------------------------------")
}
