# Header ------------------------------------------------------------------
# File name: question2.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-25
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing question 2 from assignment

# Source ------------------------------------------------------------------
source('code/custom_utils.R')    # Custom utility functions


# Functions ---------------------------------------------------------------

get_prevalence_data <- function(config){
  #' @description
  #' A function to get top 3 prevalent conditions from SQL
  #' for all practices in the qof_achievement table
  #' 
  #' 
  #' @param config loaded config yaml object
  #' 
  #' 
  #' @example
  #' # get_prevalence_data(config)
  #' 
  #' 
  #' @retun df data frame containing the prevalence table
  
  execute_query(config, prev_table_view)
  
  q2_prevalence_query <- "
    WITH top3_prev_tbl AS (
    	SELECT    prev_tbl.*, 
    	          ROW_NUMBER()
    	OVER      (PARTITION by prev_tbl.practiceid
    	           ORDER BY prev_tbl.prevalence desc
    	          ) AS RowNo
    	FROM      (SELECT    * 
    	            FROM     prev_tbl_view
    	          ) AS prev_tbl
    	)
      SELECT    * 
      FROM      top3_prev_tbl
      WHERE     RowNo <= 3
      ORDER BY  top3_prev_tbl.prevalence desc;
  "

  df <- execute_query(config, q2_prevalence_query)
  colum_to_drop <- c("rowno")
  df <- df[, !(names(df) %in% colum_to_drop)]
  return (df)
}

display_hb_prevalence_data <- function(data, plot_root_path, hb){
  #' @description
  #' A function to display table and graph for hb data for Prevalence
  #' 
  #' @param data loaded config yaml object
  #' @param plot_root_path root path for storing plots
  #' @param hb name of the health board
  #' 
  #' @example
  #' # display_hb_prevalence_data(data, plot_root_path, hb) 
  #'
  #' 
  #' @retun None
  
  # histogram for HB
  ggplot(data=data, aes(x=prevalence)) +
    geom_histogram() +
    labs(
      title=paste("Distribution of Prevalence in the subset", hb),
      x="Prevalence",
      y="Frequency"
    ) +
    theme(
      text = element_text(size=9), element_line(size=1),  
      plot.title = element_text(hjust=0.5, size=14))
  
  # save the plot
  file_name <- paste(
    plot_root_path,"/hist_prevalence_",str_trim(hb),"_plot.png")
  ggsave(filename = file_name)
  
  # filter the data for top 3 prevalence in the entire HB
  hb_df <- data.frame(
    prevalence = data$prevalence, indicator = data$indicator)
  hb_df <- hb_df[1:3,]
  
  display_txt <- paste("
  Data will open in view for Top 3 Prevalence in Health Board, hb
  ", spe="")
  display_logs(display_txt, "info")
  View(hb_df)
  
  # bar chart for HB
  hb_df <- hb_df %>%
    pivot_longer(-indicator)
  
  # NOTE:
  # The `get_scaled_lim` function is used only for achieving the zoom effect
  # as the values for the respective scale is too close to each other. The zoom
  # transforms the scale to improve clarity and not to instate / mislead values
  # as learnt in PMIM502 - TEA Framework, so its commented
  
  ggplot(data = hb_df, aes(x = indicator, y = value)) +
    geom_bar(aes(fill = indicator), stat = 'identity') +
    # coord_cartesian(ylim=get_scaled_lim(hb_df$value)) +
    # scale_y_continuous(labels=scales::label_number(0.0001)) +
    labs(
      title=paste(hb, "HB TOP 3 Prevalence"),
      x="Indicator",
      y="Prevalence",
      fill="Indicator"
    ) +
    theme(
      plot.title = element_text(hjust=0.5),
      text = element_text(size = 14),element_line(size =1)
    )
  
  # save the plot
  file_name <- paste(plot_root_path, "/prevalence_",str_trim(hb),"_plot.png")
  ggsave(filename = file_name)
  
}


draw_facet_wrap_for_gp <- function(
    distinct_practices_list, data, plot_root_path, hb){
  #' @description
  #' A function to draw and store bar charts given
  #' data for top 3 Prevalence in facet wrap
  #' 
  #' @param distinct_practices_list List if distinct practices
  #' @param plot_root_path root path to store the plots
  #' @param data input data to be used
  #' @param hb name of the health board
  #' 
  #' @example
  #' # draw_facet_wrap_for_gp(
  #'          distinct_practices_list, data, plot_root_path, hb)
  #' 
  #' 
  #' @retun None
  
  sub_df <- data %>%
    select(prevalence, indicator, practiceid)
  
  practice_data_bin <- create_data_bins(distinct_practices_list, 10)
  # progress bar
  pb <- txtProgressBar(
    min = 0,
    max = length(practice_data_bin),
    style = 3,
    width = 50,
    char = "=")

  for(i in seq(1, length(practice_data_bin))){
    filtered_df <- sub_df %>%
      filter(practiceid %in% practice_data_bin[[i]])
    
    # NOTE:
    # The `get_scaled_lim` function is used only for achieving the zoom effect
    # as the values for the respective scale is too close to each other. The zoom
    # transforms the scale to improve clarity and not to instate / mislead values
    # as learnt in PMIM502 - TEA Framework, so its commented
    
    ggplot(data=filtered_df, aes(x=prevalence, y=indicator, fill=indicator)) +
      geom_bar(position="dodge", stat="identity") +
      facet_wrap(~practiceid) +
      # coord_cartesian(xlim=get_scaled_lim(filtered_df$prevalence)) +
      # scale_x_continuous(labels=scales::label_number(0.0001)) +
      labs(
        title=paste("Per GP TOP 3 Prevalence, data record ", i),
        x="Prevalence",
        y="Indicator",
        fill="Indicators"
      ) +
      theme(
        text = element_text(size=9), element_line(size=1),
        plot.title = element_text(hjust=0.5, size=14)
      )
    
    file_name <- paste(plot_root_path,"/prevalence_facet",i,".png")
    ggsave(filename = file_name)
    setTxtProgressBar(pb, i)
  }
  close(pb) # Close the progress bar
}


draw_bar_chart_each_indicator <- function(plot_root_path, data, hb){
  #' @description
  #' A function to draw bar chart for each indicator
  #' data for prevalence values
  #' 
  #' @param plot_root_path root path to store the plots
  #' @param data input data to be used
  #' @param hb name of the health board
  #' 
  #' @example 
  #' # draw_bar_chart_each_indicator(plot_root_path, data, hb)
  #' 
  #'  
  #' @retun None
  
  sub_df <- data %>%
    select(prevalence, indicator, practiceid)

  distinct_ind <- unique(sub_df$indicator)
  pb <- txtProgressBar( # progress bar
    min = 0,
    max = length(distinct_ind),
    style = 3,
    width = 50,
    char = "-")
  
  for (i in seq(1, length(distinct_ind))){
    ind <- distinct_ind[i]
    filtered_df <- sub_df %>%
      filter(indicator == ind)
    
    # NOTE:
    # The `get_scaled_lim` function is used only for achieving the zoom effect
    # as the values for the respective scale is too close to each other. 
    # The zoom transforms the scale to improve clarity and not to instate / 
    # mislead values as learnt in PMIM502 - TEA Framework, so its commented.
    
    bar_chart_plot <- ggplot(
      data=filtered_df,
      aes(x=prevalence, y=practiceid, fill=prevalence)
    ) +
      geom_bar(stat="identity") +
      # coord_cartesian(xlim=get_scaled_lim(filtered_df$prevalence)) +
      # scale_x_continuous(labels=scales::label_number(0.0001))+
      theme(
        text = element_text(size=9),
        element_line(size=1),
        plot.title = element_text(hjust=0.5, size=14)
      ) +
      labs(
        x="Prevalence",
        y="Practices",
        title=paste("Indicator", ind),
        fill="Prevalence"
      )
    
    file_name <- paste(plot_root_path,"/prevalence_",str_trim(ind),".png")
    ggsave(filename = file_name, bar_chart_plot, width=9, height=16)
    setTxtProgressBar(pb, i)
  }
  close(pb) # Close the progress bar
}


draw_heatmap_overview <-function(plot_root_path, data, hb){
  #' @description
  #' A function to draw bar chart for each indicator
  #' data for prevalence values
  #' 
  #' @param plot_root_path root path to store the plots
  #' @param data input data to be used
  #' @param hb name of the health board
  #' 
  #' @example
  #' # draw_heatmap_overview(plot_root_path, data, hb)
  #' 
  #' 
  #' @retun None
  
  heatmap <- ggplot(
    data=data, aes(fill=prevalence, x=indicator, y=practiceid)) +
    geom_tile() +
    theme(
      text=element_text(size=9),
      element_line(size=1),
      plot.title=element_text(hjust=0.5, size=14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_fill_gradient(low="yellow", high="orange") +
    labs(
      title="Heat map for Practices vs Indicators",
      x="Indicators",
      y="Practices",
      fill="Prevalence"
    )
  
  file_name <- paste(plot_root_path,"/heatmap",hb,".png", sep="")
  ggsave(file=file_name, heatmap, width=9, height=16)
  
}


draw_grid_bar_chart_gp_collated <- function(
    distinct_practices_list, plot_root_path, data, hb){
  #' @description
  #' A function to draw bar chart for each indicator
  #' data for prevalence values
  #' 
  #' @param distinct_practices_list list of practices
  #' @param plot_root_path root path to store the plots
  #' @param data input data to be used
  #' @param hb name of the health board
  #' 
  #' 
  #' @retun None
  #' 
  
  # create data bins for iteration
  list_of_bins <- create_data_bins(distinct_practices_list, stride=10)
  # progress bar
  pb2 <- txtProgressBar(
    min = 0,
    max = length(list_of_bins),
    style = 3,
    width = 50,
    char = "=")
  
  # iterate and create plots
  for (i in seq(1: length(list_of_bins))){
    data_bin <- list_of_bins[[i]]
    p_list <- list()
    for (n in seq(1: length(data_bin))){
      p_id <- data_bin[n]
      sub_gp_df <- data %>% 
        filter(practiceid == p_id) %>%
        select(prevalence, indicator) %>%
        pivot_longer(-indicator)
      
      # NOTE:
      # The `get_scaled_lim` function is used only for achieving the zoom effect
      # as the values for the respective scale is too close to each other. The zoom
      # transforms the scale to improve clarity and not to instate / mislead values
      # as learnt in PMIM502 - TEA Framework, so its commented.
      
      # Graph
      p <- ggplot(data=sub_gp_df, aes(fill=indicator, y=value, x=indicator)) + 
        geom_bar(position="dodge", stat="identity") +
        labs(
          title=p_id,
          x="",
          y="",
          fill=""
        ) +
        # coord_cartesian(ylim=get_scaled_lim(sub_gp_df$value)) +
        # scale_y_continuous(labels=scales::label_number(0.0001)) +
        theme(
          text = element_text(size=9), element_line(size=1),  
          plot.title = element_text(hjust=1, size=12),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(palette = "Dark2")
      
      p_list <- c(p_list, list(p))
    }
    g <- grid.arrange( 
      grobs = p_list, 
      top="GP Practices", 
      yleft="Prevalence",
      bottom="Indicator")
    
    file_name <- paste(plot_root_path,"/prevalence_per_gp_",i,".pdf", sep="")
    ggsave(file=file_name, g, width = 9, height = 16)
    
    setTxtProgressBar(pb2, i)
  }
  close(pb2) # Close the connection
}

display_gp_prevalence_data <- function(data, plot_root_path, hb){
  #' @description
  #' A function to display table and graph for GP practices 
  #' data for top 3 Prevalence
  #' 
  #' @param data loaded config yaml object
  #' @param plot_root_path root path tp store the plots
  #' @param hb name of the health board
  #' 
  #' 
  #' @retun None
  
  # get unique practice id list
  distinct_practices_list <- unique(data$practiceid)
  
  # drawing a heat map for practices vs indicators
  # this chart gives an overview of the data
  draw_heatmap_overview(plot_root_path, data, hb)

  
  # faceted wrap bar chart for GP practice 
  # following logic loops over unique list of practice id, plots the graph
  # and saves  the plots considering 50 practices at once.
  # This approach is used for visual clarity as there are larger number of
  # practices to be plotted against indicators 
  # draw_facet_wrap_for_gp(distinct_practices_list, data, plot_root_path, hb)

  
  # bar chart for single indicators
  # this chart helps to compare 1 indicator at a time across a set of
  # practices that cater to the indicator
  # draw_bar_chart_each_indicator(plot_root_path, data, hb)

  
  # bar chart for single GP 
  # this shows chart for 3 top indicators for each GP in a grid format
  #draw_grid_bar_chart_gp_collated(
  #  distinct_practices_list, plot_root_path, data, hb)
  
  
  display_logs(paste("plots saved in ", plot_root_path), "success")
  
  # display table for GP x Prevalence in the given HB
  display_logs("Table will be opened in view", "info")
  view(data)
  
  
  display_text <- "
  Note: You will see majority of the prevalence values similar to each other.
  Please check the hist_prevalence plot under the given path to determine the 
  spread of the prevalence values of the given health board.
  "
  display_logs(display_text, "info")
  
}


# Handler -----------------------------------------------------------------


#. timer
run_question_2 <- function(config){
  #' @description
  #' A function to implement the Question 2
  #' Function execution time: 6-9 secs on default limit
  #' 
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example 
  #' # run_question_2(config)
  #' 
  #' @return None
  
  display_logs(" -------- Running Question 2", "info")
  display_logs(
    " Estimated Function execution time: 6-9 secs on default limit", "info")

  # get the prevalence data
  display_logs("Fetching prevalence data ...", "process")
  prevalence_data <- get_prevalence_data(config)
  user_continue_input <- "Y"
  plot_root_path <- "output/question2"
  
  while (user_continue_input == "Y") {
    # get the HB from user
    display_txt <- "
    \n \tEnter the Health Board Name  (case sensitive) Example: 7A2
    "
    
    input_pattern <- "7A[1-7]"
    hb_name <- get_user_input(
      display_txt, return_type="str", pattern=input_pattern)
    
    display_logs("Processing data ...", "process")
    
    # create a HB folder
    custom_create_dir(file_path=file.path(plot_root_path, hb_name))
    plot_root_path_hb <- paste(plot_root_path,hb_name,sep="/")
    
    # get list of GP practices as per given HB
    tryCatch(
      expr = {
        gp_practices <- get_practice_list_from_hb(config, hb_name)
      },
      error = function(e) {
        display_txt <- "
        failed to get list of GP practices as per given HB
        "
        display_logs(display_txt, "debug")
        stop()
      }
    )
    
    # filter the prevalence data as per the gp list
    filtered_prevalence_data <- prevalence_data %>%
      filter(practiceid %in% gp_practices$practiceid)
    
    # if there is no data after filtering no point in moving ahead
    if (dim(filtered_prevalence_data)[1] == 0) {
      display_logs("No prevalence data for the given practice list from HB", 
                   "error")
      stop()
    }
    
    tryCatch(
      expr = {
        # display top 3 prevalence condition as per HB
        display_hb_prevalence_data(
          filtered_prevalence_data, plot_root_path_hb, hb_name)
      },
      error = function(e){
        display_txt <- "failed display top 3 prevalence condition as per HB"
        display_logs(display_txt, "debug")
      }
    )
    
    display_logs("Drawing plots ...", "process")
    tryCatch(
      expr = {
        # display top 3 prevalence condition as per GP
        display_gp_prevalence_data(
          filtered_prevalence_data, plot_root_path_hb, hb_name)
      },
      error = function(e){
        display_txt <- "failed display top 3 prevalence condition as per GP "
        display_logs(display_txt, "debug")
      }
    )
    
    # check if the user wants to check for another HB
    display_txt <- "Do you want to check for another HB : (Y/n) "
    user_continue_input <- get_user_input(
      display_txt, return_type="str", pattern='[Y|n]')
  }
  
  display_logs("Question 2 complete", "success")
  print("-----------------------------------------------------------")
}
