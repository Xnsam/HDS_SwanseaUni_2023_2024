# Header ----------------------------------------------------------------------
# File name: open_end_analysis.R
# Folder/Module name: code
# Version: v0.1
# Created Date: 2023-10-29
# Last Edited Date: 2023-11-26
# Editor/Creator: Akson Sam Varghese
# Description: Source file for implementing question 6 from assignment
# ------------------------------------------------------------------------------


# Source ------------------------------------------------------------------
source('code/custom_utils.R')    # Custom utility functions

# Global Variables --------------------------------------------------------
qof_thresholds_df <- read_csv('./data/assets/qof_thresholds.csv')

# Functions ---------------------------------------------------------------


section_set_up <- function(sec_num, plot_root_path){
  #' @description
  #' A function to perform initial set up for each section
  #' 
  #' @param sec_num number of the section being currently invoked
  #' @param plot_root_path plot root path
  #' 
  #' @example
  #' # section_set_up(5, 'path/to/plot')
  #' 
  #' @retun plot_root_path plot root path
  
  display_logs(paste("Running Section",sec_num," ..."), "process")
  plot_root_path <- paste(plot_root_path,'/section_',sec_num, sep="")
  custom_create_dir(plot_root_path)

  return (plot_root_path)
  
}

assign_points <- function(ratio_val, indicator_val){
  #' @description
  #' A function to assign achievement points given ratio and indicator
  #' 
  #' @param ratio_val value of the ratio of num / denom
  #' @param indicator_val value of the indicator
  #' 
  #' @example
  #' # assign_points(ratio_val, indicator_val, qof_thresh_df )
  #' 
  #' 
  #' @retun pt achievement point
  
  # There are two types of indicators. 
  # One where the indicators have NI suffixed in the end ( example: AST004NI)
  # and the other one without the suffix `NI`. The suffix indicates that the
  # points are adjusted for the WALES country, so in such conditions, the
  # points threshold for suffixed should be prioritized in comparison to the
  # others.
  
  indicator_NI_suffixed <- paste(indicator_val, "NI", sep="")
  
  idx_t1 <- which(qof_thresholds_df['indicator'] == indicator_NI_suffixed)
  idx_t2 <- which(qof_thresholds_df['indicator'] == indicator_val)
  
  pt <- 0
  if (identical(idx_t1, integer(0))){ # first preference to suffixed
    if (identical(idx_t2, integer(0))){
      idx <- NULL
    } else {
      idx <- idx_t2
    }
  } else {
    idx <- idx_t1
  }
  
  if (!is.null(idx)){
    col_names <- c('points', 'threshold_in_perc', 'indicator_type')
    row_df <- qof_thresholds_df[idx, col_names]
    
    thresholds <- strsplit(row_df$threshold_in_perc, split='-')
    
    max_point <- row_df$points
    threshold_lower <- as.integer(thresholds[[1]][1]) / 100
    threshold_upper <- as.integer(thresholds[[1]][2]) / 100
    indicator_type <- row_df$indicator_type
    
    
    # Note: The logic for assigning points is from 
    # www.mysurgerywebsite.co.uk [2]
    
    # There are two types of points assignment strategy.
    # First, is when the indicators are of fractional type where the points
    # are assigned on the basis of thresholds
    # Second, is when the indicators are of boolean type where the points are
    # assigned on  the basis of thresholds.
    
    if (indicator_type == "fractional") {
      if (ratio_val >= threshold_upper) {
        pt <- max_point
      } else if (ratio_val < threshold_lower) {
        pt <- 0
      }else { 
        pt <- (ratio_val - threshold_lower) / 
               (threshold_upper - threshold_lower)
        pt <- pt * max_point
      }
    } else {
      pt <- max_point 
    }
  }
  return (pt)
}

run_section_1 <- function(config, plot_root_path) {
  #' @description
  #' A function to implement the open end analysis section 1
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' 
  #' 
  #' @example
  #' # run_section_1(config, plot_root_path)
  #' 
  #' @return None
  
  plot_root_path <- section_set_up(1, plot_root_path)

  ## Q1 ----------------------------------------------------------
  ## Brief Analysis of Actual cost and Net cost
  
  ### Q1.1 -------------------------------------------------------
  # Step 1:
  # Are there inconsistencies in the actual cost and net cost of the medication?
  
  query <- "
    SELECT    COUNT(check_table.category),
              check_table.category
    FROM      (
               SELECT    base_table_view0.bnfcode,
               CASE
                  WHEN actual_cost > net_cost THEN 'actual_cost > net_cost'
                  WHEN actual_cost < net_cost THEN 'actual_cost < net_cost'
                  WHEN actual_cost = net_cost THEN 'actual_cost = net_cost'
                  WHEN net_cost = 0 AND actual_cost != 0 
                     THEN 'net_cost = 0 AND actual_cost != 0'
                  WHEN net_cost != 0 AND actual_cost = 0 
                     THEN 'net_cost != 0 AND actual_cost = 0'
                  WHEN net_cost = 0 AND actual_cost = 0 
                     THEN 'net_cost = 0 AND actual_cost = 0'
               ELSE
                  'no_one'
               END AS   \"category\" 
               FROM     base_table_view0
               ) AS check_table
    GROUP BY  check_table.category;
    "
  
  df <- execute_query(config, query)
  plot_df <- df %>%
    filter(count > 500) %>%
    mutate(perc=round(((count/sum(count)) * 100), 2)) %>% 
    mutate(ypos = cumsum(perc) - 0.5*perc)
  
  
  g <- ggplot(data=plot_df, aes(x="", y=perc, fill=category)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        geom_text(aes(y = ypos, label = perc), color = "white", size=6) +
        labs(
          title="Percentage of inconsistencies > 500 records",
          x="",
          y="",
          fill="Category"
        )
      
  file_name <- paste(plot_root_path, '/Q1_1.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. 1.91% of the data has actual cost > net cost
  
  
  ### Q1.2 -------------------------------------------------------
  # Step 2:
  # To look at contribution of 1.91 % category towards total actual cost
  
  query <- "
    SELECT    check_table.category,
              COUNT(check_table.category),
              ROUND(CAST(SUM(check_table.actual_cost) 
                          / 547805146.03 AS NUMERIC), 4)
                    * 100 AS perc_contribution,
              ROUND(CAST(SUM(check_table.actual_cost) AS NUMERIC), 2) 
                   AS total_actual_cost
              
    FROM      (SELECT    base_table_view0.bnfcode,
                         base_table_view0.actual_cost,
               CASE
                  WHEN actual_cost > net_cost THEN 'actual_cost > net_cost'
                  WHEN actual_cost < net_cost THEN 'actual_cost < net_cost'
                  WHEN actual_cost = net_cost THEN 'actual_cost = net_cost'
                  WHEN net_cost = 0 AND actual_cost != 0 
                     THEN 'net_cost = 0 AND actual_cost != 0'
                  WHEN net_cost != 0 AND actual_cost = 0 
                     THEN 'net_cost != 0 AND actual_cost = 0'
                  WHEN net_cost = 0 AND actual_cost = 0 
                     THEN 'net_cost = 0 AND actual_cost = 0'
               ELSE
                  'no_one'
               END AS   \"category\" 
               FROM     base_table_view0
              ) AS check_table
    GROUP BY  check_table.category;
    "
  
  df <- execute_query(config, query)
  plot_df <- df %>%
    filter(count > 500) %>%
    mutate(ypos = cumsum(perc_contribution) - 0.5*perc_contribution)
  
  
  g <- ggplot(data=plot_df, aes(x="", y=perc_contribution, fill=category)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        geom_text(
          aes(y = ypos, label = perc_contribution), color = "white", size=6) +
        labs(
          title="Percentage of contribution to total actual sum",
          x="",
          y="",
          fill="Category"
        )
  
  file_name <- paste(plot_root_path, '/Q1_2.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. For every 10000 prescriptions, there are 27 prescriptions where
  #    the actual cost > net cost. It would be interesting to check 
  #    which medication category has actual cost > net cost
  
  ### Q1.3 -------------------------------------------------------
  # STEP 3:
  # To look at instances if actual cost  & net cost has any relationship,
  # when actual cost > net cost
  
  query <- "
    SELECT    *
    FROM      (
              SELECT    * 
              FROM      public.base_table_view
              ) AS base_table
    WHERE     base_table.actual_cost > base_table.net_cost;
    "
  
  cost_df <- execute_query(config, query)
  
  g <- ggplot(data=cost_df, aes(x=net_cost, y=actual_cost, color=hb_name)) +
        geom_point() +
        labs(
          title="Actual Vs Net Cost per HB",
          x="Actual Cost",
          y="Net Cost",
          color="Health Board"
        )
  
  file_name <- paste(plot_root_path, '/Q1_3.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. Scatter plot shows that net cost and actual cost are linearly correlated.
  #    Meaning, there are extreme points, with one-sided extreme values.
  
  ### Q1.4 -------------------------------------------------------
  # STEP 4:
  # To check which medications have actual cost > net cost and their difference
  
  query <- "
      SELECT    cat_table.category,
                ROUND(
                     CAST(
                          SUM(distinct cat_table.actual_cost) AS NUMERIC), 
                      4) AS total_actual_cost,
                ROUND(
                     CAST(
                          SUM(distinct cat_table.net_cost) AS NUMERIC), 
                      4) AS total_net_cost
      FROM      (
                SELECT    *
                FROM      cat_table_view
                ) AS cat_table
      GROUP BY  cat_table.category
      ORDER BY  total_actual_cost;
      
  "
  
  cost_category_df <- execute_query(config, query)
  cost_category_df$difference <- cost_category_df$total_actual_cost - 
    cost_category_df$total_net_cost
  cost_category_df$category <- trimws(cost_category_df$category)
  
  g <- ggplot(
    data=cost_category_df, 
    aes(x=reorder(category, -difference), y=difference, fill=difference)
    ) +
    geom_bar(stat="identity", position="identity") +
    labs(
      title="Chapter of medication with difference (actual - net cost)",
      x="Chapter",
      y="Difference ( actual - net cost )",
      fill="Difference Range"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold"))
  
  
  file_name <- paste(plot_root_path, '/Q1_4.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. The highest difference between actual cost and net cost is
  #    for Central nervous system medication chapter, followed by cardiovascular
  #    system, respiratory system.
  # 2. To scout further steps following inspections could be considered
  #     Inspection 1 - Is this a data error ? 
  #     Inspection 2 - could it be that container allowance is too high for the 
  #                    identified chapters?
  #                    as per data specification 
  #                    actual_cost = net_cost - discount + container_allowance
  #     Inspection 3 - is it associated to geographic location ?
  
  ### Q1.5 -------------------------------------------------------
  # STEP 5: 
  # Focus on central nervous system chapter. There is no concrete way of
  # undertaking Inspection 1 and Inspection 2, so trying Inspection 3
  
  inspect3_query <- "
    SELECT    cat_table.category,
              address_table.county
    FROM      (
              SELECT    *
              FROM      public.cat_table_view
              ) AS cat_table
    INNER JOIN (
                SELECT    *
                FROM      public.address_county_view
                ) AS address_table
    ON        address_table.practiceid = cat_table.practice_id
    WHERE     TRIM(cat_table.category) = '%s';
    "
  
  query <- sprintf(inspect3_query, 'Central Nervous System')
  
  df <- execute_query(config, query)
  
  plot_df <- df %>%
    group_by(county) %>% 
    summarise(freq=n())
  
  head(plot_df)
  
  sub_df <- plot_df %>% 
    arrange(desc(freq)) %>%
    slice_head(n=20)
  
  title_txt1 <- "Top 20 Count of Prescriptions in county"
  title_txt2 <- "under Central Nervous System chapter"
  title_txt <- paste(title_txt1, title_txt2)
  
  g <- ggplot(
    data=sub_df, 
    aes(x=reorder(county, -freq), y=freq, fill=freq)
    ) +
    geom_bar(stat="identity", position="identity") +
    labs(
      title=title_txt,
      x="County",
      y="Count",
      fill="Count"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold"))
  
  file_name <- paste(plot_root_path, '/Q1_5.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. Regions of Glamorgan, Gwent, Dyfed have the highest prescription of CNS 
  #    medication. 
  
  ### Q1.6 -------------------------------------------------------
  # STEP 6:
  # Focus on second highest difference cardiovascular system in counties
  
  
  query <- sprintf(inspect3_query, 'Cardiovascular System')
  
  df <- execute_query(config, query)
  
  plot_df <- df %>%
    group_by(county) %>% 
    summarise(freq=n())
  
  sub_df <- plot_df %>% 
    arrange(desc(freq)) %>%
    slice_head(n=20)
  
  title_txt1 <- "Top 20 Count of Prescriptions in county"
  title_txt2 <- "under Cardiovascular System chapter"
  title_txt <- paste(title_txt1, title_txt2)
  g <- ggplot(
    data=sub_df, 
    aes(x=reorder(county, -freq), y=freq, fill=freq)
    ) +
    geom_bar(stat="identity", position="identity") +
    labs(
      title=title_txt,
      x="County",
      y="Count",
      fill="count"
     ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, face="bold"))
      
  file_name <- paste(plot_root_path, '/Q1_6.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. For the second highest prescription of CVS medication, 
  #    Regions of Glamorgan, Gwent, Dyfed are in the top 5. 
  
  ### Q1.7 -------------------------------------------------------
  # STEP 7: 
  # For GWENT, MID GLAMORGAN, SOUTH GALMORGAN, WEST GLAMORGAN, DYFED
  # find  the sub categories / medications for CNS and CVS 
  
  query <- "
    SELECT    cat_table.category,
              cat_table.sub_category
    FROM      (
              SELECT    *
              FROM      public.cat_table_view
              ) AS cat_table
    INNER JOIN (
               SELECT    *
               FROM      public.address_county_view
               ) AS address_table
    ON        address_table.practiceid = cat_table.practice_id
    WHERE     TRIM(cat_table.category) IN
                       ('Central Nervous System', 'Cardiovascular System') AND
              UPPER(TRIM(county)) IN
                       ('GWENT', 'MID GLAMORGAN', 'SOUTH GALMORGAN', 
                       'WEST GLAMORGAN', 'DYFED');
  "
  
  df <- execute_query(config, query)
  df$category <- trimws(df$category)
  df$sub_category <- trimws(df$sub_category)
  
  bar_list <- list()
  for (categ in c('Cardiovascular System', 'Central Nervous System')){
    plot_df <- df %>%
      filter(category == categ) %>%
      group_by(sub_category) %>%
      summarise(count=n())
    
    d <- ggplot(data=plot_df, 
                aes(x=reorder(sub_category, -count), y=count, fill=count)) +
      geom_bar(stat="identity", position="identity") +
      labs(
        title=paste(categ),
        x="",
        y="",
        fill="Count"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    bar_list <- c(bar_list, list(d))
  }
  
  
  g <- grid.arrange(
    bottom="Section",
    left="Count",
    grobs = bar_list
  )
  
  file_name <- paste(plot_root_path, '/Q1_7.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # In the selected counties,
  # 1. For Caridovascular System, chapter sections Anti hypertensive therapy,
  #    Hyper tension and heart failure and diuretics prescribed medications
  #    are the most prominent in this system
  # 2. For Central Nervous System, chapter sections Pyschoses & Rel. Disorders,
  #    Analgensics and Hypnotics & Anxiolytics prescribed medications are the
  #    most prominent in this system
  
  
  ### Q1.8 -------------------------------------------------------
  # Step 8:
  # To check Which sections costs the most in the respective chapters
  
  
  query <- "
    SELECT    cat_table.actual_cost,
              cat_table.net_cost,
              cat_table.sub_category,
              cat_table.category
    FROM      (
              SELECT    *
              FROM      public.cat_table_view
              ) AS cat_table
    INNER JOIN (
               SELECT    *
               FROM      public.address_county_view
               ) AS address_table
    ON        address_table.practiceid = cat_table.practice_id
    WHERE     TRIM(cat_table.category) IN 
                  ('Central Nervous System', 'Cardiovascular System') AND
                  UPPER(TRIM(county)) IN 
                  ('GWENT', 'MID GLAMORGAN', 'SOUTH GALMORGAN', 
                  'WEST GLAMORGAN', 'DYFED');
  "
  
  df <- execute_query(config, query)
  df$category <- trimws(df$category)
  df$sub_category <- trimws(df$sub_category)
  
  bar_list <- list()
  for (categ in c('Cardiovascular System', 'Central Nervous System')){
    plot_df <- df %>%
      filter(category == categ) %>%
      group_by(sub_category) %>%
      summarise(
        total_actual_cost=sum(actual_cost), 
        total_net_cost=sum(net_cost)
      ) %>% 
      pivot_longer(
        cols= c("total_actual_cost", "total_net_cost"),
        names_to="cost_type",
        values_to="cost"
      )
    
    
    d <- ggplot(data=plot_df, 
                aes(x=reorder(sub_category, -cost), y=cost, fill=cost_type)) +
      geom_bar(stat="identity", position="dodge") +
      labs(
        title=paste(categ),
        x="",
        y="",
        fill="Cost Type"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    bar_list <- c(bar_list, list(d))
  }
  
  
  g <- grid.arrange(
    bottom="Section",
    left="Cost",
    grobs = bar_list
  )
  
  file_name <- paste(plot_root_path, '/Q1_8.png', sep="")
  ggsave(filename = file_name, plot=g)
  
  ##### [Interpretation] =======================
  # In the selected counties
  # 1. For Cardiovascular System chapter, sections
  #    Diuretics medication cost the most, followed by Antihypertensive Therapy
  #    and Hypertension and Heart failure medication
  # 2. For Central Nervous System chapter, sections
  #    Psychoses & Rel. Disorders, hypnotics cost the most, followed by
  #    Anxiolytics and antidepressant drugs
  
  
  #### [Summary] ===========================
  # 1. There are 0.27 % of medical prescription where the actual cost is greater
  #    than the net ingredient cost, and contributes 1.91 % to the total actual
  #    cost.
  # 2. Top 5 counties where these medical prescriptions fall under are
  #    -'GWENT', 'MID GLAMORGAN', 'SOUTH GALMORGAN', 'WEST GLAMORGAN', 'DYFED'.
  # 3. Top 2 chapters where these medical prescriptions fall under are
  #    - Central Nervous System and Cardiovascular system.
  # 4. Top 3 chapter sections for CNS, where most expensive & commonly 
  #    prescribed medications are
  #    Psychoses - Rel. Disorders, Hypnotics, Anxiolytics and antidepressant 
  #    drugs & 
  #    Pyschoses - Rel. Disorders, Analgesics, Hypnotics  Anxiolytics and 
  #    antidepressant drugs, respectively.
  # 5. Top 3 chapter sections for CVS, where most expensive & commonly 
  #    prescribed medications are 
  #    Diuretics, Anti-hypertensive Therapy, Hypertension and Heart failure 
  #    medication & 
  #    Anti hypertensive therapy, Hyper tension and heart failure 
  #    and Diuretics, respectively.
  
}

run_section_2 <- function(config, plot_root_path){
  #' @description
  #' A function to implement the open end analysis section 2
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' 
  #' @example
  #' # run_section_2(config, plot_root_path)
  #' 
  #' @return points_df data frame containing points column
  
  plot_root_path <- section_set_up(2, plot_root_path)
  
  ## Q2 ----------------------------------------------------------
  ## Assigning partial achievement points based on thresholds of the indicators
  ## for the year 2015 and analyzing the performance of the indicators.
  ## Achievement points contains multiple components when its calculated.
  ## Threshold point is just one of them and easy accessible component picked
  ## for preliminary analysis at ground level. The threshold CSV file was 
  ## created manually referring to the files from the website mentioned 
  ## in reference [1]
  
  
  ### Q2.1 -------------------------------------------------------
  # Step 1:
  # Create a points column using the manually created thresholds file and add
  # to the qof achievement table
  
  query <- "
    SELECT    * 
    FROM      qof_achievement;
  "
  qof_df <- execute_query(config, query)
  
  v_assign_points <- Vectorize(assign_points)
  points_df <- qof_df %>%
    mutate(points=v_assign_points(ratio_val=ratio, indicator_val=indicator))
  
  g <- ggplot(data=points_df, aes(x=points, y=indicator)) +
    geom_point() +
    labs(
      title="Distribution of points",
      x="Value of Points",
      y="Indicator"
    )
  
  file_name <- paste(plot_root_path, '/Q2_1.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. 33 % (14049 / 41380) of data has points assigned zero to them as the 
  #    indicator thresholds do not meet the requirements
  # 2. 33 % ( 30 / 90 ) of indicators  have points assigned zero
  # 3. There are 60 indicators to focus on
  
  ### Q2.2 -------------------------------------------------------
  # Step 2:
  # To subset the data where points are not zero 
  
  points_df <- subset(points_df, points > 0)
  
  g <- ggplot(data=points_df, aes(x=points, y=indicator)) +
    geom_point() +
    labs(
      title="Distribution of points",
      x="Value of points",
      y="Indicator"
    ) +
    theme(
      axis.text.y = element_text(size=9))
    
  
  file_name <- paste(plot_root_path, '/Q2_2.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. Majority of the values lie between 0 - 10
  # 2. only SMOK005 and SMOK004 indicator has assigned points > 20, meaning
  #    they are the top performing indicators
  
  ### Q2.3 -------------------------------------------------------
  # Step 3:
  # To find top performing indicators
  
  df <- points_df %>%
    group_by(indicator) %>%
    summarise(median_perf=median(points), mean_perf=mean(points)) %>%
    pivot_longer(
      cols=c('median_perf', 'mean_perf'),
      names_to="performance",
      values_to="value"
    )
  
  g <- ggplot(
    data=df, 
    aes(
      x=reorder(indicator, -value), 
      y=value, 
      group=performance, 
      color=performance)
    ) +
    geom_point() +
    geom_line() +
    labs(
      title="Performance of Indicators",
      x="Indicator",
      y="Points",
      color="Performance"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size=9))
  
  file_name <- paste(plot_root_path, '/Q2_3.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. Mean and Median of Points are closer to each other. Both statistic
  #    appears to be stable.
  # 2. SMOK002, SMOK005, AST003, CHD002, DM007 have received high achievement
  #    points
  # 3. PAD001, STIA001, MH010, EP001, RA001, MH009 have received low 
  #    achievement points
  
  ### Q2.4 -------------------------------------------------------
  # Step 4:
  # To find top performing HB 
  
  points_df$orgcode <- trimws(points_df$orgcode)
  
  practice_list <- unique(points_df$orgcode)
  query <- sprintf("
  select    gdut.hb,
            gdut.practiceid as orgcode
  from      gp_data_up_to_2015 gdut
  where
      gdut.practiceid in (%s)
  group by
        gdut.hb, gdut.practiceid
  
  ", paste0("'",practice_list,"'", collapse=","))
  
  df <- execute_query(config, query)
  
  df <- df %>%
    inner_join(points_df, by='orgcode') %>% 
    select(orgcode, points, hb) %>% 
    group_by(hb) %>%
    summarise(
      mean_v=mean(points), 
      median_v=median(points)) %>%
    pivot_longer(
      cols = c('mean_v', 'median_v'),
      names_to = 'performance',
      values_to = 'value'
    )
  
  g <- ggplot(
    data=df, 
    aes(
      x=reorder(hb, -value), 
      y=value, 
      group=performance, 
      color=performance)
  ) +
    geom_point() +
    geom_line() +
    labs(
      title="Points at HB level",
      x="Health Board",
      y="Points",
      color="Performance"
    )
  
  file_name <- paste(plot_root_path, '/Q2_4.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. Across all the Health boards the achievement points of median 5 is 
  #    assigned
  # 2. Across all the health boards the achievement points of mean 7 - 7.5 is
  #    assigned
  
  ### Q2.5 -------------------------------------------------------
  # Step 5:
  # To find top 5 and bottom 5 performing GP
  query <- sprintf("
  SELECT    gdut.hb,
            gdut.practiceid as orgcode
  FROM      gp_data_up_to_2015 gdut
  WHERE     gdut.practiceid IN (%s)
  GROUP BY  gdut.hb, gdut.practiceid
  ", paste0("'",practice_list,"'", collapse=","))
  
  df <- execute_query(config, query)
  
  df <- df %>%
    inner_join(points_df, by='orgcode') %>% 
    select(orgcode, points, hb) %>% 
    group_by(orgcode) %>%
    summarise(median_v=median(points)) %>%
    arrange(desc(median_v))
  
  df1 <- df %>%
    slice_head(n=5) %>%
    mutate(class='top_class')
  
  df2 <- df %>%
    slice_tail(n=5) %>%
    mutate(class='low_class')
  
  plot_df <- rbind(df1, df2)
  
  g <- ggplot(
    data=plot_df, 
    aes(
      x=reorder(orgcode, -median_v), 
      y=median_v,
      group=class,
      color=class)
  ) +
    geom_point() +
    geom_line() +
    labs(
      title="Points at GP level",
      x="GP",
      y="Points",
      color="Performance"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  file_name <- paste(plot_root_path, '/Q2_5.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. the top 5 GP median value lies between 6 - 7
  # 2. the bottom 5 GP median value lies between 4- 5
  

  
  return (points_df)
}

run_section_3 <- function(config, plot_root_path, points_df){
  #' @description
  #' A function to implement the open end analysis section 3
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' @param points_df data frame containing the points value
  #' 
  #' 
  #' @example
  #' # run_section_3(config, plot_root_path, points_df)
  #' 
  #' @return None
  
  plot_root_path <- section_set_up(3, plot_root_path)
  
  ## Q3 ----------------------------------------------------------
  ## To check the overall participation rate at the health board level
  ## and to compare the participation rate at the indicator level 
  
  ### Q3.1 -------------------------------------------------------
  # Step 1:
  # To check the overall participation rate at the HB level
  
  points_df$orgcode <- trimws(points_df$orgcode)
  
  practice_list <- unique(points_df$orgcode)
  query <- sprintf("
  SELECT    gdut.hb,
            gdut.practiceid as orgcode
  FROM      gp_data_up_to_2015 gdut
  WHERE     gdut.practiceid IN (%s)
  GROUP BY  gdut.hb, gdut.practiceid
  ", paste0("'",practice_list,"'", collapse=","))
  
  df <- execute_query(config, query)
  
  joined_df <- df %>%
    inner_join(points_df, by='orgcode')
    
  plot_df <- joined_df %>%
    group_by(hb) %>%
    summarise(avg_ratio=round(median(ratio), 4) * 100)
  
  g <- ggplot(
    data=plot_df, aes(x=reorder(hb, -avg_ratio), y=avg_ratio, 
                      fill=avg_ratio)) +
    geom_bar(stat="identity", position="identity") + 
    geom_text(aes(label=avg_ratio), vjust=0) +
    labs(
      title="Participation at HB level",
      x="HB",
      y="Median Participation Rate",
      fill="Range"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  file_name <- paste(plot_root_path, '/Q3_1.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. 7A5 has the highest participation rate
  # 2. 7A7 has the lowest participation rate
  
  
  ### Q3.2 -------------------------------------------------------
  # Step 2:
  # To check the overall participation at the indicator level
  
  plot_df <- joined_df %>%
    group_by(indicator) %>%
    summarise(avg_ratio=round(median(ratio), 4) * 100)
  
  g <- ggplot(
    data=plot_df, aes(x=reorder(indicator, -avg_ratio), y=avg_ratio, 
                      fill=avg_ratio)) +
    geom_bar(stat="identity", position="identity") + 
    labs(
      title="Participation at Indicator level",
      x="Indicator",
      y="Median Participation Rate",
      fill="Range"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_flip()
  
  ##### [Interpretation] =======================
  # 1. Overall 28 % of Indicators are under 25 % participation rate
  # 2. Overall 72 % of Indicators are above 75 % participation rate

}

run_section_4 <- function(config, plot_root_path, points_df){
  #' @description
  #' A function to implement the open end analysis section 4
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' @param points_df data frame containing the points value
  #' 
  #' @example
  #' # run_section_4(config, plot_root_path, points_df)
  #' 
  #' @return discount_df dataframe containing the discount values
  
  plot_root_path <- section_set_up(4, plot_root_path)
  
  ## Q4 ----------------------------------------------------------
  ## To calculate the discounts and check if there is any significant 
  ## correlation to points and participation rate both at GP level
  
  # ASSUMPTION ---------------------------------------------------
  # nic - net cost of ingredients
  # actual cost = nic - discount + container allowance 
  # since we do not have definite information on container allowance charged in 
  # drug tarrif, we assume it not be a small value in the calculation and 
  # consider only the discount provided by GP to consumer
  # --------------------------------------------------------------
  
  
  ### Q4.1 -------------------------------------------------------
  # Step 1:
  # calculate discounts, do a sanity check
  
  query <- "
  SELECT    *
  FROM      public.base_table_view;
  "
  
  df <- execute_query(config, query)
  
  get_discount_category <- function(perc_discount){
    if (perc_discount == 0){
      return ("=0")
    } else if(perc_discount < 0){
      return ("<0")
    } else {
      return (">0")
    }
  }
  
  v_get_disc_cat <- Vectorize(get_discount_category)
  
  df <- df %>% 
    mutate(category=v_get_disc_cat(perc_discount)) 
  
  plot_df <- df %>% 
    filter(category != ">0")
  
  g <- ggplot(data=plot_df, aes(x=perc_discount, fill=category)) +
    geom_histogram(alpha=0.6, position = 'identity') +
    labs(
      title="Dual Histogram of percentage discount <0 and =0",
      x="Percentage Discount",
      y="Frequency",
      fill=""
    )
  
  file_name <- paste(plot_root_path, '/Q4_1.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. There are more than 1e5 values that are less than zero. This intersects
  #    with the scenario where the actual cost was greater than net cost.
  #    These rows needs to be excluded from further analysis
  # 2. There are few 'close to 100' values that are equal to zero. These
  #    rows needs to be excluded from further analysis
  
  
  ### Q4.2 -------------------------------------------------------
  # Step 2:
  # compare discounts at HB level
  
  discount_df <- df %>%
    filter(category == ">0") 
  
  plot_df <- discount_df %>%
    group_by(hb_name) %>%
    summarise(
      median_perc_discount=median(perc_discount),
      mean_perc_discount=mean(perc_discount)
    ) %>% 
    pivot_longer(
      cols=c('median_perc_discount', 'mean_perc_discount'),
      names_to="perf",
      values_to="value"
    )
  
  g <- ggplot(data=plot_df, aes(x=hb_name, y=value, group=perf, color=perf)) +
    geom_point() + 
    geom_line() +
    labs(
      title="Discounts at HB level",
      x="Health Board",
      y="Median / Mean Discount",
      color="Statistic"
    )
  file_name <- paste(plot_root_path, '/Q4_2.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. 7A7 has the highest discount, and from Q3.1 interpretation the HB 
  #    had lowest participation rate
  # 2. 7A5 has the lowest discount, and from Q3.1 interpretation the HB had 
  #    highest participation rate
  
  
  ### Q4.3 -------------------------------------------------------
  # Step 3:
  # Top 10 medication prescription chapters with discounts
  
  query <- "
   SELECT    base_table.hb_name,
             base_table.perc_discount,
             bnf_table.chapterdesc AS med_category
   FROM      (
              select    *
              FROM      public.base_table_view
              ) AS base_table
   INNER JOIN bnf AS bnf_table
   ON        LEFT(base_table.bnfcode, 9) = bnf_table.bnfchemical;
  "
  med_df <- execute_query(config, query)
  med_df$med_category <- trimws(med_df$med_category)
  
  plot_df <- med_df %>%
    group_by(med_category) %>%
    summarise(median=median(perc_discount), mean=mean(perc_discount)) %>% 
    pivot_longer(
      cols=c('median', 'mean'),
      names_to="statistic",
      values_to="value"
    )
  
  g <- ggplot(
    data=plot_df, 
    aes(x=reorder(med_category, -value), 
        y=value, group=statistic, color=statistic)) +
    geom_point() + 
    geom_line() +
    labs(
      title="Discounts at Medication Prescription Chapter level",
      x="Medication Chapter",
      y="Median / Mean Discount",
      color="Statistic"
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  file_name <- paste(plot_root_path, '/Q4_3.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. Median is more stable than the mean that could be influenced by
  #    outliers
  # 2. Highest discount is given to stoma appliances chapter as per mean 
  #    and median
  # 3. Lowest discount is given to Other drugs and preparations as per mean
  #    and is given to infections chapter as per median
  
  ### Q4.4 -------------------------------------------------------
  # Step 4:
  # To check if there is any significant correlation of Points,  discounts and
  # Participation rate at GP level
  
  points_sub_df <- points_df %>%
    select(orgcode, points, ratio) %>%
    rename(practice_id=orgcode) %>% 
    group_by(practice_id) %>%
    summarize(points=median(points), p_rate=median(ratio))
  
  discounts_sub_df <- discount_df %>%
    select(perc_discount, practice_id) %>%
    group_by(practice_id) %>%
    summarize(discounts=median(perc_discount))
  
  gp_df <- points_sub_df %>%
    inner_join(discounts_sub_df, by='practice_id')
  
  file_name <- paste(plot_root_path, '/Q4_4.png', sep="")
  
  png(filename = file_name, 
      width = 480, height = 480, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"), antialias = "d")
  
  pairs.panels(
    gp_df[, c('points', 'discounts', 'p_rate')],
    main="Pearson correlation on Features")
  dev.off()
  
  ##### [Interpretation] =======================
  # 1. Points - Discounts - Participation rate does not have a linear 
  #    relationship with each other
  # 2. Points have multi-modal distribution, while discounts and p_rate have
  #    uni modal distribution
  # 3. Points follow a normal distribution, when discounts and p_rate follow
  #    non-normal distribution, both are left skewed with lighter and 
  #    longer left tail
  # 4. Pearson correlation coefficients for points - discounts state a 
  #    -ve correlation, for points - p-rate a +ve correlation, and for
  #     discounts - p_rate a -ve correlation
  
  
  return (discount_df)
}

run_section_5 <- function(config, plot_root_path, points_df){
  #' @description
  #' A function to implement the open end analysis section 5
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' @param points_df data frame containing the points value
  #' 
  #' 
  #' @example
  #' # run_section_5(config, plot_root_path, points_df)
  #' 
  #' @return prevalence_df data frame containing the prevalence data
  
  plot_root_path <- section_set_up(5, plot_root_path)
  
  ## Q5 ----------------------------------------------------------
  ## To check if there is any significant correlation between prevalence and
  ## assigned points at the indicator level
  
  
  ### Q5.1 -------------------------------------------------------
  # Step 1:
  # To check for selected 60 indicators that there is any significant 
  # correlation between them
  
  # First, get prevalence data
  query <- "
  SELECT    *
  FROM      prev_tbl_view;
  "
  prevalence_df <- execute_query(config, query)
  
  # second, merge the prevalence data with the points and then roll up the
  # data at the indicator level
  points_sub_df <- points_df %>%
    select(indicator, points) %>%
    group_by(indicator) %>%
    summarize(median_points=median(points), mean_points=mean(points))
  
  prevalence_sub_df <- prevalence_df %>%
    select(indicator, prevalence) %>%
    group_by(indicator) %>%
    summarize(median_prev=median(prevalence), mean_prev=mean(prevalence))
  
  
  combined_df <- points_sub_df %>%
    inner_join(prevalence_sub_df, by='indicator')
  
  
  # third, plot the Pearson correlation on prevalence and points
  file_name <- paste(plot_root_path, '/Q5_1.png', sep="")
  
  png(filename = file_name, 
      width = 480, height = 480, units = "px", pointsize = 12,
      bg = "white", res = NA, family = "", restoreConsole = TRUE,
      type = c("windows", "cairo", "cairo-png"), antialias = "d")
  
  pairs.panels(
    combined_df[, 
                c('median_points', 'mean_points', 'median_prev', 'mean_prev')
                ],
    main="Pearson correlation on Features")
  dev.off()
  
  ##### [Interpretation] =======================
  # 1. Median points and mean points are closer and shows a linear relation.
  #    This shows that both median and mean are stable. There is a positive
  #    correlation with median and mean points
  # 2. Median prevalence and mean prevalence are closer and shows a linear
  #    relation. This shows that both median and mean are stable for prevalence
  #    values of indicators. There is a positive correlation with median and
  #    mean points 
  # 3. Median points - median prevalence seem to have a correlation coefficient
  #    of -0.34, meaning it has a negative relation with each other. But, as,
  #    per the plot, the relationship is a non linear relationship
  
  return (prevalence_df)
}

run_section_6 <- function(
    config, plot_root_path, points_df, prevalence_df, discount_df){
  #' @description
  #' A function to implement the open end analysis section 6
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' @param points_df data frame containing the points value
  #' @param prevalence_df prevalence data frame
  #' @param discount_df data frame containing discount values
  #' 
  #' @example
  #' # run_section_6(
  #'     config, plot_root_path, points_df, prevalence_df, discount_df)
  #' 
  #' @return None
  
  plot_root_path <- section_set_up(6, plot_root_path)
  
  ## Q6 ----------------------------------------------------------
  ## To try a linear regression model to model (Y) points as dependent variable
  ## a function of independent variables (X) participation rate, prevalence and
  ## discount. Post modelling perform model diagnostics
  
  
  ### Q6.1 -------------------------------------------------------
  # Step 1: 
  # create the dataset, individually select the data
  points_df <- points_df %>% rename(practiceid=orgcode)
  
  part1_df <- points_df %>%
    select(practiceid, indicator, ratio, points)
  
  part2_df <- prevalence_df %>% 
    select(practiceid, indicator, prevalence)
  
  
  part3_df <- discount_df %>%
    select(practice_id, perc_discount) %>%
    rename(practiceid=practice_id) %>% 
    group_by(practiceid) %>%
    summarize(perc_discount=median(perc_discount))
  
  # second, join the part data sections
  
  combined_df <- part1_df %>%
    inner_join(part2_df, by=c('practiceid', 'indicator')) %>%
    inner_join(part3_df, by=c("practiceid"))
    
  
  # defining the variables
  depedent_var <- 'points'
  indepdent_var <- c('ratio', 'prevalence', 'perc_discount')
  
  plot_df <- combined_df %>% 
    select(indepdent_var) %>% 
    pivot_longer(
      cols=indepdent_var,
      names_to="features",
      values_to="values"
    )
  
  # check for outliers
  g <- ggplot(data=plot_df, aes(x=features, y=values)) +
    geom_boxplot() +
    labs(
      title="Box plot for features",
      x="Features / Independent variables",
      y="Values"
    )
  
  file_name <- paste(plot_root_path, '/Q6_1.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. Values for percentage discount are strictly around 7-8, for prevalence
  #    the values lies between 0 - 5, for ratio the values lies between 0 - 1
  #    The values of features are in different scales and ranges.
  # 2. There are outliers in prevalence
  # 3. The scales are different for percentage discount, prevalence and ratio
  #    this states that the data need to be standardized before use
  
  ### Q6.2 -------------------------------------------------------
  # Step 2: 
  # standardize the data using z-score standardize method
  # where standardized value = (X - mean) / std deviation
  
  combined_df$prevalence <-
     (combined_df$prevalence - mean(combined_df$prevalence)) /
       sd(combined_df$prevalence)
  
   combined_df$ratio <-
     (combined_df$ratio - mean(combined_df$ratio)) /
     sd(combined_df$ratio)
  
   combined_df$perc_discount <-
     (combined_df$perc_discount - mean(combined_df$perc_discount)) /
     sd(combined_df$perc_discount)

   plot_df <- combined_df %>%
     select(indepdent_var) %>%
     pivot_longer(
       cols=indepdent_var,
       names_to="features",
       values_to="values"
     )
   g <- ggplot(data=plot_df, aes(x=features, y=values)) +
     geom_boxplot() +
     labs(
       title="Box plot for features",
       x="Features / Independent variables",
       y="Values"
     )
  
   file_name <- paste(plot_root_path, '/Q6_2.png', sep="")
   ggsave(filename=file_name, plot=g)
  
  
  ##### [Interpretation] =======================
  # 1. After standardization the scales are in same range
  # 2. The data prevalence and ratio are skewed and follow non normal 
  #    distribution. There are outliers in percentage discount
  
  
  ### Q6.3 -------------------------------------------------------
  # Step 3:
  # To create a linear model
  
  model_data <- combined_df %>%
    select(ratio, points, prevalence, perc_discount)
  
  linear_reg_model <- lm(
    points ~ perc_discount + prevalence + ratio, data=model_data)
  
  print(summary(linear_reg_model))
  
  display_txt <- "
  Model Diagnostics of Baseline Model ------------------------------
  #1. Residuals
      The median is closer to mean (expected to be zero). The 1st and 3rd 
      quartile are almost.
  
  #2. Predictor - Outcome variable association
      Higher t-statistic and its associated p value suggests that in the order
      of significance, ratio and pervalence has the highest significance and
      perc_discount has no significance towards the dependent variable.
      It states that there is significant association of ratio / participation
      rate and prevalence with the outcome  variable.
     
  #3. Goodness of Fit
      R2 value for the model is 0.12, indicating that model did not explain
      much of the variablity in the outcome.
      F-statistic value are higher stating overall significance of the model of
      atleast 1 predictor has non-zero coefficient.
  
  "
  display_logs(display_txt, "success")
  ### Q6.4 -------------------------------------------------------
  # Step 4:
  # To create a linear model removing the outlier
  
  model_data_outlier_cleaned <- model_data[
    which(
      (model_data$perc_discount < 3 & model_data$perc_discount > -3) &
      (model_data$prevalence < 3 & model_data$prevalence > -3) &
      (model_data$ratio < 3 & model_data$ratio > -3)
    ),
  ]
  
  plot_df <- model_data_outlier_cleaned %>% 
    select(indepdent_var) %>% 
    pivot_longer(
      cols=indepdent_var,
      names_to="features",
      values_to="values"
    )
  g <- ggplot(data=plot_df, aes(x=features, y=values)) +
    geom_boxplot() +
    labs(
      title="Box plot for features",
      x="Features / Independent variables",
      y="Values"
    )
  
  model_data_outlier_cleaned <- model_data_outlier_cleaned %>%
    select(ratio, points, prevalence, perc_discount)
  
  linear_reg_model <- lm(
    points ~ perc_discount + prevalence + ratio, data=model_data_outlier_cleaned)
  
  display_logs("After Removing % of outliers", "info")
  print(summary(linear_reg_model))
  
  
  display_txt <- "
  #1. Trimming outliers did not improve the model. The Multiple R-Squared value
      did not improve in comparison to the earlier model
  "
  display_logs(display_txt, "success")
  
}

run_section_7 <- function(config, plot_root_path){
  #' @description
  #' A function to implement the open end analysis section 7
  #' 
  #' @param config loaded config yaml object
  #' @param plot_root_path path to store the plots
  #' 
  #' 
  #' @example
  #' # run_section_7(config, plot_root_path)
  #' 
  #' @return None
  
  plot_root_path <- section_set_up(7, plot_root_path)
  
  
  ## Q7 ----------------------------------------------------------
  ## Try a prediction model for a given HB.
  
  
  ### Q7.1 -------------------------------------------------------
  # Step 1:
  # Get the Health board name from user and create data
  display_txt <- "
  \n \tEnter the Health Board Name  (case sensitive) Example: 7A2
  "
  
  input_pattern <- "7A[1-7]"
  hb_name <- get_user_input(
    display_txt, return_type="str", pattern=input_pattern)
  
  query <- sprintf("
  SELECT    gdut.practiceid,
            gdut.items,
            gdut.quantity,
            gdut.actcost AS actual_cost, 
            gdut.period - (gdut.period / 100) * 100 AS month,
            CAST((gdut.period / 100) AS INTEGER) AS year
  FROM      (
             SELECT    *
             FROM      gp_data_up_to_2015
             ) AS gdut
  WHERE     gdut.hb = '%s';
  ", hb_name)
  
  display_logs('Preparing data ...', 'process')
  data <- execute_query(config, query)
  
  # How the data is prepared ?
  # With items, quantity, actual cost of current month, the independent variables
  # the dependent variable Y will be the actual cost of the next month.
  # for example, for actual cost of 2013 Jan month, the dependent variable Y is
  # actual cost of  next month 2013 FEB.
  # Create a sub data accordingly and merge it with the main data.
  # The predictions will be made for the year 2015 and the ground truth will be
  # actual cost of 2015
  
  modelling_data <- data.frame()
  for(year in c(2013, 2014)){
    for(month in 1:12){
      
      # get the summary statistic of the items
      sub_data <- data %>%
        group_by(practiceid, month, year) %>%
        summarize(cost=median(actual_cost), 
                  quantity=median(quantity),
                  items=median(items))
      new_year <- year + 1
      new_month <- month + 1
        
      if (month == 12){
        new_month <- 1 # to handle the last month
      }
        
      # filter the data
      current_month_data <- sub_data[
        which(sub_data$year == year & 
                sub_data$month == month),
      ]
      next_month_data <- sub_data[
        which(sub_data$year == new_year &
                sub_data$month == new_month),
      ]
      
      # add to data only if the selected month + year combination has data
      # in it.
      if (dim(current_month_data)[1] > 0 & dim(next_month_data)[1]){
        next_month_data <- next_month_data %>%
          select(cost, practiceid)
        
        merged_data <- next_month_data %>% 
          rename(y=cost) %>%
          inner_join(current_month_data, by='practiceid') %>%
          select(cost, items, quantity, y)
        
        modelling_data <- rbind(merged_data)
      }
    }
  }

  # standardizing the data in-order to make the data more centralized
  
  standardized_data <- data.frame(
    cost=(modelling_data$cost - mean(modelling_data$cost)) /
      sd(modelling_data$cost),
    items=(modelling_data$items - mean(modelling_data$items)) /
      sd(modelling_data$items),
    quantity=(modelling_data$quantity - mean(modelling_data$quantity)) /
      sd(modelling_data$quantity),
    y=modelling_data$y
    )
  
  display_logs('Drawing plots ...', 'process')
  plot_df <- modelling_data %>%
    pivot_longer(
      cols=c('cost', 'items', 'quantity'),
      names_to="features",
      values_to="values"
    )
  
  g <- ggplot(data=plot_df, aes(x=values, y=features)) +
    geom_boxplot() +
    labs(
      title="Modelling data distribution",
      x="Values",
      y="Features"
    )
  
  file_name <- paste(plot_root_path, '/Q7_1_',hb_name,'.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  ##### [Interpretation] =======================
  # 1. The features quantity, items and cost after standardization contains
  #    negative values, ranges of outliers 
  
  display_logs("Modelling Data...", "process")
  # Split the data into train test split for training and testing the model
  sample <- sample(c(TRUE, FALSE), nrow(modelling_data), replace=TRUE, 
                   prob=c(0.8,0.2))
  train  <- modelling_data[sample, ]
  test   <- modelling_data[!sample, ]
  
  ### Q7.2 -------------------------------------------------------
  # Step 2:
  # Create a regression model
  linear_model <- lm(y ~ cost + items + quantity, data=train)
  
  print(summary(linear_model))
  
  # test the model
  print("Mean Absolute Error:")
  print(mae(test$y, predict(linear_model, test)))
  
  
  # plotting the graph
  res <- resid(linear_model)
  fitted_model <- fitted(linear_model)
  residual_df <- data.frame(res=res, fitted=fitted_model)
  display_logs("Drawing Plots...", "process")
  g <- ggplot(data=residual_df, aes(x=res, y=fitted_model)) +
    geom_point() +
    labs(
      title="Residual of lm y ~ cost + items + quantity",
      x="Residual",
      y="Fitted mdel"
    )
  file_name <- paste(plot_root_path, '/Q7_2_',hb_name,'.png', sep="")
  ggsave(filename=file_name, plot=g)
  
  display_txt <- "
  Predictive model interpretation ------------------------------
  As per the given Health Board, in the model summary
  
  #1. If the t-statistic value is higher and its associated pvalue is lower, 
      then the predictor variable has strong association with the outcome 
      variable. Higher t-statistic is better and lower associated p-value is
      better.
  
  #2. If the Mean Absolute Error is lesser than 2, the model is predicting well
      test dataset, but more optimization and model tuning is required for 
      smaller value of MAE. Lower is better.
  
  #3. If the Mutliple R-Squared value is closer to 1, then the model was able to
      able outcome variable with the predictor variables. Higher closer to 1 is
      better.
      
  #4. If the Residual Standard Error is lower closer to 0 is better.
      
  
  "
  
  display_logs(display_txt, "success")
  
}


# Handler -----------------------------------------------------------------


#. timer
run_analysis <- function(config){
  #' @description
  #' A function to implement the Open End Analysis
  #' Function execution time: 5 mins
  #' 
  #' @param config loaded config yaml object
  #' 
  #' @example
  #' # run_analysis(config)
  #' 
  #' @return None
  
  display_logs("Running Analysis ...", "info")
  display_logs("Estimated function execution time: 5 mins", "info")
  
  
  # Purpose -----------------------------------------
  # The exploratory data analysis focuses on two concepts 
  # 
  # Concept #1:- 
  #   1. With assumptions, carry out an initial analysis on net cost and 
  #      actual cost of medication, derive an estimated discounts column,
  #      derive a achievement point columns and understand correlation and 
  #      relationship between Discounts, Prevalence,assigned Achievement Points 
  #      and Participation Rate. (section 1 - 6)
  # 
  #   2. Try out a predictive model for predicting 1 months actual cost, for
  #      a given HB. (section 7)
  # 
  # -------------------------------------------------
  
  
  # Assumption --------------------------------------
  # Since in QOF Achievement the data has only 2015, the exploration is
  # limited to period 2015
  # -------------------------------------------------
  
  plot_root_path_main <- "./output/question5"
  
  run_section_1(config, plot_root_path_main)

  points_df <- run_section_2(config, plot_root_path_main)

  run_section_3(config, plot_root_path_main, points_df)

  discount_df <- run_section_4(config, plot_root_path_main, points_df)

  prevalence_df <- run_section_5(config, plot_root_path_main, points_df)

  run_section_6(
    config, plot_root_path_main, points_df, prevalence_df, discount_df)
  
  user_continue_input <- "Y"
  while(user_continue_input == "Y"){
    run_section_7(config, plot_root_path_main)
    # check if the user wants to check for another HB
    display_txt <- "Do you want to check for another HB : (Y/n) "
    user_continue_input <- get_user_input(
      display_txt, return_type="str", pattern='[Y|n]')
  }
  print("-----------------------------------------------------------")
}


# References -------------------------------------------------
## [1] https://www.health-ni.gov.uk/publications/qof-guidance-regional-board-and-practices
## [2] https://www.mysurgerywebsite.co.uk/website/IGP217/files/103%20QOF_Payments.pdf

