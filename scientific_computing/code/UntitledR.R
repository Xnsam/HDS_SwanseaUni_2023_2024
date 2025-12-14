library(RPostgreSQL)    # To access the database.
library(GetoptLong)     # To substitute variables into strings.
      # used to reshape the data
library(tidyverse)
library(ggplot2)
library(plotly)
library(reshape2) 

getConnection <- function(){
  driver <- dbDriver("PostgreSQL")
  db_con <- dbConnect(
    driver, dbname="gp_practice_data", host="localhost",
    port=5432, user="postgres",
    password="PgAdmin@123"
  )
  return(db_con)
}

getYearData <- function(hb_name, gp, query, db_con){
  # description
  
  result <- dbGetQuery(db_con, query)
  
  # if there period does not have more than 1 yr worth data then it is not 
  # possible to do year on year analysis
  period_range <- result %>%
    mutate(year=as.integer(period/100)) %>%
    distinct(year)
  
  if(length(period_range$year) > 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


openEndAnalysis <- function(){
  # description
  # spend analysis
  
  # replace this with the get db connection function
  conn <- getConnection()
  
  
  # cost across all the health boards
  # year on year expense of a GP from a health board
  print("Calculating year on year expense of a GP from a health board")
  query <- "select distinct(hb) from gp_data_up_to_2015;"
  hb_list <- dbGetQuery(conn, query)$hb
  
  hb_monthly_yearly_cost <- data.frame()
  for(hb in hb_list){
    query <- paste("select * from gp_data_up_to_2015 where hb='",hb,"';", 
                   sep="")
    hb_data <- dbGetQuery(conn, query)
    
    # create month and year columns
    hb_data <- hb_data %>%
      mutate(year=as.integer(period/100), 
             month=as.integer(period - (year * 100))) 
  
    
    hb_data2 <- hb_data %>%
      group_by(year, month) %>%
      summarise(net_cost=sum(nic))
    
    hb_data2[["hb_name"]] <- rep(hb, dim(hb_data2)[[1]])
    
    hb_monthly_yearly_cost <- rbind(hb_monthly_yearly_cost, hb_data2)
    
  }
  
  # scale the net cost for better visualization
  hb_monthly_yearly_cost <- hb_monthly_yearly_cost %>%
    mutate(net_cost=round(net_cost / 100000, 2))
  
  
  # Aggregate data to get average quantity sold per month for each product
  agg_data <- aggregate(net_cost ~ month + hb_name + year, hb_monthly_yearly_cost, 
                        mean)
  
  # Create the plot
  ggplot(agg_data, aes(x = factor(month), y = net_cost, 
                       group = hb_name, color = hb_name)) +
    geom_line() +
    geom_point(size = 3) +
    labs(x = "Month", y = "Average Cost of prescriptions (in 100k) ", 
         color = "Health Board") +
    theme_minimal() +
    ggtitle("Monthly Net Cost Trends Across Health Board (2013-2015)") +
    facet_grid(year~.)
  
  
  print("Given health board and GP calculate the Net cost descriptive statistics")
  # replace this with the already written function
  hb_name <- readline("enter hb name ")
  gp <- readline("Enter GP id ")
  query <- sprintf("select * from gp_data_up_to_2015 where hb='%s' and practiceid='%s'", hb_name, gp)
  gotData <- FALSE
  while(gotData != TRUE){
    status <- getYearData(hb_name, gp, query, conn)
    if(status){
      gotData <- TRUE
    }
  }
  
  result <- dbGetQuery(conn, query)
  
  result <- result %>%
    mutate(year=as.factor(as.integer(period/100)))
    
  # compare the mean and median net cost to understand how skewed the data is
  g <- result %>%
    group_by(year) %>%
    summarize(average_net_cost=mean(nic), median_net_cost=median(nic)) %>%
    melt(id="year") %>%
    ggplot(aes(y=year, x=value, fill=variable)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Mean vs Median Net cost", x="Value", y="Year")
  plot(g)
  
  # to understand the range of the data
  g <- result %>%
    group_by(year) %>%
    summarize(max_net_cost=max(nic), min_net_cost=min(nic)) %>%
    melt(id="year") %>%
    ggplot(aes(y=year, x=value, fill=variable)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Max vs Min Net cost", x="Value", y="Year")
  plot(g)
  
  # to understand the distribution of the data
  g <- result %>%
    ggplot(aes(x=nic, fill=year)) +
      geom_density() +
      labs(title="Year on year Distribution of Net cost", x="Net Cost")
  plot(g)
  
  # check for outliers in each year
  g <- result %>%
    ggplot(aes(x=nic, fill=year)) +
      geom_boxplot() +
      labs(title="Outliers comparison for year on year of net cost", 
           x="Net cost", y="Year")
  plot(g)
  
  print("Given health board and GP calculate the Net cost descriptive statistics")
  # year on year cost vs quantity for top 10 medicines quantity prescribed 
  g <- result %>%
    group_by(year, bnfname) %>%
    summarize(median_nic=median(nic), median_qty=median(quantity)) %>%
    arrange(desc(median_qty)) %>%
    slice_head(n=10) %>%
    mutate(median_qty=median_qty)
    ggplot(aes(x=median_qty, y=bnfname, size=median_nic, color=year)) +
      geom_point(alpha=0.5) +
      scale_size(range=c(0.1, 24), name="Net Cost") +
      labs(x="Quantity", y="BNF Name", 
           title="Year on year top 10 medicines prescribed")
  plot(g)
  
  # year on  year cost vs quantity for bottom 10 medicines
  g <- result %>%
    group_by(year, bnfname) %>%
    summarize(median_nic=median(nic), median_qty=median(quantity)) %>%
    arrange(desc(median_qty)) %>%
    slice_tail(n=10) %>%
    ggplot(aes(x=median_qty, y=bnfname, size=median_nic, color=year)) +
      geom_point(alpha=0.5) +
      scale_size(range=c(0.1, 24), name="Net Cost") +
      labs(x="Quantity", y="BNF Name",
           title="Year on year bottom 10 medicines prescribed")
  plot(g)
  
  
  # linear regression for estimating cost of the given HB
  
  # get HB
  # get gp
  
  gotData <- FALSE
  while(gotData != TRUE){
    # change this as per the input
    hb_name <- readline("enter the hb name ")
    gp_id <- readline("enter the gp id ")
    
    query <- sprintf("select * from gp_data_up_to_2015 where hb='%s' and practiceid='%s'", hb_name, gp_id)
    status <- getYearData(hb_name, gp, query, conn)
    if(status){
      gotData <- TRUE
    } else {
      print("data not found please try other hb name and gp id ")
    }
  }
  result <- dbGetQuery(conn, query)
  
  # check total monthly spending over the year
  result %>% 
    mutate(month=as.factor(period - (as.integer(period/100) * 100)),
           year=as.factor(as.integer(period/100))) %>%
    group_by(year, month) %>%
    summarize(total_nic=sum(nic)/10000) %>% 
    ggplot(aes(y=total_nic, x=month, fill=year)) +
      geom_bar(position="dodge", stat="identity") +
      labs(title="Month wise net cost for year", x="Month", 
           y=" Net Cost in (100k)")
  
  # write the insight
  #
  # _________________________
  
  # create features for regression
  head(result)
  
  # --- identify features for the model
  bnf_df <- result %>% 
    group_by(bnfname) %>% 
    summarise(bnf_count=n()) %>% 
    arrange(desc(bnf_count))
  
  model_data <- result %>%
    left_join(bnf_df, by=c("bnfname" = "bnfname")) %>%
    mutate(
      month=as.factor(period - (as.integer(period/100) * 100)),
      year=as.factor(as.integer(period/100)),
      cost_diff=nic-actcost,
    ) %>%
    group_by(year, month) %>%
    summarize(
      # number of unique prescriptions per month
      num_prescriptions=n(),
      # number of total items per month
      total_items=sum(items),
      mean_items=mean(items),
      median_items=median(items),
      # difference of net cost and actual cost per month
      total_cost_difference=sum(cost_diff),
      mean_cost_difference=mean(cost_diff),
      median_cost_difference=median(cost_diff),
      # quantity per month 
      total_quantity=sum(quantity),
      mean_quantity=mean(quantity),
      median_quantity=median(quantity),
      # month cost
      month_cost=sum(nic)
    )
  dim(model_data)
  
  # train data
  train_data <- model_data %>% slice_head(n=30)
  test_data  <- model_data %>% slice_tail(n=3)
  
  # train a regression model
  lr_model <- lm(month_cost ~ .,  data=train_data)
  summary(lr_model)
  
  # test the regression model
  test_data$prediction <- predict(lr_model, test_data 
                                     %>% select(-month_cost))
  head(test_data)
  print(mean(abs(test_data$prediction - test_data$month_cost)))
  # plot the residuals
  plot(lr_model$residuals, pch = 16, col = "red")
  
}


openEndAnalysis()






