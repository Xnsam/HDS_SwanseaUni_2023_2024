source('./code/custom_utils.R')


library(tidyverse)
library(stringr)


config <- set_up()


# Functions --------------------------------------------------

wait_for_user_input <- function(){
  #' @description
  #' A function to wait for the user to put the input
  #' 
  #' @param None
  #' 
  #' @retun None
  
  invisible(readline(prompt="Press [enter] to continue"))
}

create_query_views <- function(){
  #' @description
  #' A function to create query views for repurposing the queries
  #' 
  #' @param None
  #' 
  #' @retun None
  
  
}



assign_points <- function(ratio_val, indicator_val){
  #' @description
  #' A function to assign achievement points given ratio and indicator
  #' 
  #' @param ratio_val value of the ratio of num / denom
  #' @param indicator_val value of the indicator
  #' 
  #' 
  #' @retun pt achievement point
  
  indicator_NI_suffixed <- paste(indicator_val, "NI", sep="")
  
  idx_t1 <- which(qof_points_df['indicator'] == indicator_NI_suffixed)
  idx_t2 <- which(qof_points_df['indicator'] == indicator_val)
  
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
    row_df <- qof_points_df[idx, col_names]
    
    thresholds <- strsplit(row_df$threshold_in_perc, split='-')
    
    max_point <- row_df$points
    threshold_lower <- as.integer(thresholds[[1]][1]) / 100
    threshold_upper <- as.integer(thresholds[[1]][2]) / 100
    indicator_type <- row_df$indicator_type
    
    
    # Note: The logic for assigning points as per reference [2]
    
    
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
      # Boolean indicators are awarded points without percentage
      pt <- max_point 
    }
  }
  return (pt)
}


# Main ----------------------------------------------------------

# Since in QOF Achievement the data has only 2015, the exploration is
# limited to period 2015

## Q1 ----------------------------------------------------------
## Brief Analysis of Actual cost and Net cost

### Q1.1 -------------------------------------------------------
# First move
# Are there inconsistencies in the actual cost and net cost of the medication?

query <- "
select    count(check_table.category),
          check_table.category
from      (select    base_table_view0.bnfcode,
           case
              when actual_cost > net_cost then 'actual_cost > net_cost'
              when actual_cost < net_cost then 'actual_cost < net_cost'
              when actual_cost = net_cost then 'actual_cost = net_cost'
              when net_cost = 0 and actual_cost != 0 
                 then 'net_cost = 0 and actual_cost != 0'
              when net_cost != 0 and actual_cost = 0 
                 then 'net_cost != 0 and actual_cost = 0'
              when net_cost = 0 and actual_cost = 0 
                 then 'net_cost = 0 and actual_cost = 0'
           else
              'no_one'
           end as   \"category\" 
           from     base_table_view0) as check_table
group by
	  check_table.category;
"

df <- execute_query(config, query)

head(df)

plot_df <- df %>%
  filter(count > 500) %>%
  mutate(perc=round(((count/sum(count)) * 100), 2)) %>% 
  mutate(ypos = cumsum(perc) - 0.5*perc)

head(plot_df)


ggplot(data=plot_df, aes(x="", y=perc, fill=category)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = perc), color = "white", size=6) +
  labs(
    title="Percentage of inconsistencies > 500 records",
    x="",
    y="",
    fill="Category"
  ) +
  theme_void()

# Interpretation
# only 1.91% of the data has actual cost > net cost


### Q1.2 -------------------------------------------------------
# next move:
# To look at contribution of 1.91 % category towards total actual cost

query <- "
select    check_table.category,
          count(check_table.category),
          round(cast(sum(check_table.actual_cost) / 547805146.03 as NUMERIC), 4)
                * 100 as perc_contribution,
          round(cast(sum(check_table.actual_cost) as NUMERIC), 2) 
               as total_actual_cost
          
from      (select    base_table_view0.bnfcode,
                     base_table_view0.actual_cost,
           case
              when actual_cost > net_cost then 'actual_cost > net_cost'
              when actual_cost < net_cost then 'actual_cost < net_cost'
              when actual_cost = net_cost then 'actual_cost = net_cost'
              when net_cost = 0 and actual_cost != 0 
                 then 'net_cost = 0 and actual_cost != 0'
              when net_cost != 0 and actual_cost = 0 
                 then 'net_cost != 0 and actual_cost = 0'
              when net_cost = 0 and actual_cost = 0 
                 then 'net_cost = 0 and actual_cost = 0'
           else
              'no_one'
           end as   \"category\" 
           from     base_table_view0) as check_table
group by
	  check_table.category;
"

df <- execute_query(config, query)

head(df)

plot_df <- df %>%
  filter(count > 500) %>%
  mutate(ypos = cumsum(perc_contribution) - 0.5*perc_contribution)


ggplot(data=plot_df, aes(x="", y=perc_contribution, fill=category)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = perc_contribution), color = "white", size=6) +
  labs(
    title="Percentage of contribution to total actual sum",
    x="",
    y="",
    fill="Category"
  ) +
  theme_void()


# Interpretation
# For every 10000 prescriptions, there are 27 prescriptions where
# the actual cost > net cost. it would be interesting to check 
# which medication category has actual cost > net cost

### Q1.3 -------------------------------------------------------
# next move:
# To look at instances where actual cost  > net cost relationship

query <- "
select    *
from      (
          select    * 
          from      public.base_table_view
          ) as base_table
where 
	base_table.actual_cost > base_table.net_cost;
"

cost_df <- execute_query(config, query)
dim(cost_df)
head(cost_df)

ggplot(data=cost_df, aes(x=net_cost, y=actual_cost, color=hb_name)) +
  geom_point() +
  labs(
    title="Actual Vs Net Cost per HB",
    x="Actual Cost",
    y="Net Cost",
    color="Health Board"
  )

# Interpretation
# Scatter plot shows that net cost and actual cost are linearly correlated
# meaning, there are no such data points which as marginally 
# higher net cost and lower actual actual cost and vice versa

### Q1.4 -------------------------------------------------------
# next move:
# To check which medications have actual cost > net cost and their difference

query <- "
select    cat_table.category,
          round(
               cast(
                    sum(distinct cat_table.actual_cost) as numeric), 
                4) as total_actual_cost,
          round(
               cast(
                    sum(distinct cat_table.net_cost) as numeric), 
                4) as total_net_cost
from     (
          select    net_actcost_tbl.bnfcode,
                    net_actcost_tbl.net_cost,
                    net_actcost_tbl.actual_cost,
                    b.chapterdesc as category,
                    b.sectiondesc as sub_category
          from   (
                  select    *
                  from      (
                             select    * 
                             from      public.base_table_view
                             ) as base_table
                  where 
                      base_table.actual_cost > base_table.net_cost
					        ) as net_actcost_tbl
					inner join bnf as b
					on 
					  b.bnfchemical = left(net_actcost_tbl.bnfcode, 9)
				 ) as cat_table
group by
	 cat_table.category
order by
	total_actual_cost;

"

cost_category_df <- execute_query(config, query)

cost_category_df$difference <- cost_category_df$total_actual_cost - 
  cost_category_df$total_net_cost

cost_category_df$category <- trimws(cost_category_df$category)


head(cost_category_df)

ggplot(data=cost_category_df, 
       aes(x=reorder(category, -difference), y=difference, fill=difference)) +
  geom_bar(stat="identity", position="identity") +
  labs(
    title="Chapter of medication with difference (actual - net cost)",
    x="Chapter ",
    y="Difference ( actual - net cost )",
    fill="Difference Range"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, face="bold"))



# Interpretation
# Evidently, the highest difference between actual cost and net cost is
# for Central nervous system medication chapter, followed by cardiovascular
# system, respiratory system. but why ?
# Inspection 1 - Is this a data error ? 
# as per data specification actual_cost = net_cost - discount + container_allowance
# Inspection 2 - could it be that container allowance is too high for the 
# identified chapters?
# Inspection 3 - is it associated to geographic location ?


### Q1.5 -------------------------------------------------------
# next move: 
# focus on central nervous system chapter 
# There is no concrete way of undertaking Inspection 1 and Inspection 2
# trying Inspection 3

execute_query(config, cat_table_view)

inspect3_query <- "
select    cat_table.category,
          address_table.county
from     (select    *
          from      public.cat_table_view
          ) as cat_table
inner join (select    *
            from      public.address_county_view) as address_table
on 
  address_table.practiceid = cat_table.practice_id
where
  trim(cat_table.category) = '%s';
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


title_txt <- "Top 20 Count of Prescriptions in county 
under Central Nervous System chapter"
ggplot(data=sub_df, 
       aes(x=reorder(county, -freq), y=freq, fill=freq)) +
  geom_bar(stat="identity", position="identity") +
  labs(
    title=title_txt,
    x="County",
    y="Count",
    fill="Frequency Range"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, face="bold"))



# Interpretation
# Regions of Glamorgan, Gwent, Dyfed have the highest prescription of CNS 
# medication. is there any social / geographical impact ? 

### Q1.6 -------------------------------------------------------
# next move:
# focus on second highest difference cardiovascular system
# in counties


query <- sprintf(inspect3_query, 'Cardiovascular System')

df <- execute_query(config, query)

plot_df <- df %>%
  group_by(county) %>% 
  summarise(freq=n())

head(plot_df)

sub_df <- plot_df %>% 
  arrange(desc(freq)) %>%
  slice_head(n=20)


title_txt <- "Top 20 Count of Prescriptions in county 
under Cardiovascular System chapter"
ggplot(data=sub_df, 
       aes(x=reorder(county, -freq), y=freq, fill=freq)) +
  geom_bar(stat="identity", position="identity") +
  labs(
    title=title_txt,
    x="County",
    y="Count",
    fill="Frequency Range"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, face="bold"))


# Interpretation
# For the second highest prescription of CVS medication, Regions of Glamorgan, 
# Gwent, Dyfed are in the top 5. interesting. but why ?
# why does these regions have higher actual cost than net cost for CVS & CNS

### Q1.7 -------------------------------------------------------
# next move: 
# for GWENT, MID GLAMORGAN, SOUTH GALMORGAN, WEST GLAMORGAN, DYFED
# find  the sub categories / medications for CNS and CVS 

query <- "
select    cat_table.category,
          cat_table.sub_category
from     (select    *
          from      public.cat_table_view
          ) as cat_table
inner join (select    *
            from      public.address_county_view) as address_table
on 
  address_table.practiceid = cat_table.practice_id
where
    trim(cat_table.category) in 
    ('Central Nervous System', 'Cardiovascular System') and
    upper(trim(county)) in 
    ('GWENT', 'MID GLAMORGAN', 'SOUTH GALMORGAN', 'WEST GLAMORGAN', 'DYFED');

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
      title=paste("Sections in the", categ, "Chapter"),
      x="",
      y="",
      fill="Count"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  bar_list <- c(bar_list, list(d))
}


grid.arrange(
  bottom="Section",
  left="Count",
  grobs = bar_list
)



# Interpretation
# In the selected counties,
# for Caridovascular System
#   Anti hypertensive therapy,  Hyper tension and heart failure and diuretics
#   prescribed medicines are the most prominent in this system
# for Central Nervous System
#   Pyschoses & Rel. Disorders, Analgensics and Hypnotics & Anxiolytics
#   prescribed medicines are the most prominent in this system


### Q1.8 -------------------------------------------------------
# next move:
# To check Which sections costs the most in the respective chapters


query <- "
select    cat_table.actual_cost,
          cat_table.net_cost,
          cat_table.sub_category,
          cat_table.category
from     (select    *
          from      public.cat_table_view
          ) as cat_table
inner join (select    *
            from      public.address_county_view) as address_table
on 
  address_table.practiceid = cat_table.practice_id
where
    trim(cat_table.category) 
       in ('Central Nervous System', 'Cardiovascular System') and
    upper(trim(county)) 
      in ('GWENT', 'MID GLAMORGAN', 'SOUTH GALMORGAN', 'WEST GLAMORGAN', 
          'DYFED');
"

df <- execute_query(config, query)
df$category <- trimws(df$category)
df$sub_category <- trimws(df$sub_category)


head(df)
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
      title=paste("Costs of Sections in the", categ, "Chapter"),
      x="",
      y="",
      fill="Cost Type"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  bar_list <- c(bar_list, list(d))
}


grid.arrange(
  bottom="Section",
  left="Cost",
  grobs = bar_list
)


# Interpretation
# In the selected counties
# For Cardiovascular System chapter
#  Diuretics medication cost the most, followed by Antihypertensive Therapy
#  and Hypertension and Heart failure medication
# For Central Nervous System chapter
#  Psychoses & Rel. Disorders, hypnotics cost the most, followed by Anxiolytics 
# and antidepressant drugs




## Q2 ----------------------------------------------------------
# Achievement Points - Derivation and Significance


### Q2.1 -------------------------------------------------------
## 1. Create a points column in the qof achievement table

query <- "
select    * 
from      qof_achievement;
"

qof_df <- execute_query(config, query)

# Note: This CSV file was created manually referring to the files from the
# website mentioned in reference [1]
qof_points_df <- read_csv('./data/assets/qof_thresholds.csv')

v_assign_points <- Vectorize(assign_points)

points_df <- qof_df %>%
  mutate(points=v_assign_points(ratio_val=ratio, indicator_val=indicator))


ggplot(data=points_df, aes(x=points, y=indicator)) +
  geom_point()



# points_df <- subset(points_df, points > 0)

query <- "
select    distinct(gdut.hb) as health_board_name
from      (select    hb
           from      gp_data_up_to_2015
           where     cast(period / 100 as integer) = 2015
          ) as gdut
group by
      gdut.hb
"
hb_data <- execute_query(config, query)
avg_points_hb <- c()
top5_indicator_df <- data.frame()
highest_performing_gp <- data.frame()
lowest_performing_gp <- data.frame()

for (row in 1:nrow(hb_data)){
  hb <- hb_data[row, "health_board_name"]
  
  practice_list <- get_practice_list_from_hb(config, hb)
  
  filtered_df <- new_df %>%
    filter(orgcode %in% practice_list$practiceid)
  
  mean_count <- filtered_df %>%
    select(points, indicator) %>% 
    pull(points) %>%
    mean()
  
  # 1.1 For year 2015, show average points per HB
  avg_points_hb <- c(avg_points_hb, mean_count)
  
  
  # 2.1 for year 2015, show top 5 indicator points per HB
  sub_df <- filtered_df %>%
    group_by(indicator) %>% 
    summarise(max_point=max(points)) %>%
    arrange(desc(max_point)) %>% 
    slice_head(n=5)
  
  tmp_df <- data.frame(
    hb_name=rep(hb, dim(sub_df)[1]),
    indicator=sub_df$indicator,
    points=sub_df$max_point
  )
  
  top5_indicator_df <- rbind(top5_indicator_df, tmp_df)
  
  # 3. for year 2015, show top 5 highest performing GP in every HB
  sub_df <- filtered_df %>%
    group_by(orgcode) %>%
    summarise(total_points=sum(points)) %>% 
    select(orgcode, total_points) %>%
    arrange(desc(total_points)) %>%
    slice_head(n=5)
  
  tmp_df <- data.frame(
    hb_name=rep(hb, dim(sub_df)[1]),
    practice_id=sub_df$orgcode,
    total_points=sub_df$total_points
  )
  
  highest_performing_gp <- rbind(highest_performing_gp, tmp_df)
  
  # 4. for year 2015, show top 5 lowest performing GP in every HB
  sub_df <- filtered_df %>%
    group_by(orgcode) %>%
    summarise(total_points=sum(points)) %>%
    select(orgcode, total_points) %>%
    arrange(total_points) %>%
    slice_head(n=5)
  
  tmp_df <- data.frame(
    hb_name=rep(hb, dim(sub_df)[1]),
    practice_id=sub_df$orgcode,
    total_points=sub_df$total_points
  )
  
  lowest_performing_gp <- rbind(lowest_performing_gp, tmp_df)
}

hb_data <- cbind(hb_data, avg_points = avg_points_hb)


# 1.2 display health board average points data

print(hb_data)

# 2.2 display top 5 indicator points per HB

ggplot(data=top5_indicator_df, aes(x=hb_name)) +
  geom_bar(
    aes(y=points, fill=indicator, group=indicator),
    stat="identity", position="dodge"
  ) +
  scale_fill_hue(c=45, l=80)
  
# 3.2 for year 2015, show top 5 highest performing GP in every HB
ggplot(data=highest_performing_gp, 
       aes(x=hb_name, y=practice_id, fill=total_points)) +
  geom_tile() + 
  scale_fill_gradient(low='yellow', high='orange') +
  labs(
    x='Health Board Name',
    y='Practice Id',
    title='Top 5 performing GP in health board',
    fill='Points Received'
  )
  

# 4.2 for year 2015, show lowest 5 performing GP in every HB
ggplot(data=lowest_performing_gp, 
       aes(x=hb_name, y=practice_id, fill=total_points)) +
  geom_tile() + 
  scale_fill_gradient(low='yellow', high='orange') +
  labs(
    x='Health Board Name',
    y='Practice Id',
    title='Top 5 lowest performing GP in health board',
    fill='Points Received'
  )




get_post_code <- function(practice_id) {
  query <- sprintf("
  select    postcode
  from      address
  where
      practiceid = '%s'
  ", practice_id)
  
  df <- execute_query(config, query)
  return (df$postcode)
}


get_county_from_practice_id <- function(practice_id) {
  query <- sprintf("
  select    county
  from      address
  where
      practiceid = '%s'
  ", practice_id)
  
  df <- execute_query(config, query)
  return (df$county)
}

v_get_county <- Vectorize(get_county)

coordinates_df <- read_csv('./data/assets/ukpostcodes.csv')

get_coordinates <- function(postcode) {
  idx <- which(coordinates_df$postcode == postcode)
  sub_df <- coordinates_df[idx,]
  
}


# 5. for year 2015, show top 10 locations / county with highest points by GP

h_perf_county <- highest_performing_gp %>%
  mutate(county=v_get_county(practice_id))


l_perf_county <- lowest_performing_gp %>%
  mutate(county=v_get_county(practice_id))


merged_df <- rbind(h_perf_county, l_perf_county)

# library(cowplot)
# library(patchwork)

ggplot(data=merged_df, 
       aes(x=hb_name, y=practice_id, fill=county)) +
  geom_tile() + 
  geom_text(aes(label = round(total_points, 1))) +
  labs(
    x='Health Board Name',
    y='Practice Id',
    title='Top 5 highest & lowest performing GP in health board as per county',
    fill='County Names'
  )

ggplot(data=merged_df, 
             aes(x=hb_name, y=practice_id, fill=total_points)) +
  geom_tile() + 
  geom_text(aes(label = round(total_points, 1))) +
  scale_fill_gradient(low='yellow', high = 'orange') +
  labs(
    x='Health Board Name',
    y='Practice Id',
    title='Top 5 highest & lowest performing GP in health board',
    fill='Performance'
  )

# 6. for which med categories the points are higher per HB





# Discounts ------------------------------------------------

# ASSUMPTION
# 
# nic - net cost of ingredients
# actual cost = nic - discount + container allowance 
# since we do not have definite information on container allowance charged in 
# drug tarrif, we assume it not be non existant in the calculation and consider
# only the discount provided by to consumer
#


# create views for reuse of queries
source('./data/queries.R')


execute_query(config, prev_table_view)
execute_query(config, address_county_view)
execute_query(config, qof_practiceid_view)
execute_query(config, base_table_view)


# drop views
query <- "
drop view 
  address_county_view, qof_practiceid_view, base_table_view;
"

execute_query(config, query)


# 1. For year 2015, show average discounts per HB

query <- "
  select    base_table.hb_name,
            round(cast(avg(base_table.perc_discount) as NUMERIC), 4) 
              as avg_perc_discount
  from      (
            select    * 
            from      public.base_table_view
            ) as base_table
  group by
         base_table.hb_name
  
"

gp_data <- execute_query(config, query)
hb_list <- gp_data$hb_name



# 2. for year 2015, show top 5 discounts medication category per HB
query <- "
select    category_table.hb_name,
          category_table.med_category,
          avg(category_table.perc_discount) as avg_perc_discount
from      (select    base_table.hb_name,
                     base_table.perc_discount,
                     bnf_table.chapterdesc as med_category
           from      (
                      select    *
                      from      public.base_table_view
                      ) as base_table
           inner join 
                 bnf as bnf_table
           on 
             left(base_table.bnfcode, 9) = bnf_table.bnfchemical
          ) as category_table
group by
	  category_table.hb_name, category_table.med_category;
"




hb_med_cat_df <- execute_query(config, query)
plot_root_path <- "./output/question5"
library(scales)



for (i in 1:length(hb_list)){
  hb <- hb_list[i]
  
  
  sub_df <- filter(hb_med_cat_df, hb_name == hb)
  
  sub_df <- sub_df %>%
    arrange(desc(avg_perc_discount)) %>% 
    slice_head(n=5)
  
  bar_chart_plot <- ggplot(
    data=sub_df, 
    aes(x=med_category, y=avg_perc_discount, fill=med_category)
    ) +
    geom_bar(stat="identity") +
    scale_fill_hue(c=45, l=80) +
    coord_cartesian(ylim=get_scaled_lim(sub_df$avg_perc_discount)) +
    scale_y_continuous(labels=scales::label_number(0.0001))+
    labs(
      title=paste("Top 5 medications category discount in",hb),
      y="Medication category",
      x="Average % Discount"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9, face="bold"))
    
  file_name <- paste(plot_root_path,
                     "/med_cat_bar_chart_",hb,".png", sep="")
  ggsave(file=file_name,  bar_chart_plot, width=9, height=16)
}


# 3. for year 2015, show top locations with discounts
query <- "
select    discount_county_table.county,
          avg(discount_county_table.perc_discount) as avg_perc_discount
from      (
          select    base_table.*,
                    address_table.county
          from      public.base_table_view as base_table
          inner join (
                     select   a.practiceid as practice_id, a.county
                     from     (
                              select    * 
                              from      public.address_county_view
                              ) as a
                    where
                        a.practiceid in (select * from public.qof_practiceid_view)
                    ) as address_table
          on 
            base_table.practice_id = address_table.practice_id
	        ) as discount_county_table
group by
    discount_county_table.county;
"

df <- execute_query(config, query)

df <- df %>%
  arrange(desc(avg_perc_discount))
 
bar_chart_plot <- ggplot(data=df, aes(x=reorder(county, -avg_perc_discount), y=avg_perc_discount)) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim=get_scaled_lim(df$avg_perc_discount)) +
  scale_y_continuous(labels=scales::label_number(0.0001))+
  labs(
    title="Discount County Wise",
    y="Average % Discount",
    x="County"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

file_name <- paste(plot_root_path, "/county_discount.png", sep="")
ggsave(file=file_name,  bar_chart_plot, width=9, height=16)




# OVERLAP ----------------------------------------------------


## Discounts and point

# 1. for year 2015, overlap locations for discount - points

# how to know which bnfcode corresponds to which indicator ? unresolved! 
# if resolved then it becomes possible to connect prevalence - discount - points
# table


# get data from discount table
query <- "select * from public.base_table_view;"
discount_table <- execute_query(config, query)

# get data from points df
head(points_df)

# to avoid the average bias median is preferred
# calculate the median points per GP for points
median_points_df <- points_df %>%
  group_by(orgcode) %>%
  summarize(median_points=median(points), mean_points=mean(points))

# calcualte the median points per GP for discounts
median_discounts_df <- discount_table %>%
  group_by(practice_id) %>%
  summarize(median_discount=median(perc_discount), mean_discount=mean(perc_discount))


discounts_points_df <- median_points_df %>%
  inner_join(median_discounts_df, by=c("orgcode" = "practice_id"))


head(discounts_points_df)

v_get_county <- Vectorize(get_county_from_practice_id)

discounts_points_df <- discounts_points_df %>%
  mutate(county=v_get_county(orgcode))

head(discounts_points_df)


county_df <- discounts_points_df %>%
  group_by(county) %>%
  summarize(discounts=median(mean_discount), points=median(mean_points)) %>%
  pivot_longer(
    cols=c('discounts', 'points'),
    names_to='type',
    values_to='val'
  ) %>% 
  drop_na()


ggplot(data=county_df, aes(x=reorder(county, -val), y=val, fill=type)) +
  geom_bar(stat="identity", position="dodge") +
  coord_cartesian(ylim=get_scaled_lim(county_df$val)) +
  scale_y_continuous(labels=scales::label_number(0.0001))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title="Average of points and discounts per county wise",
    x="County",
    y="Points / Discounts"
  )



# 2. Is there any correlation between points and discounts

p1 <- ggplot(data=discounts_points_df, aes(x=median_discount, y=median_points)) +
  geom_point()

p2 <- ggplot(data=discounts_points_df, aes(x=mean_discount, y=mean_points)) +
  geom_point()


p_list <- list(p1, p2)


grid.arrange( 
  grobs = p_list, 
  top="Scatter plot Points Vs Discounts", 
  yleft="Points",
  bottom="Discounts")

# from the scatter shows for both mean and median values
# increase in discount has resulted in increase in points
# its not a linear relationship but the points are more clustered
# in a specific range 




p1 <- ggplot(data=discounts_points_df, aes(sample=mean_points))+
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  labs(title="Mean points")

p2 <- ggplot(data=discounts_points_df, aes(sample=median_points))+
  stat_qq() +
  stat_qq_line() +
  theme_bw() + 
  labs(title="Median points")

p3 <- ggplot(data=discounts_points_df, aes(sample=mean_discount))+
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  labs(title="Mean discount")

p4 <- ggplot(data=discounts_points_df, aes(sample=median_discount))+
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  labs(title="Median discount")


p_list <- list(p1, p2, p3, p4)
grid.arrange( 
  grobs = p_list, 
  top="QQ Plot for Mean - Median of Discount and Points")


shapiro.test(discounts_points_df$median_discount)
shapiro.test(discounts_points_df$mean_discount)
shapiro.test(discounts_points_df$median_points)
shapiro.test(discounts_points_df$mean_points)

# QQ plot and shapiro test for normality 
# shows that data comes from normal distribution and median is more
# stable static than mean


# Pearson correlation test to check correlation between points and discount
cor.test(
  discounts_points_df$median_points, 
  discounts_points_df$median_discount, 
  method = "pearson")

# interpretation
# from Scatter plot & Correlation test we can conclude that
# p value of the test is 0.0007183 which is < 0.05
# It can be concluded that points and discounts are significantly 
# correlated with a non linear relationship
# with a correlation coefficient of 0.1573
#
# 

## Prevalence and Points

# is there any correlation between prevalence and points received ?
# prevalence and points are calculated using the same components
# there is a highly likely chance that they will be correlated


# get prevalence data
query <- "
select    *
from      prev_tbl_view;
"
prev_data <- execute_query(config, query)
head(prev_data)

# get points data
head(points_df)


# merge points and prevalence based on indicator and practice id
points_prev_data <- prev_data %>%
  inner_join(points_df, by=c("practiceid" = "orgcode", "indicator")) %>%
  select(practiceid, indicator, prevalence, points)

head(points_prev_data)
indicator_list <- unique(points_prev_data$indicator)


p_value_list <- c()
corr_coeff_list <- c()
for (i in 1:length(indicator_list)){
  ind <- indicator_list[i]
  
  sub_df <- subset(points_prev_data,indicator == ind)
  
  # perform correlation test
  corr_test_result <- cor.test(sub_df$prevalence, sub_df$points, method="pearson")
  p_value <- corr_test_result$p.value[[1]]
  corr_coeff <- corr_test_result$estimate[[1]]
  
  p_value_list <- c(p_value_list, p_value)
  corr_coeff_list <- c(corr_coeff_list, corr_coeff)
  
  
  p1 <- ggplot(data=sub_df, aes(x=prevalence, y=points)) +
    geom_point() +
    labs(title="Prevalence vs Points Scatter Plot")
  
  
  p2 <- ggplot(data=sub_df, aes(sample=prevalence))+
    stat_qq() +
    stat_qq_line() +
    theme_bw() +
    labs(title="Prevalence QQ plot")
  
  
  p3 <- ggplot(data=sub_df, aes(sample=points))+
    stat_qq() +
    stat_qq_line() +
    theme_bw() +
    labs(title="Points QQ plot")
  
  
  p_list <- list(p1, p2, p3)
  g <- grid.arrange( 
    grobs = p_list,
    top=paste("Plots for ", ind))
  
  file_name <- paste(plot_root_path, "/scatter_", ind,".png", sep="")
  ggsave(filename=file_name, plot=g)
  
}

ind_df <- data.frame(
  indicator=indicator_list,
  p_value=p_value_list,
  corr_coeff=corr_coeff_list
)



ind_df <- ind_df %>% drop_na() # dropped where caused NA as points were zero
dim(ind_df)


ggplot(data=ind_df, aes(x=corr_coeff, y=indicator, fill=corr_coeff)) +
  geom_bar(stat="identity", position="identity") +
  labs(
    title="Correlation bar plot", 
    x="Correlation Coefficient",
    y="Indicator") +
  scale_fill_gradient(low="yellow", high="orange")


ggplot(data=ind_df, aes(x=p_value, y=indicator, fill=p_value)) +
  geom_bar(stat="identity", position="identity") +
  labs(
    title="P-Value bar plot",
    x="P value",
    y="Indicator"
  ) +
  scale_fill_gradient(low="yellow", high="orange")

p_list <- list(p1, p2)
grid.arrange( 
  grobs = p_list)


ind_df_relevant <- subset(ind_df, p_value < 0.05)

head(ind_df_relevant)
dim(ind_df_relevant)
# focus indicators
ggplot(data=ind_df_relevant, aes(x=corr_coeff, y=indicator, fill=corr_coeff)) +
  geom_bar(stat="identity", position="identity") +
  labs(
    title="Correlation bar plot where p value < 0.05", 
    x="Correlation Coefficient",
    y="Indicator") +
  scale_fill_gradient(low="yellow", high="orange")

# 11 indicators has significant correlation of prevalence with points
# 31 indicators has non significant correlation of prevalence with points
# 48 indicators were dropped as the points were zero







## References -------------------------------------------------
# [1] https://www.health-ni.gov.uk/publications/qof-guidance-regional-board-and-practices
# [2] https://www.mysurgerywebsite.co.uk/website/IGP217/files/103%20QOF_Payments.pdf
