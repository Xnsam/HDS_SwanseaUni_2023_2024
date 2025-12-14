library(tidyverse)

options(digits=15)

df <- read_csv("output/preval_data.csv")

hist(df$prevalence)

head(df)

# selecting top 3 for HB prevalence
min(df$prevalence)
max(df$prevalence)


sub_df <- data.frame(prevalence=df$prevalence, indicator=df$indicator)


sub_df <- sub_df[1:3,]


# bar chart for HB
sub_df <- sub_df %>%
  pivot_longer(-indicator)

ggplot(data = sub_df, aes(x = indicator, y = value)) +
  geom_bar(aes(fill = indicator), stat = 'identity') +
  ggtitle("HB TOP 3 Prevalence") +
  ylab("Prevalance") +
  xlab("Indicator") +
  guides(fill=guide_legend(title="Prevalence")) +
  theme(
    plot.title = element_text(hjust=0.5),
    text = element_text(size = 14),element_line(size =1)
  )


length(unique(df$practiceid))


# bar chart for GP practice 
sub_df2 <- df %>%
  select(prevalence, indicator, practiceid)


# head(sub_df2)
# 
# ggplot(data = sub_df2, aes(x=practiceid, y=prevalence)) +
#   geom_bar(aes(fill = indicator), stat = 'identity') +
#   ggtitle("GP TOP 3 Prevalence") +
#   ylab("Prevalence") +
#   xlab("Practice Id") +
#   guides(fill=guide_legend(title="Prevalence")) +
#   theme(
#     plot.title = element_text(hjust=0.5),
#     text = element_text(size=14), element_line(size=1)
#   )

library(ggplot2)
library(ggforce)
library(gridExtra)

distinct_practices_list <- unique(df$practiceid)


# pdf("output/prevalence_plot.pdf", onefile = TRUE)

prev_number <- 1
for(i in seq(50, length(distinct_practices_list[1:50]), by=50)){
  sub_df2 %>%
    filter(practiceid %in% distinct_practices_list[prev_number+1:i]) %>%
    ggplot(aes(x=prevalence, y=indicator, fill=indicator)) + 
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~practiceid) +
    xlab("Prevalence") +
    ylab("Practices") +
    ggtitle(paste("Per GP TOP 3 Prevalence, For GP: ", prev_number,"-", i)) +
    theme(text = element_text(size=7), element_line(size=1),  plot.title = element_text(hjust=0.5, size=14))
  
  prev_number <- i
  ggsave(filename = paste("output/question2/prevalence_facet_plot",i,".png"))
}
# 
# par(mfrow=c(2, 2))
# sub_df2 %>%
#   filter(indicator == "STIA001") %>%
#   ggplot(aes(x=prevalence, y=practiceid, fill=prevalence)) +
#   geom_bar(stat="identity")
# 
# ind <- "AF001"
# sub_df2 %>%
#   filter(indicator == ind) %>%
#   ggplot(aes(x=prevalence, y=practiceid, fill=prevalence)) +
#   geom_bar(stat="identity") +
#   xlab("Prevalence") +
#   ylab("Practices") +
#   ggtitle(paste("For GP vs ", ind)) +
#   theme(
#     text = element_text(size=9), element_line(size=1), 
#     plot.title = element_text(hjust=0.5, size=14))



prev_number <- 0
for (i in seq(12, length(distinct_practices_list), by=19)){
  p_list <- list()
  for (p_id in distinct_practices_list[prev_number+1:i]){
    sub_gp_df <- sub_df2 %>% 
      filter(practiceid == p_id) %>%
      select(prevalence, indicator) %>%
      pivot_longer(-indicator)
    
    
    # Graph
    p <- ggplot(data=sub_gp_df, aes(fill=indicator, y=value, x=indicator)) + 
      geom_bar(position="dodge", stat="identity") +
      xlab("") +
      ylab("") +
      ggtitle(p_id) +
      theme(
        text = element_text(size=9), element_line(size=1),  
        plot.title = element_text(hjust=0.5, size=14),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Dark2")
    
    p_list <- c(p_list, list(p))
  }
  g <- grid.arrange( 
    grobs = p_list, 
    top="GP Practices", 
    yleft="Prevalence",
    bottom="Indicator")
  
  file_name <- paste("output/question2/prevalence_per_gp_",i,".pdf")
  ggsave(file=file_name, g, width = 8, height = 10)
  
  prev_number <- i
}

# library(gridExtra)
# n <- length(p_list)
# nCol <- floor(sqrt(n))
# do.call("grid.arrange", c(p_list, ncol=nCol))
# 
# library(ggplot2)
# p1 = qplot(1:10,rnorm(10))
# p2 = qplot(1:10,rnorm(10))

# library(gridExtra)
# grid.arrange(p1, p2, ncol=2,top="Main Title")

ggplot(data=df, aes(x=df$prevalence)) +
  geom_histogram() +
  xlab("Prevalence") +
  ylab("Frequency") +
  ggtitle("Distribution of Prevalence in the subset") +
  theme(
    text = element_text(size=9), element_line(size=1),  
    plot.title = element_text(hjust=0.5, size=14),
    axis.text.x = element_text(angle = 45, hjust = 1))


# logic for data binning


vector_a <- as.vector(c(
  "W94010", "W91054","W94034", "W91629", "W91056",
  "W94028", "W94005", "W91051", "W91618", 
  "W91020", "W94039"))

start <- 1
stride <- 2
stop <- start + stride
length_a <- length(vector_a)
steps <- round(length_a / stride)
list_of_bins <- list()
for (i in 1:steps){
  # print(paste("bin ", i))
  # print(paste(start, stop))
  
  list_of_bins[[i]] <- vector_a[start:stop]
  
  start <- stop
  stop <- stop + stride
  if(stop >= length_a){
    stop <- length_a
  } 
}

list_of_bins[[3]]



# try heat map

library(tidyverse)

my_data <- data.frame(
  practiceid=c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10"),
  indicator=c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10"),
  prevalence=c(1.1, 0.4, 0.6, 0.7, 5.1, 0.8, 1.4, 2.0, 1.8, 4)
)

ggplot(data=my_data, aes(practiceid, indicator, fill=prevalence)) +
  geom_tile() +
  theme_minimal() +
  scale_fill_gradient(low="white", high="blue") +
  labs(title="Heatmap") +
  labs(subtitle="A simple heatmap ") +
  labs(x="Practices", y="indicator")



# ------------------- Q3

data <- read_csv('output/question3/7A3/data.csv')
data



transformed_df <- data %>%
  pivot_longer(names_to = "type", values_to = "val", 
               num_prescriptions:spend_prescriptions) %>%
  arrange("type") %>%
  mutate(
    alphayr = ifelse(type=="num_prescriptions", 0.3, 0.9),
    text_align = val + 0.0001 * val
  )


transformed_df

ggplot(data=transformed_df, aes(x=subsectiondesc)) +
  geom_bar(
    aes(y=val, fill=type, group=type, alpha = alphayr),
    stat="identity", position="dodge",
    color="black"
  ) +
  scale_alpha_identity() +
  scale_y_continuous(
    name = "Number",
    labels = scales::comma,
    sec.axis = sec_axis(
      ~.*1, name="Spend", labels=scales::comma)
  )+
  theme(
    legend.position = 'top',
    plot.title = element_text(color='black',
                              face='bold',hjust=0.5),
    axis.title.y = element_text(color = "blue", size=13),
    axis.text.x = element_text(angle = 45, hjust = 1, face="bold"))+
  ggtitle('Top 5 prescriptions by') +
  xlab("Names of BNF subsection description") +
  # scale_x_discrete(
#    limits=unique(transformed_df$chapterdesc))
  geom_text(position = position_identity(), 
            aes(y=text_align, label=chapterdesc, hjust=1), angle=0)
  # labs(list(x = "x", y = "count",fill = "group"))


# ----------------------------- Q4

library(purrr)

convert_to_date <- function(x){
  year <- x %/% 100
  month <- x - (x %/% 100) * 100
  
  if (month < 10) {
    format_str <- "%s-0%s-01"
  } else {
    format_str <- "%s-%s-01"
  }
  
  yr_str <- sprintf(format_str, year, month)
  
  return (yr_str)
}

convert_to_date_v <- Vectorize(convert_to_date)

data <- read.csv('output/question4/data.csv')

head(data)

hb_list <- unique(data$hb)

# data$period <- lapply(data$period, convert_to_date_v)
# data$period <- as.Date(data$period, format="%Y-%M-%d")
data$period <- as.Date(data$period)
# data$period <- dmap(data$period, convert_to_date)

# draw scatter plot
ggplot(data=data, aes(x=period, y=avg_spend, color=hb)) + 
  geom_line() +
  scale_x_date(date_labels = "%b-%Y") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face="bold"))

# draw a histogram for all the data
ggplot(data=sub_df, aes(x=avg_spend)) +
  geom_histogram()


# first range is > 5000
# second range is 100 > x > 5000
# third range is < 100

# draw a box plot
outlier_df <- data %>%
  filter(avg_spend >= 100)

dim(outlier_df)
dim(data)

outlier_perc <- (dim(outlier_df)[1] / dim(data)[1]) * 100
outlier_perc

sub_df <- data %>%
  filter(avg_spend < 5000)


sub_df <- data %>%
  filter(avg_spend < 100)


ggplot(data=sub_df, aes(x=hb, y=avg_spend)) +
  geom_boxplot()

# check mean and median for the each hb
for (i in 1:length(hb_list)){
  hb_name <- hb_list[[i]]
  
  sub_df <- data %>%
    filter(hb == hb_name) %>%
    filter(avg_spend < 100)
  
  print(paste(hb_name, mean(sub_df$avg_spend), median(sub_df$avg_spend)))
  
}




# draw histogram

for (i in 1:length(hb_list)){
  hb_name <- hb_list[[i]]
  
  sub_df <- data %>%
    filter(hb == hb_name)
  g_plot <- ggplot(data=sub_df, aes(x=avg_spend)) +
    geom_histogram() 
}


# draw histogram

# wilcox.test(data$hb ~ data$avg_spend, paired=TRUE)
kruskal.test(avg_spend ~ hb, data=data)






# REgex ------------------------------------ 

library(stringr)

str_value <- "5A1"
result <- str_extract(str_value, '7A[1-7]')
is.na(result)

