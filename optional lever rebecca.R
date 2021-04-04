library(data.table)
library(lubridate)
library(tidyverse)

td = fread('GitHub/DataAnalyticsR/Data/transactional_data.csv')
pd = fread('GitHub/DataAnalyticsR/Data/product_data.csv')
md = fread('GitHub/DataAnalyticsR/Data/machine_data.csv')


#Adding small machines field to the transactional data
td = td[md, small_machine := small_machine, on="machine"]


#Creating two data.frames with the top-seller and worst seller of each machine
max <- td %>%
    group_by(machine, product_name) %>%
    summarize(sales = n_distinct(timestamp)) %>%
    group_by(machine) %>%
    filter(sales == max(sales))

min <- td %>%
    group_by(machine, product_name) %>%
    summarize(sales = n_distinct(timestamp)) %>%
    group_by(machine) %>%
    filter(sales == min(sales))

max <- max[!duplicated(max$machine),]
min <- min[!duplicated(min$machine),]

#Adjusting the name of the fields to avoid conflict when merging
max <- max %>% rename('max' = product_name,
                      'sales_max'= sales)
min <- min %>% rename('min' = product_name,
                      'sales_min'= sales)

#Merging the two data.frames together
total <- merge(min,max,by="machine")

#Creating pairs of products "minmax"and "maxmin" in order to find machines that have opposite products
total$min_max <- paste(total$min, "-", total$max)
total$max_min <- paste(total$max, "-", total$min)

#Creating a data.frame called df where the paired machines will be stored
df <- data.frame(matrix(ncol = 4, nrow = 0))
col <- c("machine", "remove", "add", "paired_machine")
colnames(df) <- col

#Running a loop through the data and storing every pairs into the data.frame.
for(x in 1:nrow(total)) {
    for(y in 1:nrow(total)) {
        if (total[x,'min_max'] == total[y,'max_min']) {
            df <- rbind(df,data.frame(total[x,'machine'], total[x,'min'],total[x,'max'],total[y,'machine'] ))
            
        }
    }
}

colnames(df) <- col
df
 