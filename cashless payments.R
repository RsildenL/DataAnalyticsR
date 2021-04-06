#data
library(data.table)
library(tidyverse)

machine_data <- fread("Data/machine_data.csv")
product_data <- fread("Data/product_data.csv")
transaction_data <- fread("Data/transactional_data.csv")
transaction_data$date <- as.Date(transaction_data$date)
merged_data <- merge(transaction_data, product_data, by.x = "product_name", by.y ="product_name")
merged_data <- merge(merged_data, machine_data, by= "machine")
merged_data <- merged_data %>% drop_na()

#margin wo/ cashless
daily_sales <- merged_data[ , items_machine_day:= as.numeric(uniqueN(timestamp)), by = .(machine, date)]

daily_revenue<-merged_data[, revenue:= items_machine_day *price, by = .(machine, date)]

daily_cost<- merged_data[, costs:= items_machine_day * cost, by= .(machine, date)]

daily_profit<- merged_data[, margin:=(price/(1+tax_rate)*items_machine_day) - cost*items_machine_day, by=.(machine, date)]

profit<- sum(daily_profit$margin)

#margin w/ cashless

#Assumptions
#Real time data collection
#"For example, vending machines advertising the option to pay via Apple Pay have accounted for 22% overall revenue growth, New York Post reported."
#Implementing cashless payments solutions will increase daily sales in 22% 
#"A major component of the cost that should be taken into account is the expense for buying or leasing the credit/debit contact/contactless card reader units. If you decide to buy the hardware that brings the cashless transaction technology to your machine, be prepared to spend some $40 to $200 per machine"
#"The National Retail Association suggests that the swipe fees are usually between 2% and 4% per transaction and can vary depending on a merchants’ card volume and other factors."
#"Kids dont have cards"
#cost is divided by 365 to see daily

daily_sales_cl<-merged_data[ , items_machine_day_cl:=items_machine_day*1.22, by = .(machine, date)]

daily_profit_cl<- merged_data[, margin_cl:=(price/(1+tax_rate)*items_machine_day_cl) - (cost*items_machine_day_cl), by=.(machine, date)]

cost_cl_hardware<- uniqueN(merged_data$machine) * 240

profit_cl<-sum(daily_profit_cl$margin_cl)-cost_cl_hardware

profit_diff<- profit_cl-profit
percentage_increase_cl<- profit_diff/profit

# next quarter
daily_sales_cl<-merged_data[ , items_machine_day_cl:=items_machine_day*1.22, by = .(machine,date)]

daily_profit_cl<- merged_data[, margin_cl:=(price/(1+tax_rate)*items_machine_day_cl) - (cost*items_machine_day_cl), by=.(machine, date)]

profit_cl<-sum(daily_profit_cl$margin_cl)

# increase in revenue
profit_diff<- profit_cl-profit
avg_percentage_increase_cl<- profit_diff/profit


#with the other cost structure

daily_sales_cl_fee<-merged_data[ , items_machine_day_cl:=items_machine_day*1.22, by = .(machine,date)]

daily_profit_cl_fee<- merged_data[, margin_cl_fee:=(price/(1+tax_rate)*items_machine_day_cl) - (cost*items_machine_day_cl)- (0.03*price/(1+tax_rate)*items_machine_day_cl), by=.(machine, date)]

profit_cl_fee<- sum(daily_profit_cl_fee$margin_cl_fee)


# increase in revenue
profit_diff_fee<- profit_cl_fee-profit
percentage_increase_fee<- profit_diff_fee/profit





