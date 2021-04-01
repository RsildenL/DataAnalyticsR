#data
library(data.table)

machine_data <- fread("Data/machine_data.csv")
product_data <- fread("Data/product_data.csv")
transaction_data <- fread("Data/transactional_data.csv")
transaction_data$date <- as.Date(transaction_data$date)
merged_data <- merge(transaction_data, product_data, by.x = "product_name", by.y ="product_name")
merged_data <- merge(merged_data, machine_data, by= "machine")
merged_data <- merged_data %>% drop_na()

#margin wo/ cashless

daily_margin <- merged_data[ , .(margin=price/(1+tax_rate) - cost), by = .(machine, category)]  #is this the entire margin per machine?

daily_sales <- merged_data[ , items_machine_day:= uniqueN(timestamp), by = .(machine, category)]
merge
total_margin_per_machine<-merged_data[ , .(total_margin= as.numeric(items_machine_day * margin)), by = .(machine, category)]
sum(total_margin_per_machine$total_margin)

#margin w/ cashless

#Assumptions
#Real time data collection
#"For example, vending machines advertising the option to pay via Apple Pay have accounted for 22% overall revenue growth, New York Post reported."
#Implementing cashless payments solutions will increase daily sales in 22% 
#"A major component of the cost that should be taken into account is the expense for buying or leasing the credit/debit contact/contactless card reader units. If you decide to buy the hardware that brings the cashless transaction technology to your machine, be prepared to spend some $40 to $200 per machine"
#"The National Retail Association suggests that the swipe fees are usually between 2% and 4% per transaction and can vary depending on a merchants’ card volume and other factors."
#"Kids dont have cards"
#cost is divided by 365 to see daily

daily_sales_cl<-merged_data[ , items_machine_day_cl:=items_machine_day*1.22, by = .(machine, category)]
number_machines=uniqueN(merged_data$machine)
daily_margin_cl <- merged_data[ , margin_cl:=price/(1+tax_rate) - cost+ (120/365), by = .(machine, category)]

total_margin_per_machine_cl<-merged_data[ , .(total_margin_cl= as.numeric(items_machine_day_cl * margin_cl)), by = .(machine, category)]
sum(total_margin_per_machine_cl$total_margin_cl)

#where does it make sense to immplement
# increase in revenue
profit_diff<- sum(total_margin_per_machine_cl$total_margin_cl) -sum(total_margin_per_machine$total_margin) #should I make a table to see where is the biggest increase? should I apply diff rates per location?
percentage_increase<- profit_diff/sum(total_margin_per_machine$total_margin)
