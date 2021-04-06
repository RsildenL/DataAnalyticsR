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
daily_profit<- product_data[, margin:=(price/(1+tax_rate) - cost)]
tdp = merge(transaction_data,daily_profit,by='product_name',all.x=T)
profit<-sum(tdp$margin)


#margin w/ cashless

#Assumptions
#Real time data collection
#"For example, vending machines advertising the option to pay via Apple Pay have accounted for 22% overall revenue growth, New York Post reported."
#Implementing cashless payments solutions will increase daily sales in 22% 
#"A major component of the cost that should be taken into account is the expense for buying or leasing the credit/debit contact/contactless card reader units. If you decide to buy the hardware that brings the cashless transaction technology to your machine, be prepared to spend some $40 to $200 per machine"
#"The National Retail Association suggests that the swipe fees are usually between 2% and 4% per transaction and can vary depending on a merchants’ card volume and other factors."
#"Kids dont have cards"
#cost is divided by 365 to see daily
cost_cl_hardware<- uniqueN(merged_data$machine) * 240
n_products<-tdp[,.N, by= product_name]
n_products[, n:= 1.22*N]
cl=merge(n_products,tdp[,.(product_name,margin)],by='product_name', all.x = T)
cl=cl[!duplicated(product_name),.(product_name,N,n,margin,new_added_margin=(n-N)*margin), ]
profit_cl<- sum(cl$new_added_margin)-cost_cl_hardware
profit_cl+profit
percentage_increase_cl<- profit_cl/profit

# next quarter
n_products<-tdp[,.N, by= product_name]
n_products[, n:= 1.22*N]
cl=merge(n_products,tdp[,.(product_name,margin)],by='product_name', all.x = T)
cl=cl[!duplicated(product_name),.(product_name,N,n,margin,new_added_margin=(n-N)*margin), ]
profit_cl<- sum(cl$new_added_margin)
percentage_increase_cl<- profit_cl/profit

#with the other cost structure

daily_profit_fee<- product_data[, margin_fee:=(price/(1+tax_rate) - cost-0.03*price)]
tdp_fee = merge(transaction_data,daily_profit,by='product_name',all.x=T)
n_products<-tdp_fee[,.N, by= product_name]
n_products[, n:= 1.22*N]
cl_fee=merge(n_products,tdp_fee[,.(product_name,margin_fee)],by='product_name', all.x = T)
cl_fee=cl_fee[!duplicated(product_name),.(product_name,N,n,margin_fee,new_added_margin_fee=(n-N)*margin_fee), ]

profit_cl_fee<- sum(cl_fee$new_added_margin_fee)
profit_cl_fee+profit
percentage_increase_cl_fee<- profit_cl_fee/profit






