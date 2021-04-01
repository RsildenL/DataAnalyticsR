#data
library(data.table)
library(lubridate)
library(tidyverse)
machine_data <- fread("Data/machine_data.csv")
product_data <- fread("Data/product_data.csv")
transaction_data <- fread("Data/transactional_data.csv")
transaction_data$date <- as.Date(transaction_data$date)
merged_data <- merge(transaction_data, product_data, by.x = "product_name", by.y ="product_name")
merged_data <- merge(merged_data, machine_data, by= "machine")
merged_data <- merged_data %>% drop_na()

#number of transactions per day
summary(transaction_data)
summary(machine_data$machine)


daily_sales <- merged_data[ , .(items_machine_day = uniqueN(timestamp)), by = .(machine, date)]
sum(daily_sales$items_machine_day)


# avg revenue daily
margin<-merged_data[,margin:=(price/(1+tax_rate)) - cost] # price = (cost+margin)*(1+tax)
daily_margin <- margin[ ,.(avg_daily_margin = mean(margin)), by = machine]
sum(daily_margin)

#Assumptions

#where oes it make sense to immplement
# increase in revenue
annual_perc_incr <- function(x, delta_x) {
    conversion_factor <- 24/x$avg_daily_sales
    x[, threshold_hours:=delta_x*conversion_factor]
    x[, threshold_hours_fixed := threshold_hours + 1.5]  # 1.5 represents the average time it takes to fix a machine.
    x[, delta_fixed:= threshold_hours_fixed * (1/(24/avg_daily_sales))]
    x[, won_sales := failure*(delta-delta_fixed)]
    add_rev = sum(x$won_sales*1.7) # Additional revenue of the EWS for at least medium risk alarms.
    sys_cost = 10*(nrow(x[failure == 0])) # Cost of the system
    vendex_cost_prev = uniqueN(merged_data$machine)*2.2*10 # Cost of current system per year (2.2 false alarm per machine per YEAR)
    annual_profit_incr = 4*(add_rev - sys_cost ) + vendex_cost_prev # As we are only given data from Jan 1st to April 1st we have to multiply our estimated profit increase by 4 to obtain the actual annual profit increase. The cost of the old system is added to reflect the actual difference in revenue between the systems accounting for the costs of either one. 
    perc_incr = (annual_profit_incr/(nrow(merged_data)*1.7*4))*100 # Again, we have to multiply the denominator by 4 so that we receive the annual profit increase in percentage.
    return(perc_incr)
}
```

#### i) If we set the EWS only with the med-risk alarms, what is the annual profit we will generate vs the current system as a % of the total profit? [For simplicity, consider the total profit to be the margin per item times the number of items in the period]
```{r}
print(paste('Average annual percentage increase in profit for med-risk EWS: ', round(annual_perc_incr(dt_alarm_med, delta_med),4)))
```

#### ii) And if we set the EWS only with the high-risk alarms?
```{r}
print(paste('Average annual percentage increase in profit for high-risk EWS: ', round(annual_perc_incr(dt_alarm_high, delta_high),4)))

tot_annual_profit = nrow(merged_data)*1.7*4 #profit of all transactions assuming an avg. profit of 1.7 per sale
```

If we configure an EWS for medium-risk alarms instead of relying on the current system we estimate an increase in revenue of 2.40%.
Applying an EWS for high-risk alarms only we forecast an increase in profit of 2.37%.

```{r}
print(paste('Total annual profit: ',
            round(tot_annual_profit,0)))
print(paste('Monetary value of 0.03%: ',
            round(tot_annual_profit*0.0003,0)))
```
We recommend implementing the medium-risk EWS, as the average annual increase in revenue is 0.03% higher than for only high-risk alarms. This difference seems negligible, but in fact equates to $3749 making the EWS for med-risk alarms the obvious choice from a business point of view.
