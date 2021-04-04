############################################################
# Example lever: create Menus or fruits
############################################################

# Auxliary function -------------------------------------------------------
tbr = function(a, b){
  top = mean(a[order(-b)][1:floor(0.2*length(a))])
  bot = mean(a[order(b)][1:floor(0.2*length(a))])
  return(top/bot)
}

# Library loading ---------------------------------------------------------
library(data.table)
library(lubridate)

# Data loading ------------------------------------------------------------
td = fread('GitHub/DataAnalyticsR/Data/transactional_data.csv')
md = fread('GitHub/DataAnalyticsR/Data/machine_failures.csv')

# Fix variable formats ----------------------------------------------------
td[,class(timestamp)]
td[,timestamp][1]
td[,timestamp:=ymd_hms(timestamp)]
td[,class(timestamp)]

# Menus -------------------------------------------------------------------
td = td[order(machine,timestamp)]

td[,last_vend_timestamp:=shift(timestamp,1),by=machine]
td[,last_product_name:=shift(product_name,1),by=machine]
td[,time_lapse:=difftime(timestamp,last_vend_timestamp,units='secs')]

product_bundle = td[time_lapse<60]

product_bundle[,.(num_times = .N),
               by=.(product_name,last_product_name)][order(-num_times)]

product_bundle[,product_pair:=paste(sort(c(product_name,last_product_name)),collapse=','),by=1:nrow(product_bundle)]

product_bundle[1:3]
product_bundle[product_name!=last_product_name][,.N,by=product_pair][order(-N)]


# Where to place fruits or products that perish soon ----------------------
pd = fread('GitHub/DataAnalyticsR/Data/product_data.csv')
# Replacing worst performers by best performers ---------------------------
pd[,margin:=(price/(1+tax_rate)) - cost] # price = (cost+margin)*(1+tax)
tdp = merge(td,pd,by='product_name',all.x=T)

x = tdp[,.(items = .N),by=.(machine,column)]
x[,pct_column_machine:=items/sum(items),by=machine]
credible_columns = x[pct_column_machine>0.01][,.(machine,column,credible=1)]

tpd2 = merge(tdp, credible_columns, by=c('machine', 'column'),all.x=T)
tpd2[is.na(credible),credible:=0]

daily_sales_capacity = tpd2[credible==1][,.(daily_sales = .N/uniqueN(date),
                                            n_dates = uniqueN(date),
                                            total_capacity=6*uniqueN(column[type_drink_snack=='drink']) +
                                8*uniqueN(column[type_drink_snack=='snack'])),by=.(machine)]

# Imagine the policy for refill is to go when 25% of the total capacity has been sold
daily_sales_capacity[,days_till_refill:=(total_capacity*0.25)/daily_sales]
daily_sales_capacity[days_till_refill<5][daily_sales>20][order(-daily_sales)]

## REBECCA code 

refill_alarm <- function(percent, number_of_days) {
    daily_sales_capacity[,days_till_refill:=(total_capacity*percent)/daily_sales]
    x <- daily_sales_capacity[days_till_refill<number_of_days][daily_sales>20][order(-daily_sales)]
    return(x)
}

refill_alarm(0.50,5)


# First, we filter by the instances that we classified as values with at least a medium risk of failure.
dt_alarm_med = merged_data[delta >= delta_med]
dt_alarm_high = merged_data[delta >= delta_high]# Assumption: Filter both medium and high risk because high risk instances imply having a medium risk of failure as well.

annual_perc_incr <- function(x, delta_x) {
    conversion_factor <- 24/x$avg_daily_sales
    x[, threshold_hours:=delta_x*conversion_factor]
    x[, threshold_hours_fixed := threshold_hours + 0.5]  # 0.5 represents the average time it takes to refill a machine.
    x[, delta_fixed:= threshold_hours_fixed * (1/(24/avg_daily_sales))]
    x[, won_sales := failure*(delta-delta_fixed)]
    add_rev = sum(x$won_sales*1.7) # Additional revenue of the EWS for at least medium risk alarms.
    sys_cost = 10*(nrow(x[failure == 0])) # Cost of the system
    vendex_cost_prev = uniqueN(merged_data$machine)*2.2*10 # Cost of current system per year (2.2 false alarm per machine per YEAR)
    annual_profit_incr = 4*(add_rev - sys_cost ) + vendex_cost_prev # As we are only given data from Jan 1st to April 1st we have to multiply our estimated profit increase by 4 to obtain the actual annual profit increase. The cost of the old system is added to reflect the actual difference in revenue between the systems accounting for the costs of either one. 
    perc_incr = (annual_profit_incr/(nrow(merged_data)*1.7*4))*100 # Again, we have to multiply the denominator by 4 so that we receive the annual profit increase in percentage.
    return(perc_incr)
}




