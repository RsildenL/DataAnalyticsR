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
td = fread('./MIBA 21/CHALLENGES/final_data/transactional_data.csv')
md = fread('./MIBA 21/CHALLENGES/final_data/machine_data.csv')

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
pd = fread('./MIBA 21/CHALLENGES/final_data/product_data.csv')
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

