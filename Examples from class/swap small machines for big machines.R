############################################################
# Example lever: swap small machines for big machines
############################################################

# Auxliary function -------------------------------------------------------
tbr = function(a, b){
    top = mean(a[order(-b)][1:floor(0.2*length(a))])
    bot = mean(a[order(b)][1:floor(0.2*length(a))])
    return(top/bot)
}

# Library loading ---------------------------------------------------------
library(data.table)
library(ggplot2)
library(lubridate)
library(gbm)

# Data loading ------------------------------------------------------------
td = fread('GitHub/DataAnalyticsR/Data/transactional_data.csv')
md = fread('GitHub/DataAnalyticsR/Data/machine_data.csv')

# Model data creation -----------------------------------------------------
daily_sales_dt = td[,.(daily_sales = .N/uniqueN(date)),by=machine]
data = merge(daily_sales_dt,
             md,
             by='machine',all.x=T)

# Driver importance -------------------------------------------------------
vars = setdiff(names(data),c('machine','daily_sales','location_type'))

dt = data.table()
for (v in vars) {
    print(v)
    aux = data[complete.cases(eval(parse(text=v)))]
    dt = rbind(dt, data.table(var = v,
                              tbr = tbr(aux$daily_sales, aux[[v]]),
                              cor = cor(aux$daily_sales, aux[[v]])))
}

dt[, sign := ifelse(tbr<1, '-', '+')]
dt[tbr<1, tbr := 1/tbr]
dt[order(-tbr)]

# Treat variables ---------------------------------------------------------
data[,income_average:=as.numeric(income_average)]
data[,log_transport:=log10(total_number_of_routes_600)]
data[is.na(log_transport),
     log_transport:=mean(data$log_transport,na.rm=T)]
data[,isna_train_AvgDailyPassengers:=ifelse(is.na(train_AvgDailyPassengers),1,0)]
data[,no_income:=ifelse(is.na(income_average),1,0)]
data[is.na(income_average),income_average:=mean(data$income_average,na.rm=T)]

# Calculate the big machine boost with a GLM ---------------------------------
set.seed(1)
r = runif(nrow(data))
train = data[r>0.3]
test = data[r<=0.3]

model_vars = c('isna_train_AvgDailyPassengers',
               'num_hotels_45', 'log_transport',
               'num_vendex_nearby_300', 'income_average')

# We will model sales taking into account location factors but EXCLUDING small machine variable to see how do we overestimate or understimate sales because of that. The relative jump will be the boost in sales

model = glm(daily_sales~., data=train[,c(model_vars,'daily_sales'),with=F],family='gaussian')

data[,pred_sales:=predict(model,data)]
data[,.(ratio_sales_vs_benchmark = mean(daily_sales/pred_sales)),by=small_machine][,ratio_sales_vs_benchmark[small_machine==0] / ratio_sales_vs_benchmark[small_machine==1]] 
        # Big machines sell ~20% more than small machines at a similar location


# Alternative: calculate the big machine boost with a GBM -----------------
# In this part we will model sales using small machine variable and see what would be the prediction of the model in each location for each type of machine, calculating the boost of big machines

model_vars = c('small_machine','isna_train_AvgDailyPassengers',
               'num_hotels_45', 'log_transport',
               'num_vendex_nearby_300', 'income_average')

n_trees = 100
model = gbm(daily_sales~.,
            n.trees = n_trees,
            shrinkage = 0.1,
            n.minobsinnode =30,
            data=train[,c(model_vars,'daily_sales'),with=F],
            distribution ='gaussian')
summary(model)
train[,pred:=predict(model, train,n.trees = n_trees)]
test[,pred:=predict(model, test,n.trees = n_trees)]
train[,sum(abs(pred-daily_sales))/sum(daily_sales)]
test[,sum(abs(pred-daily_sales))/sum(daily_sales)]
data[,pred_gbm:=predict(model, data, n.trees = n_trees)]

aux_data = copy(data)
aux_data[,small_machine:=0]
aux_data[,pred_gbm_big:=predict(model, aux_data, n.trees = n_trees)]
aux_data[,small_machine:=1]
aux_data[,pred_gbm_small:=predict(model, aux_data, n.trees = n_trees)]
aux_data[,mean(pred_gbm_big/pred_gbm_small)] # Big machines sell ~20% than small machines at a similar location

# Business case -----------------------------------------------------------
# We will calculate how much would each small machine sell if they were replaced by big machines and viceversa (how much less would big machines sell if they were replaced by a small machine). Then we will make the "swaps" pairing by impact

small_machines = data[small_machine==1]
small_machines[,sales_if_big:=daily_sales*1.2]

big_machines = data[small_machine==0]
big_machines[,sales_if_small:=daily_sales*(1/1.2)]

small_machines[,increase:=sales_if_big - daily_sales]
big_machines[,decrease:=daily_sales - sales_if_small]

# Paring small and big machines
small_machines[,.N] # 959
big_machines[,.N] # 1536
# we can make at most 959 pairings

a = big_machines[order(decrease)][,.(big_machine = machine,decrease)][1:959] # ordered by machines loosing the least
b = small_machines[order(-increase)][,.(small_machine = machine,increase)] # ordered by machines winning the most

c = cbind(a,b) # pairing
c[,profit:= increase - decrease]
c[,yearly_profit:=profit* (365*0.9)] # 365 days with 10% of inactive days

cost_of_moving = 500 # 500 euros of cost of both relocations
c[yearly_profit>cost_of_moving][,sum(yearly_profit)/1e3]
