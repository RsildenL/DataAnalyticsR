library(data.table)
machine_data<-fread(file = "/Applications/escola/MASTERS/Data with R/Challenge 1+2/challenge1/machine_data.csv",
                                   header = TRUE, 
                                   sep = ",",
                                   stringsAsFactors = FALSE, 
                                   dec= ".")
machine_products<-fread(file = "/Applications/escola/MASTERS/Data with R/Challenge 1+2/challenge1/product_data.csv",
                    header = TRUE, 
                    sep = ",",
                    stringsAsFactors = FALSE, 
                    dec= ".")
transactional_data<-fread(file = "/Applications/escola/MASTERS/Data with R/Challenge 1+2/challenge1/transactional_data.csv",
                        header = TRUE, 
                        sep = ",",
                        stringsAsFactors = FALSE, 
                        dec= ".")
merge_data<-merge(x=machine_data,y = transactional_data,by.x = 'machine', by.y = 'machine')

head(machine_data)
#1 Machines
machine_count<- length(unique(machine_data[,machine]))
machine_distribution <- machine_data[,.(.N), by=small_machine]
machine_distribution[,percentage:=(N/machine_count)]
machine_distribution_type<-machine_data[,.(.N), by=.(small_machine, location_type)]
machine_distribution_type[,percentage:=(N/machine_count)]

#2  Products
machine_products_count<-length(unique(machine_products[,product_name]))
max(machine_products$category)
machine_products_avg<-machine_products[,.(.N, mean_price=mean(price)), by=(category)]
machine_products_avg[order(mean_price)]
machine_products_avg_type<-machine_products[,.(.N, mean_price=mean(price)), by=type_drink_snack]

#3  Transactional data (not sure)
head(merge_data)
march<-merge_data[year(date)=='2017' & month(date)=='3',.(.N), by=.(date,machine,small_machine)]
march_avg_small<-march[,.(mean_items=mean(N)),by=.(machine, small_machine)]
march_avg<-march_avg_small[,.(mean_iteams1=sum(mean_items)/uniqueN(machine)),by=small_machine]

#Exercise 2
#1
daily_items_sold <- transactional_data[, .(.N), by=.(machine, date)]
type_drinks_snacks<-machine_products[,.(.N),by=type_drink_snack]
plot(daily_items_sold$date,type_drinks_snacks$type_drink_snack)




#Exercise 3
#identificare tratar outliers
#hist
library(ggplot2)
ggplot(machine_data) +
  aes(x = income_average) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()
#boxplot
ggplot(machine_data) +
  aes(x = "", y = income_average) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

out <- boxplot.stats(machine_data$income_average)$out
out_ind <- which(machine_data$income_average %in% c(out))
out_ind

#tratar NA
#1: delete missing values
is.na(machine_data$income_average)
na.omit(machine_data$income_average)
#2: substitute missing values with the average value
machine_data$income_average[machine_data$income_average==mean(machine_data$income_average)]<-NA
#3:

#Exercise 4
boxplot(machine_data$num_hotels)

ggplot(machine_data, aes(y=num_hotels, fill=num_hotels)) 
 
ggplot(machine_data) +
  aes(x = "", y = num_hotels) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


