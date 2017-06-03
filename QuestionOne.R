install.packages("data.table")
require("data.table")

set.seed(0)

#read the data using a traditional method
ptm <- proc.time()
market <- read.csv("Poloniex.csv", skip=1, header=T)
proc.time() - ptm
#--------------------------------
#part c ends here
#--------------------------------

#compare time with fread
ptm <- proc.time()
market1 <- fread("C:/RScripts/AssignmentTwo/Poloniex.csv", skip=1, header=T)
proc.time() - ptm

#compare data writing operations with binary versions
ptm <- proc.time()
write.csv(x=market, file = "test.csv")
proc.time() - ptm

ptm <- proc.time()
saveRDS(object=market, file = "test.rds")
proc.time() - ptm

ptm <- proc.time()
data.table::fwrite(x=market1, file="test.csv") 
proc.time() - ptm

ptm <- proc.time()
saveRDS(object=market1, file = "test.rds")
proc.time() - ptm
#--------------------------------
#part d ends here
#--------------------------------

#create a data field prices by extracting columns whose name ends with _last
price_columns <- grep("_last",x=colnames(market1), ignore.case = T, value = T)
price_column_idx <- grep("_last",x=colnames(market1), ignore.case = T) 
last_prices <- market1[,..price_columns]
#--------------------------------
#part e ends here
#--------------------------------


#Extract the column represting the time and convert to calendar time objects
time <- market1[,TIME];
time_stamps <- as.POSIXct(time, origin="1970-01-01", tz = "GMT")

#Time range start
print(time_stamps[1])
#Time range end
print(time_stamps[NROW(time_stamps)])

#average difference in time using reverse loop
total_entries <- NROW(time_stamps);
difference_vector <- numeric()
for(i in total_entries:2) {
  difference_vector <- append(difference_vector, time[i] - time[i-1])  
}
time_difference_vector <- as.POSIXct(difference_vector, origin="1970-01-01", tz = "GMT")
print(mean(time_difference_vector))
#--------------------------------
#part f ends here
#--------------------------------

#sample at 5 minute intervals and then merge the columns 
TIME <- time[seq(1, nrow(market1),60)]
price_data <- last_prices[seq(1, nrow(market1),60)] 
new_data_file <- cbind.data.frame(TIME, price_data)

saveRDS(object=new_data_file, file = "gen_five_min_prices.rds")

#--------------------------------
#part g ends here
#--------------------------------

