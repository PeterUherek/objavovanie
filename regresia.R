library(RMySQL)
library(dplyr)

mydb = dbConnect(MySQL(), user='root', password='', dbname='sme2', host='localhost')
rank <-c()
vec_of_times <- c()
date <- myfunction(5070426,mydb)
r <- c(1:length(date))
rank <- append(rank,r)
vec_of_times <- myfunction2(date,r,vec_of_times)

plot(vec_of_times,rank)
res=lm(rank~vec_of_times)
abline(res)

myfunction <- function(id,mydb){
rs = dbSendQuery(mydb, sprintf("select distinct(cookie), happened_at  from sme2.visits where sme_id = %d",id))
data = fetch(rs, n=-1)
date <- as.POSIXct(data[[2]], format = "%Y-%m-%d %H:%M:%S")
return(date)
}

myfunction2 <- function(date,r,vec_of_times){
for(n in r){
different_time <- difftime(date[n],date[1],units='sec')
vec_of_times <- append(vec_of_times,different_time)
}
return(vec_of_times)
}



