library(RMySQL)
library(dplyr)
library(yaml)

VISITS_MINIMUM = 5000

db_config = yaml.load_file("db_config.yml")
mydb = dbConnect(MySQL(), host=db_config$db$host, dbname=db_config$db$name, user=db_config$db$user, password=db_config$db$pass)
run(mydb)
dbDisconnect(mydb)

run <- function(mydb) {
  rank <-c()
  vec_of_times <- c()
  rs = dbSendQuery(mydb, sprintf("SELECT id FROM articles where visits > %d and id_category = 11", VISITS_MINIMUM))
  data = fetch(rs, n=-1)
  
  for (article_id in data$id){
    print(article_id)
    output <- get_rank_and_time(article_id,vec_of_times,rank,mydb)
    rank <- output[[1]]
    vec_of_times <- output[[2]] 
  }
  
  plot(vec_of_times,rank)
  res=lm(rank~vec_of_time)
  abline(res)
}

get_rank_and_time <- function(article_id,vec_of_times,rank,mydb){
  date <- get_date(article_id,mydb)
  r <- c(1:length(date))
  rank <- append(rank,r)
  vec_of_times <- get_time(date,r,vec_of_times)
  output <- list(rank,vec_of_times)
  return (output)

}

get_date <- function(article_id,mydb){
  rs = dbSendQuery(mydb, sprintf("SELECT distinct(cookie), happened_at FROM visits WHERE sme_id = %d",article_id))
  data = fetch(rs, n=-1)
  date <- as.POSIXct(data[[2]], format = "%Y-%m-%d %H:%M:%S")
  return(date)
}

get_time <- function(date,r,vec_of_times){
  for(n in r){
    different_time <- difftime(date[n],date[1],units='sec')
    vec_of_times <- append(vec_of_times,different_time)
  }
  return(vec_of_times)
}





