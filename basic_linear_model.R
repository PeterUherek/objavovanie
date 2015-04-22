library(RMySQL)
library(yaml)

db_config = yaml.load_file("db_config.yml")
mydb = dbConnect(MySQL(), host=db_config$db$host, dbname=db_config$db$name, user=db_config$db$user, password=db_config$db$pass)

rs = dbSendQuery(mydb, "SELECT id, visits, id_category, words, length_of_reading
                 FROM articles as a
                 WHERE date(a.published_at) >= '2009-10-07' AND date(a.published_at) < '2009-10-14'")
training_set = fetch(rs, n=-1)

# Categories are so-called categorical variables, we use factor to include them
#training_set$categories <- factor(training_set$id_category)

result.lm <- lm(visits ~ words + length_of_reading, data = training_set)

# summary is useful to determine if regression is useful...
summary(result.lm)

rs = dbSendQuery(mydb, "SELECT id, visits, id_category, words, length_of_reading
                 FROM articles as a
                 WHERE date(a.published_at) >= '2009-10-14' AND date(a.published_at) < '2009-10-21'")
testing_set = fetch(rs, n=-1)

# this gives error
#testing_set$categories <- factor(testing_set$id_category)

predict.lm(result.lm, testing_set, interval = "predict")

dbDisconnect(mydb)
