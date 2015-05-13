library(RMySQL)
library(yaml)
library(lattice)
library(DAAG)

db_config = yaml.load_file("C:/Users/Peter/Documents/GitHub/objavovanie/db_config.yml")
mydb = dbConnect(MySQL(), host=db_config$db$host, dbname=db_config$db$name, user=db_config$db$user, password=db_config$db$pass)

rs = dbSendQuery(mydb, "SELECT id, visits, id_category, words, COALESCE(length_of_reading, 0) as length_of_reading
                 FROM articles as a
                 WHERE date(a.published_at) >= '2009-10-07' AND date(a.published_at) < '2009-10-14'")
training_set = fetch(rs, n=-1)


CVlm(df=training_set,form.lm=formula(visits ~ words + length_of_reading ), m=3) # 3 fold cross-validation

dbDisconnect(mydb)
