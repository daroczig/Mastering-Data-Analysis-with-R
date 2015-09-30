## Extracted code chunks from
##
##  Gergely Daróczi (2015): Mastering Data Analysis with R.
##
##    Chapter #1: Hello, Data! pp. 1-36.
##
##
##  This file includes the code chunks from the above mentioned
##  chapter except for the leading ">" and "+" characters, which
##  stand for the prompt in the R console. The prompt was
##  intentionally removed here along with arbitrary line-breaks,
##  so that you copy and paste the R expressions to the R console
##  in a more convenient and seamless way.
##
##  Code chunks are grouped here by the printed pages of the book.
##  Two hash signs at the beginning of a line stands for a page
##  break, while an extra empty line between the code chunks
##  represents one or more paragraphs in the original book between
##  the examples for easier navigation.
##
##  Sometimes extra instructions starting with a double hash are
##  also provided on how to run the below expressions.
##
##
##  Find more information on the book at http://bit.ly/mastering-R
##  and you can contact me on Twitter and GitHub by the @daroczig
##  handle, or mail me at daroczig@rapporter.net
##

library('hflights')
write.csv(hflights, 'hflights.csv', row.names = FALSE)

##

str(hflights)

##

system.time(read.csv('hflights.csv'))

colClasses <- sapply(hflights, class)
system.time(read.csv('hflights.csv', colClasses = colClasses))

library(microbenchmark)
f <- function() read.csv('hflights.csv')
g <- function() read.csv('hflights.csv', colClasses = colClasses, nrows = 227496, comment.char = '')
res <- microbenchmark(f(), g())
res

## p. 3 infobox
library(R.utils)
countLines('hflights.csv')

##

boxplot(res, xlab  = '', main = expression(paste('Benchmarking ', italic('read.table'))))

library(sqldf)
system.time(read.csv.sql('hflights.csv'))

##

library(ff)
system.time(read.csv.ffdf(file = 'hflights.csv'))

library(bigmemory)
system.time(read.big.matrix('hflights.csv', header = TRUE))

##

library(data.table)
system.time(dt <- fread('hflights.csv'))

df <- as.data.frame(dt)

##

is.data.frame(dt)

## p. 6
.read.csv.orig   <- function() read.csv('hflights.csv')
.read.csv.opt    <- function() read.csv('hflights.csv', colClasses = colClasses, nrows = 227496, comment.char = '', stringsAsFactors = FALSE)
.read.csv.sql    <- function() read.csv.sql('hflights.csv')
.read.csv.ffdf   <- function() read.csv.ffdf(file = 'hflights.csv')
.read.big.matrix <- function() read.big.matrix('hflights.csv', header = TRUE)
.fread           <- function() fread('hflights.csv')

res <- microbenchmark(.read.csv.orig(), .read.csv.opt(), .read.csv.sql(), .read.csv.ffdf(), .read.big.matrix(), .fread(), times = 10)

print(res, digits = 6)

##

df <- read.csv.sql('hflights.csv', "select * from file where Dest = '\"BNA\"'")

##

df <- read.csv.sql('hflights.csv', "select * from file where Dest = 'BNA'", filter = 'tr -d ^\\" ')

str(df)

system.time(read.csv.sql('hflights.csv', "select * from file"))
system.time(read.csv.sql('hflights.csv', "select * from file where Dest = '\"BNA\"'"))

system.time(system('cat hflights.csv | grep BNA', intern = TRUE))

##

sqldf("attach 'hflights_db' as new")

read.csv.sql('hflights.csv', sql = 'create table hflights as select * from file', dbname = 'hflights_db')

system.time(df <- sqldf("select * from hflights where Dest = '\"BNA\"'", dbname = "hflights_db"))

##

## to be pasted in the MySQL command-line:
create database hflights_db;
grant all privileges on hflights_db.* to 'user'@'localhost' identified by 'password';
flush privileges;
exit;

##

library(RMySQL)
con <- dbConnect(dbDriver('MySQL'), user = 'user', password = 'password', dbname = 'hflights_db')

dbWriteTable(con, 'hflights', hflights)
dbListTables(con)

system.time(dbReadTable(con, 'hflights'))

system.time(dbGetQuery(con, 'select * from hflights'))

##

options('sqldf.connection' = con)
system.time(sqldf('select * from hflights'))

system.time(sqldf('SELECT * FROM hflights WHERE Dest = "BNA"'))

##

dbSendQuery(con, 'CREATE INDEX Dest_idx ON hflights (Dest(3));')

system.time(sqldf('SELECT * FROM hflights WHERE Dest = "BNA"'))

options(sqldf.driver = 'SQLite')
sqldf("CREATE INDEX Dest_idx ON hflights (Dest);", dbname = "hflights_db")
system.time(sqldf("select * from hflights where Dest = '\"BNA\"'", dbname = "hflights_db"))

##

## to be pasted in the command-line:
createuser --pwprompt user
createdb hflights_db
psql

## to be pasted in the PostgreSQL command-line:
\du
\list
grant all privileges on hflights_db to user
\q

##

library(RPostgreSQL)

##

con <- dbConnect(dbDriver('PostgreSQL'), user = 'user', password = 'password', dbname = 'hflights_db')

dbListTables(con)
dbExistsTable(con, 'hflights')

dbWriteTable(con, 'hflights', hflights)
system.time(dbReadTable(con, 'hflights'))

system.time(dbGetQuery(con, "SELECT * FROM hflights WHERE \"Dest\" = 'BNA';"))

##

## to be pasted in bash to install ROracle on Arch Linux
R CMD INSTALL --configure-args='--with-oci-lib=/usr/include/ --with-oci-inc=/usr/share/licenses/oracle-instantclient-basic' ROracle_1.1-11.tar.gz

library(ROracle)
con <- dbConnect(dbDriver('Oracle'), user = 'pmuser', password = 'oracle', dbname = '//192.168.0.16:1521/PDB1')
summary(con)

dbListTables(con)

##

dbWriteTable(con, 'hflights', hflights)
system.time(dbReadTable(con, 'hflights'))

system.time(dbGetQuery(con, "SELECT * FROM \"hflights\" WHERE \"Dest\" = 'BNA'"))

dbSendQuery(con, 'CREATE INDEX Dest_idx ON "hflights" ("Dest")')

##

system.time(dbGetQuery(con, "SELECT * FROM \"hflights\" WHERE \"Dest\" = 'BNA'"))

##

## the content of /etc/odbcinst.ini on Linux
[MySQL]
Description     = ODBC Driver for MySQL
Driver          = /usr/lib/libmyodbc.so
Setup           = /usr/lib/libodbcmyS.so
FileUsage       = 1

## the content of /etc/odbc.ini on Linux
[hflights]
Description     = MySQL hflights test
Driver          = MySQL
Server          = localhost
Database        = hflights_db
Port            = 3306
Socket          = /var/run/mysqld/mysqld.sock

##

library(RODBC)
con <- odbcConnect("hflights", uid = "user", pwd = "password")

system.time(hflights <- sqlQuery(con, "select * from hflights"))

sqlDrop(con, 'hflights')
sqlSave(con, hflights, 'hflights')

##

close(con)

library(dbConnect)
DatabaseConnect()

##

library(devtools)
install_github('bigrquery', 'hadley')
