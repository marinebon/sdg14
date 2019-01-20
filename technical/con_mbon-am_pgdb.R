library(dplyr)
library(DBI)
library(RPostgreSQL)

# create Postgres db
# docker exec -it nrel-postgis bash
# su postgres -c "createdb -O bbest mbon-am"
# echo "**#******" > ~/.nrel_db_pass
# connect to postgis database
con_params <- list(
  dbname   = "mbon-am",
  host     = "localhost",
  port     = 5432,
  user     = "bbest",
    password = readLines("~/.nrel_db_pass"))
con <- do.call(dbConnect, c(list(drv = RPostgreSQL::PostgreSQL()), con_params))
# dbExecute(src$con, "create extension postgis;")
# TODO: set synchronous_commit off
#   For example, to make a single multistatement transaction commit asynchronously when the default is the opposite, issue SET LOCAL synchronous_commit TO OFF within the transaction.
#   https://www.postgresql.org/docs/9.1/static/runtime-config-wal.html#GUC-SYNCHRONOUS-COMMIT
#   https://www.postgresql.org/docs/9.1/static/wal-async-commit.html
# dbDisconnect(con)
