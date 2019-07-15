con <- dbConnect(RSQLite::SQLite(), dbPath, flags = SQLITE_RW)

sql <- 'select * from appUsers'

 res <- dbSendQuery(con, sql)
 sns <- dbFetch(res)
 dbClearResult(res)
 sns



rs <- dbSendStatement(conUpdate, "Update appUsers SET daysSince = :y, defaultSite = :z where  usr = :x")
dbBind(rs, param = list(x = str_to_lower(appAuth$currentUsr), y = defDaysSince, z = defSite))
dbGetRowsAffected(rs)
dbClearResult(rs)
dbDisconnect(conUpdate)