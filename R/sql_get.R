#' Get MySQL query results in 1 shot
#'
#' @param selection Columns to grab
#' @param table Table to choose
#' @param database Database on the MySQL server
#' @param connect A previously opened connection, see example
#' @export
#' @examples
#' \dontrun{
#' ## Establish database connection
#' con <- dbConnect(MySQL(),
#'                  user = 'myname',
#'                  password = 'mypassword',
#'                  host = 'google.com',
#'                  dbname='mydbname')
#'
#' ## Will pull the entire table "icd_codes"
#' sql_get("icd_codes")
#' }

sql_get <- function(selection="*", table, database="user_johnsk26", connect=con){
    q1 <- paste0("select ", selection, " from ")
    query.string <- paste0(q1, database, ".", table)
    query.results <- as.data.table(dbGetQuery(conn=connect, statement=query.string))
    return(query.results)
}
