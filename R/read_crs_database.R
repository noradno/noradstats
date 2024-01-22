#' Read international activity level CRS data (1973-). Create a remote tibble connected to the crs table in the DuckDB database file stored in Sharepoint
#'
#' @return Returns a remote table (tibble) linked to the 'crs' table in the DuckDB database. Use dplyr syntax to collect data from the remote tibble and to disconnect when done, using DBI::dbDisconnect(con, shutdown=TRUE)
#'
#' @export
#' @examples
#' ?read_crs_database()

read_crs_database <- function() {
  # Find user-specific path to database file --------------------------------
  
  # Find the user id to input in the file path
  wd <- getwd()
  
  # Check if the second character of the Userid is a digit (new style) or not (old style)
  # Use regexpr to find the position of "Users/"
  pos <- regexpr("Users/", wd)
  
  # Find the second character after "Users/"
  second_char <- substr(wd, pos + 7, pos + 7)
  
  # Check if the second character is a digit (new user id format) or not (old user id format)
  is_digit <- grepl("\\d", second_char)
  
  # Extract the user id from the wd dependent on it is a new or old user id format
  if (is_digit) {
    vec_user <- substr(wd, pos + 6, pos + 11)
  } else {
    vec_user <- substr(wd, pos + 6, pos + 9)
  }
  
  # Path to database file. Notice that the user id is replaced by the general "placeholder_user" text
  default_path_raw <-
    "C:/Users/placeholder_user/UD Office 365 AD/Norad-Avd-Kunnskap - Statistikk og analyse/13. Annen data/CRS bulk files/crs_database.duckdb"
  
  # Replace placeholder_user in path with vec_user
  default_path <- gsub("placeholder_user", vec_user, default_path_raw)
  
  # Connecting to database and create a remote tibble linked to crs --------
  
  # Establish a connection to a DuckDB database file
  con <- DBI::dbConnect(duckdb::duckdb(), default_path)
  
  # List all tables in the connected DuckDB database to verify existing tables
  DBI::dbListTables(con)
  
  # Create a remote tibble linked to the 'crs' table in the DuckDB database
  # This allows for using dplyr syntax on database queries
  df_crs_remote <- dplyr::tbl(con, "crs")
  
  DBI::dbDisconnect(con, shutdown=TRUE)
  
  # Return remote tibble
  return(df_crs_remote)
  
}
