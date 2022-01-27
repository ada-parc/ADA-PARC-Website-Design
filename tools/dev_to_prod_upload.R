

# Setup -------------------------------------------------------------------


# Libraries
library(config);library(mongolite);library(here)


# Parameters --------------------------------------------------------------


# Folder to store Dev data dump before using restore to Prod
param_dir <- "C:/Users/sconn/Downloads/test"

# MongoDB database tools
# link to download MongoDB Database Tools
# https://www.mongodb.com/try/download/database-tools
# Should be a part of your config file
mongodb_tools <- config::get("mongodb_tools")

# Dev server connection
url_dev <- sprintf("mongodb+srv://%s:%s@%s/%s",
                   config::get("user"),
                   config::get("password"),
                   config::get("host_dev"),
                   "ADA-PARC")

# Prod server connection
url_prod <- sprintf("mongodb+srv://%s:%s@%s/%s",
                    config::get("user"),
                    config::get("password"),
                    config::get("host_prod"),
                    "ADA-PARC")


# Get binary dump from Dev server -----------------------------------------


# Build string for shell
exec_mongodump <- sprintf('%s\\mongodump.exe --uri "%s" --out "%s"',
                          mongodb_tools,
                          url_dev,
                          param_dir)

# Execute mongodump command
shell(exec_mongodump)


# Restore Prod Server using Dev data --------------------------------------


# Build string for shell
# This includes "--drop" option to remove collections with the same name before restoring
exec_mongorestore <- sprintf('%s\\mongorestore.exe --uri "%s" --dir "%s/ADA-PARC" --drop',
                             mongodb_tools,
                             url_prod,
                             param_dir)

# Execute mongorestore command
shell(exec_mongorestore)

