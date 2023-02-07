################################################################################
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus
# January 06, 2023

# Better Together randomization scheme. The following will download the RedCap
# project information and create the randomization tables for the Better 
# Together project.

# Purpose: Generates randomization tables that can be uploaded into RedCap.
# Requires: Token from RedCap API module
# Outputs: Generates two csv files one for development and one for production
#           that can be uploaded into RedCap. Development is for testing while
#           the production is for when the project is "live" and in production.

# allocationTable() is from the RedCap API package and facilitates the creation
# of the randomization tables.
################################################################################

# Load libraries  --------------------------------------------------------------
library(redcapAPI)

# Set redcap connection  -------------------------------------------------------
rcon <- redcapConnection(url="https://redcap.ucdenver.edu/api/", 
                         token="BBAD510758872538AD4AC6FBD1A8B0C7")


# FIRST ROUND ------------------------------------------------------------------
Randomize <- allocationTable(rcon, random="randomization", 
                             strata= c("department_randomization", 
                                       "gender_randomization"),
                             replicates = 32, 
                             block.size = 4, 
                             block.size.shift = 0,
                             seed.dev=205, seed.prod=1510)

#write.csv(Randomize$dev_allocation, file="bt_randomization_dev.csv", row.names = FALSE)
#write.csv(Randomize$prod_allocation, file="bt_randomization_pro.csv", row.names = FALSE)


# SECOND ROUND  ----------------------------------------------------------------
Randomize <- allocationTable(rcon, random="randomization", 
                             strata= c("department_randomization", 
                                       "gender_randomization"),
                             replicates = 32, 
                             block.size = 4, 
                             block.size.shift = 0,
                             seed.dev=123, seed.prod=6908)

#write.csv(Randomize$dev_allocation, file="bt_randomization_dev2.csv", row.names = FALSE)
#write.csv(Randomize$prod_allocation, file="bt_randomization_pro2.csv", row.names = FALSE)

