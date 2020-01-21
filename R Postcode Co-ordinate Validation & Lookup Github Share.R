library(DBI) #required for SQL connection
library(dplyr) #for data manipulation commands
library(dbplyr) #for sql data manipulation commands
library(odbc) #required for SQL connection
library(rgdal) #for manipulating spatial data
library(sf) #for writing to MapInfo tables

#Connect to SQL postcode lookup
#Rerun this after loading R
con <- dbConnect(drv = odbc::odbc(), Driver="SQL Server", Server="Server Name", Database="Database Name", Trusted_Connection="True")

#Read in test data this is a csv file with two columns (Example and Name), The postcode is in the Example column
TestData <- read.csv('TestData3.csv')

#Remove all spaces from postcode
TestData$NoSpaces <- trimws(gsub(" ", "", TestData$Example, fixed = TRUE))

#Insert spaces to make all codes length 8
TestData$Postcode8 <- ifelse(nchar(TestData$NoSpaces) == 5, 
                             sub('([[:alnum:]]{2})', '\\1   ', TestData$NoSpaces),
                             ifelse(nchar(TestData$NoSpaces) == 6,
                                    sub('([[:alnum:]]{3})', '\\1  ', TestData$NoSpaces),
                                    ifelse(nchar(TestData$NoSpaces) == 7,
                                           sub('([[:alnum:]]{4})', '\\1 ', TestData$NoSpaces), TestData$NoSpaces)))

#Check length of postcode after inserting spaces (optional)
TestData$Postcode8Len <- nchar(TestData$Postcode8)

#Validate postcode using regex as per https://stackoverflow.com/questions/164979/uk-postcode-regex-comprehensive
TestData$Validation <- grepl('^([A-Za-z][A-Ha-hJ-Yj-y]?[0-9][A-Za-z0-9]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})$', trimws(gsub(" ", "", TestData$Postcode8, fixed = TRUE)))

#Create dataframe of invalid postcodes for checking and correction
InvalidPostcodes <- TestData %>%
  filter(Validation == FALSE)

#Remove invalid postcode formats
TestData <- TestData %>%
  filter(Validation == TRUE)

#Use dbplyr to copy Test data to temp table in SQL database
copy_to(con, MatchData)

#Join test data to lookup to return grid references ## infront of table name as the test data table is a temp table. Returns dataframe in R. 
#Table and column names need to be amended as required. Collation is specific to Birmingham CC requirements
#Lookup table is gridall postcode file from here https://digital.nhs.uk/services/organisation-data-service/data-downloads/office-for-national-statistics-data
DataWithCoords <- DBI::dbGetQuery(
  con,
  'SELECT [Example]
  ,[Name]
  ,[Postcode8]
  ,[Validation]
  ,[National Grid Reference Easting (1m resolution)]
  ,[National Grid Reference Northing (1m resolution)]
  FROM ##MatchData T1
  inner join PH_LookUps.dbo.vw_tblGreatBritainFullPostcodeFile T2 on
  T1.Postcode8 = T2.Postcod8 collate Latin1_General_CI_AS'
)

#Disconnects SQL connection and drops any temporary tables
dbDisconnect(con)

## create spatial points data. Code taken from https://stephendavidgregory.github.io/useful/UKgrid_to_LatLon
## Also see http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf

# Variables for holding the coordinate system types (see: # http://www.epsg.org/ for details)
ukgrid = "+init=epsg:27700"
latlong = "+init=epsg:4326"

# Create a unique ID for each point
DataWithCoords$pcs_ID <- 1:nrow(DataWithCoords)

# Create coordinates variable
coords <- cbind(Easting = as.numeric(as.character(DataWithCoords$`National Grid Reference Easting (1m resolution)`)),
                Northing = as.numeric(as.character(DataWithCoords$`National Grid Reference Northing (1m resolution)`)))

# Create the SpatialPointsDataFrame
pcs_SP <- SpatialPointsDataFrame(coords, data = data.frame(DataWithCoords$Example,
                                                           DataWithCoords$pcs_ID), proj4string = CRS("+init=epsg:27700"))

plot (pcs_SP) #visualise points as a sense check

# save SpatialPointsDataFrame as a ESRI shapefile (use funcetion ogrDrivers() for other types)
writeOGR(obj=pcs_SP, dsn = "S:/Intelligence New/Guidance & Templates/R Code/geocoding", layer = 'pcs_SP2', driver = 'ESRI Shapefile')
head(pcs_SP@data)

# Save MapInfo Table
st_write(st_as_sf(pcs_SP), dsn = "PCodes", layer = "Pcodes", driver = "MapInfo File")
