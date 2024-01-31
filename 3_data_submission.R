
year <- 2021
for(year in yearsToSubmit){
  

load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )

# 2.5 Assign year, month, quarter, area and create table 2
# --------------------------------------------------------

# Extract the year and month from the date-time
eflalo$Year <- year(eflalo$FT_LDATIM)
eflalo$Month <- month(eflalo$FT_LDATIM)

# Set interval to 1 day
eflalo$INTV <- 1

# Create a dummy variable for aggregation
eflalo$dummy <- 1

# Aggregate the dummy variable by VE_COU, VE_REF, and LE_CDAT
res <- aggregate(
  eflalo$dummy,
  by = as.list(eflalo[, c("VE_COU", "VE_REF", "LE_CDAT")]),
  FUN = sum,
  na.rm = TRUE
)

# Rename the columns of the aggregated data frame
colnames(res) <- c("VE_COU", "VE_REF", "LE_CDAT", "nrRecords")

# Merge the aggregated data frame with eflalo
eflalo <- merge(eflalo, res, by = c("VE_COU", "VE_REF", "LE_CDAT"))

# Adjust the interval and calculate kilowatt-days
eflalo$INTV <- eflalo$INTV / eflalo$nrRecords
eflalo$kwDays <- eflalo$VE_KW * eflalo$INTV

# Define the record type
RecordType <- "LE"

# Define the columns to be included in the table
cols <- c(
  "VE_REF", "VE_COU", "Year", "Month", "LE_RECT", "LE_GEAR", "LE_MET",
  "VE_LEN", "tripInTacsat", "INTV", "kwDays", "LE_KG_TOT", "LE_EURO_TOT"
)

# Create or append to table2 based on the year
if (year == yearsToSubmit[1]) {
  table2 <- cbind(RT = RecordType, eflalo[, cols])
} else {
  table2 <- rbind(table2, cbind(RT = RecordType, eflalo[, cols]))
}


# Save table2   ====================



save(
  table2,
  file = file.path(outPath, "table2.RData" )
)

message(glue ("Table 2 for year {year} is completed") )









tacsatEflalo <- readRDS(paste0(outPath, "tacsatEflalo_", year, ".rds"))
tacsatEflalo <- data.frame(tacsatEflalo)

# setDT(tacsatEflalo)

# tacsatEflalo[,.(LE_KG_TOT = sum(LE_KG_TOT, na.rm = T), 
#                 LE_EURO_TOT = sum(LE_EURO_TOT, na.rm = T)),
#              by = .(VE_REF, VE_COU, Year, Month, Csquare, LE_GEAR, LE_MET,
#                     SI_SP, INTV)]

# "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
# "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT"


# Define the record type
RecordType <- "VE"

# table1 <- tacsatEflalo[,.(RT = "VE", )]



# Define the columns to be included in the table
cols <- c(
  "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
  "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT",
  "MSFD_BBHT", "SI_GEARWIDTH", "SA_M2"
)

# Create or append to table1 based on the year
if (year == yearsToSubmit[1]) {
  table1 <- cbind(RT = RecordType, tacsatEflalo[, cols])
} else {
  table1 <- rbind(table1, cbind(RT = RecordType, tacsatEflalo[, cols]))
}


# Save table1   ====================



save(
  table1,
  file = file.path(outPath, "table1.RData" )
)

message(glue ("Table 1 for year {year} is completed") )
}


