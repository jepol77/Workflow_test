year <- 2009

for(year in 2009:2023){
  print(year)
  
  eflalo <- fread(paste0("//ait-pdfs.win.dtu.dk/Qdrev/AQUA/dfad/data/Data/eflalo/eflalo4_", year, ".csv"))

  #Corrections
  if(year %in% 2009:2011){
    eflalo[VE_REF == "DNK000011949" & is.na(as.numeric(VE_LEN)), VE_LEN := 8.11]
    eflalo[VE_REF == "DNK000011949" & is.na(as.numeric(VE_TON)), VE_TON := 4.99]
  }
  
  if(year %in% 2010:2017){
    eflalo[VE_REF == "DNK000013968" & is.na(as.numeric(VE_LEN)), VE_LEN := 40.8]
    eflalo[VE_REF == "DNK000013968" & is.na(as.numeric(VE_TON)), VE_TON := 441]
  }
  
  
  #Sometimes departure time will be after landing time because landing time is recorded as "0.00"
  #in the eflalo. In this case, set the landing time to "23.00"
  
  # Apply the convert to date-time function to the FT_DDAT and FT_DTIME columns
  eflalo$FT_DDATIM <- convert_to_datetime(eflalo$FT_DDAT, eflalo$FT_DTIME)
  # Apply the function to the FT_LDAT and FT_LTIME columns
  eflalo$FT_LDATIM <- convert_to_datetime(eflalo$FT_LDAT, eflalo$FT_LTIME)
  eflalo[FT_LDATIM < FT_DDATIM & FT_LTIME == "0:00", FT_LTIME := "23:00"]
  
  eflalo$FT_DDATIM <- NULL
  eflalo$FT_LDATIM <- NULL
  
  eflalo <- eflalo_clean(eflalo)
  save(eflalo, file = paste0(dataPath, "eflalo_", year, ".RData"))
  
  tacsat <- fread(paste0("//ait-pdfs.win.dtu.dk/Qdrev/AQUA/dfad/data/Data/eflalo/tacsat2_", year, ".csv"))
  tacsat <- tacsat_clean(tacsat)
  save(tacsat, file = paste0(dataPath, "tacsat_", year, ".RData"))
}



tacsat_clean <- function(tacsat){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  cols <- c("SI_LATI", "SI_LONG", "SI_SP", "SI_HE")
  if(any(cols %!in% names(tacsat)))
    stop("Column missing in tacsat:", paste(cols[cols %!in% names(tacsat)], collapse = ", "))
  
  tacsat[,(cols):= lapply(.SD, function(x) as.numeric(x)), .SDcols=cols]
  
}
  
  
    
   
     
eflalo_clean <- function(eflalo){
'%!in%' <- function(x,y)!('%in%'(x,y))
cols <- c("VE_KW", "VE_LEN", "VE_TON", "LE_MSZ", grep("KG|EURO", colnames(eflalo), value = T))

if(any(cols %!in% names(eflalo)))
  stop("Column missing in eflalo:", paste(cols[cols %!in% names(eflalo)], collapse = ", "))

eflalo[,(cols):= lapply(.SD, function(x) as.numeric(x)), .SDcols=cols]

if(nrow(eflalo[is.na(VE_KW)]) != 0){
  warning(paste("No engine recorded for the following vessels:", paste(unique(eflalo[is.na(VE_KW)]$VE_REF), collapse = ", ")), 
          ". Setting their KW to 0")
  eflalo[is.na(VE_KW), VE_KW := 0]
}

if(nrow(eflalo[is.na(LE_MSZ)]) != 0){
  warning(paste("No mesh size recorded for the following vessels:", paste(unique(eflalo[is.na(LE_MSZ)]$VE_REF), collapse = ", ")))
  warning("Setting their mesh size to 0")
  eflalo[is.na(VE_KW), VE_KW := 0]
}

if(nrow(eflalo[is.na(VE_LEN)]) != 0){
  stop(paste("No lengths recorded for the following vessels:", paste(unique(eflalo[is.na(VE_LEN)]$VE_REF), collapse = ", ")), 
        ". Vessels needs a length, please supply one")
}

if(nrow(eflalo[is.na(VE_TON)]) != 0){
  stop(paste("No weight recorded for the following vessels:", paste(unique(eflalo[is.na(VE_TON)]$VE_REF), collapse = ", ")), 
        ". Vessels needs a weight, please supply one")
}

if("FT_DDATIM" %!in% names(eflalo))
  eflalo[, FT_DDATIM := as.POSIXct(
    paste(FT_DDAT, FT_DTIME, sep = " "),
    tz = "GMT",
    format = "%d/%m/%Y  %H:%M")]

if("FT_LDATIM" %!in% names(eflalo))
  eflalo[, FT_LDATIM := as.POSIXct(
    paste(FT_LDAT, FT_LTIME, sep = " "),
    tz = "GMT",
    format = "%d/%m/%Y  %H:%M")]

}





eflalo[is.na(kw)]

ft <- haven::read_sas("//ait-pdfs.win.dtu.dk/Qdrev/AQUA/dfad/data/Data/Ftjreg/fartoejer.sas7bdat")
setDT(ft)

ft[Efid == "DNK000011949"]
ft[Efid == "DNK000013968", 1:30]

DNK000013968

ft
ft[Efid %in% eflalo[is.na(kw)]$VE_REF]$Max_Effekt

table(eflalo$FT_LTIME)
class(eflalo$FT_LTIME)
eflalo[FT_LTIME == "." & FT_LDAT != "."]
