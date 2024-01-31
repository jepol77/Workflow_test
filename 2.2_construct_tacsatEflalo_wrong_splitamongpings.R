#Load speedaar

speedarr <- readRDS(paste0(outPath, "speedarr.rds"))

# Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
yearsToSubmit <- 2022:2023
year <- 2021
for(year in yearsToSubmit){
  
  print(year)
  
  tacsatp <- readRDS(paste0(outPath, "tacsatp_", year, ".rds"))
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
  
  tacsatp <- tacsatp[tacsatp$VE_REF == "DNK000034452",]
  eflalo <- eflalo[eflalo$VE_REF == "DNK000034452",]
  
  
  # tacsatp <- tacsatp[tacsatp$FT_REF == "0000900713",]
  # eflalo <- eflalo[eflalo$FT_REF == "0000900713",]
  # # 
  
  uq <- unique(eflalo$FT_REF)
  
  tacsatp <- tacsatp[tacsatp$FT_REF %in% uq[1:20], ]
  eflalo <- eflalo[eflalo$FT_REF %in% uq[1:20], ]
  
  
  
              
  
  
  
  
  #Add min and max 
  tacsatp <- tacsatp |> 
    left_join(speedarr, by = "LE_SEG")
  
  tacsatp$SI_STATE <- 0
  
  tacsatp[tacsatp$SI_SP >= tacsatp$min &
            tacsatp$SI_SP <= tacsatp$max, "SI_STATE"] <- 1
  
  table(tacsatp$SI_STATE, useNA = "always")
  
  
  message("Defining activity completed")
  
  
  # 2.3 Dispatch landings of merged eflalo at the ping scale
  # -------------------------------------------------
  
  # Get the indices of columns in eflalo that contain "LE_KG_" or "LE_EURO_"
  idx_kg <- grep("LE_KG_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_KG_TOT")])
  idx_euro <- grep("LE_EURO_", colnames(eflalo)[colnames(eflalo) %!in% c("LE_EURO_TOT")])
  
  # Calculate the total KG and EURO for each row
  if("LE_KG_TOT" %!in% names(eflalo))
    eflalo$LE_KG_TOT <- rowSums(eflalo[, idx_kg], na.rm = TRUE)
  if("LE_EURO_TOT" %!in% names(eflalo))
    eflalo$LE_EURO_TOT <- rowSums(eflalo[, idx_euro], na.rm = TRUE)
  
  # Remove the columns used for the total calculation
  eflalo <- eflalo[, -c(idx_kg, idx_euro)]
  
  # Split eflalo into two data frames based on the presence of FT_REF in tacsatp
  eflaloNM <- subset(eflalo, !FT_REF %in% unique(tacsatp$FT_REF))
  eflaloM <- subset(eflalo, FT_REF %in% unique(tacsatp$FT_REF))
  
  # Convert SI_STATE to binary (0/1) format
  # tacsatp$SI_STATE <- ifelse(tacsatp$SI_STATE == "f", 1, 0)
  
  # Filter rows where SI_STATE is 1
  tacsatEflalo <- tacsatp[tacsatp$SI_STATE == 1,]
  
  # Check the type of linking required and call splitAmongPings accordingly
  if (!"trip" %in% linkEflaloTacsat) stop("trip must be in linkEflaloTacsat")
  
  if (all(c("day", "ICESrectangle", "trip") %in% linkEflaloTacsat)) {
    level <- "day"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  } else if (all(c("day","trip") %in% linkEflaloTacsat) & !"ICESrectangle" %in% linkEflaloTacsat) {
    level <- "day"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
    tmpTa$LE_RECT <- "ALL"
    tmpEf$LE_RECT <- "ALL"
  } else if (all(c("ICESrectangle", "trip") %in% linkEflaloTacsat) & !"day" %in% linkEflaloTacsat) {
    level <- "ICESrectangle"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  } else if (linkEflaloTacsat == "trip" & length(linkEflaloTacsat) == 1) {
    level <- "trip"
    tmpTa <- tacsatp
    tmpEf <- eflaloM
  }
  
  tacsatEflalo <- splitAmongPings(
    tacsat = tmpTa,
    eflalo = tmpEf,
    variable = "all",
    level = level,
    conserve = level != "trip"
  )
  
  
  print("Dispatching landings completed")
  
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatEflalo$FT_REF, "Y", "N")
  
  e <- data.table(eflalo)
  te <- data.table(tacsatEflalo)
  
  
  a <- e[,.(LE_EURO_TOT_EFLALO = sum(LE_EURO_TOT, na.rm =  T)), by = .(tripInTacsat, FT_REF)]
  b <- te[,.(LE_EURO_TOT_TACSATEFLALO = sum(LE_EURO_TOT, na.rm =  T)), by = .(year(SI_DATIM), FT_REF)]
  
  xx <- merge(a, b)
  
  xx[round(LE_EURO_TOT_EFLALO) != round(LE_EURO_TOT_TACSATEFLALO)]
  xx[round(LE_EURO_TOT_EFLALO) == round(LE_EURO_TOT_TACSATEFLALO)]
  
  e[FT_REF == "0000900713"]
  te[FT_REF == "0000900713"]
  
  sum(e[FT_REF == "0000900713"]$LE_KG_TOT)
  sum(te[FT_REF == "0000900713"]$LE_KG_TOT)
  
  # 2.4 Assign c-square, year, month, quarter, area and create table 1
  # ------------------------------------------------------------------
  
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  save(
    eflalo,
    file = file.path(outPath, paste0("/cleanEflalo", year, ".RData"))
  )
  
  
}
 