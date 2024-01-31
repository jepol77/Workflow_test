yearsToSubmit <- 2018:2023
year <- 2021

for(year in yearsToSubmit){
  
  print(year)
  
  load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
  load(file = paste0(outPath, paste0("/cleanTacsat", year, ".RData")) )
  
  tacsat$geometry <- NULL
  
  # 2.1 Merge the TACSAT and EFLALO data together --------------------------------------------
  
  # Merge eflalo and tacsat =================================
  
  tacsatp <- mergeEflalo2Tacsat(eflalo,tacsat)
  
  
  # Assign gear and length to tacsat =================================
  
  # Define the columns to be added
  cols <- c("LE_GEAR", "LE_MSZ", "VE_LEN", "VE_KW", "LE_RECT", "LE_MET", "LE_WIDTH", "VE_FLT", "VE_COU")
  
  # Use a loop to add each column
  for (col in cols) {
    # Match 'FT_REF' values in 'tacsatp' and 'eflalo' and use these to add the column from 'eflalo' to 'tacsatp'
    tacsatp[[col]] <- eflalo[[col]][match(tacsatp$FT_REF, eflalo$FT_REF)]
  }
  

  tacsatpa_LE_GEAR <- trip_assign(tacsatp, eflalo, col = "LE_GEAR", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_GEAR$FT_REF,], tacsatpa_LE_GEAR), fill = T)
  
  tacsatpa_LE_MSZ <- trip_assign(tacsatp, eflalo, col = "LE_MSZ", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MSZ$FT_REF,], tacsatpa_LE_MSZ), fill = T)

  tacsatpa_LE_RECT <- trip_assign(tacsatp, eflalo, col = "LE_RECT", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_RECT$FT_REF,], tacsatpa_LE_RECT), fill = T)
  
  tacsatpa_LE_MET <- trip_assign(tacsatp, eflalo, col = "LE_MET", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_MET$FT_REF,], tacsatpa_LE_MET), fill = T)
  
  if("LE_WIDTH" %in% names(eflalo)){
  tacsatpa_LE_WIDTH <- trip_assign(tacsatp, eflalo, col = "LE_WIDTH", trust_logbook = T)
  tacsatp <- rbindlist(list(tacsatp[tacsatp$FT_REF %!in% tacsatpa_LE_WIDTH$FT_REF,], tacsatpa_LE_WIDTH), fill = T)
  }
  
  #Set catch date to be equal to SI_DATE 
  tacsatp$LE_CDAT <- tacsatp$SI_DATE
  
  tacsatp <- data.frame(tacsatp)
  
  # Save not merged tacsat data
  # Subset 'tacsatp' where 'FT_REF' equals 0 (not merged)
  tacsatpmin <- subset(tacsatp, FT_REF == 0)
  
  # Save 'tacsatpmin' to a file named "tacsatNotMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatpmin,
    file = file.path(outPath, paste0("tacsatNotMerged", year, ".RData"))
  )
  
  # Subset 'tacsatp' where 'FT_REF' does not equal 0 (merged)
  tacsatp <- subset(tacsatp, FT_REF != 0)
  
  # Save 'tacsatp' to a file named "tacsatMerged<year>.RData" in the 'outPath' directory
  save(
    tacsatp,
    file = file.path(outPath, paste0("tacsatMerged", year, ".RData"))
  )
  
  # 2.2  Define activity  ---------------------------------------------------------------------
  
  
  # Calculate time interval between points ===================================
  tacsatp <- intvTacsat(tacsatp, level = "trip", fill.na = TRUE)
  
  # Reset values that are simply too high to 2x the regular interval rate  
  tacsatp$INTV[tacsatp$INTV > intvThres] <- 2 * intvThres
  
  
  # Remove points with NA's in them in critial places ========================
  idx <-
    which(
      is.na(tacsatp$VE_REF) == TRUE |
        is.na(tacsatp$SI_LONG) == TRUE |
        is.na(tacsatp$SI_LATI) == TRUE |
        is.na(tacsatp$SI_DATIM) == TRUE |
        is.na(tacsatp$SI_SP) == TRUE
    )
  if (length(idx) > 0) {
    tacsatp <- tacsatp[-idx, ]
  }
  
  
  # Define speed thresholds associated with fishing for gears =====================
  
  
  # Investigate speed pattern through visual inspection of histograms # 
  
  # Create a histogram of speeds for different gears
  # Start a new PNG device
  # Create a histogram of speeds for different gears
  diag.plot <- ggplot(data = tacsatp, aes(SI_SP)) +
    geom_histogram(aes(fill = LE_GEAR), breaks = seq(0, 20, by = 1), color = "white") +
    facet_wrap(~ LE_GEAR, ncol = 4, scales = "free_y") +
    labs(x = "Speed (knots)", y = "Frequency", title = "Histogram of Speeds by Gear") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(colour = "black"),
      axis.text.x = element_text(colour = "black"),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 20),
      strip.text.x = element_text(size = 12, face = "bold"),
      # strip.background = element_rect(fill = "grey60", colour = "black", size = 1),
      panel.background = element_blank()
    ) +
    scale_fill_manual(values = c("#000000", "#FCCF3F", "#FF0000", "#00FF00", "#0000FF",
                                 "#FF00FF", "#808080", "#800000", "#808000",
                                 "#008000", "#800080", "#008080", "#000080", "#666699", "#808080",
                                 "#003366", "#CCA099", "#333300", "#993300", "#993366", "#333399",
                                 "#333333"))
  
  ggsave(diag.plot, filename = file.path(outPath, paste0("SpeedHistogram_", year, ".jpg")))
  
  # Create speed threshold object # 
  
  # start by making a list of fleet segments from lvl 5 metiers
  tacsatp$LE_SEG <-  sapply(strsplit(tacsatp$LE_MET, "_"), function(x) paste(x[1:2], collapse = "_"))  

  
  # Change wrongly assigned TBB_DEF (with small mesh size) to TBB_CRU
  tacsatp[(tacsatp$LE_SEG %in% c("TBB_CRU", "TBB_DEF") & tacsatp$LE_MSZ < 40) |
            (tacsatp$LE_SEG == "TBB_DEF" & is.na(tacsatp$LE_MSZ)), "LE_SEG"] <- "TBB_CRU"
  
  # Change wrongly assigned TBB_CRU (with large mesh size) to TBB_DEF
  tacsatp[(tacsatp$LE_SEG %in% c("TBB_CRU", "TBB_DEF") & tacsatp$LE_MSZ >= 40) |
            (tacsatp$LE_SEG == "TBB_CRU" & is.na(tacsatp$LE_MSZ)), "LE_SEG"] <- "TBB_DEF"
 
  #Add special speed thresholds for some shrimp metiers
  tacsatp[tacsatp$LE_MET == "OTB_CRU_40-59_0_0", "LE_SEG"] <- "OTB_CRU_40-59_0_0"
  tacsatp[tacsatp$LE_MET == "OTB_CRU_32-69_0_0", "LE_SEG"] <- "OTB_CRU_32-69_0_0"
  tacsatp[tacsatp$LE_MET == "OTB_CRU_16-31_0_0", "LE_SEG"] <- "OTB_CRU_16-31_0_0"
  tacsatp[tacsatp$LE_MET == "OTB_DEF_32-69_0_0", "LE_SEG"] <- "OTB_DEF_32-69_0_0"
  
  #And also for one herring and blue whiting metier
  tacsatp[tacsatp$LE_MET == "OTM_SPF_32-69_0_0", "LE_SEG"] <- "OTM_SPF_32-69_0_0"
  
  saveRDS(tacsatp, paste0(outPath, "tacsatp_", year, ".rds"))
  
  
  tp <- data.table(tacsatp)[,.(year = year(SI_DATIM), .N), by = .(LE_MSZ, LE_MET, LE_GEAR, LE_SEG, SI_SP)]
                              
       
  
  #                        
  # 
  # 
  # tp <- data.table(tacsatp)[,.(LE_MSZ = toString(range(LE_MSZ, na.rm = T)), 
  #                              LE_MET = toString(unique(LE_MET)),
  #                              LE_GEAR = toString(unique(LE_GEAR)), 
  #                              year = year, .N), by = .(LE_SEG)]
  # 
  # View(tp)
  
  
  saveRDS(tp, paste0(outPath, "tp_", year, ".rds"))
  
  }
  

tps <- rbindlist(lapply(paste0(outPath, "tp_", yearsToSubmit, ".rds"), readRDS))  

  
  # Create a data frame with minimum and maximum speed thresholds for each gear
  speedarr <- data.frame(LE_SEG = sort(unique(tps$LE_SEG)),
      min = 2,
      max = 4
    )
  
  # Fill out the minimum and maximum speed thresholds
  fix(speedarr)
  
  dput(speedarr)
  
  speedarr <- structure(list(LE_SEG = c("DRB_MOL", "FPO_CRU", "FPO_MOL", 
                                          "GNC_DEF", "GNS_CRU", "GNS_DEF", "GNS_SPF", "LHP_DEF", "LLS_DEF", 
                                          "NA_NA", "OTB_CRU", "OTB_CRU_16-31_0_0", "OTB_CRU_32-69_0_0", 
                                          "OTB_DEF", "OTB_DEF_32-69_0_0", "OTB_MCD", "OTB_SPF", "OTM_DEF", 
                                          "OTM_SPF", "OTM_SPF_32-69_0_0", "PTM_DEF", "PTM_SPF", "SDN_DEF", 
                                          "SSC_DEF", "TBB_CRU", "TBB_DEF", "TBC_CRU"), 
                             min = c(2, 2, 2, 0, 0, 0, 0, 0, 0, 2, 2, 1.2, 1.5, 2, 1.5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 5, 2), 
                             max = c(4, 4, 4, 4, 4, 4, 4, 0.1, 0.1, 4, 4, 2.5, 2.5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7, 4)), 
                        row.names = c(NA, 27L), class = "data.frame")
  
  
  
  
  
   
  # Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
  yearsToSubmit <- 2022:2023
  year <- 2022
  for(year in yearsToSubmit){
    
    tacsatp <- readRDS(paste0(outPath, "tacsatp_", year, ".rds"))
    load(file = paste0(outPath,paste0("/cleanEflalo",year,".RData")) )
    
    
    
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
  idxkgeur <- grep("LE_KG_|LE_EURO_", colnames(eflalo))
  
  # Calculate the total KG and EURO for each row
  if("LE_KG_TOT" %!in% names(eflalo))
    eflalo$LE_KG_TOT <- rowSums(eflalo[, idxkgeur], na.rm = TRUE)
  if("LE_EURO_TOT" %!in% names(eflalo))
    eflalo$LE_EURO_TOT <- rowSums(eflalo[, idxkgeur], na.rm = TRUE)
  
  # Remove the columns used for the total calculation
  eflalo <- eflalo[, -idxkgeur]
  
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
  
  save(
    tacsatEflalo,
    file = file.path(outPath, paste0("tacsatEflalo", year, ".RData"))
  )
  
  print("Dispatching landings completed")
  
  
  # 2.4 Assign c-square, year, month, quarter, area and create table 1
  # ------------------------------------------------------------------
  
  # Calculate the c-square based on longitude and latitude
  tacsatEflalo$Csquare <- CSquare(tacsatEflalo$SI_LONG, tacsatEflalo$SI_LATI, degrees = 0.05)
  
  # Extract the year and month from the date-time
  tacsatEflalo$Year <- year(tacsatEflalo$SI_DATIM)
  tacsatEflalo$Month <- month(tacsatEflalo$SI_DATIM)
  
  # Calculate the kilowatt-hour and convert interval to hours
  tacsatEflalo$kwHour <- tacsatEflalo$VE_KW * tacsatEflalo$INTV / 60
  tacsatEflalo$INTV <- tacsatEflalo$INTV / 60
  
  # Define the record type
  RecordType <- "VE"
  
  # Define the columns to be included in the table
  cols <- c(
    "VE_REF", "VE_COU", "Year", "Month", "Csquare", "LE_GEAR",
    "LE_MET", "SI_SP", "INTV", "VE_LEN", "kwHour", "VE_KW", "LE_KG_TOT", "LE_EURO_TOT"
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
  
  # Check if FT_REF is in tacsatp
  eflalo$tripInTacsat <- ifelse(eflalo$FT_REF %in% tacsatp$FT_REF, "Y", "N")
  
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
  
  
  
  
}
