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
                              
       
  a <- 486.8646
  b <- 0.2
  round(a/b)*b
  
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
  


