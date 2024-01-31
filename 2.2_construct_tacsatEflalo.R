#Load summary for all fleet segments and plot them

tps <- rbindlist(lapply(paste0(outPath, "tp_", yearsToSubmit, ".rds"), readRDS))  

#By segment
his <- tps[,.(N = sum(N)), by = .(SI_SP = round(SI_SP/0.2)*0.2, LE_SEG)]

i <- "OTB_MCD" 
for(i in unique(tps$LE_SEG)){
ggplot(his[LE_SEG == i & SI_SP < 8], aes(x = SI_SP, y = N)) + 
  geom_line(aes(color=LE_SEG))+
    scale_x_continuous(breaks = seq(0, 8, by = 1))

ggsave(paste0(plotPath, i, ".png"), dpi = 400,
       width = 15,
       height = 10,
       units = c("cm"))
}


# Create a data frame with minimum and maximum speed thresholds for each gear
speedarr <- data.frame(LE_SEG = sort(unique(tps$LE_SEG)),
                       min = 2,
                       max = 4
)

# Fill out the minimum and maximum speed thresholds
fix(speedarr)

# speedarr <- structure(list(LE_SEG = c("DRB_MOL", "FPN_CAT", "FPN_CRU", "FPN_DEF", 
#                                       "FPO_CRU", "FPO_DEF", "FPO_MOL", "GNC_DEF", "GND_SPF", "GNS_ANA", 
#                                       "GNS_CRU", "GNS_DEF", "GNS_SPF", "LHP_DEF", "LHP_SPF", "LLD_ANA", 
#                                       "LLS_DEF", "MIS_DEF", "OTB_CRU", "OTB_CRU_16-31_0_0", "OTB_CRU_32-69_0_0", 
#                                       "OTB_DEF", "OTB_DEF_32-69_0_0", "OTB_DWS", "OTB_MCD", "OTB_SPF", 
#                                       "OTM_DEF", "OTM_SPF", "OTM_SPF_32-69_0_0", "PS_SPF", "PTB_CRU", 
#                                       "PTB_DEF", "PTB_SPF", "PTM_DEF", "PTM_SPF", "SDN_DEF", "SDN_SPF", 
#                                       "SSC_DEF", "TBB_CRU", "TBB_DEF"), 
#                            min = c(2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 1.2, 1.5, 2, 1.5, 2, 2, 2, 
#                                    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 5), 
#                            max = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 0.1, 0.1, 0.1, 0.1, 4, 4, 2.5, 2.5, 4, 4, 4, 
#                                    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 7)), 
#                       row.names = c(NA, 40L), class = "data.frame")



saveRDS(speedarr, paste0(outPath, "speedarr.rds"))

# 
# #Load speedaar that was created earlier
# speedarr <- readRDS(paste0(outPath, "speedarr.rds"))

# Analyse activity automated for common gears only. Use the speedarr for the other gears =============== 
# yearsToSubmit <- 2022:2023
year <- 2021
for(year in yearsToSubmit){
  
  print(year)
  
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
  # saveRDS(eflalo, paste0(outPath, "eflalo_", year, ".rds"))
  
  
}
 