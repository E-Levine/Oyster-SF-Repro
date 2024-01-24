##Data summary output to Excel
#Use while running analyses code
#Grouped by code file names

library(openxlsx)
#
####Formatting and tools####
hsl <- createStyle(fgFill = "#CCCCCC", halign = "CENTER", valign = "CENTER", textDecoration = "bold", border = "Top")
headstyle <- createStyle(textDecoration = "bold")
info <- createStyle(halign = "left", textDecoration = "bold")
#
#
##Tools to modify for use
wb = createWorkbook() #For first use
wb <- loadWorkbook("Output/Repro_summary.xlsx") #For loading and updating a file
addWorksheet(wb, "Overall_trend") #Adding worksheets
removeWorksheet(wb, "initialMLR_%Sec") #Removing worksheets
#
writeData(wb, "Sheet 1", df, startColumn=1, startRow = 1, rowNames=FALSE, borders = "surrounding", keepNA = T)
saveWorkbook(wb, "My_File.xlsx", overwrite = T)
#
openXL(wb) #Open in excel without saving file (Check)
#
####Viz tab
insertImage(summary, "Viz", "../Figures/Log_boxplot.png", startCol = "A", startRow = 2)
####Data tab
writeData(summary, "Data_Prelim", Perna_output, startCol = "A", startRow = 1, rowNames = F)
#
#
####Repro data analyses_summaries####
#
openXL(wb)
#
wb <- loadWorkbook("../Output/Repro_summary.xlsx") #For loading and updating a file
addWorksheet(wb, "Overall_trend")
#
#Years
writeData(wb, "Overall_trend", as.character("Beta regression - Year * Stage"), startCol = "A", startRow = 2, rowNames = F)
writeData(wb, "Overall_trend", Prop_yr_summ, startCol = "A", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = F)
#
writeData(wb, "Overall_trend", as.character("Summary Annual Props by Stage"), startCol = "A", startRow = 9, rowNames = F)
writeData(wb, "Overall_trend", Prop_means, startCol = "A", startRow = 10, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = F)
#
writeData(wb, "Overall_trend", as.character("Pairwise Tukey"), startCol = "H", startRow = 2, rowNames = F)
writeData(wb, "Overall_trend", Prop_m1_pairs %>% filter(p.value < 0.06), startCol = "H", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
#Months
writeData(wb, "Overall_trend", as.character("Beta regression - Month * Stage"), startCol = "P", startRow = 2, rowNames = F)
writeData(wb, "Overall_trend", Prop_mn_summ, startCol = "P", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = F)
#
writeData(wb, "Overall_trend", as.character("Summary Monthly Props by Stage"), startCol = "P", startRow = 9, rowNames = F)
writeData(wb, "Overall_trend", Prop_means2, startCol = "P", startRow = 10, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = F)
#
writeData(wb, "Overall_trend", as.character("Pairwise Tukey"), startCol = "W", startRow = 2, rowNames = F)
writeData(wb, "Overall_trend", Prop_m2_pairs %>% filter(p.value < 0.06), startCol = "W", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
addStyle(wb, "Overall_trend", style = createStyle(valign = "center", halign = "center"), cols = 1:49, rows = 1:150, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Overall_trend", style = info, cols = c("A", "A", "H", "P", "P", "W"), rows = c(2, 9, 2, 2, 9, 2))
#
saveWorkbook(wb, "Output/Repro_summary.xlsx", overwrite = T)
