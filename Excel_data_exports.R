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
summary <- loadWorkbook("../CSV/Summary/Perna_summary.xlsx") #For loading and updating a file
addWorksheet(wb, "Sheet 1") #Adding worksheets
removeWorksheet(summary, "initialMLR_%Sec") #Removing worksheets
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
wb <- loadWorkbook("../Output/Repro_summary_info.xlsx") #For loading and updating a file
addWorksheet(wb, "All Data")
#
##Header then data
writeData(wb, "All Data", as.character("Overall Monthly Counts"), startCol = "A", startRow = 2, rowNames = F)
writeData(wb, "All Data", Monthly_mean_counts_All, startCol = "A", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = F)
#
writeData(wb, "All Data", as.character("Monthly Counts by Site"), startCol = "A", startRow = 17, rowNames = F)
writeData(wb, "All Data", Monthly_mean_counts_All_Sites, startCol = "A", startRow = 18, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = F)
#
writeData(wb, "All Data", as.character("Overall Monthly Chi-squared"), startCol = "S", startRow = 3, rowNames = F)
writeData(wb, "All Data", tidy(All_test), startCol = "S", startRow = 4, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
writeData(wb, "All Data", All_test_pvalues, startCol = "S", startRow = 6, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
addStyle(wb, "All Data", style = createStyle(valign = "center", halign = "center"), cols = 1:49, rows = 1:90, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "All Data", style = info, cols = c("A", "A", "S"), rows = c(2, 17, 3))
#
saveWorkbook(wb, "Output/Repro_summary_info.xlsx", overwrite = T)
