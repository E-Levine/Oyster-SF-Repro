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
addWorksheet(wb, "Sizes")
#
##Header then data
writeData(wb, "Sizes", as.character("Overall size summary"), startCol = "A", startRow = 2, rowNames = F)
writeData(wb, "Sizes", Overall_SH, startCol = "A", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
writeData(wb, "Sizes", as.character("Overall size summary by site"), startCol = "A", startRow = 7, rowNames = F)
writeData(wb, "Sizes", Site_SH, startCol = "A", startRow = 8, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
writeData(wb, "Sizes", as.character("Annual size summary all sites"), startCol = "M", startRow = 2, rowNames = F)
writeData(wb, "Sizes", Annual_SH, startCol = "M", startRow = 3, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
writeData(wb, "Sizes", as.character("Annual size summary by site"), startCol = "A", startRow = 17, rowNames = F)
writeData(wb, "Sizes", Crossed_SH, startCol = "A", startRow = 18, rowNames = F, borders = "surrounding", 
          headerStyle = hsl, keepNA = T)
#
addStyle(wb, "Sizes", style = createStyle(valign = "center", halign = "center"), cols = 1:49, rows = 1:90, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Sizes", style = info, cols = c("A", "A", "A", "M"), rows = c(2, 7, 17, 2))
#
saveWorkbook(wb, "Output/Repro_summary_info.xlsx", overwrite = T)
