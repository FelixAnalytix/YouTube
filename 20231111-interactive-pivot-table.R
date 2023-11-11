# title: Create an Interactive Pivot Table using R
# author: FelixAnalytix.com
# YouTube video: https://youtu.be/C3oCd4_iQ0M

## ----Install and load R packages---------
if (!require(rpivotTable)) install.packages("rpivotTable")
if (!require(htmlwidgets)) install.packages("htmlwidgets")
if (!require(htmltools)) install.packages("htmltools")
if (!require(dplyr)) install.packages("dplyr")

library(rpivotTable)
library(dplyr) # only for the dataset "starwars"

rpivotTable::rpivotTable(data = starwars)

# customize the pivot table
pvt <- rpivotTable::rpivotTable(
  data = starwars, 
  rows = c("gender", "hair_color"),
  cols = "species",
  aggregatorName = c("Average"),
  vals = "height", 
  rendererName = "Heatmap", 
  renderers = list(
    "Table" = htmlwidgets::JS('$.pivotUtilities.renderers["Table"]'),
    "Table Barchart" = htmlwidgets::JS('$.pivotUtilities.renderers["Table Barchart"]'),
    "Heatmap" = htmlwidgets::JS('$.pivotUtilities.renderers["Heatmap"]')
  )
)

pvt <- htmlwidgets::prependContent(
  x = pvt,
  htmltools::tags$style(".pvtTotalLabel, .colTotal, .rowTotal, .pvtGrandTotal { display: none; }")
)

pvt
