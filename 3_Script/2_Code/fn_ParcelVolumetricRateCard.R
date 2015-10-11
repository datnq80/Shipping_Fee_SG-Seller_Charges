loadParcelVolumetricRatecard <- function(){
    require(XLConnect)
    tempwb <- loadWorkbook(file.path(inputDir,"Ratecards","Parcel_Volumetric_Ratecard.xlsx"))
    rateCard <- readWorksheet(tempwb, sheet=1, rownames = 1)
    rateCard <- setNames(rateCard, c("1","2","3","4"))
    rateCard
}