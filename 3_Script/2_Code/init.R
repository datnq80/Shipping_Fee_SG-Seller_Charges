options( java.parameters = "-Xmx6g" )

library(dplyr)
library(reshape2)
library(XLConnect)
library(lubridate)
library(magrittr)
library(tools)

#Variable setting
venture <- "Singapore"
extractName <- "MP_Shipping_Fee_Monthly_Raw"
dateExtract <- paste0(format(Sys.Date(), "%Y-%m-"),"07")
reportUrlBase <- "http://reports.pmp.lazada.com/"
dateBegin <- as.POSIXct("2015-09-01 00:00:00","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok")
dateEnd <- as.POSIXct("2015-09-30 23:59:59","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok")
dateRun <- format(now(),"%Y%m%d")
monthRun <- format(Sys.Date()-30,"%Y%m")

# Input Output Directory
inputDir <- "../../1_Input"
runningInputDir <- file.path("../../1_Input", monthRun)
outputDir <- "../../2_Output"
dir.create(file.path(outputDir, dateRun))
outputDir <- paste(file.path(outputDir, dateRun),"/", sep = "")
if (!dir.exists(outputDir)){
    dir.create(outputDir)
}

source("../2_Code/fn_ParcelVolumetricRateCard.R")
rateCard <- loadParcelVolumetricRatecard()

# Load Delivery Company
tempwb <- loadWorkbook(file.path(runningInputDir,"Standard_Delivery_Company.xlsx"))
deliveryCompany <- readWorksheet(tempwb,sheet = 1)
deliveryCompany %<>% 
    mutate(Delivery.Company.Type="Standard")

# Load Seller Rate cards
tempwb <- NULL
tempwb <- loadWorkbook(file.path(runningInputDir, "Seller_rate_card.xlsx"))
sellerRateCards <- readWorksheet(tempwb, sheet = 1)
sellerRateCards <- setNames(sellerRateCards, c("Seller.Center.ID","Seller.Center.Name","Ratecards",
                                               "Seller.Account.Deactivated","New.rate.card.effective.date",
                                               "Discount","Finance.Action","Direct.Billing","SpecialTier"))
sellerRateCards %<>% 
    filter(!is.na(Seller.Center.ID) & !is.na(Ratecards)) %>%
    mutate(SpecialTier=ifelse(is.na(SpecialTier),"No",
                              ifelse(SpecialTier=="Yes","Yes","No"))) %>%
    mutate(Ratecard.Type=ifelse(SpecialTier=="Yes","Special","Normal"))

# Load 3PL Billing
tempwb <- NULL
tempwb <- loadWorkbook(file.path(runningInputDir,"3PL_Billing/IM_Delivery.xlsx"))
IMBilling <- readWorksheet(tempwb,sheet = "Sheet1", startRow = 1, header = TRUE)
IMBilling <- setNames(IMBilling, c("Tracking.Number","ProviderParcel",
                                   "BillingAmount")) 
IMBilling %<>% 
    mutate(lazada.Parcel.ID=ifelse(ProviderParcel<4,1,
                                   ifelse(ProviderParcel<10,2,
                                          ifelse(ProviderParcel<25,3,4)))) %>%
    mutate(ProviderParcel=as.character(ProviderParcel))
IMBilling %<>%
    filter(!duplicated(IMBilling))

tempwb <- NULL
tempwb <- loadWorkbook(file.path(runningInputDir,"3PL_Billing/NV_Delivery.xlsx"))
NVBilling <- readWorksheet(tempwb, sheet = 1)
NVBilling <- setNames(NVBilling, c("Tracking.Number","ProviderParcel","BillingAmount"))
NVBilling %<>%
    mutate(lazada.Parcel.ID=ProviderParcel+1) %>%
    mutate(ProviderParcel=as.character(ProviderParcel))
NVBilling %<>%
    filter(!duplicated(NVBilling))

tempwb <- NULL
tempwb <- loadWorkbook(file.path(runningInputDir,"3PL_Billing/TQB_Delivery.xlsx"))
TQBBilling <- readWorksheet(tempwb, sheet = 1)
TQBBilling <- setNames(TQBBilling, c("Tracking.Number", "ProviderParcel","short_parcel_size","BillingAmount"))
TQBBilling %<>%
    filter(!is.na(Tracking.Number)) %>%
    mutate(short_parcel_size=as.numeric(substr(ProviderParcel,regexpr("/",ProviderParcel)+1,50))) %>%
    mutate(lazada.Parcel.ID=ifelse(short_parcel_size<=80,1,
                                   ifelse(short_parcel_size<=120,2,
                                          ifelse(short_parcel_size<=160,3,NA)))) %>%
    select(Tracking.Number,
           ProviderParcel,
           BillingAmount,
           lazada.Parcel.ID)
TQBBilling %<>%
    filter(!duplicated(TQBBilling))

Parcel3PLBillingInfo <- rbind_list(IMBilling, TQBBilling, NVBilling)

#SKU Dimesion Data
SKU_Dimension <- NULL
for (ifile in list.files(file.path(runningInputDir,"SKU_File"))) {
    if (file_ext(ifile)=="csv") {
        skuData <- read.csv(file.path(runningInputDir,"SKU_File",ifile),
                            stringsAsFactors = FALSE)
        if (is.null(SKU_Dimension)) SKU_Dimension <- skuData
        else SKU_Dimension <- rbind_list(SKU_Dimension, skuData)
    }
}

SKU_Dimension <- select(SKU_Dimension,
                        SKU,SKU.CONFIG,PACKAGE.LENGTH,PACKAGE.WIDTH,PACKAGE.HEIGHT,PACKAGE.WEIGHT)
SKU_Dimension <- SKU_Dimension %>%
    mutate(VolumetricWeight=PACKAGE.LENGTH*PACKAGE.WIDTH*PACKAGE.HEIGHT/5000,
           FinalWeight=ifelse(VolumetricWeight>PACKAGE.WEIGHT,VolumetricWeight,PACKAGE.WEIGHT),
           lazada.Parcel.ID=ifelse(FinalWeight<4,1,
                                   ifelse(FinalWeight<10,2,
                                          ifelse(FinalWeight<25,3,4))))
SKU_Dimension_Final <- SKU_Dimension %>% group_by(SKU) %>%
    summarize(PACKAGE.LENGTH=max(PACKAGE.LENGTH),
              PACKAGE.WIDTH=max(PACKAGE.WIDTH),
              PACKAGE.HEIGHT=max(PACKAGE.HEIGHT),
              FinalWeight=max(FinalWeight),
              VolumetricWeight=max(VolumetricWeight),
              lazada.Parcel.ID=max(lazada.Parcel.ID))