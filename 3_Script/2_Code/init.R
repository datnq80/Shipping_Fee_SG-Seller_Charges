if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)} #load / install+load dplyr
if(!require(reshape2)) { install.packages("reshape2"); require(reshape2)} #load / install+load dplyr
if(!require(XLConnect)) { install.packages("XLConnect"); require(XLConnect)} #load / install+load dplyr
if(!require(lubridate)) { install.packages("lubridate"); require(lubridate)} #load / install+load dplyr
library(tools)

#Variable setting
venture <- "Singapore"
extractName <- "MP_Shipping_Fee_Monthly_Raw"
dateExtract <- paste0(format(Sys.Date(), "%Y-%m-"),"07")
reportUrlBase <- "http://reports.pmp.lazada.com/"
dateBegin <- as.POSIXct("2015-08-01 00:00:00","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok")
dateEnd <- as.POSIXct("2015-08-31 23:59:59","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok")
dateRun <- format(now(),"%Y%m%d")
monthRun <- format(Sys.Date()-30,"%Y%m")

# Input Output Directory
inputDir <- "../../1_Input/"
runningInputDir <- file.path("../../1_Input", monthRun)
outputDir <- "../../2_Output"
dir.create(file.path(outputDir, dateRun))
outputDir <- paste(file.path(outputDir, dateRun),"/", sep = "")

# Load Delivery Company
tempwb <- loadWorkbook(file.path(runningInputDir,"Delivery_Company.xlsx"))
deliveryCompany <- readWorksheet(tempwb,sheet = 1)
names(deliveryCompany) <- c("Delivery.Company","Delivery.Company.Type")

# Load Seller Billing Type
tempwb <- loadWorkbook(file.path(runningInputDir,"Seller_billing_type.xlsx"))
sellerBillingType <- readWorksheet(tempwb,sheet = 1)
names(sellerBillingType) <- c("Seller.ID","Seller","Billing.Type", "Effective.Date")

# Load Seller Rate cards
sellerRateCards_1 <- read.csv(file.path(runningInputDir,"Ratecards","Seller_rate_cards_1.csv"),
                              col.names = c("Seller.ID","Seller.Center.Name","Rate_card_1",
                                            "Direct_Billing","IsDiscount"),
                              colClasses = c("character","character","factor","factor","factor"))
sellerRateCards_1 <- mutate(sellerRateCards_1,
                            Rate_card_1_Begin=as.POSIXct("2015-07-20 00:00:00","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok"),
                            Rate_card_1_End=as.POSIXct("2015-08-02 23:59:59","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok"))
sellerRateCards_2 <- read.csv(file.path(runningInputDir,"Ratecards","Seller_rate_cards_2.csv"),
                              col.names = c("Seller.ID","Seller.Center.Name","Rate_card_2",
                                            "Direct_Billing","IsDiscount"),
                              colClasses = c("character","character","factor","factor","factor"))
sellerRateCards_2 <- mutate(sellerRateCards_2,
                            Rate_card_2_Begin=as.POSIXct("2015-08-03 00:00:00","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok"),
                            Rate_card_2_End=as.POSIXct("2015-08-31 23:59:59","%Y-%m-%d %H:%M:%S", tz = "Asia/Bangkok"))
names(sellerRateCards_1)
sellerRateCards <- left_join(select(sellerRateCards_1, 
                                    Seller.ID, Rate_card_1,Rate_card_1_Begin,Rate_card_1_End)
                             ,sellerRateCards_2,
                             by=c("Seller.ID"))

# Load 3PL Billing
tempwb <- loadWorkbook(file.path(runningInputDir,"3PL_Billing/IM.xlsx"))
IMBilling <- readWorksheet(tempwb,sheet = "Sheet1", startRow = 2, header = TRUE)
setnames(IMBilling, c("Date","Tracking.Number","Weight",
                      "BillingAmount")) 
IMBilling <- filter(IMBilling, !is.na(Tracking.Number))

tempwb <- loadWorkbook(file.path(runningInputDir,"3PL_Billing/NV.xlsx"))
NVBilling <- readWorksheet(tempwb, sheet = 1)
setnames(NVBilling, c("billing_name","Tracking.Number","granular_status","to_name",
                      "parcel_size_id","name","created_at","BillingAmount"))
NVBilling <- mutate(NVBilling, lazada.Parcel.ID=parcel_size_id+1)

NVBilling <- NVBilling %>%
        select(Tracking.Number,
               ProviderParcel=parcel_size_id,
               BillingAmount,
               lazada.Parcel.ID)
NVBilling <- mutate(NVBilling, ProviderParcel=as.character(ProviderParcel))

tempwb <- loadWorkbook(file.path(runningInputDir,"3PL_Billing/TQB.xlsx"))
TQBBilling <- readWorksheet(tempwb, sheet = 1,header = FALSE)
setnames(TQBBilling, c("Col1","Col2","Col3","Col4","Col5",
                       "Tracking.Number","parcel_size_id","fee_no_tax","tax","BillingAmount"))
TQBBilling <- select(TQBBilling, Tracking.Number, parcel_size_id,fee_no_tax,tax,BillingAmount)
TQBBilling <- mutate(TQBBilling, short.Parcel.Size=as.numeric(substr(parcel_size_id,regexpr("/",parcel_size_id)+1,50)),
                     lazada.Parcel.ID=ifelse(short.Parcel.Size<=80,1,
                                             ifelse(short.Parcel.Size<=120,2,
                                                    ifelse(short.Parcel.Size<=160,3,NA))))



TQBBilling <- filter(TQBBilling, !is.na(Tracking.Number))

TQBBilling <- TQBBilling %>%
        select(Tracking.Number,
               ProviderParcel=parcel_size_id,
               BillingAmount,
               lazada.Parcel.ID)

Parcel3PLBillingInfo <- rbind_list(TQBBilling, NVBilling)


#SKU Dimesion Data
SKU_Dimension <- NULL
for (ifile in list.files(file.path(inputDir,"BOB_Data"))) {
        if (file_ext(ifile)=="csv") {
                skuData <- read.csv(file.path(inputDir,"BOB_Data",ifile))
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

# Download Raw Data
destFile <- paste(inputDir,"OMS_Data/",extractName,"_",venture,"_",dateExtract,".csv",sep = "")
extractURL <- paste(reportUrlBase,extractName,"_",venture,"_",dateExtract,".csv",sep = "")
download.file(extractURL,destFile)
