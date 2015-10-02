# Read OMS Monthly Data
setClass('myDateTime')
setAs("character","myDateTime", function(from) as.POSIXct(from, format="%Y-%m-%d %H:%M:%S")) #covnert datetime string to datetime
monthData <- read.csv(destFile,
                      colClasses = c("integer","character","character","character","integer","character","character",
                                     "character","character","character","character","myDateTime",
                                     "myDateTime","myDateTime","character","character","character",
                                     "numeric"),
                      col.names = c("Bob.Item.Number","Tracking.Number","Delivery.Company","sku",
                                    "Order.Number","SCID","Payment.Method","Shipping.Type","Seller.ID",
                                    "Seller.Name","Item.Status","Shipped.Date","Cancelled.Date",
                                    "Returned.Date","cancel_reason","failed_delivery","return_reason","unit_price"))

masterData <- read.csv(file.path(inputDir,"/OMS_Data/MP_Shipping_Fee_Monthly_Raw_Singapore_All.csv"),
                       sep = ",",na.strings = "",
                       colClasses = c("integer","character","character","character","integer","character","character",
                                      "character","character","character","character","myDateTime",
                                      "myDateTime","myDateTime","character","character","character",
                                      "numeric"))

masterDatanoUpdate <- filter(masterData, !(Bob.Item.Number %in% monthData$Bob.Item.Number))
newMasterData <- rbind_list(masterDatanoUpdate,monthData)

write.csv(newMasterData,file = file.path(inputDir,"/OMS_Data/MP_Shipping_Fee_Monthly_Raw_Singapore_All.csv"),
          row.names = FALSE)

masterData <- filter(newMasterData,Tracking.Number %in% monthData$Tracking.Number)

# Merging all data into one merged data
masterData <- left_join(masterData,deliveryCompany,
                        c=("Delivery.Company"))
masterData <- left_join(masterData,sellerRateCards,
                        c=("Seller.ID"))

# Mark data to be counted
masterData <- masterData %>%
        mutate(shipCharged=ifelse(Shipped.Date>=dateBegin & Shipped.Date<=dateEnd,1,0))
masterData <- masterData %>%
        mutate(rebateReview=ifelse(Cancelled.Date>=dateBegin & Cancelled.Date<=dateEnd &
                                            Payment.Method=="CashOnDelivery",1,0))
masterData <- masterData %>%
        mutate(returnedCharge=ifelse((!is.na(Returned.Date) & Returned.Date>=dateBegin & Returned.Date<=dateEnd) |
                                             (!is.na(Cancelled.Date) & Cancelled.Date>=dateBegin & Cancelled.Date<=dateEnd &
                                                       Payment.Method!="CashOnDelivery"),1,0))
masterData <- masterData %>%
        mutate(returnedReview=ifelse((Returned.Date>=dateBegin & Returned.Date<=dateEnd & 
                                              (grepl("customer_-_changed_mind",return_reason))),1,0))
masterData <- masterData %>%
        mutate(Rate.cards=as.character(ifelse((!is.na(Shipped.Date) & Shipped.Date>=Rate_card_1_Begin & Shipped.Date<=Rate_card_1_End) |
                                                      (!is.na(Cancelled.Date) & Cancelled.Date>=Rate_card_1_Begin & Cancelled.Date<=Rate_card_1_End) |
                                                      (!is.na(Returned.Date) & Returned.Date>=Rate_card_1_Begin & Returned.Date<=Rate_card_1_End),
                                              as.character(Rate_card_1),
                                 ifelse((!is.na(Shipped.Date) & Shipped.Date>=Rate_card_2_Begin & Shipped.Date<=Rate_card_2_End) |
                                                (!is.na(Cancelled.Date) & Cancelled.Date>=Rate_card_2_Begin & Cancelled.Date<=Rate_card_2_End) |
                                                (!is.na(Returned.Date) & Returned.Date>=Rate_card_2_Begin & Returned.Date<=Rate_card_2_End),
                                        as.character(Rate_card_2),
                                        NA))))
masterData <- masterData %>%
        mutate(Billing.Type=ifelse(grepl("Direct Billing with subsidies",Direct_Billing),"Direct_Billing",
                                   ifelse(grepl("Direct Billing with NO subsidies",Direct_Billing),"Direct_Billing_NO_subsidies",
                                                NA)))

# Extract missing seller rate cards
missingRateCards <- filter(masterData, is.na(Rate.cards), !is.na(Shipped.Date),
                           shipCharged==1 | rebateReview==1 | returnedReview==1)
sellerMissingRateCards <- missingRateCards %>% group_by(Seller.ID,Seller.Name) %>%
        summarize(sales.item.count=n())
if (nrow(missingRateCards)>0){
        write.csv(missingRateCards, paste(outputDir,"MissingRateCards.csv", sep = ""),
                  row.names = FALSE)
}