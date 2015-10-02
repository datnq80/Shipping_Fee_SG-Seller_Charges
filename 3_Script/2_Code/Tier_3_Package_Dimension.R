# Group by package
ParcelVolumnetricSoi <- filter(validSoi, Rate.cards=="Tier @ Parcel Volumetric")

# Mapping with Parcel Volumetric billing info
ParcelBillingInfo <- Parcel3PLBillingInfo
ParcelVolumnetricSoi <- left_join(ParcelVolumnetricSoi,ParcelBillingInfo)


# Non Lex Delivery
ParcelVolumnetricSoi3PLRaw <- filter(ParcelVolumnetricSoi, !grepl("LEX",Delivery.Company),
                                     !is.na(Tracking.Number))

ParcelVolumnetricSoi3PL <- ParcelVolumnetricSoi3PLRaw %>% 
    group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
             Billing.Type,Rate.cards,Delivery.Company,Delivery.Company.Type,
             ProviderParcel) %>%
    summarize(Order.Numbers=paste(Order.Number,collapse="/"),
              Shipped.Date=min(Shipped.Date),
              Cancelled.Date=max(Cancelled.Date),
              Returned.Date=max(Returned.Date),
              Failed.Delivery.Reason=paste(failed_delivery,collapse="-"),
              Returned.Reasons=paste(return_reason,collapse="-"),
              shipping_charged=sum(shipCharged),
              Rebates_charge=sum(rebateReview),
              ItemCounts=n(),
              lazada.Parcel.ID=max(lazada.Parcel.ID),
              ShippingFee=(ifelse(lazada.Parcel.ID==1,5.9,
                                  ifelse(lazada.Parcel.ID==2,8.0,
                                         ifelse(lazada.Parcel.ID==3,10.7,
                                                ifelse(lazada.Parcel.ID==4,16.1,NA))))),
              ShippingFeeCharging=ShippingFee*ifelse(shipping_charged>0,1,0),
              RebatesItemCount=sum(ifelse(Rebates_charge>=1,1,0) * 
                                       ifelse(!is.na(Cancelled.Date),1,0)),
              ShippingFeeRebating=ifelse(RebatesItemCount==ItemCounts,ShippingFee,0),
              ShippingFeeRebating=ifelse(is.na(ShippingFeeRebating),0,ShippingFeeRebating))

# Non Lex Shipping Fee Charging
ParcelVolumnetricSoi3PLCharge <- ParcelVolumnetricSoi3PL %>%
    filter(ShippingFeeCharging>0) %>%
    mutate(Final_Fee=ShippingFeeCharging,
           Notes=paste("Order#",Order.Numbers,"- TN#",Tracking.Number,
                       ifelse(Final_Fee>0,
                              paste(" - Shipping Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                              "- Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoi3PLCharge, 
          paste(outputDir, "ParcelVolumnetricSoi3PL_Charged.csv", sep = "")
          ,row.names = FALSE)

# Non Lex Shipping Fee Rebate
ParcelVolumnetricSoi3PLRebate <- ParcelVolumnetricSoi3PL %>%
    filter(ShippingFeeRebating>0) %>%
    mutate(Final_Fee=ShippingFeeRebating,
           Notes=paste("Order#",Order.Numbers,"- TN#",Tracking.Number,
                       ifelse(Final_Fee<0,
                              paste(" - Shipping Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                              "- Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoi3PLRebate, 
          paste(outputDir, "ParcelVolumnetricSoi3PL_Rebates.csv", sep = "")
          ,row.names = FALSE)

# Lex Delivery
ParcelVolumnetricSoiLexRaw <- filter(ParcelVolumnetricSoi, grepl("LEX",Delivery.Company))
ParcelVolumnetricSoiLexRaw <- left_join(ParcelVolumnetricSoiLexRaw,SKU_Dimension_Final, by = c("sku"="SKU"))

ParcelVolumnetricSoiLex <- ParcelVolumnetricSoiLexRaw %>% 
    group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
             Billing.Type,Rate.cards,Delivery.Company,Delivery.Company.Type) %>%
    summarize(Order.Numbers=paste(Order.Number,collapse="/"),
              Shipped.Date=min(Shipped.Date),
              Cancelled.Date=max(Cancelled.Date),
              Returned.Date=max(Returned.Date),
              Failed.Delivery.Reason=paste(failed_delivery,collapse="-"),
              Returned.Reasons=paste(return_reason,collapse="-"),
              shipping_charged=sum(shipCharged),
              Rebates_charge=sum(rebateReview),
              ItemCounts=n(),
              TotalWeight=sum(FinalWeight),
              lazada.Parcel.ID=ifelse(TotalWeight<4,1,
                                      ifelse(TotalWeight<10,2,
                                             ifelse(TotalWeight<25,3,4))),
              ShippingFee=(ifelse(lazada.Parcel.ID==1,5.9,
                                  ifelse(lazada.Parcel.ID==2,8.0,
                                         ifelse(lazada.Parcel.ID==3,10.7,
                                                ifelse(lazada.Parcel.ID==4,16.1,NA))))),
              ShippingFeeCharging=ShippingFee*ifelse(shipping_charged>0,1,0),
              RebatesItemCount=ifelse(Rebates_charge>=1,1,0) * 
                  sum(ifelse(!is.na(Cancelled.Date) | !is.na(Returned.Date),1,0)),
              ShippingFeeRebating=ifelse(RebatesItemCount==ItemCounts,ShippingFee,0))

# Lex Delivery Charging
ParcelVolumnetricSoiLexCharge <- ParcelVolumnetricSoiLex %>%
    filter(ShippingFeeCharging>0) %>%
    mutate(Final_Fee=ShippingFeeCharging,
           Notes=paste("Order#",Order.Numbers,"- TN#",Tracking.Number,
                       ifelse(Final_Fee>0,
                              paste(" - Shipping Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                              "- Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoiLexCharge,
          paste(outputDir,"ParcelVolumnetricSoiLex_Charged.csv", sep = "")
          ,row.names = FALSE)

# Lex Delivery Rebate
ParcelVolumnetricSoiLexRebate <- ParcelVolumnetricSoiLex %>%
    filter(ShippingFeeRebating>0) %>%
    mutate(Final_Fee=ShippingFeeRebating,
           Notes=paste("Order#",Order.Numbers,"- TN#",Tracking.Number,
                       ifelse(Final_Fee<0,
                              paste(" - Shipping Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                              "- Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoiLexRebate,
          paste(outputDir,"ParcelVolumnetricSoiLex_Rebates.csv", sep = "")
          ,row.names = FALSE)

# FD Returned Charged
ParcelVolumnetricSoiFDReturn <- ParcelVolumnetricSoi %>%
    filter(returnedCharge==1)
ParcelVolumnetricSoiFDReturn <- left_join(select(ParcelVolumnetricSoiFDReturn, -(lazada.Parcel.ID)),
                                          SKU_Dimension_Final, by = c("sku"="SKU"))

ParcelVolumnetricSoiFDReturn <- mutate(ParcelVolumnetricSoiFDReturn,
                                       fd_returned_fee=ifelse(lazada.Parcel.ID==1,5.9,
                                                              ifelse(lazada.Parcel.ID==2,8.0,
                                                                     ifelse(lazada.Parcel.ID==3,10.7,
                                                                            ifelse(lazada.Parcel.ID==4,16.1,NA)))),
                                       FinalFee=fd_returned_fee,
                                       Notes=paste("Order#",Order.Number,"- TN#",Tracking.Number,
                                                   ifelse(FinalFee>0,
                                                          paste(" - Return Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                                                          "- Return Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoiFDReturn,
          paste(outputDir,"ParcelVolumnetric_FDReturnCharge.csv", sep = ""),
          row.names = FALSE)

# FD Returned Rebates
ParcelVolumnetricSoiReturnRebate <- ParcelVolumnetricSoi %>%
    filter(returnedReview==1)
ParcelVolumnetricSoiReturnRebate <- left_join(select(ParcelVolumnetricSoiReturnRebate, -(lazada.Parcel.ID)),
                                              SKU_Dimension_Final, by = c("sku"="SKU"))

ParcelVolumnetricSoiReturnRebate <- mutate(ParcelVolumnetricSoiReturnRebate,
                                           fd_returned_fee=ifelse(lazada.Parcel.ID==1,5.9,
                                                                  ifelse(lazada.Parcel.ID==2,8.0,
                                                                         ifelse(lazada.Parcel.ID==3,10.7,
                                                                                ifelse(lazada.Parcel.ID==4,16.1,NA)))),
                                           FinalFee=fd_returned_fee,
                                           Notes=paste("Order#",Order.Number,"- TN#",Tracking.Number,
                                                       ifelse(FinalFee<0,
                                                              paste(" - Return Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                                                              "- Return Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoiReturnRebate,
          paste(outputDir,"ParcelVolumnetric_ReturnRebate.csv", sep = ""),
          row.names = FALSE)


# Missing Package Billing Information
ParcelVolumnetric <- ParcelVolumnetricSoi %>% group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
                                                       Billing.Type,Rate.cards,Delivery.Company,Delivery.Company.Type) %>%
    summarize(Order.Numbers=paste(Order.Number,collapse="/"),
              Shipped.Date=min(Shipped.Date),
              Cancelled.Date=max(Cancelled.Date),
              Returned.Date=max(Returned.Date),
              lazada.Parcel.ID=max(lazada.Parcel.ID, na.rm=TRUE),
              Failed.Delivery.Reason=paste(failed_delivery,collapse="-"),
              Returned.Reasons=paste(return_reason,collapse="-"),
              shipping_charged=sum(shipCharged),
              Rebates_charge=sum(rebateReview),
              ReturnChargeFee=sum(ReturnCharge, na.rm=TRUE),
              TotalPackageUnit=n(),
              ItemCounts=n())

ParcelVolumnetricMissing <- filter(ParcelVolumnetric,
                                   is.na(lazada.Parcel.ID),
                                   !grepl("LEX", Delivery.Company))
write.csv(ParcelVolumnetricMissing,paste(outputDir,"ParcelVolumnetricMissing3PLBilling.csv", sep = ""),
          row.names = FALSE)
