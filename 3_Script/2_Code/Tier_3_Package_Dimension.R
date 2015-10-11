# Group by package
ParcelVolumnetricSoi <- filter(validSoi, Ratecards=="Tier @ Parcel Volumetric")

# Mapping with Parcel Volumetric billing info
ParcelBillingInfo <- Parcel3PLBillingInfo
ParcelVolumnetricSoi <- left_join(ParcelVolumnetricSoi,ParcelBillingInfo)

# Non Lex Delivery
ParcelVolumnetricSoi3PLRaw <- filter(ParcelVolumnetricSoi, !grepl("LEX",Delivery.Company),
                                     !is.na(Tracking.Number))

missingInvoice <- filter(ParcelVolumnetricSoi3PLRaw,is.na(lazada.Parcel.ID))
if (nrow(missingInvoice)>0){
    write.csv(missingInvoice, paste(outputDir,"missing3PLInvoice.csv", sep = ""),
              row.names = FALSE)
}

ParcelVolumnetricSoi3PLRaw %<>% filter(!is.na(lazada.Parcel.ID))

ParcelVolumnetricSoi3PL <- ParcelVolumnetricSoi3PLRaw %>% 
    group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
             Billing.Type,Ratecards,Ratecard.Type,Delivery.Company,Delivery.Company.Type,
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
              lazada.Parcel.ID=max(lazada.Parcel.ID, na.rm=TRUE),
              RebatesItemCount=sum(ifelse(Rebates_charge>=1,1,0) * 
                                       ifelse(!is.na(Cancelled.Date),1,0)))
ParcelVolumnetricSoi3PL %<>% ungroup() %>%
    mutate(ShippingFee=as.numeric(ifelse(Ratecard.Type=="Special",
                                         rateCard["Special", lazada.Parcel.ID],
                                         rateCard["Normal", lazada.Parcel.ID]))) %>%
    mutate(ItemCounts=as.numeric(ItemCounts)) %>%
    mutate(ShippingFeeCharging=ShippingFee*as.numeric(ifelse(shipping_charged>0,1,0))) %>%
    mutate(ShippingFeeRebating=ifelse(RebatesItemCount==ItemCounts,ShippingFee,0.0)) %>%
    mutate(ShippingFeeRebating=ifelse(is.na(ShippingFeeRebating),0,ShippingFeeRebating))

# Non Lex Shipping Fee Charging
ParcelVolumnetricSoi3PLCharge <- ParcelVolumnetricSoi3PL %>%
    filter(ShippingFeeCharging>0) %>%
    mutate(Final_Fee=ShippingFeeCharging,
           Notes=paste("Order#",Order.Numbers,"- TN#",Tracking.Number,
                       ifelse(Final_Fee>0,
                              paste(" - Shipping Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                              "- Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(select(ParcelVolumnetricSoi3PLCharge,-c(ShippingFee,ShippingFeeRebating,Final_Fee)), 
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

write.csv(select(ParcelVolumnetricSoi3PLRebate, -c(ShippingFee,ShippingFeeCharging,Final_Fee)),
          paste(outputDir, "ParcelVolumnetricSoi3PL_Rebates.csv", sep = ""),
          row.names = FALSE)

# Lex Delivery
ParcelVolumnetricSoiLexRaw <- ParcelVolumnetricSoi %>% 
    filter(grepl("LEX",Delivery.Company)) %>%
    select(-c(lazada.Parcel.ID))
ParcelVolumnetricSoiLexRaw <- left_join(ParcelVolumnetricSoiLexRaw,SKU_Dimension_Final, by = c("sku"="SKU"))

ParcelVolumnetricSoiLexRaw %<>% filter(!is.na(lazada.Parcel.ID))

ParcelVolumnetricSoiLex <- ParcelVolumnetricSoiLexRaw %>%
    group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
             Billing.Type,Ratecards,Ratecard.Type,Delivery.Company,Delivery.Company.Type,
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
              lazada.Parcel.ID=max(lazada.Parcel.ID, na.rm=TRUE),
              RebatesItemCount=sum(ifelse(Rebates_charge>=1,1,0) * 
                                       ifelse(!is.na(Cancelled.Date),1,0)))
ParcelVolumnetricSoiLex %<>% ungroup() %>%
    mutate(ShippingFee=as.numeric(ifelse(Ratecard.Type=="Special",
                                         rateCard["Special", lazada.Parcel.ID],
                                         rateCard["Normal", lazada.Parcel.ID]))) %>%
    mutate(ItemCounts=as.numeric(ItemCounts)) %>%
    mutate(ShippingFeeCharging=ShippingFee*as.numeric(ifelse(shipping_charged>0,1,0))) %>%
    mutate(ShippingFeeRebating=ifelse(RebatesItemCount==ItemCounts,ShippingFee,0.0)) %>%
    mutate(ShippingFeeRebating=ifelse(is.na(ShippingFeeRebating),0,ShippingFeeRebating))

# Lex Delivery Charging
ParcelVolumnetricSoiLexCharge <- ParcelVolumnetricSoiLex %>%
    filter(ShippingFeeCharging>0) %>%
    mutate(Final_Fee=ShippingFeeCharging,
           Notes=paste("Order#",Order.Numbers,"- TN#",Tracking.Number,
                       ifelse(Final_Fee>0,
                              paste(" - Shipping Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                              "- Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(select(ParcelVolumnetricSoiLexCharge, -c(ShippingFee,ShippingFeeCharging,Final_Fee)),
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

write.csv(select(ParcelVolumnetricSoiLexRebate, -c(ShippingFee,ShippingFeeCharging,Final_Fee)),
          paste(outputDir,"ParcelVolumnetricSoiLex_Rebates.csv", sep = "")
          ,row.names = FALSE)


#########################################

ParcelVolumnetricSoiSKU <- left_join(select(ParcelVolumnetricSoi, -(lazada.Parcel.ID)),
          SKU_Dimension_Final, by = c("sku"="SKU"))

missingSKUData <- filter(ParcelVolumnetricSoiSKU,is.na(lazada.Parcel.ID))
if (nrow(missingSKUData)>0){
    write.csv(missingSKUData, paste(outputDir,"missingSKUData.csv", sep = ""),
              row.names = FALSE)
}

ParcelVolumnetricSoiSKU %<>% filter(!is.na(lazada.Parcel.ID))

# FD Returned Charged
ParcelVolumnetricSoiFDReturn <- ParcelVolumnetricSoiSKU %>%
    filter(returnedCharge==1)

ParcelVolumnetricSoiFDReturn <- mutate(ParcelVolumnetricSoiFDReturn,
                                       Returned_fee=as.numeric(ifelse(Ratecard.Type=="Special",
                                                                         rateCard["Special", lazada.Parcel.ID],
                                                                         rateCard["Normal", lazada.Parcel.ID])),
                                       Notes=paste("Order#",Order.Number,"- TN#",Tracking.Number,
                                                   ifelse(FinalFee>0,
                                                          paste(" - Return Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                                                          "- Return Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoiFDReturn,
          paste(outputDir,"ParcelVolumnetric_FDReturnCharge.csv", sep = ""),
          row.names = FALSE)

# FD Returned Rebates
ParcelVolumnetricSoiReturnRebate <- ParcelVolumnetricSoiSKU %>%
    filter(returnedReview==1)

ParcelVolumnetricSoiReturnRebate <- mutate(ParcelVolumnetricSoiReturnRebate,
                                           Returned_fee_rebates=as.numeric(ifelse(Ratecard.Type=="Special",
                                                                             rateCard["Special", lazada.Parcel.ID],
                                                                             rateCard["Normal", lazada.Parcel.ID])),
                                           Notes=paste("Order#",Order.Number,"- TN#",Tracking.Number,
                                                       ifelse(FinalFee<0,
                                                              paste(" - Return Fee -  Charged under Tiered Parcel Volumetric tiering#",lazada.Parcel.ID),
                                                              "- Return Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(ParcelVolumnetricSoiReturnRebate,
          paste(outputDir,"ParcelVolumnetric_ReturnRebate.csv", sep = ""),
          row.names = FALSE)


# Missing Package Billing Information
ParcelVolumnetric <- ParcelVolumnetricSoi %>% group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
                                                       Billing.Type,Ratecards,Delivery.Company,Delivery.Company.Type) %>%
    summarize(Order.Numbers=paste(Order.Number,collapse="/"),
              Shipped.Date=min(Shipped.Date),
              Cancelled.Date=max(Cancelled.Date),
              Returned.Date=max(Returned.Date),
              lazada.Parcel.ID=max(lazada.Parcel.ID, na.rm=TRUE),
              Failed.Delivery.Reason=paste(failed_delivery,collapse="-"),
              Returned.Reasons=paste(return_reason,collapse="-"),
              shipping_charged=sum(shipCharged),
              Rebates_charge=sum(rebateReview),
              ReturnChargeFee=sum(returnedCharge, na.rm=TRUE),
              TotalPackageUnit=n(),
              ItemCounts=n())

ParcelVolumnetricMissing <- filter(ParcelVolumnetric,
                                   is.na(lazada.Parcel.ID),
                                   !grepl("LEX", Delivery.Company))
write.csv(ParcelVolumnetricMissing,paste(outputDir,"ParcelVolumnetricMissing3PLBilling.csv", sep = ""),
          row.names = FALSE)
