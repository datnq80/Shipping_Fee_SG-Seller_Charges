# Tier Fee
validSoi <- filter(masterData,!is.na(Rate.cards),!is.na(Shipped.Date),Delivery.Company.Type=="Standard")

# calculate Fee charging & rebating
TierFeeItem <- filter(validSoi, Rate.cards=="Tier @ Parcel value")
TierFee <- TierFeeItem %>% group_by(Tracking.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
                                    Billing.Type,Rate.cards,Delivery.Company,Delivery.Company.Type) %>%
        summarize(Order.Numbers=paste(Order.Number,sep="/"),
                  Shipped.Date=min(Shipped.Date),
                  Cancelled.Date=max(Cancelled.Date),
                  Returned.Date=max(Returned.Date),
                  Failed.Delivery.Reason=paste(failed_delivery,"-"),
                  Returned.Reasons=paste(return_reason,"-"),
                  shipping_charged=sum(shipCharged),
                  Rebates_charge=sum(rebateReview),
                  TotalPackageUnit=sum(unit_price),
                  ItemCounts=n(),
                  ShippingFee=(ifelse(TotalPackageUnit<10,0.99,
                                      ifelse(TotalPackageUnit<20,1.99,
                                             ifelse(TotalPackageUnit<30,2.99,3.99)))*1.07),
                  ShippingFeeCharging=ShippingFee*ifelse(shipping_charged>0,1,0),
                  RebatesItemCount=ifelse(Rebates_charge>=1,1,0) * 
                          sum(ifelse(!is.na(Cancelled.Date),1,0)),
                  ShippingFeeRebating=ifelse(RebatesItemCount==ItemCounts,ShippingFee,0))

# Non direct Billing Shipping Fee Charge
TierFeeNonDirectBilling <- filter(TierFee, is.na(Billing.Type),
                                  ShippingFeeCharging>0)
TierFeeNonDirectBilling <- mutate(TierFeeNonDirectBilling,
                                  FinalFee=ShippingFeeCharging,
                                  Notes=paste("Order#",Order.Numbers," - TN#",Tracking.Number,
                                              ifelse(FinalFee>0,
                                                     paste(" - Shipping Fee -  Charged under Tiered Fee: Total package value: S$",
                                                           TotalPackageUnit),
                                                     " - Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))
TierFeeNonDirectBilling <- filter(TierFeeNonDirectBilling, FinalFee!=0)

write.csv(filter(TierFeeNonDirectBilling, FinalFee>0),paste(outputDir,"TierFeeNonDirectBilling_Charged.csv", sep = ""),
          row.names = FALSE)

# Non direct Billing Shipping Fee Rebating
TierFeeNonDirectBillingRebate <- filter(TierFee, is.na(Billing.Type),
                                        ShippingFeeRebating>0)
TierFeeNonDirectBillingRebate <- mutate(TierFeeNonDirectBillingRebate,
                                  FinalFee=ShippingFeeRebating,
                                  Notes=paste("Order#",Order.Numbers," - TN#",Tracking.Number,
                                              ifelse(FinalFee<0,
                                                     paste(" - Shipping Fee -  Charged under Tiered Fee: Total package value: S$",
                                                           TotalPackageUnit),
                                                     " - Shipping Fee Rebates - Refund due to customer change of mind / COD rejection")))

write.csv(filter(TierFeeNonDirectBillingRebate, FinalFee>0),paste(outputDir,"TierFeeNonDirectBilling_Rebates.csv", sep = ""),
          row.names = FALSE)


# FD/Returned Fee Charge
TierFeeItemFDReturned <- left_join(filter(TierFeeItem, returnedCharge==1), SKU_Dimension_Final,
                                   by = c("sku"="SKU"))
TierFeeItemFDReturned <- mutate(TierFeeItemFDReturned,
                                fd_returned_fee=ifelse(lazada.Parcel.ID==1,5.9,
                                                       ifelse(lazada.Parcel.ID==2,8.0,
                                                              ifelse(lazada.Parcel.ID==3,10.7,
                                                                     ifelse(lazada.Parcel.ID==4,16.1,NA)))),
                                FinalFee=fd_returned_fee)
TierFeeItemFDReturned <- select(TierFeeItemFDReturned, 
                                -c(Rate_card_1,Rate_card_1_Begin,Rate_card_1_End,
                                  Rate_card_2,Rate_card_2_Begin,Rate_card_2_End))

write.csv(TierFeeItemFDReturned,paste(outputDir,"TierFee_FDReturnCharge.csv", sep = ""),
          row.names = FALSE)

# FD/Returned Fee Rebate
TierFeeItemFDReturnRebate <- left_join(filter(TierFeeItem, returnedReview==1), SKU_Dimension_Final,
                                   by = c("sku"="SKU"))
TierFeeItemFDReturnRebate <- mutate(TierFeeItemFDReturnRebate,
                                fd_returned_fee=ifelse(lazada.Parcel.ID==1,5.9,
                                                       ifelse(lazada.Parcel.ID==2,8.0,
                                                              ifelse(lazada.Parcel.ID==3,10.7,
                                                                     ifelse(lazada.Parcel.ID==4,16.1,NA)))),
                                FinalFee=fd_returned_fee)
TierFeeItemFDReturnRebate <- select(TierFeeItemFDReturnRebate, 
                                -c(Rate_card_1,Rate_card_1_Begin,Rate_card_1_End,
                                   Rate_card_2,Rate_card_2_Begin,Rate_card_2_End))

write.csv(TierFeeItemFDReturnRebate,paste(outputDir,"TierFee_ReturnRebate.csv", sep = ""),
          row.names = FALSE)

# Direct Billing
TierFeeDirectBilling <- filter(TierFee, !is.na(Billing.Type) & (ShippingFeeCharging>0 | ShippingFeeRebating>0))
TrackingBilling <- filter(rbind_list(select(IMBilling, Tracking.Number, BillingAmount),
                                     select(NVBilling, Tracking.Number, BillingAmount),
                                     select(TQBBilling, Tracking.Number, BillingAmount)),!is.na(Tracking.Number))

TierFeeDirectBilling <- left_join(TierFeeDirectBilling,TrackingBilling)

TierFeeDirectBillingMissing <- filter(TierFeeDirectBilling, is.na(BillingAmount))
TierFeeDirectBilling <- filter(TierFeeDirectBilling, !is.na(BillingAmount))
TierFeeDirectBilling <- mutate(TierFeeDirectBilling, 
                               FinalFee=ShippingFeeCharging-ifelse(is.na(ShippingFeeRebating),0,ShippingFeeRebating)-BillingAmount,
                               Notes=paste("Order#",Order.Numbers," - TN#",Tracking.Number,
                                           ifelse(FinalFee>0,
                                                  paste(" - Shipping Fee -  Charged under Tiered Fee: Total package value: S$",TotalPackageUnit),
                                                  ifelse(ShippingFeeRebating>0,
                                                         " - Shipping Fee Rebates - Refund due to customer change of mind / COD rejection",
                                                         " - Rebates for direct billing fee charged by 3PL"))))

if(nrow(TierFeeDirectBillingMissing) > 0) {
        write.csv(TierFeeDirectBillingMissing,paste(outputDir,"TierFeeDirectBillingMissing.csv", sep = ""),
                  row.names = FALSE)
}
if(nrow(TierFeeDirectBilling) > 0) {
        write.csv(TierFeeDirectBilling,paste(outputDir,"TierFeeDirectBilling.csv", sep = ""),
                  row.names = FALSE)
}