# Group by order
FixedPerOrderItem <- filter(validSoi, Rate.cards=="Fixed per order")
FixedPerOrder <- FixedPerOrderItem %>% group_by(Order.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
                                                Billing.Type,Rate.cards,Delivery.Company,Delivery.Company.Type) %>%
        summarize(Shipped.Date=min(Shipped.Date),
                  Cancelled.Date=max(Cancelled.Date),
                  Returned.Date=max(Returned.Date),
                  Failed.Delivery.Reason=paste(failed_delivery,"-"),
                  Returned.Reasons=paste(return_reason,"-"),
                  shipping_charged=sum(shipCharged),
                  Rebates_charge=sum(rebateReview),
                  TotalPackageUnit=sum(unit_price),
                  ItemCounts=n(),
                  ShippingFee=(5*1.07),
                  ShippingFeeCharging=ShippingFee*ifelse(shipping_charged>0,1,0),
                  RebatesItemCount=ifelse(Rebates_charge>=1,1,0) * 
                          sum(ifelse(!is.na(Cancelled.Date) | !is.na(Returned.Date),1,0)),
                  ShippingFeeRebating=ifelse(RebatesItemCount==ItemCounts,ShippingFee,0),
                  ShippingFeeRebating=ifelse(is.na(ShippingFeeRebating),0,ShippingFeeRebating))

# Non direct Billing Shipping Fee Charging
FixedPerOrderNonDirectBilling <- filter(FixedPerOrder, is.na(Billing.Type),
                                        ShippingFeeCharging>0)
FixedPerOrderNonDirectBilling <- mutate(FixedPerOrderNonDirectBilling,
                                        FinalFee=ShippingFeeCharging,
                                        Notes=ifelse(FinalFee>0,
                                                     paste('Shipping Fee - Order#',as.character(Order.Number),
                                                           '- Charged under Fixed fee per order'),
                                                     paste('Shipping Fee Rebates -',as.character(Order.Number),
                                                           '- Refund due to customer change of mind / COD rejection')))
FixedPerOrderNonDirectBilling <- filter(FixedPerOrderNonDirectBilling, FinalFee!=0)

write.csv(filter(FixedPerOrderNonDirectBilling,FinalFee>0),
          paste(outputDir,"FixedPerOrderNonDirectBilling_Charged.csv", sep = ""),
          row.names = FALSE)

# Non direct Billing Shipping Fee Rebate
FixedPerOrderNonDirectBillingRebate <- filter(FixedPerOrder, is.na(Billing.Type),
                                        ShippingFeeRebating>0)
FixedPerOrderNonDirectBillingRebate <- mutate(FixedPerOrderNonDirectBillingRebate,
                                        FinalFee=ShippingFeeRebating,
                                        Notes=ifelse(FinalFee<0,
                                                     paste('Shipping Fee - Order#',as.character(Order.Number),
                                                           '- Charged under Fixed fee per order'),
                                                     paste('Shipping Fee Rebates -',as.character(Order.Number),
                                                           '- Refund due to customer change of mind / COD rejection')))
FixedPerOrderNonDirectBillingRebate <- filter(FixedPerOrderNonDirectBillingRebate, FinalFee!=0)

write.csv(filter(FixedPerOrderNonDirectBillingRebate,FinalFee>0),
          paste(outputDir,"FixedPerOrderNonDirectBillingRebates.csv", sep = ""),
          row.names = FALSE)

# FD Returned Fee
FixedPerOrderItemFDReturned <- left_join(filter(FixedPerOrderItem, returnedCharge==1),
                                         SKU_Dimension_Final, by = c("sku"="SKU"))
FixedPerOrderItemFDReturned <- mutate(FixedPerOrderItemFDReturned,
                                      fd_returned_fee=ifelse(lazada.Parcel.ID==1,5.9,
                                                             ifelse(lazada.Parcel.ID==2,8.0,
                                                                    ifelse(lazada.Parcel.ID==3,10.7,
                                                                           ifelse(lazada.Parcel.ID==4,16.1,NA)))),
                                      FinalFee=fd_returned_fee)
FixedPerOrderItemFDReturned <- select(FixedPerOrderItemFDReturned, 
                                      -c(Rate_card_1,Rate_card_1_Begin,Rate_card_1_End,
                                         Rate_card_2,Rate_card_2_Begin,Rate_card_2_End))

write.csv(FixedPerOrderItemFDReturned,
          paste(outputDir,"FixedPerOrderItem_FDReturnCharge.csv", sep = ""),
          row.names = FALSE)

# Returned Rebate Fee
FixedPerOrderItemFDReturnedRebate <- left_join(filter(FixedPerOrderItem, returnedReview==1),
                                         SKU_Dimension_Final, by = c("sku"="SKU"))
FixedPerOrderItemFDReturnedRebate <- mutate(FixedPerOrderItemFDReturnedRebate,
                                      fd_returned_fee=ifelse(lazada.Parcel.ID==1,5.9,
                                                             ifelse(lazada.Parcel.ID==2,8.0,
                                                                    ifelse(lazada.Parcel.ID==3,10.7,
                                                                           ifelse(lazada.Parcel.ID==4,16.1,NA)))),
                                      FinalFee=fd_returned_fee)
FixedPerOrderItemFDReturnedRebate <- select(FixedPerOrderItemFDReturnedRebate, 
                                      -c(Rate_card_1,Rate_card_1_Begin,Rate_card_1_End,
                                         Rate_card_2,Rate_card_2_Begin,Rate_card_2_End))

write.csv(FixedPerOrderItemFDReturnedRebate,
          paste(outputDir,"FixedPerOrderItem_ReturnRebate.csv", sep = ""),
          row.names = FALSE)



# Direct Billing
FixedPerOrderDirectBilling <- filter(FixedPerOrder, !is.na(Billing.Type) &
                                             (ShippingFeeCharging>0 | ShippingFeeRebating>0))
TrackingBilling <- filter(rbind_list(select(IMBilling, Tracking.Number, BillingAmount),
                                     select(NVBilling, Tracking.Number, BillingAmount),
                                     select(TQBBilling, Tracking.Number, BillingAmount)),
                          !is.na(Tracking.Number))
TrackingBilling <- left_join(TrackingBilling,select(validSoi,Order.Number,Tracking.Number))

FixedPerOrderDirectBilling <- left_join(FixedPerOrderDirectBilling,TrackingBilling)
FixedPerOrderDirectBilling <- FixedPerOrderDirectBilling %>% 
        group_by(Order.Number,Payment.Method,Shipping.Type,Seller.ID,Seller.Name,
                 Billing.Type,Rate.cards,Delivery.Company,Delivery.Company.Type,
                 Shipped.Date,Cancelled.Date,Returned.Date,Failed.Delivery.Reason,
                 Returned.Reasons,Rebates_charge,Rebates_charge,ShippingFee,
                 ShippingFeeCharging,RebatesItemCount,ShippingFeeRebating) %>%
        summarize(Billing.Amount=sum(BillingAmount))

FixedPerOrderDirectBilling <- mutate(FixedPerOrderDirectBilling,
                                     FinalFee=ShippingFeeCharging - Billing.Amount - 
                                             ifelse(is.na(ShippingFeeRebating),0,ShippingFeeRebating),
                                     Notes=ifelse(FinalFee>0,
                                                  paste("Shipping Fee - Order#",as.character(Order.Number),
                                                        "- Charged under Fixed fee per order"),
                                                  paste("Shipping Fee Rebates -",as.character(Order.Number),
                                                        ifelse(!is.na(FixedPerOrderDirectBilling$ShippingFeeRebating) & FixedPerOrderDirectBilling$ShippingFeeRebating>0,
                                                               "- Refund due to customer change of mind / COD rejection",
                                                               "- Rebates for direct billing fee charged by 3PL"))))

FixedPerOrderDirectBillingMissing <- filter(FixedPerOrderDirectBilling, is.na(Billing.Amount))
FixedPerOrderDirectBilling <- filter(FixedPerOrderDirectBilling, !is.na(Billing.Amount))

write.csv(FixedPerOrderDirectBillingMissing,paste(outputDir,"FixedPerOrderDirectBillingMissing.csv", sep = ""),
          row.names = FALSE)
write.csv(FixedPerOrderDirectBilling,paste(outputDir,"FixedPerOrderDirectBilling.csv", sep = ""),
          row.names = FALSE)
