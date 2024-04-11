#' This function maximizes the expected profit.
#' @param jj                 is the loop number. Defaults to 1.
#' @return                   returns the optimized values
#' @export


FN_optim2 = function(jj = 1) {
  foo_irr_3 <- well_capacity_data[well_capacity_data[[9]] == jj, ]
  max_IFREQ <- max(KS_DSSAT[["IFREQ"]], na.rm = TRUE)
  foo_irr_3[foo_irr_3$ifreq > max_IFREQ, "ifreq"] <- max_IFREQ
  foo_irr_3_0  = foo_irr_3[foo_irr_3$ifreq == 0,]
  foo_irr_3_N0 = foo_irr_3[foo_irr_3$ifreq != 0,]
  foo_irr_3_N0$ifreq = as.numeric(as.character(foo_irr_3_N0$ifreq))
  
  tryCatch({
    foo_irr_3_0 = merge(foo_irr_3_0, KS_DSSAT,
                        by.x = c("Soil_Type", "weather_station", "CR", "ifreq"),
                        by.y = c("SOIL_ID", "WSTA", "CR", "IFREQ"), allow.cartesian = T)
  }, error=function(e){})
  
  foo_irr_3_N0$diff <- (max_IFREQ - foo_irr_3_N0$ifreq) * (1/IFREQ_interpolate)
  foo_irr_3_N0$id <- seq_len(nrow(foo_irr_3_N0))
  
  foo = data.table(foo_irr_3_N0)
  selected_rows <- foo[rep(seq_len(nrow(foo)), foo$diff + 1), ]

  tryCatch({
    foo_irr_3_N0 = foo_irr_3_N0[rep(seq_len(nrow(foo_irr_3_N0)), foo_irr_3_N0$diff+1),]
    foo_irr_3_N0$foo <- ave(rep(1, nrow(foo)), foo$id, FUN = seq_along)
    
    # library(dplyr)
    # foo_irr_3_N0 <- foo_irr_3_N0 %>% 
    #   group_by(id) %>% 
    #   mutate(foo = row_number())
    
    foo_irr_3_N0$foo   = foo_irr_3_N0$foo - 1
    foo_irr_3_N0$ifreq = foo_irr_3_N0$ifreq + (foo_irr_3_N0$foo * IFREQ_interpolate)
    foo_irr_3_N0$ifreq = as.numeric(as.character(foo_irr_3_N0$ifreq))
  }, error=function(e){})
  
  
  foo_irr_3_N0 = merge(foo_irr_3_N0, KS_DSSAT,
                       by.x = c("Soil_Type", "weather_station", "CR", "ifreq"),
                       by.y = c("SOIL_ID", "WSTA", "CR", "IFREQ"), allow.cartesian = T)
  foo_irr_3_N0[, c("id", "diff", "foo")] = NULL
  foo_irr_3 = rbind(foo_irr_3_N0, foo_irr_3_0)
  foo_irr_3$profit[foo_irr_3$ifreq == max_IFREQ] <- foo_irr_3$profit[foo_irr_3$ifreq == max_IFREQ] / 10
  rm(foo_irr_3_N0, foo_irr_3_0)
  
  library(dplyr)
  
  # Calculate 'col1' by group using dplyr
  foo_irr_3 <- foo_irr_3 %>%
    group_by(Well_capacity, SDAT, tot_acres, CR, PAW) %>%
    mutate(ifreq_cap = min(ifreq)) %>%
    ungroup()
  
  foo_irr_3$ifreq_cap <- foo_irr_3$ifreq_cap + 2
  foo_irr_3 = foo_irr_3[foo_irr_3$ifreq <= foo_irr_3$ifreq_cap,]
  foo_irr_3 = unique(foo_irr_3, by=colnames(foo_irr_3))
  foo_irr_3 <- foo_irr_3[order(foo_irr_3$Well_capacity, foo_irr_3$tot_acres, foo_irr_3$CR, foo_irr_3$PAW, foo_irr_3$SDAT, foo_irr_3$ifreq, foo_irr_3$quarter), ]

  foo_irr_3 <- foo_irr_3 %>%
    group_by(Well_capacity, tot_acres, ifreq) %>%
    mutate(id = group_indices())
  
  # #----
  foo_dt1_000 <- subset(foo_irr_3, quarter == 1 & tot_acres == 0,  select = c(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq,  CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation,  profit_1 = profit))
  foo_dt2_000 <- subset(foo_irr_3, quarter == 2 & tot_acres == 0,  select = c(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq,  CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation,  profit_2 = profit))
  foo_dt3_000 <- subset(foo_irr_3, quarter == 3 & tot_acres == 0,  select = c(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq,  CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation,  profit_3 = profit))
  foo_dt4_000 <- subset(foo_irr_3, quarter == 4 & tot_acres == 0,  select = c(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq,  CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation,  profit_4 = profit))
  
  foo_dt3_000 = merge(foo_dt3_000, foo_dt4_000, by = c("Well_capacity", "SDAT", "tot_acres"), allow.cartesian = T)
  foo_dt2_000 = merge(foo_dt2_000, foo_dt3_000, by = c("Well_capacity", "SDAT", "tot_acres"), allow.cartesian = T)
  foo_dt1_000 = merge(foo_dt1_000, foo_dt2_000, by = c("Well_capacity", "SDAT", "tot_acres"), allow.cartesian = T)
  rm(foo_dt2_000, foo_dt3_000, foo_dt4_000)
  
  foo_dt1_000$irrigation_sum   <- foo_dt1_000$irrigation_1 + foo_dt1_000$irrigation_2 + foo_dt1_000$irrigation_3 + foo_dt1_000$irrigation_4
  
  foo_dt1_000 <- foo_dt1_000 %>%
    mutate(irrigation_below = ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum, 0))
  
  # foo_dt1_000$subsidy_threshold <- subsidy_threshold
  # foo_dt1_000$irrigation_below <- ifelse(foo_dt1_000$irrigation_sum < foo_dt1_000$subsidy_threshold, foo_dt1_000$subsidy_threshold - foo_dt1_000$irrigation_sum, 0)
  
  # foo_dt1_000$profit_sum <- foo_dt1_000$profit_1 + foo_dt1_000$profit_2 + foo_dt1_000$profit_3 + foo_dt1_000$profit_4
  # foo_dt1_000$subsidy_payment <- foo_dt1_000$irrigation_below * subsidy_amount
  # foo_dt1_000$profit_sum_sub <- foo_dt1_000$profit_sum + (foo_dt1_000$irrigation_below * subsidy_amount)

  # foo_dt1_000$row <- ave(seq_len(nrow(foo_dt1_000)), 
  #                        foo_dt1_000$CR_1, foo_dt1_000$PAW_1, 
  #                        foo_dt1_000$CR_2, foo_dt1_000$PAW_2, 
  #                        foo_dt1_000$CR_3, foo_dt1_000$PAW_3, 
  #                        foo_dt1_000$CR_4, foo_dt1_000$PAW_4, 
  #                        FUN = function(x) seq_along(x))  
  # 
  # 
  # foo <- foo_dt1_000[, c("row", "profit_sum", "profit_sum_sub", "irrigation_sum")]
  # foo$mean_profit_combination     <- ave(foo$profit_sum,     foo$row, FUN = mean)
  # foo$mean_profit_combination_sub <- ave(foo$profit_sum_sub, foo$row, FUN = mean)
  # foo$mean_irrigation_combination <- ave(foo$irrigation_sum, foo$row, FUN = mean)
  # foo <- foo[!duplicated(foo$row), ]
  # max_p <- max(foo$mean_profit_combination_sub)
  # foo <- foo[foo$mean_profit_combination_sub == max_p, c("row", "mean_irrigation_combination", "mean_profit_combination", "mean_profit_combination_sub")]
  # 
  # foo_dt1_000 <- merge(foo_dt1_000, foo, by = "row")
  # 
  # # Subset the data frame for each quarter
  # quarter_1 <- foo_dt1_000[foo_dt1_000$SDAT == min(foo_dt1_000$SDAT), c("Well_capacity", "tot_acres", "quarter" = 1, "ifreq" = ifreq_1, "CR" = CR_1, "PAW" = PAW_1, "mean_irrigation_combination", "mean_profit_combination", "mean_profit_combination_sub")]
  # quarter_2 <- foo_dt1_000[foo_dt1_000$SDAT == min(foo_dt1_000$SDAT), c("Well_capacity", "tot_acres", "quarter" = 2, "ifreq" = ifreq_2, "CR" = CR_2, "PAW" = PAW_2, "mean_irrigation_combination", "mean_profit_combination", "mean_profit_combination_sub")]
  # quarter_3 <- foo_dt1_000[foo_dt1_000$SDAT == min(foo_dt1_000$SDAT), c("Well_capacity", "tot_acres", "quarter" = 3, "ifreq" = ifreq_3, "CR" = CR_3, "PAW" = PAW_3, "mean_irrigation_combination", "mean_profit_combination", "mean_profit_combination_sub")]
  # quarter_4 <- foo_dt1_000[foo_dt1_000$SDAT == min(foo_dt1_000$SDAT), c("Well_capacity", "tot_acres", "quarter" = 4, "ifreq" = ifreq_4, "CR" = CR_4, "PAW" = PAW_4, "mean_irrigation_combination", "mean_profit_combination", "mean_profit_combination_sub")]
  # 
  # # Combine the subsets
  # foo_dt1_000 <- rbind(quarter_1, quarter_2, quarter_3, quarter_4)
  return(foo_dt1_000)
  
  # #----------
  # 
  # foo_dt1_325 = foo_irr_3[quarter == 1 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit)]
  # foo_dt2_325 = foo_irr_3[quarter == 2 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit)]
  # foo_dt3_325 = foo_irr_3[quarter == 3 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit)]
  # foo_dt4_325 = foo_irr_3[quarter == 4 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  # 
  # 
  # foo_dt3_325 = merge(foo_dt3_325, foo_dt4_325, by = c("Well_capacity", "SDAT", "tot_acres"),
  #                     allow.cartesian = T)
  # foo_dt2_325 = merge(foo_dt2_325, foo_dt3_325, by = c("Well_capacity", "SDAT", "tot_acres"),
  #                     allow.cartesian = T)
  # foo_dt1_325 = merge(foo_dt1_325, foo_dt2_325, by = c("Well_capacity", "SDAT", "tot_acres"),
  #                     allow.cartesian = T)
  # rm(foo_dt2_325, foo_dt3_325, foo_dt4_325)
  # foo_dt1_325[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  # foo_dt1_325[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
  #                                             0))]
  # foo_dt1_325[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # # foo_dt1_325[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  # foo_dt1_325[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # # foo_dt1_325[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  # 
  # foo_dt1_325[, row := .GRP, by=c("CR_1", "PAW_1",
  #                                 "CR_2", "PAW_2",
  #                                 "CR_3", "PAW_3",
  #                                 "CR_4", "PAW_4", "ifreq_1")]
  # 
  # foo = foo_dt1_325[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  # foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  # foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  # foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  # foo =          unique(foo,                                     by = c("row"))
  # foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  # foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # setkey(foo, row)
  # setkey(foo_dt1_325, row)
  # foo_dt1_325 = foo_dt1_325[foo]
  # 
  # foo_dt1_325 = rbind(foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # )
  # 
  # #----------
  # 
  # foo_dt1_650 = foo_irr_3[quarter == 1 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit, id)]
  # foo_dt2_650 = foo_irr_3[quarter == 2 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit, id)]
  # foo_dt3_650 = foo_irr_3[quarter == 3 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit)]
  # foo_dt4_650 = foo_irr_3[quarter == 4 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  # 
  # 
  # foo_dt3_650 = merge(foo_dt3_650, foo_dt4_650, by = c("Well_capacity", "SDAT", "tot_acres"),
  #                     allow.cartesian = T)
  # foo_dt2_650 = merge(foo_dt2_650, foo_dt3_650, by = c("Well_capacity", "SDAT", "tot_acres"),
  #                     allow.cartesian = T)
  # foo_dt1_650 = merge(foo_dt1_650, foo_dt2_650, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
  #                     allow.cartesian = T)
  # rm(foo_dt2_650, foo_dt3_650, foo_dt4_650)
  # foo_dt1_650[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  # foo_dt1_650[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
  #                                             0))]
  # foo_dt1_650[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # # foo_dt1_650[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  # foo_dt1_650[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # # foo_dt1_650[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  # 
  # foo_dt1_650[, row := .GRP, by=c("CR_1", "PAW_1",
  #                                 "CR_2", "PAW_2",
  #                                 "CR_3", "PAW_3",
  #                                 "CR_4", "PAW_4", "id")]
  # 
  # foo = foo_dt1_650[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  # foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  # foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  # foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  # foo =          unique(foo,                                     by = c("row"))
  # foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  # foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # setkey(foo, row)
  # setkey(foo_dt1_650, row)
  # foo_dt1_650 = foo_dt1_650[foo]
  # 
  # foo_dt1_650 = rbind(foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # )
  # 
  # #----------
  # 
  # foo_dt1_975 = foo_irr_3[quarter == 1 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit, id)]
  # foo_dt2_975 = foo_irr_3[quarter == 2 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit, id)]
  # foo_dt3_975 = foo_irr_3[quarter == 3 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit, id)]
  # foo_dt4_975 = foo_irr_3[quarter == 4 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  # 
  # 
  # foo_dt3_975 = merge(foo_dt3_975, foo_dt4_975, by = c("Well_capacity", "SDAT", "tot_acres"),
  #                     allow.cartesian = T)
  # foo_dt2_975 = merge(foo_dt2_975, foo_dt3_975, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
  #                     allow.cartesian = T)
  # foo_dt1_975 = merge(foo_dt1_975, foo_dt2_975, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
  #                     allow.cartesian = T)
  # rm(foo_dt2_975, foo_dt3_975, foo_dt4_975)
  # foo_dt1_975[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  # foo_dt1_975[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
  #                                             0))]
  # foo_dt1_975[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # # foo_dt1_975[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  # foo_dt1_975[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # # foo_dt1_975[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  # 
  # foo_dt1_975[, row := .GRP, by=c("CR_1", "PAW_1",
  #                                 "CR_2", "PAW_2",
  #                                 "CR_3", "PAW_3",
  #                                 "CR_4", "PAW_4", "id")]
  # 
  # foo = foo_dt1_975[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  # foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  # foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  # foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  # foo =          unique(foo,                                     by = c("row"))
  # foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  # foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # setkey(foo, row)
  # setkey(foo_dt1_975, row)
  # foo_dt1_975 = foo_dt1_975[foo]
  # 
  # foo_dt1_975 = rbind(foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # )
  # 
  # #----------
  # 
  # foo_dt1_130 = foo_irr_3[quarter == 1 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit, id)]
  # foo_dt2_130 = foo_irr_3[quarter == 2 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit, id)]
  # foo_dt3_130 = foo_irr_3[quarter == 3 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit, id)]
  # foo_dt4_130 = foo_irr_3[quarter == 4 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit, id)]
  # 
  # foo_dt3_130 = merge(foo_dt3_130, foo_dt4_130, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
  #                     allow.cartesian = T)
  # foo_dt2_130 = merge(foo_dt2_130, foo_dt3_130, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
  #                     allow.cartesian = T)
  # foo_dt1_130 = merge(foo_dt1_130, foo_dt2_130, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
  #                     allow.cartesian = T)
  # rm(foo_dt2_130, foo_dt3_130, foo_dt4_130)
  # foo_dt1_130[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  # foo_dt1_130[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
  #                                             0))]
  # foo_dt1_130[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # # foo_dt1_130[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  # foo_dt1_130[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # # foo_dt1_130[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  # 
  # foo_dt1_130[, row := .GRP, by=c("CR_1", "PAW_1",
  #                                 "CR_2", "PAW_2",
  #                                 "CR_3", "PAW_3",
  #                                 "CR_4", "PAW_4", "id")]
  # 
  # foo = foo_dt1_130[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  # foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  # foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  # foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  # foo =          unique(foo,                                     by = c("row"))
  # foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  # foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # setkey(foo, row)
  # setkey(foo_dt1_130, row)
  # foo_dt1_130 = foo_dt1_130[foo]
  # 
  # foo_dt1_130 = rbind(foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  #                     , foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  # )
  # 
  # #----------
  # 
  # foo_dt1 = rbind(foo_dt1_000, foo_dt1_325, foo_dt1_650, foo_dt1_975, foo_dt1_130)
  # rm(foo_dt1_000, foo_dt1_325, foo_dt1_650, foo_dt1_975, foo_dt1_130)
  # foo_dt1[, max_mean_p := max(mean_profit_combination_sub, na.rm = T)]
  # foo_dt1 = foo_dt1[max_mean_p == mean_profit_combination_sub]
  # return(foo_dt1)
  
}
