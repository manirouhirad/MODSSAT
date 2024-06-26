#' This function maximizes the expected profit.
#' @param jj                 is the loop number. Defaults to 1.
#' @return                   returns the optimized values
#' @export


FN_optim2 = function(jj = 1) {
  
  foo_irr_3 = well_capacity_data[Well_ID_grp == jj]
  foo_irr_3[ifreq > KS_DSSAT[, max(IFREQ)], ifreq := KS_DSSAT[, max(IFREQ)]]
  foo_irr_3_0  = foo_irr_3[ifreq == 0]
  foo_irr_3_N0 = foo_irr_3[ifreq != 0]
  foo_irr_3_N0[, ifreq := as.numeric(as.character(ifreq))]
  
  tryCatch({
    foo_irr_3_0 = merge(foo_irr_3_0, KS_DSSAT,
                        by.x = c("Soil_Type", "weather_station", "CR", "ifreq"),
                        by.y = c("SOIL_ID", "WSTA", "CR", "IFREQ"), allow.cartesian = T)
  }, error=function(e){})
  
  foo_irr_3_N0[, diff := (KS_DSSAT[, max(IFREQ)] - ifreq) * (1/IFREQ_interpolate)]
  foo_irr_3_N0[, id := 1:.N]
  
  tryCatch({
    foo_irr_3_N0 = foo_irr_3_N0[rep(1:.N, (diff+1))]
    foo_irr_3_N0[, foo := 1:.N, by=c("id")]
    foo_irr_3_N0[, foo := foo - 1]
    foo_irr_3_N0[, ifreq := ifreq + foo * IFREQ_interpolate]
    foo_irr_3_N0[, ifreq := as.numeric(as.character(ifreq))]
  }, error=function(e){})
  
  foo_irr_3_N0 = merge(foo_irr_3_N0, KS_DSSAT,
                       by.x = c("Soil_Type", "weather_station", "CR", "ifreq"),
                       by.y = c("SOIL_ID", "WSTA", "CR", "IFREQ"), allow.cartesian = T)
  foo_irr_3_N0[, c("id", "diff", "foo") := NULL]
  
  
  foo_irr_3 = rbind(foo_irr_3_N0, foo_irr_3_0)
  # foo_irr_3[, ifreq_denom := ifreq/.2]
  # foo_irr_3 = foo_irr_3[ifreq_denom %in% 0:200]
  
  foo_irr_3[ifreq == KS_DSSAT[, max(IFREQ)], profit := profit/10]
  
  rm(foo_irr_3_N0, foo_irr_3_0)
  
  foo_irr_3[, ifreq_cap := min(ifreq), by=c("Well_capacity", "SDAT", "tot_acres", "CR", "PAW")]
  foo_irr_3[, ifreq_cap := ifreq_cap + 2]
  foo_irr_3 = foo_irr_3[ifreq <= ifreq_cap]
  # foo_irr_3[, id := .GRP, by=c("Soil_Type", "weather_station", "CR", "Well_ID", "Well_capacity", "tot_acres", "quarter", "PAW", "SDAT")]
  foo_irr_3 = unique(foo_irr_3, by=colnames(foo_irr_3))
  setkey(foo_irr_3, Well_capacity, tot_acres, CR, PAW, SDAT, ifreq, quarter)
  foo_irr_3[, id := .GRP, by=c("Well_capacity", "tot_acres", "ifreq")]
  
  #----
  foo_dt1_000 = foo_irr_3[quarter == 1 & tot_acres == 0, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit)]
  foo_dt2_000 = foo_irr_3[quarter == 2 & tot_acres == 0, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit)]
  foo_dt3_000 = foo_irr_3[quarter == 3 & tot_acres == 0, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit)]
  foo_dt4_000 = foo_irr_3[quarter == 4 & tot_acres == 0, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  
  
  foo_dt3_000 = merge(foo_dt3_000, foo_dt4_000, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt2_000 = merge(foo_dt2_000, foo_dt3_000, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt1_000 = merge(foo_dt1_000, foo_dt2_000, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  rm(foo_dt2_000, foo_dt3_000, foo_dt4_000)
  foo_dt1_000[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  foo_dt1_000[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
                                              0))]
  foo_dt1_000[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # foo_dt1_000[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  foo_dt1_000[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # foo_dt1_000[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  
  foo_dt1_000[, row := .GRP, by=c("CR_1", "PAW_1",
                                  "CR_2", "PAW_2",
                                  "CR_3", "PAW_3",
                                  "CR_4", "PAW_4")]
  
  foo = foo_dt1_000[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  foo =          unique(foo,                                     by = c("row"))
  foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  setkey(foo, row)
  setkey(foo_dt1_000, row)
  foo_dt1_000 = foo_dt1_000[foo]
  
  foo_dt1_000 = rbind(foo_dt1_000[SDAT == min(foo_dt1_000$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_000[SDAT == min(foo_dt1_000$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_000[SDAT == min(foo_dt1_000$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_000[SDAT == min(foo_dt1_000$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  )
  
  #----------
  
  foo_dt1_325 = foo_irr_3[quarter == 1 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit)]
  foo_dt2_325 = foo_irr_3[quarter == 2 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit)]
  foo_dt3_325 = foo_irr_3[quarter == 3 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit)]
  foo_dt4_325 = foo_irr_3[quarter == 4 & tot_acres == 32.5, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  
  
  foo_dt3_325 = merge(foo_dt3_325, foo_dt4_325, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt2_325 = merge(foo_dt2_325, foo_dt3_325, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt1_325 = merge(foo_dt1_325, foo_dt2_325, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  rm(foo_dt2_325, foo_dt3_325, foo_dt4_325)
  foo_dt1_325[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  foo_dt1_325[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
                                              0))]
  foo_dt1_325[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # foo_dt1_325[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  foo_dt1_325[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # foo_dt1_325[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  
  foo_dt1_325[, row := .GRP, by=c("CR_1", "PAW_1",
                                  "CR_2", "PAW_2",
                                  "CR_3", "PAW_3",
                                  "CR_4", "PAW_4", "ifreq_1")]
  
  foo = foo_dt1_325[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  foo =          unique(foo,                                     by = c("row"))
  foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  setkey(foo, row)
  setkey(foo_dt1_325, row)
  foo_dt1_325 = foo_dt1_325[foo]
  
  foo_dt1_325 = rbind(foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_325[SDAT == min(foo_dt1_325$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  )
  
  #----------
  
  foo_dt1_650 = foo_irr_3[quarter == 1 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit, id)]
  foo_dt2_650 = foo_irr_3[quarter == 2 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit, id)]
  foo_dt3_650 = foo_irr_3[quarter == 3 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit)]
  foo_dt4_650 = foo_irr_3[quarter == 4 & tot_acres == 65, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  
  
  foo_dt3_650 = merge(foo_dt3_650, foo_dt4_650, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt2_650 = merge(foo_dt2_650, foo_dt3_650, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt1_650 = merge(foo_dt1_650, foo_dt2_650, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
                      allow.cartesian = T)
  rm(foo_dt2_650, foo_dt3_650, foo_dt4_650)
  foo_dt1_650[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  foo_dt1_650[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
                                              0))]
  foo_dt1_650[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # foo_dt1_650[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  foo_dt1_650[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # foo_dt1_650[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  
  foo_dt1_650[, row := .GRP, by=c("CR_1", "PAW_1",
                                  "CR_2", "PAW_2",
                                  "CR_3", "PAW_3",
                                  "CR_4", "PAW_4", "id")]
  
  foo = foo_dt1_650[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  foo =          unique(foo,                                     by = c("row"))
  foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  setkey(foo, row)
  setkey(foo_dt1_650, row)
  foo_dt1_650 = foo_dt1_650[foo]
  
  foo_dt1_650 = rbind(foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_650[SDAT == min(foo_dt1_650$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  )
  
  #----------
  
  foo_dt1_975 = foo_irr_3[quarter == 1 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit, id)]
  foo_dt2_975 = foo_irr_3[quarter == 2 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit, id)]
  foo_dt3_975 = foo_irr_3[quarter == 3 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit, id)]
  foo_dt4_975 = foo_irr_3[quarter == 4 & tot_acres == 97.5, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit)]
  
  
  foo_dt3_975 = merge(foo_dt3_975, foo_dt4_975, by = c("Well_capacity", "SDAT", "tot_acres"),
                      allow.cartesian = T)
  foo_dt2_975 = merge(foo_dt2_975, foo_dt3_975, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
                      allow.cartesian = T)
  foo_dt1_975 = merge(foo_dt1_975, foo_dt2_975, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
                      allow.cartesian = T)
  rm(foo_dt2_975, foo_dt3_975, foo_dt4_975)
  foo_dt1_975[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  foo_dt1_975[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
                                              0))]
  foo_dt1_975[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # foo_dt1_975[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  foo_dt1_975[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # foo_dt1_975[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  
  foo_dt1_975[, row := .GRP, by=c("CR_1", "PAW_1",
                                  "CR_2", "PAW_2",
                                  "CR_3", "PAW_3",
                                  "CR_4", "PAW_4", "id")]
  
  foo = foo_dt1_975[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  foo =          unique(foo,                                     by = c("row"))
  foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  setkey(foo, row)
  setkey(foo_dt1_975, row)
  foo_dt1_975 = foo_dt1_975[foo]
  
  foo_dt1_975 = rbind(foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_975[SDAT == min(foo_dt1_975$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  )
  
  #----------
  
  foo_dt1_130 = foo_irr_3[quarter == 1 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_1 = ifreq, CR_1 = CR, PAW_1 = PAW, irrigation_1 = irrigation, profit_1 = profit, id)]
  foo_dt2_130 = foo_irr_3[quarter == 2 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_2 = ifreq, CR_2 = CR, PAW_2 = PAW, irrigation_2 = irrigation, profit_2 = profit, id)]
  foo_dt3_130 = foo_irr_3[quarter == 3 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_3 = ifreq, CR_3 = CR, PAW_3 = PAW, irrigation_3 = irrigation, profit_3 = profit, id)]
  foo_dt4_130 = foo_irr_3[quarter == 4 & tot_acres == 130, .(Well_capacity, SDAT, tot_acres, ifreq_4 = ifreq, CR_4 = CR, PAW_4 = PAW, irrigation_4 = irrigation, profit_4 = profit, id)]
  
  foo_dt3_130 = merge(foo_dt3_130, foo_dt4_130, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
                      allow.cartesian = T)
  foo_dt2_130 = merge(foo_dt2_130, foo_dt3_130, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
                      allow.cartesian = T)
  foo_dt1_130 = merge(foo_dt1_130, foo_dt2_130, by = c("Well_capacity", "SDAT", "tot_acres", "id"),
                      allow.cartesian = T)
  rm(foo_dt2_130, foo_dt3_130, foo_dt4_130)
  foo_dt1_130[, `:=`(irrigation_sum, irrigation_1 + irrigation_2 + irrigation_3 + irrigation_4)]
  foo_dt1_130[, `:=`(irrigation_below, ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - irrigation_sum,
                                              0))]
  foo_dt1_130[, `:=`(profit_sum, profit_1 + profit_2 + profit_3 + profit_4)]
  # foo_dt1_130[, `:=`(subsidy_payment, irrigation_below * subsidy_amount)]
  foo_dt1_130[, `:=`(profit_sum_sub, profit_sum + irrigation_below * subsidy_amount)]
  # foo_dt1_130[, `:=`(profit_sum_sub, profit_sum - irrigation_sum * tax_amount)]
  
  foo_dt1_130[, row := .GRP, by=c("CR_1", "PAW_1",
                                  "CR_2", "PAW_2",
                                  "CR_3", "PAW_3",
                                  "CR_4", "PAW_4", "id")]
  
  foo = foo_dt1_130[, .(row, profit_sum, profit_sum_sub, irrigation_sum)]
  foo[, `:=`(mean_profit_combination,     mean(profit_sum)),     by = c("row")]
  foo[, `:=`(mean_profit_combination_sub, mean(profit_sum_sub)), by = c("row")]
  foo[, `:=`(mean_irrigation_combination, mean(irrigation_sum)), by = c("row")]
  foo =          unique(foo,                                     by = c("row"))
  foo[, `:=`(max_p, max(mean_profit_combination_sub))]
  foo = foo[mean_profit_combination_sub == max_p,.(row, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  setkey(foo, row)
  setkey(foo_dt1_130, row)
  foo_dt1_130 = foo_dt1_130[foo]
  
  foo_dt1_130 = rbind(foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 1, ifreq = ifreq_1, CR = CR_1, PAW = PAW_1, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 2, ifreq = ifreq_2, CR = CR_2, PAW = PAW_2, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 3, ifreq = ifreq_3, CR = CR_3, PAW = PAW_3, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
                      , foo_dt1_130[SDAT == min(foo_dt1_130$SDAT),.(Well_capacity, tot_acres, quarter = 4, ifreq = ifreq_4, CR = CR_4, PAW = PAW_4, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  )
  
  #----------
  
  foo_dt1 = rbind(foo_dt1_000, foo_dt1_325, foo_dt1_650, foo_dt1_975, foo_dt1_130)
  rm(foo_dt1_000, foo_dt1_325, foo_dt1_650, foo_dt1_975, foo_dt1_130)
  foo_dt1[, max_mean_p := max(mean_profit_combination_sub, na.rm = T)]
  foo_dt1 = foo_dt1[max_mean_p == mean_profit_combination_sub]
  return(foo_dt1)
  
}
