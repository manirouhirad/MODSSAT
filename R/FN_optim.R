#' This function returns maximum expected profits for each quarter circle.
#' @param jj                             is the loop that goes through the wells in the study area
#' @return                               returns the output table.
#' @export
FN_optim = function(jj){
  library(data.table)
  foo_dt1 = foo_irr[Well_ID_grp == jj & quarter == 1,
                    .(Well_ID, Well_capacity, SOIL_ID, tot_acres,
                      SDAT, quarter, CR, PAW, irrigation, profit)]
  foo_dt2 = foo_irr[Well_ID_grp == jj & quarter == 2,
                    .(Well_ID, Well_capacity, SOIL_ID, tot_acres,
                      SDAT, quarter, CR, PAW, irrigation, profit)]
  foo_dt3 = foo_irr[Well_ID_grp == jj & quarter == 3,
                    .(Well_ID, Well_capacity, SOIL_ID, tot_acres,
                      SDAT, quarter, CR, PAW, irrigation, profit)]
  foo_dt4 = foo_irr[Well_ID_grp == jj & quarter == 4,
                    .(Well_ID, Well_capacity, SOIL_ID, tot_acres,
                      SDAT, quarter, CR, PAW, irrigation, profit)]
  foo_dt1[, `:=`(row_1, 1:.N)]
  foo_dt2[, `:=`(row_2, 1:.N)]
  foo_dt3[, `:=`(row_3, 1:.N)]
  foo_dt4[, `:=`(row_4, 1:.N)]
  foo_dt3 = merge(foo_dt3, foo_dt4, by = c("Well_ID",
                                           "tot_acres", "SOIL_ID", "SDAT"), allow.cartesian = T)
  foo_dt3[, `:=`(row_34, 1:.N)]
  foo_dt2 = merge(foo_dt2, foo_dt3, by = c("Well_ID",
                                           "tot_acres", "SOIL_ID", "SDAT"), allow.cartesian = T)
  foo_dt2[, `:=`(row_23, 1:.N)]
  setnames(foo_dt2, old = c("Well_capacity.x", "quarter.x",
                            "CR.x", "PAW.x", "irrigation.x", "profit.x"),
           new = c("Well_capacity.xx", "quarter.xx", "CR.xx",
                   "PAW.xx", "irrigation.xx", "profit.xx"))
  setnames(foo_dt2, old = c("Well_capacity.y", "quarter.y",
                            "CR.y", "PAW.y", "irrigation.y", "profit.y"),
           new = c("Well_capacity.yy", "quarter.yy", "CR.yy",
                   "PAW.yy", "irrigation.yy", "profit.yy"))
  foo_dt1 = merge(foo_dt1, foo_dt2, by = c("Well_ID",
                                           "tot_acres", "SOIL_ID", "SDAT"), allow.cartesian = T)
  foo_dt1[, `:=`(row_12, 1:.N)]
  foo_dt4 = foo_dt1[, .(Well_ID, Well_capacity = Well_capacity.yy,
                        SOIL_ID, tot_acres, SDAT, quarter = quarter.yy, CR = CR.yy,
                        PAW = PAW.yy, irrigation = irrigation.yy,
                        profit = profit.yy, row = row_4,
                        row_34, row_23, row_12)]
  foo_dt3 = foo_dt1[, .(Well_ID, Well_capacity = Well_capacity.xx,
                        SOIL_ID, tot_acres, SDAT, quarter = quarter.xx, CR = CR.xx,
                        PAW = PAW.xx, irrigation = irrigation.xx,
                        profit = profit.xx, row = row_3,
                        row_34, row_23, row_12)]
  foo_dt2 = foo_dt1[, .(Well_ID, Well_capacity = Well_capacity.y,
                        SOIL_ID, tot_acres, SDAT, quarter = quarter.y, CR = CR.y,
                        PAW = PAW.y, irrigation = irrigation.y, profit = profit.y,
                        row = row_2, row_34, row_23, row_12)]
  foo_dt1 = foo_dt1[, .(Well_ID, Well_capacity = Well_capacity.x,
                        SOIL_ID, tot_acres, SDAT, quarter = quarter.x, CR = CR.x,
                        PAW = PAW.x, irrigation = irrigation.x, profit = profit.x,
                        row = row_1, row_34, row_23, row_12)]
  foo_dt1 = rbind(foo_dt1, foo_dt2, foo_dt3, foo_dt4)
  setkey(foo_dt1, row_12, row_23, row_34, row, tot_acres,
         quarter)
  rm(foo_dt2, foo_dt3, foo_dt4)
  foo_dt1[, `:=`(irrigation_sum, sum(irrigation)),
          by = "row_12"]
  foo_dt1[, `:=`(irrigation_below, ifelse(irrigation_sum <
                                            subsidy_threshold, subsidy_threshold - irrigation,
                                          0))]
  foo_dt1[, `:=`(profit_sum, sum(profit)), by = "row_12"]
  foo_dt1[, `:=`(profit_sum_sub, sum(profit) +
                   irrigation_below * subsidy_amount), by = "row_12"] ### do we need to save it at this point? I don't think so. Because we already know what happens when we have CR-PAW combinations in irr_foo2
  foo_dt1[, CR_PAW := paste0(CR, "-", PAW, "-")]
  foo_dt1[, allAts := Reduce(paste0, sort(CR_PAW)), by="row_12"][, group:=as.integer(factor(allAts))]
  foo_dt1[, c("allAts", "CR_PAW") := NULL]
  foo_dt1[, mean_profit_combination     := mean(profit_sum), by=c("group")]
  foo_dt1[, mean_profit_combination_sub := mean(profit_sum_sub), by=c("group")]
  foo_dt1[, mean_irrigation_combination := mean(irrigation_sum), by=c("group")]
  foo_dt1 = unique(foo_dt1, by=c("group", "quarter"))
  foo_dt1[, max_p := max(mean_profit_combination_sub)]
  foo_dt1 = foo_dt1[mean_profit_combination_sub == max_p]
  foo_dt1 = foo_dt1[, .(Well_ID, Well_capacity, SOIL_ID,
                        tot_acres, quarter, CR, PAW, mean_irrigation_combination, mean_profit_combination, mean_profit_combination_sub)]
  return(foo_dt1)
}
