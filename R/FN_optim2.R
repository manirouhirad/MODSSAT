#' This function maximizes the expected profit.
#' @param jj                 is the loop number. Defaults to 1.
#' @return                   returns the optimized values
#' @export


FN_optim2 <- function(jj = 1) {
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
  foo[rep(1:.N, (diff+1))]
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
  
  return(foo_irr_3)
  
}
