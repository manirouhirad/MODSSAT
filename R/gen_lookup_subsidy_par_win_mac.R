#' This function is the parallelized version of the gen_lookup_subsidy function.
#' @param subsidy_amount                 is the amount of subsidy per acre-inch of groundwater extracted. Defaults to 1.
#' @param subsidy_threshold              is the threshold of subsidy, i.e., the amount of groundwater extraction above which subsidy is zero. Defaults to 400.
#' @param DSSAT_files                    is the directory where DSSAT files are located. Defaults to "C:/Users/manirad/Dropbox/DSSAT subregions work pre-2018 annual meeting/subregion KS files/outputs_for_econ/revised".
#' @param soil_file                      is the file that contains base soil file. Defaults to "C:/Users/manirad/Downloads/test/Well_Soil Type_generator_07.csv".
#' @param well_capacity_file             is the file that contains base well capacity. Defaults to "C:/Users/manirad/Downloads/test/Well_Capacity_ganarator.csv".
#' @param price_file                     is the file that includes crop prices. Defaults to "C:/Users/manirad/Dropbox/DSSAT subregions work pre-2018 annual meeting/subregion KS files/crop_prices.csv".
#' @param fixed_cost_file                is the file tha includes fixed costs (per acre) costs of production. Defaults to "C:/Users/manirad/Downloads/test/fixed_cost_input.csv".
#' @param pumping_cost                   is the cost of pumping an acre-inch of groundwater excluding taxes. Defaults to 3.56 which is the cost of pumpin an acre-inch of groundwater in parts of Kansas.
#' @param default_well_capacity_col_name is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.
#' @param soil_moisture_targets          is the vector of soil moisture targets that are generated by the DSSAT model. (25, 35, 45, 55, 65, 75).
#' @param IFREQ_seq                      is the difference between two consequtive irrigation frequencies (IFREQ) in DSSAT. Defaults to 2.
#' @param IFREQ_interpolate              is the size of interpolation interval. Defaults to 0.1 so that IFREQ_seq of 2 adds 0, 0.1, 0.2, ..., 1.9.
#' @param num_clusters                   is the number of cores for parallelization.
#' @return                               returns the output table.
#' @examples
#' \dontrun{
#' gen_lookup_tax(subsidy_amount = 2)
#' }
#' @export
gen_lookup_subsidy_par_win_mac = function(subsidy_amount = 1,
                                          subsidy_threshold = 1,
                                          DSSAT_files = "./input_files/DSSAT_files",
                                          maximum_well_capacity = 1000,
                                          well_capacity_intervals = 20,
                                          price_file = "./input_files/crop_prices.csv",
                                          fixed_cost_file = "./input_files/fixed_cost_input.csv",
                                          pumping_cost = 3.21,
                                          default_well_capacity_col_name = "Well_Capacity(gpm)",
                                          soil_moisture_targets = c(25, 35, 45, 55, 65, 75),
                                          IFREQ_seq = 2,
                                          IFREQ_interpolate = 0.1,
                                          num_clusters = parallel::detectCores()-2
)
{
  library(data.table)
  library(parallel)
  subsidy_amount = (subsidy_amount - 1)/10
  print(paste("this is the marginal tax rate:", subsidy_amount,
              "per acre-inch", sep = " "))
  print(paste("this is the subsidy threshold:", subsidy_threshold,
              "acre-inch", sep = " "))

  col_new = c("RUNNO", "TRNO", "R_pound", "O_pound", "C_pound",
              "CR", "MODEL", "EXNAME", "FNAM", "WSTA", "SOIL_ID", "SDAT",
              "PDAT", "EDAT", "ADAT", "MDAT", "HDAT", "DWAP", "CWAM",
              "HWAM", "HWAH", "BWAH", "PWAM", "HWUM", "H_AM", "H_UM",
              "HIAM", "LAIX", "IR_M", "IRCM", "PRCM", "ETCM", "EPCM",
              "ESCM", "ROCM", "DRCM", "SWXM", "NI_M", "NICM", "NFXM",
              "NUCM", "NLCM", "NIAM", "CNAM", "GNAM", "N2OEC", "PI_M",
              "PICM", "PUPC", "SPAM", "KI_M", "KICM", "KUPC", "SKAM",
              "RECM", "ONTAM", "ONAM", "OPTAM", "OPAM", "OCTAM", "OCAM",
              "CO2EC", "DMPPM", "DMPEM", "DMPTM", "DMPIM", "YPPM",
              "YPEM", "YPTM", "YPIM", "DPNAM", "DPNUM", "YPNAM", "YPNUM",
              "NDCH", "TMAXA", "TMINA", "SRADA", "DAYLA", "CO2A", "PRCP",
              "ETCP", "ESCP", "EPCP", "PAW", "IFREQ")
  filenames = list.files(path = DSSAT_files, pattern = "*.OSU",
                         full.names = TRUE, recursive = TRUE)
  ldf <- lapply(filenames, read.table, fill = T)
  ldf <- lapply(ldf, function(x) x[-2, ])
  ldf <- mapply(cbind, ldf, file_name = filenames, SIMPLIFY = F)
  KS_DSSAT = data.table::rbindlist(ldf)
  KS_DSSAT = data.table(KS_DSSAT)
  KS_DSSAT = KS_DSSAT[complete.cases(V85)]
  KS_DSSAT[, `:=`(file_name, NULL)]
  rm(ldf)
  KS_DSSAT[, `:=`(foo, nchar(as.character(V9)))]
  KS_DSSAT[foo < 4, `:=`(V9, paste(V9, V11, V10, sep = "_"))]
  KS_DSSAT[foo < 4, `:=`(V10, NA)]
  KS_DSSAT[foo < 4, `:=`(V11, NA)]
  KS_DSSAT = KS_DSSAT[, !sapply(KS_DSSAT, function(x) all(is.na(x))),
                      with = FALSE]
  KS_DSSAT[, `:=`(V9, as.character(V9))]
  KS_DSSAT[, `:=`(PAW, substr(V9, 1, regexpr("_", V9) - 2))]
  KS_DSSAT[, `:=`(IFREQ, substr(V9, regexpr("Q", V9) + 2, nchar(V9)))]
  KS_DSSAT[, `:=`(V9, NULL)]
  KS_DSSAT[, `:=`(foo, NULL)]
  data.table::setnames(KS_DSSAT, old = colnames(KS_DSSAT),
                       new = col_new)
  KS_DSSAT[, `:=`(WSTA, substr(WSTA, 1, 4))]
  KS_DSSAT[, `:=`(group, .GRP), by = c("WSTA", "SOIL_ID")]
  unique_soil = unique(KS_DSSAT[,.(SOIL_ID, WSTA)])

  KS_DSSAT_2 = data.table::copy(KS_DSSAT)
  lookup_table_all_years_2 = data.table::data.table()
  lookup_table_quarter_2 = data.table::data.table()
  lookup_table_well_2 = data.table::data.table()

  for (i in 1:max(KS_DSSAT_2$group)) {
    soil_type = data.table(Well_ID = seq(1001, (1000+maximum_well_capacity), by=well_capacity_intervals),
                           Soil_Type = unique_soil[i, SOIL_ID], weather_station = unique_soil[i, WSTA])
    well_capacity = data.table(Well_ID = seq(1001, (1000+maximum_well_capacity), by=well_capacity_intervals),
                               `Well_Capacity(gpm)` = seq(1, (maximum_well_capacity), by=well_capacity_intervals))

    data.table::setkey(soil_type, Well_ID)
    data.table::setkey(well_capacity, Well_ID)
    well_capacity_data = soil_type[well_capacity]
    data.table::setnames(well_capacity_data, old= default_well_capacity_col_name,
                         "Well_capacity")
    well_capacity_data = well_capacity_data[, .(Well_ID,
                                                Soil_Type, weather_station, Well_capacity)]

    price_dt = data.table::fread(price_file)
    data.table::setkey(price_dt, CR)
    cost_dt = well_capacity_data[, .(Well_ID)]
    data.table::setkey(cost_dt, Well_ID)
    cost_dt[, `:=`(cost_per_acre_in, (pumping_cost)/1)]
    fixed_cost = data.table::fread(fixed_cost_file)
    fixed_cost[irr == 0, `:=`(Crop, paste("dry", Crop, sep = "-"))]
    fixed_cost[, `:=`(irr, NULL)]
    fixed_cost = rbind(fixed_cost, data.table::data.table(Crop = "FA",
                                                          f_cost = 0))
    data.table::setkey(fixed_cost, Crop)
    KS_DSSAT = KS_DSSAT_2[group == i, .(SOIL_ID, WSTA, CR,
                                        IFREQ, PAW, SDAT, IRCM, PRCP, PRCM, HWAM)]
    KS_DSSAT[, `:=`(IRCM, as.numeric(as.character(IRCM)))]
    KS_DSSAT = KS_DSSAT[complete.cases(IRCM)]
    KS_DSSAT[, `:=`(SDAT, substr(SDAT, 1, 4))]
    cols_change = c("IFREQ", "PAW", "SDAT", "IRCM", "PRCP",
                    "PRCM", "HWAM")
    KS_DSSAT[, `:=`((cols_change), lapply(.SD, function(x) as.numeric(as.character(x)))),
             .SDcols = cols_change]
    cols_change = c("SOIL_ID", "WSTA", "CR")
    KS_DSSAT[, `:=`((cols_change), lapply(.SD, as.character)),
             .SDcols = cols_change]
    KS_DSSAT = KS_DSSAT[IFREQ < 17]
    qux1 = KS_DSSAT[IFREQ == 6 & PAW == 75]
    qux2 = KS_DSSAT[IFREQ == 8 & PAW == 75]
    qux1[, `:=`(IFREQ, 8)]
    qux2[, `:=`(IFREQ, 6)]
    KS_DSSAT = KS_DSSAT[!((IFREQ == 6 | IFREQ == 8) & PAW ==
                            75)]
    KS_DSSAT = rbind(KS_DSSAT, qux1)
    KS_DSSAT = rbind(KS_DSSAT, qux2)
    KS_DSSAT = KS_DSSAT[PAW %in% soil_moisture_targets |
                          IFREQ == 0]
    KS_DSSAT_0 = KS_DSSAT[IFREQ == 0]
    KS_DSSAT = KS_DSSAT[IFREQ != 0]
    KS_DSSAT_0 <- KS_DSSAT_0[rep(seq_len(nrow(KS_DSSAT_0)),
                                 each = length(soil_moisture_targets))]
    KS_DSSAT_0[, `:=`(PAW, rep(soil_moisture_targets, nrow(KS_DSSAT_0)/length(soil_moisture_targets)))]
    KS_DSSAT = rbind(KS_DSSAT, KS_DSSAT_0)
    data.table::setkey(KS_DSSAT, SOIL_ID, WSTA, CR, IFREQ,
                       PAW, SDAT)
    KS_DSSAT = KS_DSSAT[PRCP > -10 & HWAM > -10]
    KS_DSSAT = KS_DSSAT[, .(SOIL_ID, WSTA, CR, PAW, SDAT,
                            IFREQ, IRCM, PRCP, PRCM, HWAM)]
    data.table::setkey(KS_DSSAT, SOIL_ID, WSTA, CR, PAW,
                       SDAT, IFREQ)
    # foo = copy(KS_DSSAT)
    # foo[, mean_HWAM := mean(HWAM), by=c("SOIL_ID", "WSTA", "CR", "IFREQ", "PAW")]
    # foo[, mean_IRCM := mean(IRCM), by=c("SOIL_ID", "WSTA", "CR", "IFREQ", "PAW")]
    # foo = unique(foo,              by=c("SOIL_ID", "WSTA", "CR", "IFREQ", "PAW"))
    #
    # ggplot(foo[IFREQ == 4], aes(x=mean_IRCM, y= mean_HWAM, group=CR, col=CR))+
    #   geom_point()+
    #   geom_line()
    # ggplot(KS_DSSAT[IFREQ == 4], aes(x=IRCM, y= HWAM, group=CR, col=CR))+
    #   geom_point()+
    #   geom_smooth()
    #
    # ggplot(KS_DSSAT[CR == "MZ"], aes(x=IRCM, y= HWAM, group=factor(IFREQ), col=factor(IFREQ)))+
    #   geom_point()

    KS_DSSAT[, `:=`(lead_yield, dplyr::lead(HWAM, n = 1L)),
             by = c("SOIL_ID", "WSTA", "CR", "PAW", "SDAT")]
    KS_DSSAT[, `:=`(lead_irr_mm, dplyr::lead(IRCM, n = 1L)),
             by = c("SOIL_ID", "WSTA", "CR", "PAW", "SDAT")]
    KS_DSSAT <- KS_DSSAT[rep(seq_len(nrow(KS_DSSAT)), each = IFREQ_seq *
                               10)]
    df_foo = data.table::data.table(x = seq(0, IFREQ_seq -
                                              0.1, IFREQ_interpolate))
    df_foo = do.call(rbind, replicate(nrow(KS_DSSAT)/nrow(df_foo),
                                      df_foo, simplify = F))
    KS_DSSAT = data.table::data.table(KS_DSSAT, IFREQ_int = df_foo$x)
    KS_DSSAT[, `:=`(foo, max(IFREQ)), by = c("SOIL_ID", "WSTA",
                                             "CR")]
    KS_DSSAT[, `:=`(IFREQ, IFREQ + IFREQ_int)]
    KS_DSSAT = KS_DSSAT[IFREQ <= foo]
    KS_DSSAT[, `:=`(foo, NULL)]
    KS_DSSAT = KS_DSSAT[complete.cases(lead_yield)]
    KS_DSSAT[, `:=`(yield_int, HWAM + (lead_yield - HWAM)/IFREQ_seq *
                      IFREQ_int)]
    KS_DSSAT[, `:=`(irr_int, IRCM + (lead_irr_mm - IRCM)/IFREQ_seq *
                      IFREQ_int)]
    KS_DSSAT = KS_DSSAT[, .(SOIL_ID, WSTA, CR, IFREQ, PAW,
                            SDAT, irr_mm = irr_int, PRCP, PRCM, yield_kg_ac = yield_int)]
    KS_DSSAT[, `:=`(yield_kg_ac, yield_kg_ac * 0.4046)]
    data.table::setkey(KS_DSSAT, SOIL_ID, WSTA, CR, IFREQ,
                       PAW, SDAT)
    KS_DSSAT = KS_DSSAT[IFREQ == 0 | IFREQ >= IFREQ_seq]
    KS_DSSAT = KS_DSSAT[IFREQ != 0 | PAW == soil_moisture_targets[1]]
    KS_DSSAT = KS_DSSAT[SDAT < 2016]
    KS_DSSAT[IFREQ == 0, `:=`(CR, paste("dry", CR, sep = "-"))]
    KS_DSSAT[CR %like% "MZ", `:=`(yield_kg_ac, yield_kg_ac *
                                    1.183)]
    KS_DSSAT[CR %like% "SG", `:=`(yield_kg_ac, yield_kg_ac *
                                    1.183)]
    KS_DSSAT[CR %like% "WH", `:=`(yield_kg_ac, yield_kg_ac *
                                    1.156)]

    number_of_crops = length(KS_DSSAT[, unique(CR)])
    well_capacity_data = rbind(data.table::data.table(Well_ID = 1,
                                                      Soil_Type = unique(well_capacity_data$Soil_Type),
                                                      weather_station = unique(well_capacity_data$weather_station),
                                                      Well_capacity = 0), well_capacity_data)
    well_capacity_data[, `:=`(quarter_1, 0)]
    well_capacity_data[, `:=`(quarter_2, 0)]
    well_capacity_data[, `:=`(quarter_3, 0)]
    well_capacity_data[, `:=`(quarter_4, 0)]
    well_capacity_data <- well_capacity_data[rep(seq_len(nrow(well_capacity_data)),
                                                 each = 5)]
    well_capacity_data[, `:=`(tot_acres, rep(c(0, 32.5, 65,
                                               97.5, 130), nrow(well_capacity_data)/5))]
    well_capacity_data <- well_capacity_data[rep(seq_len(nrow(well_capacity_data)),
                                                 each = (number_of_crops + 1))]
    well_capacity_data[, `:=`(quarter_4, rep(0:number_of_crops,
                                             nrow(well_capacity_data)/(number_of_crops + 1)))]
    well_capacity_data[, `:=`(quarter_3, rep(0:number_of_crops,
                                             nrow(well_capacity_data)/(number_of_crops + 1)))]
    well_capacity_data[, `:=`(quarter_2, rep(0:number_of_crops,
                                             nrow(well_capacity_data)/(number_of_crops + 1)))]
    well_capacity_data[, `:=`(quarter_1, rep(0:number_of_crops,
                                             nrow(well_capacity_data)/(number_of_crops + 1)))]
    well_capacity_data[, `:=`(ifreq, round((tot_acres * 1)/(Well_capacity *
                                                              0.053030303149011), 1))]
    # well_capacity_data[!(complete.cases(ifreq) & ifreq < 100), `:=`(ifreq, 0)]
    well_capacity_data[!complete.cases(ifreq), `:=`(ifreq, 0)]
    well_capacity_data[ifreq < 2 & ifreq > 0, `:=`(ifreq,
                                                   2)]
    well_capacity_data = reshape2::melt(well_capacity_data,
                                        id = c("Well_ID", "Soil_Type", "weather_station",
                                               "Well_capacity", "tot_acres", "ifreq"))
    well_capacity_data = data.table::data.table(well_capacity_data)
    data.table::setkey(well_capacity_data, Well_ID, Soil_Type,
                       weather_station, tot_acres)
    quz = data.table(CR = KS_DSSAT[, unique(CR)])
    foo = data.table(value = 0:number_of_crops, rbind(data.table(CR = "FA"),
                                                      quz))
    setkey(well_capacity_data, value)
    setkey(foo, value)
    well_capacity_data = well_capacity_data[foo]
    well_capacity_data[, `:=`(value, NULL)]
    well_capacity_data[, `:=`(quarter, ifelse(variable ==
                                                "quarter_1", 1, ifelse(variable == "quarter_2", 2,
                                                                       ifelse(variable == "quarter_3", 3, 4))))]
    well_capacity_data[, `:=`(variable, NULL)]
    tot_acres_0 = well_capacity_data[tot_acres == 0]
    tot_acres_0 = tot_acres_0[data.table::like(CR, "FA") |
                                data.table::like(CR, "dry")]
    tot_acres_325 = well_capacity_data[tot_acres == 32.5]
    tot_acres_325 = tot_acres_325[(quarter == 1 & !(data.table::like(CR,
                                                                     "FA") | data.table::like(CR, "dry"))) | (quarter !=
                                                                                                                1 & (data.table::like(CR, "FA") | data.table::like(CR,
                                                                                                                                                                   "dry")))]
    tot_acres_65 = well_capacity_data[tot_acres == 65]
    tot_acres_65 = tot_acres_65[(quarter < 3 & !(data.table::like(CR,
                                                                  "FA") | data.table::like(CR, "dry"))) | (quarter >
                                                                                                             2 & (data.table::like(CR, "FA") | data.table::like(CR,
                                                                                                                                                                "dry")))]
    tot_acres_975 = well_capacity_data[tot_acres == 97.5]
    tot_acres_975 = tot_acres_975[(quarter < 4 & !(data.table::like(CR,
                                                                    "FA") | data.table::like(CR, "dry"))) | (quarter >
                                                                                                               3 & (data.table::like(CR, "FA") | data.table::like(CR,
                                                                                                                                                                  "dry")))]
    tot_acres_130 = well_capacity_data[tot_acres == 130]
    tot_acres_130 = tot_acres_130[!(data.table::like(CR,
                                                     "FA") | data.table::like(CR, "dry"))]
    well_capacity_data = rbind(tot_acres_0, tot_acres_325,
                               tot_acres_65, tot_acres_975, tot_acres_130)
    rm(tot_acres_0, tot_acres_325, tot_acres_65, tot_acres_975,
       tot_acres_130)
    data.table::setkey(well_capacity_data, Well_ID, tot_acres,
                       quarter)
    KS_DSSAT[, `:=`(IFREQ, round(IFREQ, 1))]
    number_of_years = nrow(unique(KS_DSSAT, by = "SDAT"))
    foo = data.table::data.table(SOIL_ID = KS_DSSAT_2[group ==
                                                        i, unique(SOIL_ID)], WSTA = KS_DSSAT_2[group == i,
                                                                                               unique(WSTA)], CR = "FA", IFREQ = 0, PAW = soil_moisture_targets[1],
                                 SDAT = min(KS_DSSAT$SDAT), irr_mm = 0, PRCP = 200,
                                 PRCM = 200, yield_kg_ac = 0)
    foo = foo[rep(seq_len(nrow(foo)), each = number_of_years)]
    baz = unique(KS_DSSAT, by = "SDAT")
    foo[, `:=`(SDAT, rep(baz$SDAT, nrow(foo)/nrow(baz)))]
    foo[, `:=`(PRCP, rep(baz$PRCP, nrow(foo)/nrow(baz)))]
    KS_DSSAT = rbind(KS_DSSAT, foo)
    data.table::setkey(KS_DSSAT, SOIL_ID, WSTA, CR, IFREQ)


    data.table::setkey(KS_DSSAT, CR)
    KS_DSSAT = KS_DSSAT[price_dt]
    KS_DSSAT[, cost_per_acre_in := unique(cost_dt$cost_per_acre_in)] # fix this one
    data.table::setkey(KS_DSSAT, CR)
    fixed_cost = unique(fixed_cost, by=colnames(fixed_cost))
    data.table::setkey(fixed_cost, Crop)
    KS_DSSAT = KS_DSSAT[fixed_cost]
    KS_DSSAT[, `:=`(irrigation, 32.5 * irr_mm * 0.0393701)]
    KS_DSSAT[, `:=`(profit, 32.5 * (yield_kg_ac * price -
                                      f_cost) - irrigation * cost_per_acre_in)]
    KS_DSSAT = KS_DSSAT[complete.cases(IFREQ)]

    well_capacity_data[data.table::like(CR, "dry"), `:=`(ifreq,
                                                         0)]
    well_capacity_data[CR == "FA", `:=`(ifreq, 0)]
    cols = colnames(well_capacity_data)
    well_capacity_data = unique(well_capacity_data, by = cols)
    data.table::setkey(well_capacity_data, Well_ID, tot_acres, quarter)
    well_capacity_data[, `:=`(Well_ID_grp, .GRP), by = "Well_ID"]
    # well_capacity_data[, `:=`(group_1, .GRP), by = c("Well_ID", "tot_acres", "SDAT")]
    # well_capacity_data[, `:=`(group_2, 1:.N), by = c("Well_ID", "tot_acres", "SDAT", "quarter")]
    aa = max(well_capacity_data$Well_ID_grp)

    foo_irr_2 = merge(well_capacity_data, KS_DSSAT, by.x = c("Soil_Type",
                                                             "weather_station", "CR", "ifreq"), by.y = c("SOIL_ID",
                                                                                                         "WSTA", "CR", "IFREQ"), allow.cartesian = T)
    foo_irr_2 = foo_irr_2[, .(Well_ID, SOIL_ID = Soil_Type, WSTA = weather_station,
                              Well_capacity, tot_acres, IFREQ = ifreq, CR, quarter,
                              PAW, SDAT, irr_mm, PRCP, PRCM, irrigation, yield_kg_ac, profit)]

    if (Sys.info()[1] == "Windows") {
      library(snow)
      cl <- makeCluster(num_clusters)
      print(Sys.info()[1])
      parallel::clusterExport(cl, varlist = c("foo_irr_2", "data.table", ".", "aa", "FN_optim2",
                                              "well_capacity_data", "KS_DSSAT", "IFREQ_interpolate",
                                              "setnames", "setkey", "subsidy_amount",
                                              "subsidy_threshold"), envir = environment())
      foo_dt_all_1 <- parLapply(cl, 1:floor(aa/4),                     FN_optim2)
      foo_dt_all_2 <- parLapply(cl, (floor(aa/4)+1):(2*floor(aa/4)),   FN_optim2)
      foo_dt_all_3 <- parLapply(cl, (2*floor(aa/4)+1):(3*floor(aa/4)), FN_optim2)
      foo_dt_all_4 <- parLapply(cl, (3*floor(aa/4)+1):aa,              FN_optim2)
      stopCluster(cl)
    }
    else {
      print(Sys.info()[1])
      foo_dt_all_1 <- mclapply(X = 1:floor(aa/4), FUN = FN_optim2,
                               mc.cores = num_clusters)

      foo_dt_all_2 <- mclapply(X = (floor(aa/4)+1):(2*floor(aa/4)), FUN = FN_optim2,
                               mc.cores = num_clusters)

      foo_dt_all_3 <- mclapply(X = (2*floor(aa/4)+1):(3*floor(aa/4)), FUN = FN_optim2,
                               mc.cores = num_clusters)

      foo_dt_all_4 <- mclapply(X = (3*floor(aa/4)+1):aa, FUN = FN_optim2,
                               mc.cores = num_clusters)

    }


    foo_dt_all_1 <- do.call(rbind, foo_dt_all_1)
    foo_dt_all_1 =      data.table(foo_dt_all_1)
    foo_dt_all_2 <- do.call(rbind, foo_dt_all_2)
    foo_dt_all_2 =      data.table(foo_dt_all_2)
    foo_dt_all_3 <- do.call(rbind, foo_dt_all_3)
    foo_dt_all_3 =      data.table(foo_dt_all_3)
    foo_dt_all_4 <- do.call(rbind, foo_dt_all_4)
    foo_dt_all_4 =      data.table(foo_dt_all_4)

    foo_dt_all = rbind(foo_dt_all_1, foo_dt_all_2, foo_dt_all_3, foo_dt_all_4)
    foo_dt_all = unique(foo_dt_all, by=c("Well_capacity", "quarter"))

    foo_dt_all = foo_dt_all[, .(Well_capacity, tot_acres, quarter, ifreq,
                                CR, PAW, mean_irrigation_combination, mean_profit_combination,
                                mean_profit_combination_sub)]
    setkey(foo_dt_all, Well_capacity, tot_acres, quarter, CR, PAW)
    setkey(foo_irr_2,  Well_capacity, tot_acres, quarter, CR, PAW)
    lookup_table_quarter = foo_irr_2[foo_dt_all]
    lookup_table_quarter = unique(lookup_table_quarter, by=colnames(lookup_table_quarter))
    lookup_table_quarter = unique(lookup_table_quarter, by=c("SOIL_ID", "WSTA", "Well_capacity",
                                                             "tot_acres", "IFREQ", "CR", "quarter", "PAW", "SDAT"))

    lookup_table_well = unique(lookup_table_quarter, by = "Well_capacity")
    lookup_table_quarter = lookup_table_quarter[, .(Well_capacity,
                                                    SOIL_ID, WSTA, tot_acres, quarter, ifreq, SDAT, CR, PAW,
                                                    irrigation_quarter = irrigation, yield_kg_ac, profit_quarter = profit, #subsidy_amount
                                                    subsidy_amount)]

    data.table::setkey(lookup_table_well, Well_capacity)
    lookup_table_well = lookup_table_well[, .(Well_capacity,
                                              SOIL_ID, WSTA, tot_acres, irr_tot_acres = mean_irrigation_combination,
                                              profit_Well_ID = mean_profit_combination, profit_Well_ID_subsidy = mean_profit_combination_sub, #subsidy_amount
                                              subsidy_amount)]

    lookup_table_quarter = unique(lookup_table_quarter, by=colnames(lookup_table_quarter))
    lookup_table_well    = unique(lookup_table_well,    by=colnames(lookup_table_well))



    lookup_table_all_years = copy(lookup_table_quarter)
    lookup_table_all_years[, `:=`(irr_tot_acres, sum(irrigation_quarter)),
                           by = c("Well_capacity", "SDAT")]
    lookup_table_all_years[, `:=`(profit_Well_ID, sum(profit_quarter)),
                           by = c("Well_capacity", "SDAT")]
    lookup_table_all_years[, `:=`(irr_below, ifelse(irr_tot_acres <
                                                      subsidy_threshold, subsidy_threshold - irr_tot_acres,
                                                    0))]
    lookup_table_all_years = unique(lookup_table_all_years,
                                    by = c("Well_capacity", "SDAT"))
    lookup_table_all_years[, `:=`(profit_Well_ID_sub,
                                  profit_Well_ID + irr_below * subsidy_amount)]
    lookup_table_all_years = lookup_table_all_years[, .(Well_capacity,
                                                        SOIL_ID, WSTA, tot_acres, SDAT, irr_tot_acres, profit_Well_ID,
                                                        irr_below, profit_Well_ID_sub)]

    lookup_table_all_years_2 = rbind(lookup_table_all_years_2,
                                     lookup_table_all_years)
    lookup_table_quarter_2 = rbind(lookup_table_quarter_2,
                                   lookup_table_quarter)
    lookup_table_well_2 = rbind(lookup_table_well_2, lookup_table_well)
    print(i)
  }
  data.table::setkey(lookup_table_all_years_2, SOIL_ID, Well_capacity,
                     SDAT)
  data.table::setkey(lookup_table_quarter_2, SOIL_ID, Well_capacity,
                     quarter)
  data.table::setkey(lookup_table_well_2, SOIL_ID, Well_capacity)
  write.csv(lookup_table_well_2, "lookup_table_well_2.csv")
  saveRDS(lookup_table_quarter_2, "lookup_table_quarter_2.rds")
  saveRDS(lookup_table_all_years_2, "lookup_table_all_years_2.rds")
}

