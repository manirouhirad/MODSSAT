#' This function generates the lookup table with a subsidy policy that includes a threshold and a marginal subsidy rate.
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
#' @return                               returns the output table.
#' @examples
#' \dontrun{
#' gen_lookup_tax(subsidy_amount = 2)
#' }
#' @export
gen_lookup_subsidy = function (subsidy_amount = 2,
                              subsidy_threshold = 200,
                              DSSAT_files = "./input_files/DSSAT_files",
                              soil_file = "./input_files/Well_Soil Type_generator_07.csv",
                              well_capacity_file = "./input_files/Well_Capacity_ganarator.csv",
                              price_file = "./input_files/crop_prices.csv",
                              fixed_cost_file = "./input_files/fixed_cost_input.csv",
                              pumping_cost = 3,
                              default_well_capacity_col_name = "Well_Capacity(gpm)",
                              soil_moisture_targets = c(25, 35, 45, 55, 65, 75),
                              IFREQ_seq = 2,
                              IFREQ_interpolate = 0.1)
{
  library(data.table)
  subsidy_amount = (subsidy_amount - 1)/10
  print(paste("this is the marginal subsidy rate:", subsidy_amount,
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
  unique_soil = KS_DSSAT[, unique(SOIL_ID)]
  KS_DSSAT_2 = data.table::copy(KS_DSSAT)
  lookup_table_all_years_2 = data.table::data.table()
  lookup_table_quarter_2 = data.table::data.table()
  lookup_table_well_2 = data.table::data.table()
  i = 5
  for (i in 1:length(unique_soil)) {
    soil_type = data.table::fread(soil_file)
    soil_type[, `:=`(Soil_Type, unique_soil[i])]
    soil_type[, `:=`(Soil_Type, gsub("KSFC00000", "KS0000000",
                                     Soil_Type))]
    well_capacity = data.table::fread(well_capacity_file)
    data.table::setkey(soil_type, Well_ID)
    data.table::setkey(well_capacity, Well_ID)
    well_capacity_data = soil_type[well_capacity]
    data.table::setnames(well_capacity_data, old = default_well_capacity_col_name,
                         "Well_capacity")
    price_dt = data.table::fread(price_file)
    data.table::setkey(price_dt, CR)
    cost_dt = well_capacity_data[, .(Well_ID)]
    data.table::setkey(cost_dt, Well_ID)
    cost_dt[, `:=`(cost_per_acre_in, (pumping_cost)/1)]
    fixed_cost = data.table::fread(fixed_cost_file)
    fixed_cost[Crop == "MZ", `:=`(f_cost, 500)]
    fixed_cost[irr == 0, `:=`(Crop, paste("dry", Crop, sep = "-"))]
    fixed_cost[, `:=`(irr, NULL)]
    fixed_cost = rbind(fixed_cost, data.table::data.table(Crop = "FA",
                                                          f_cost = 0))
    data.table::setkey(fixed_cost, Crop)
    KS_DSSAT = KS_DSSAT_2[, .(SOIL_ID, CR, IFREQ, PAW, SDAT,
                              IRCM, PRCP, PRCM, HWAM)]
    KS_DSSAT[, `:=`(IRCM, as.numeric(as.character(IRCM)))]
    KS_DSSAT = KS_DSSAT[complete.cases(IRCM)]
    KS_DSSAT[, `:=`(SDAT, substr(SDAT, 1, 4))]
    cols_change = colnames(KS_DSSAT)[c(3:9)]
    KS_DSSAT[, `:=`((cols_change), lapply(.SD, function(x) as.numeric(as.character(x)))),
             .SDcols = cols_change]
    cols_change = colnames(KS_DSSAT)[c(1, 2)]
    KS_DSSAT[, `:=`((cols_change), lapply(.SD, as.character)),
             .SDcols = cols_change]
    KS_DSSAT = KS_DSSAT[SOIL_ID == unique_soil[i]]
    qux = KS_DSSAT[(IFREQ == 16 | IFREQ == 18 | IFREQ ==
                      20) & CR == "MZ"]
    qux[, `:=`(HWAM_6, ifelse(IFREQ == 16, HWAM, 0))]
    qux[, `:=`(HWAM_10, ifelse(IFREQ == 20, HWAM, 0))]
    qux[, `:=`(HWAM_6, max(HWAM_6)), by = c("SOIL_ID", "CR",
                                            "PAW", "SDAT")]
    qux[, `:=`(HWAM_10, max(HWAM_10)), by = c("SOIL_ID",
                                              "CR", "PAW", "SDAT")]
    qux[, `:=`(HWAM, ifelse(IFREQ == 18, (HWAM_6 + HWAM_10)/2,
                            HWAM))]
    qux[, `:=`(IRCM_6, ifelse(IFREQ == 16, IRCM, 0))]
    qux[, `:=`(IRCM_10, ifelse(IFREQ == 20, IRCM, 0))]
    qux[, `:=`(IRCM_6, max(IRCM_6)), by = c("SOIL_ID", "CR",
                                            "PAW", "SDAT")]
    qux[, `:=`(IRCM_10, max(IRCM_10)), by = c("SOIL_ID",
                                              "CR", "PAW", "SDAT")]
    qux[, `:=`(IRCM, ifelse(IFREQ == 18, (IRCM_6 + IRCM_10)/2,
                            IRCM))]
    qux = qux[IFREQ == 18, .(SOIL_ID, CR, IFREQ, PAW, SDAT,
                             IRCM, PRCP, PRCM, HWAM)]
    KS_DSSAT = KS_DSSAT[IFREQ != 18 | CR != "MZ"]
    KS_DSSAT = rbind(KS_DSSAT, qux)
    setkey(KS_DSSAT, SOIL_ID, CR, PAW, SDAT, IFREQ)
    qux1 = KS_DSSAT[IFREQ == 6 & PAW == 75]
    qux2 = KS_DSSAT[IFREQ == 8 & PAW == 75]
    qux1[, `:=`(IFREQ, 8)]
    qux2[, `:=`(IFREQ, 6)]
    KS_DSSAT = KS_DSSAT[!((IFREQ == 6 | IFREQ == 8) & PAW ==
                            75)]
    KS_DSSAT = rbind(KS_DSSAT, qux1)
    KS_DSSAT = rbind(KS_DSSAT, qux2)
    KS_DSSAT_0 = KS_DSSAT[IFREQ == 0]
    KS_DSSAT = KS_DSSAT[IFREQ != 0]
    KS_DSSAT_0 <- KS_DSSAT_0[rep(seq_len(nrow(KS_DSSAT_0)),
                                 each = 6)]
    KS_DSSAT_0[, `:=`(PAW, rep(soil_moisture_targets, nrow(KS_DSSAT_0)/length(soil_moisture_targets)))]
    KS_DSSAT = rbind(KS_DSSAT, KS_DSSAT_0)
    data.table::setkey(KS_DSSAT, SOIL_ID, CR, IFREQ, PAW,
                       SDAT)
    KS_DSSAT = KS_DSSAT[PRCP > -10 & HWAM > -10]
    KS_DSSAT = KS_DSSAT[, .(SOIL_ID, CR, PAW, SDAT, IFREQ,
                            IRCM, PRCP, PRCM, HWAM)]
    data.table::setkey(KS_DSSAT, SOIL_ID, CR, PAW, SDAT,
                       IFREQ)
    KS_DSSAT[, `:=`(lead_yield, dplyr::lead(HWAM, n = 1L)),
             by = c("SOIL_ID", "CR", "PAW", "SDAT")]
    KS_DSSAT[, `:=`(lead_irr_mm, dplyr::lead(IRCM, n = 1L)),
             by = c("SOIL_ID", "CR", "PAW", "SDAT")]
    KS_DSSAT <- KS_DSSAT[rep(seq_len(nrow(KS_DSSAT)), each = IFREQ_seq *
                               10)]
    df_foo = data.table::data.table(x = seq(0, IFREQ_seq -
                                              0.1, IFREQ_interpolate))
    df_foo = do.call(rbind, replicate(nrow(KS_DSSAT)/nrow(df_foo),
                                      df_foo, simplify = F))
    KS_DSSAT = data.table::data.table(KS_DSSAT, IFREQ_int = df_foo$x)
    KS_DSSAT[, `:=`(foo, max(IFREQ)), by = c("SOIL_ID", "CR")]
    KS_DSSAT[, `:=`(IFREQ, IFREQ + IFREQ_int)]
    KS_DSSAT = KS_DSSAT[IFREQ <= foo]
    KS_DSSAT[, `:=`(foo, NULL)]
    KS_DSSAT = KS_DSSAT[complete.cases(lead_yield)]
    KS_DSSAT[, `:=`(yield_int, HWAM + (lead_yield - HWAM)/IFREQ_seq *
                      IFREQ_int)]
    KS_DSSAT[, `:=`(irr_int, IRCM + (lead_irr_mm - IRCM)/IFREQ_seq *
                      IFREQ_int)]
    KS_DSSAT = KS_DSSAT[, .(SOIL_ID, CR, IFREQ, PAW, SDAT,
                            irr_mm = irr_int, PRCP, PRCM, yield_kg_ac = yield_int)]
    KS_DSSAT[, `:=`(yield_kg_ac, yield_kg_ac * 0.4046)]
    data.table::setkey(KS_DSSAT, SOIL_ID, CR, IFREQ, PAW,
                       SDAT)
    KS_DSSAT = KS_DSSAT[IFREQ == 0 | IFREQ >= IFREQ_seq]
    KS_DSSAT = KS_DSSAT[IFREQ != 0 | PAW == soil_moisture_targets[1]]
    KS_DSSAT[IFREQ == 0, `:=`(CR, paste("dry", CR, sep = "-"))]
    number_of_crops = length(KS_DSSAT[, unique(CR)])
    well_capacity_data = rbind(data.table::data.table(Well_ID = 1,
                                                      Soil_Type = unique(well_capacity_data$Soil_Type),
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
    well_capacity_data[!(complete.cases(ifreq) & ifreq <
                           100), `:=`(ifreq, 0)]
    well_capacity_data[ifreq < 2 & ifreq > 0, `:=`(ifreq,
                                                   2)]
    well_capacity_data = reshape2::melt(well_capacity_data,
                                        id = c("Well_ID", "Soil_Type", "Well_capacity", "tot_acres",
                                               "ifreq"))
    well_capacity_data = data.table::data.table(well_capacity_data)
    data.table::setkey(well_capacity_data, Well_ID, Soil_Type,
                       tot_acres)
    well_capacity_data[, `:=`(CR, ifelse(value == 1, "MZ",
                                         ifelse(value == 2, "WH", ifelse(value == 3, "SG",
                                                                         ifelse(value == 4, "dry-MZ", ifelse(value ==
                                                                                                               5, "dry-WH", ifelse(value == 6, "dry-SG", "FA")))))))]
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
    foo = data.table::data.table(SOIL_ID = unique_soil, CR = "FA",
                                 IFREQ = 0, PAW = soil_moisture_targets[1], SDAT = min(KS_DSSAT$SDAT),
                                 irr_mm = 0, PRCP = 200, PRCM = 200, yield_kg_ac = 0)
    foo = foo[rep(seq_len(nrow(foo)), each = number_of_years)]
    baz = unique(KS_DSSAT, by = "SDAT")
    foo[, `:=`(SDAT, rep(baz$SDAT, nrow(foo)/nrow(baz)))]
    foo[, `:=`(PRCP, rep(baz$PRCP, nrow(foo)/nrow(baz)))]
    KS_DSSAT = rbind(KS_DSSAT, foo)
    data.table::setkey(KS_DSSAT, SOIL_ID, CR, IFREQ)
    well_capacity_data[data.table::like(CR, "dry"), `:=`(ifreq,
                                                         0)]
    well_capacity_data[CR == "FA", `:=`(ifreq, 0)]
    cols = colnames(well_capacity_data)
    well_capacity_data = unique(well_capacity_data, by = cols)
    data.table::setkey(well_capacity_data, Soil_Type, CR,
                       ifreq)
    foo_irr = merge(well_capacity_data, KS_DSSAT, by.x = c("Soil_Type",
                                                           "CR", "ifreq"), by.y = c("SOIL_ID", "CR", "IFREQ"),
                    allow.cartesian = T)
    foo_irr = foo_irr[, .(Well_ID, SOIL_ID = Soil_Type, Well_capacity,
                          tot_acres, IFREQ = ifreq, CR, quarter, PAW, SDAT,
                          irr_mm, PRCP, PRCM, yield_kg_ac)]
    data.table::setkey(foo_irr, CR)
    foo_irr = foo_irr[price_dt]
    foo_irr = foo_irr[complete.cases(Well_ID)]
    data.table::setkey(foo_irr, Well_ID)
    cost_dt = rbind(data.table::data.table(Well_ID = 1, cost_per_acre_in = unique(cost_dt$cost_per_acre_in)),
                    cost_dt)
    data.table::setkey(cost_dt, Well_ID)
    foo_irr = foo_irr[cost_dt]
    data.table::setkey(foo_irr, CR)
    data.table::setkey(fixed_cost, Crop)
    foo_irr = foo_irr[fixed_cost]
    foo_irr = foo_irr[complete.cases(Well_ID)]
    foo_irr[, `:=`(irrigation, 32.5 * irr_mm * 0.0393701)]
    # foo_irr[, `:=`(profit, 32.5 * (yield_kg_ac * price -
    #                                  f_cost) - irrigation * cost_per_acre_in + irrigation_below *
    #                  subsidy_amount)]
    foo_irr[, `:=`(profit, 32.5 * (yield_kg_ac * price - f_cost) - irrigation * cost_per_acre_in)]
    setkey(foo_irr, Well_ID, quarter, SDAT)
    data.table::setkey(foo_irr, Well_ID, tot_acres, quarter, SDAT)
    foo_irr_2 = data.table::copy(foo_irr)
    foo_irr[, `:=`(mean_profit_PAW, mean(profit)),        by = c("Well_ID", "tot_acres", "quarter", "CR", "PAW")]
    foo_irr[, `:=`(sd_profit_PAW, sd(profit)),            by = c("Well_ID", "tot_acres", "quarter", "CR", "PAW")]
    foo_irr[, `:=`(mean_irr_PAW, mean(irrigation)),       by = c("Well_ID", "tot_acres", "quarter", "CR", "PAW")]
    foo_irr = unique(foo_irr,                             by = c("Well_ID", "tot_acres", "quarter", "CR", "PAW"))

    #####
    foo_irr[, Well_ID_grp     := .GRP, by = "Well_ID"]
    j=2
    foo_dt_all = data.table()
    for (j in 2:max(foo_irr$Well_ID_grp)) {
      foo_dt1 = foo_irr[Well_ID_grp == j                       & quarter ==1 ,.(Well_ID, Well_capacity, SOIL_ID, tot_acres, quarter, CR, PAW, mean_irr_PAW, mean_profit_PAW)]
      foo_dt2 = foo_irr[Well_ID_grp == j                       & quarter ==2 ,.(Well_ID, Well_capacity, SOIL_ID, tot_acres, quarter, CR, PAW, mean_irr_PAW, mean_profit_PAW)]
      foo_dt3 = foo_irr[Well_ID_grp == j                       & quarter ==3 ,.(Well_ID, Well_capacity, SOIL_ID, tot_acres, quarter, CR, PAW, mean_irr_PAW, mean_profit_PAW)]
      foo_dt4 = foo_irr[Well_ID_grp == j                       & quarter ==4 ,.(Well_ID, Well_capacity, SOIL_ID, tot_acres, quarter, CR, PAW, mean_irr_PAW, mean_profit_PAW)]

      foo_dt1[, row_1 := 1:.N]
      foo_dt2[, row_2 := 1:.N]
      foo_dt3[, row_3 := 1:.N]
      foo_dt4[, row_4 := 1:.N]

      foo_dt3 = merge(foo_dt3, foo_dt4, by=c("Well_ID", "tot_acres", "SOIL_ID"), allow.cartesian=T)
      foo_dt3[, row_34 := 1:.N]
      foo_dt2 = merge(foo_dt2, foo_dt3, by=c("Well_ID", "tot_acres", "SOIL_ID"), allow.cartesian=T)
      foo_dt2[, row_23 := 1:.N]
      setnames(foo_dt2, old = c("Well_capacity.x", "quarter.x", "CR.x", "PAW.x", "mean_irr_PAW.x", "mean_profit_PAW.x"), new = c("Well_capacity.xx", "quarter.xx", "CR.xx", "PAW.xx", "mean_irr_PAW.xx", "mean_profit_PAW.xx"))
      setnames(foo_dt2, old = c("Well_capacity.y", "quarter.y", "CR.y", "PAW.y", "mean_irr_PAW.y", "mean_profit_PAW.y"), new = c("Well_capacity.yy", "quarter.yy", "CR.yy", "PAW.yy", "mean_irr_PAW.yy", "mean_profit_PAW.yy"))
      foo_dt1 = merge(foo_dt1, foo_dt2, by=c("Well_ID", "tot_acres", "SOIL_ID"), allow.cartesian=T)
      foo_dt1[, row_12 := 1:.N]

      foo_dt4 = foo_dt1[,.(Well_ID, Well_capacity = Well_capacity.yy, SOIL_ID, tot_acres, quarter=quarter.yy, CR=CR.yy, PAW=PAW.yy, mean_irr_PAW = mean_irr_PAW.yy, mean_profit_PAW = mean_profit_PAW.yy, row =row_4, row_34, row_23, row_12)]
      foo_dt3 = foo_dt1[,.(Well_ID, Well_capacity = Well_capacity.xx, SOIL_ID, tot_acres, quarter=quarter.xx, CR=CR.xx, PAW=PAW.xx, mean_irr_PAW = mean_irr_PAW.xx, mean_profit_PAW = mean_profit_PAW.xx, row =row_3, row_34, row_23, row_12)]
      foo_dt2 = foo_dt1[,.(Well_ID, Well_capacity = Well_capacity.y , SOIL_ID, tot_acres, quarter=quarter.y , CR=CR.y , PAW=PAW.y , mean_irr_PAW = mean_irr_PAW.y , mean_profit_PAW = mean_profit_PAW.y , row =row_2, row_34, row_23, row_12)]
      foo_dt1 = foo_dt1[,.(Well_ID, Well_capacity = Well_capacity.x , SOIL_ID, tot_acres, quarter=quarter.x , CR=CR.x , PAW=PAW.x , mean_irr_PAW = mean_irr_PAW.x , mean_profit_PAW = mean_profit_PAW.x , row =row_1, row_34, row_23, row_12)]
      foo_dt1 = rbind(foo_dt1, foo_dt2, foo_dt3, foo_dt4)
      setkey(foo_dt1, row_12, row_23, row_34, row, tot_acres, quarter)
      rm(foo_dt2, foo_dt3, foo_dt4)

      foo_dt1[, irrigation_sum   := sum(mean_irr_PAW), by="row_12"]
      foo_dt1[, irrigation_below := ifelse(irrigation_sum < subsidy_threshold, subsidy_threshold - mean_irr_PAW, 0)]
      # foo_dt1[, profit_sum       := sum(mean_profit_PAW), by="row_12"]
      foo_dt1[, profit_sum       := sum(mean_profit_PAW) + irrigation_below * subsidy_amount, by="row_12"]
      foo_dt1[, max_p := max(profit_sum)]
      foo_dt1 = foo_dt1[profit_sum == max_p]
      foo_dt1 = foo_dt1[,.(Well_ID, Well_capacity, SOIL_ID, tot_acres, quarter, CR, PAW, irrigation_sum, irrigation_below, profit_sum, row_12)]
      foo_dt_all = rbind(foo_dt_all, foo_dt1)
      # print(j)
    }

    foo_dt0 = foo_irr[Well_ID == 1]
    foo_dt0 = foo_dt0[tot_acres == 0]
    foo_dt0[, mean_profit_PAW := mean_profit_PAW]
    foo_dt0[, profit_quarter := max(mean_profit_PAW), by="quarter"] # they all receive the same subsidy anyway
    foo_dt0 = foo_dt0[profit_quarter == mean_profit_PAW]
    foo_dt0[, irrigation_sum   := 0]
    foo_dt0[, irrigation_below := subsidy_threshold]
    foo_dt0[, profit_sum := sum(mean_profit_PAW) + subsidy_amount * subsidy_threshold]
    foo_dt0 = foo_dt0[,.(Well_ID, Well_capacity, SOIL_ID, tot_acres, quarter, CR, PAW, irrigation_sum, irrigation_below, profit_sum, row_12=1)]

    foo_dt_all = rbind(foo_dt_all, foo_dt0)
    setkey(foo_dt_all, Well_ID, tot_acres, SOIL_ID, quarter, CR, PAW)
    setkey(foo_irr_2,  Well_ID, tot_acres, SOIL_ID, quarter, CR, PAW)
    lookup_table_quarter = foo_irr_2[foo_dt_all]
    lookup_table_quarter = lookup_table_quarter[,.(Well_capacity, SOIL_ID, tot_acres, quarter, SDAT, CR, PAW, irrigation_quarter = irrigation, profit_quarter = profit)]

    lookup_table_well = unique(foo_dt_all, by = "Well_capacity")
    data.table::setkey(lookup_table_well, Well_capacity)
    lookup_table_well = lookup_table_well[,.(Well_capacity, SOIL_ID, tot_acres, irr_tot_acres = irrigation_sum, irrigation_below, profit_Well_ID= profit_sum)]

    lookup_table_all_years = copy(lookup_table_quarter)
    lookup_table_quarter[, irrigation_quarter := mean(irrigation_quarter), by=c("Well_capacity", "quarter")]
    lookup_table_quarter[, profit_quarter     := mean(profit_quarter),     by=c("Well_capacity", "quarter")]
    lookup_table_quarter = unique(lookup_table_quarter,                  by=c("Well_capacity", "quarter"))
    lookup_table_quarter[, SDAT := NULL]

    lookup_table_all_years_2 = rbind(lookup_table_all_years, lookup_table_all_years_2)
    lookup_table_quarter_2   = rbind(lookup_table_quarter_2, lookup_table_quarter)
    lookup_table_well_2      = rbind(lookup_table_well_2, lookup_table_well)
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
