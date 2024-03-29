% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annual_model_subsidy_subregion_annual.R
\name{annual_model_subsidy_subregion_annual}
\alias{annual_model_subsidy_subregion_annual}
\title{This function produces annual amounts of water use and profits.}
\usage{
annual_model_subsidy_subregion_annual(
  subsidy_amount = 1,
  subsidy_threshold = 1,
  soil_weather_file = "./input_files/Well_SoilType_WeatherStation.csv",
  well_capacity_files = "./Well Capacity",
  econ_output_folder = "./Econ_output/results_with_subsidy/annual_results/",
  well_capacity_file_year = "./KS_DSSAT_output.csv",
  subregion_file = "./input_files/all_well_IDs_small.csv",
  dryland_profit_file = "./input_files/dryland_profits.rds",
  look_up_table_inside = "lookup_table_all_years_2.rds",
  look_up_table_outside = "lookup_table_all_years_2_0.rds",
  base_year_well_capacity = "./Well_Capacity.csv",
  which_year_well_capacity = "./Well Capacity/",
  first_year_of_simulation = 2000,
  default_well_capacity_col_name = "Well_Capacity(gpm)",
  missing_soil_types = "KS00000007",
  minimum_well_capacity = 0,
  maximum_well_capacity = 800,
  first_year_of_GW = 1997,
  last_year_of_GW = 2007,
  well_capacity_intervals = 20,
  capital_cost = (64 + 42 - 48) * 130,
  irrigation_season_days = 70
)
}
\arguments{
\item{subsidy_amount}{is the amount of subsidy per acre-inch of groundwater extracted. Defaults to 1.}

\item{subsidy_threshold}{is the threshold of subsidy, i.e., the amount of groundwater extraction above which subsidy is zero. Defaults to 400.}

\item{soil_weather_file}{is the file that contains the soil types and weather station for each well in the region. Defaults to "./input_files/Well_SoilType_WeatherStation.csv".}

\item{well_capacity_files}{is the directory where well capacity files are located. Defaults to "C:/Users/manirad/Downloads/test/Well Capacity".}

\item{econ_output_folder}{is the name of the folder that contains irrigated acres, irrigation, and profits for each well. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/KS_DSSAT_output.csv",}

\item{well_capacity_file_year}{is the name of the output file that contains irrigation for each well which will be used by MODFLOW. Defaults to "C:/Users/manirad/Downloads/test/KS_DSSAT_output.csv",}

\item{subregion_file}{is the subset of wells that the subsidy is applied to. The rest of the wells adjust their water use only due to changes in aquifer levels.}

\item{dryland_profit_file}{is the profit of dryland wells.}

\item{look_up_table_inside}{is the lookup table for wells that are inside  the selected policy area or are   affected by the policy.}

\item{look_up_table_outside}{is the lookup table for wells that are outside the selected policy area or are unaffected by the policy.}

\item{base_year_well_capacity}{is the base year well capacity.}

\item{which_year_well_capacity}{is the well capacity that determines which year the model is in.}

\item{first_year_of_simulation}{is the first year that the hydro-economic simulation starts. Defaults to 2000.}

\item{default_well_capacity_col_name}{is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.}

\item{missing_soil_types}{is the soil type that is assigned to missing soil types for wells. Defaults to "KS00000007".}

\item{minimum_well_capacity}{is the minimum well capacity in the model. If well capacity falls below this capacity, it is set to this minimum. Defaults to 100  gallons per minute.}

\item{maximum_well_capacity}{is the maximum well capacity in the model. If well capacity falls above this capacity, it is set to this maximum. Defaults to 3000 gallons per minute.}

\item{first_year_of_GW}{is the first year that the GW model exists. This may be different than the first year of simulation. Defaults to 1997.}

\item{last_year_of_GW}{is the last  year that the GW model exists. This may be different than the last  year of simulation. Defaults to 2007.}

\item{well_capacity_intervals}{is the intervals in well capacities. Defaults to 20.}

\item{capital_cost}{The cost of capital.}

\item{irrigation_season_days}{Number of days in an irrigation season. Defaults to 70.}
}
\value{
returns the output table.
}
\description{
This function produces annual amounts of water use and profits.
}
\examples{
\dontrun{
gen_lookup_subsidy(subsidy_amount = 2)
}
}
