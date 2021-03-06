% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_output_tax.R
\name{gen_output_tax}
\alias{gen_output_tax}
\title{This function produces annual well capacity for each well in each year.}
\usage{
gen_output_tax(
  tax_amount = 1,
  well_capacity_files = "./Well Capacity",
  well_capacity_for_econ = "./Econ_output/well_capacity.csv",
  first_year_of_simulation = 2000,
  default_well_capacity_col_name = "Well_Capacity(gpm)"
)
}
\arguments{
\item{tax_amount}{is the amount of tax on the unit of groundwater extracted. Defaults to 1.1.}

\item{well_capacity_for_econ}{is the file name that all the output well capacities are written into. Defaults to "C:/Users/manirad/Downloads/test/Econ_output/well_capacity.csv".}

\item{first_year_of_simulation}{is the first year that the hydro-economic simulation starts. Defaults to 2000.}

\item{default_well_capacity_col_name}{is the name of the well capacity column generated from the MODFLOW model. Defaults to 'Well_Capacity(gpm)' as this is the original column name we started with.}
}
\value{
returns the output table.
}
\description{
This function produces annual well capacity for each well in each year.
}
\examples{
\dontrun{
gen_lookup_tax(tax_amount = 2)
}
}
