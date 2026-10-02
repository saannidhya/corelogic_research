# Welfare external inputs

`welfare_cpi.csv` contains the 12 monthly observations for 2017 and 2023 from
the BLS CPI-U, all items, U.S. city average, not seasonally adjusted
(1982–1984 = 100). Source checked September 21, 2026:
https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexhistorical_us_table.htm

`11_welfare.R` averages each year's monthly observations and rounds the
annual index to three decimals: 245.120 and 304.702. The conversion to
2023 dollars is 304.702 / 245.120 = 1.24307278067885. This is a price-level
conversion, separate from the real discount rate and discount-date origin.

The annual tax-saving benchmark of $3,000–$4,000 comes from the California
Legislative Analyst's Office, *The Property Tax Inheritance Exclusion*,
October 9, 2017: https://lao.ca.gov/Publications/Report/3706.
It describes long-held Los Angeles homes, not the statewide marginal
properties responding to Proposition 19. The source does not specify the
dollar vintage; treating it as 2017 dollars is an explicit assumption.
The $3,500 midpoint and $1,750/$7,000 stress cases are calibration choices.
The source's separate 60,000–80,000 figure is an annual property count.

No CoreLogic market valuation is imputed from these external benchmarks.
The generated `welfare_external_inputs.csv` records source URLs and status;
the manuscript input table separates these benchmarks from empirical
estimates and assumptions.
