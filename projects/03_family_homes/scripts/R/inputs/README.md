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

## Prop 13 inflation factors

`ca_boe_inflation_factors.csv` lists the final California inflation factors
applied to base-year values under Revenue and Taxation Code section 51
(capped at 2 percent). `roll_year` is the calendar year of the January 1
lien date; `fiscal_year` is the assessment roll it governs. Values were
transcribed on October 2, 2026 from the enclosure "Final Inflation Factors
for Current and Prior Years" in BOE Letter To Assessors No. 2023/056
(December 22, 2023): https://www.boe.ca.gov/proptaxes/pdf/lta23056.pdf.

`04a_build_wedge.R` rolls a base value established in year `b` forward to
lien year `Y` by multiplying the factors for roll years `b+1, ..., Y`. It
asserts every factor is at most 1.02.
