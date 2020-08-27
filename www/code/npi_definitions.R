##################################################
## Project: C19HCC COVID-19 Modeling Dashboard and Tools
## Purpose: NPI Definitions for tabular view
## Date: June 2020
## Developers: Brendan Abraham, Kristin Fitzgerald, Kyle Furlong, Dr. Chris Glazner
## Copyright 2020, The MITRE Corporation
## Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
##################################################

NPI_DEFS <- data.frame(
  Policy = c("Bar/Restaurant Limits", 
             "Large Gatherings Ban",
             "Mandatory Quarantine for Travelers",
             "Non-Essential Business Closures", 
             "State-Mandated School Closures", 
             "Stay At Home Order", 
             "Mandatory Masks"),
  Description = c("Closure of bars and restaurants except for takeout/delivery, or limited to on-site service with social distancing restrictions.",
                  "Restrictions on the number of people allowed to gather together in one place; the size of group/number of people varies by state, but is often 10 or more per CDC guidance.",
                  "Typically a requirement for people who travel from outside the state to quarantine for 14 days. Specifics vary by state.",
                  "The physical closure all non-essential businesses or all non-essential retail businesses (as defined by state government). Employees are permitted to work remotely if possible.",
                  "Public and private K-12 schools are physically closed, either by state-mandate or due to Stay-at-Home order.",
                  "Statewide order or advisory for residents to remain at his or her place of residence except to perform essential activities such as grocery shopping. Implementation and enforcement varies by state.",
                  "Residents are required to wear masks in public areas where social distancing is not achievable")
)