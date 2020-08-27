---
title: "Sources"
bibliography: references.bib
output:
  html_document:
    df_print: paged
    css: ["custom_style.css", "rrd2zlt.css"]
---

<style type="text/css">
.main-container {
  max-width: 1800px;
  margin-left: 120px;
  margin-right: 120px;
}
</style>

## Dashboard Data Sources

The data used in the various dashboard views comes from a variety of sources, listed in Table 1.

Data Element(s)                                                 Source       
-------------------------------------------------------         -------------------------------------
United States County-Level COVID-19 Case and Death Counts       [USAFacts](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/)
International COVID-19 Case and Death Counts                    [Johns Hopkin's COVID data repository](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/) [@jhu]
U.S. COVID-19 Testing and Hospitalizations                      [COVID Tracking Project](https://covidtracking.com/) [@covid_tracking]
CDC NHSN COVID-19 Hospitalization Estimates                     [CDC NHSN COVID-19 Module](https://www.cdc.gov/nhsn/covid19/report-overview.html)
Hospital Bed Counts, ICU Bed Counts, and Bed Utilization        [Definitive Healthcare dashboard](https://coronavirus-disasterresponse.hub.arcgis.com/datasets/definitivehc::definitive-healthcare-usa-hospital-beds?geometry=52.207%2C-16.820%2C-77.168%2C72.123) [@def_hc_hosp_beds]
Mobility                                                        [Descartes Labs](https://github.com/descarteslabs/DL-COVID-19) [@descartes_labs]
U.S. State Non-Pharmaceutical Interventions (NPIs)              Originally derived from the [Kaiser Family Foundation's Health Cost Website](https://www.kff.org/health-costs/issue-brief/state-data-and-policy-actions-to-address-coronavirus/)  [@kff_npi_data], but is now being updated by MITRE from various local news sources.
Reproduction Rate                                               Calculated by MITRE using a modified version of the script found at [Rt.live](https://rt.live/)
International Non-Pharmaceutical Interventions (NPIs)           Collected from a variety of local and global news sources including Reuters, the BBC, the Washington Post, and the Wold Health Organization. A complete list of sources is available upon request.
United States Population and Population Density, Median Income  [U.S. Census Bureau](https://www.census.gov/)
International Populaiton                                        [Worldometers](https://www.worldometers.info/world-population/population-by-country/) [@world_pops]
Demographic Similarity Features                                 [HRSA Area Health Resource Files (AHRF) and Health Porfesional Shortage Areas (HPSA) Files](https://data.hrsa.gov/data/download); [American Community Survey](https://www.census.gov/programs-surveys/acs/data.html); [National Center for Migrant Health](http://www.ncfh.org/population-estimates.html); [Medicare Long Term Care Providers](https://data.medicare.gov/Nursing-Home-Compare/Provider-Info/4pq5-n9py)

Table 1: Dashboard data sources

## Description and sources for SEIR Model simulation parameters

### Model structure

The basic model structure is inspired by many studies on the natural clinical progression of COVID-19 infection. For a nice summary, see [@wu_characteristics_2020]. Infected individuals do not immediately develop severe symptoms, but instead pass through milder phases of infection first. In some studies, what we call *mild* infections are grouped into two different categories, *mild* and *moderate*, where individuals with *moderate* infection show radiographic signs of mild pneumonia. These *mild* and *moderate* cases occur at roughly equal proportions (for ex. see @yang_epidemiological_2020). There is some debate about the role of pre-symptomatic transmission (occurring from exposed stage) and asymptomatic infection and transmission for COVID-19. The current model version does not include these effects. 

### Dynamic model parameters

The behavior of the dynamic model is determined by a set of rate parameters, including the transmission rates $\beta_i$, the progression rates $a$ and $p_i$, the recovery rates $\gamma_i$, and the death rate $\mu$. While these rates themselves are not generally measured directly in studies, other quantities that are measurable can be used to back out these rate parameters. 

The time spent in the exposed class is called the *incubation period* and is generally assumed to be equal to the time between exposure to an infected source and the development of symptoms. In the model the average incubation period is $1/a$. 

The *infectious period* is the time during which an individual can transmit to others. In our model there are potentially three different infectious periods, occurring during each clinical stage of infection ($I_1$, $I_2$, $I_3$). We need to know the duration of each of these stages. We think it's likely that an individual is most infectious during the stage of mild infection, when they would still be in the community and feeling well enough to interact with others, but in the model there is also the option for transmission in the other stages, for example transmission from hospitalized patients to their healthcare providers. At a population level, we expect most transmission to occur from these individuals with mild infection, since most patients do not progress past this stage.  For COVID-19 we can estimate the duration of the first stage from a) the duration of mild symptoms, b) the time from symptom onset to hospitalization (e.g. progress to severe stage), or c) the duration of viral shedding via sputum or throat swabs, d) the serial interval between symptom onset in an index case and a secondary case they infect. In the model the quantities a)-c) equal $1/(p_1 + \gamma_1)$, whereas d) is $1/a + (1/2) 1/(p_1 + \gamma_1)$. These estimates converge on similar values for $p_1+\gamma_1$.  The probability of progressing to the *severe* stage is equal to the proportion of all infections that end up being either severe or critical, and must equal the parameter combination $p_1/(p_1+\gamma_1)$. 

Individuals with severe infection ($I_2$) require hospitalization. The duration of severe infections, which could be reported as the time from hospital admission to recover for individuals who did not progress to the critical stage, or the time from hospital admission to ICU admission (since critical cases require ICU-level care), corresponds to the model parameters $1/(p_2+\gamma_2)$. Since there are not direct estimates of this duration, we instead used estimates of the total time from symptom onset to ICU-admission (e.g. combined length of mild + severe infection), and subtracted the inferred duration of mild infection. Then we used the observed probability of progressing to critical infection, equal to the ratio of critical to critical + severe infections, which equals $p_2/(p_2 + \gamma_2$), to separately work out $p_2$ and $\gamma_2$. At the critical infection stage ($I_3$) ICU care, generally with mechanical ventilation, is required. The duration of this stage of infection, e.g. the time from ICU admission to recovery or death, is equal to $1/(\gamma_3 + \mu$) but not often reported. Instead, studies often report the total time from hospital admission to death, which can approximate the sum of the duration of the severe and critical stages. Thus by subtracting the duration of $I_2$, the duration of $I_3$ can be estimated. The observed case fatality ratio (CFR) describes the fraction of all symptomatic infected individuals who eventually die. Since individuals must progress to critical infection to die, the conditional probability of someone in the critical stage dying vs recovering is given by the CFR divided by the fraction of all infections that are severe. This must equal the model parameter combination $\mu/(\gamma_3 + \mu)$. 

Table 2 summarizes the literature sources we used to estimate default values for all these model parameters. Users can choose their own values based on other studies or particular regional contexts. 

Quantity                                  Parameter                                  Value        Source       
--------------------------------------    -------------------------------------      ----------   --------------------------------------------------
Incubation Period                         $1/a$                                      5.1 days     [@lauer_the_2020 ; @linton_incubation_2020; @lauer_incubation_2020; @bi_epidemiology_2020; @sanche_novel_2020]
Proportion of mild infections             $\gamma_1/(p_1+\gamma_1)$                  66%          [@CDCCOVID-19ResponseTeam2020; @wu_characteristics_2020; @yang_epidemiological_2020; @liu_nl_2020]
Duration of mild infections               $1/(p_1+\gamma_1)$                         7 days       [@wang_clinical_2020; @woelfel_clinical_2020], Time from symptoms to hospitalization: [@sanche_novel_2020; @tindale_transmission_2020]
Proportion of severe infections           $\gamma_1/(p_1+\gamma_1)$                  28%          [@CDCCOVID-19ResponseTeam2020; @wu_characteristics_2020; @yang_epidemiological_2020]
Time from symptoms to ICU admission       --                                         10.5 days    [@huang_clinical_2020; @yang_clinical_2020; @liu_nl_2020]
Duration of severe infection              $1/(p_2+\gamma_2)$                         3.5 days     [Time from symptoms to ICU admit] - [Duration of mild infections]
Proportion of critical infections         $\%Severe\times p_2/(p_2+\gamma_2)$        6%           [@CDCCOVID-19ResponseTeam2020; @wu_characteristics_2020; @yang_epidemiological_2020; @liu_nl_2020]
Time from hospital admission to death     --                                         11 days      [@sanche_novel_2020; @linton_incubation_2020]
During of critical infection              $1/(\mu+\gamma_3)$                         7.5 days     [Time from hospital admit to death] - [Duration of severe infections]
Case fatality ratio                       $\%Crit\times\mu/(\mu+\gamma_3)$           2.6%         [@baud_real_2020; @wu_characteristics_2020; @russell_estimating_2020; @riou_adjusted_2020]
Symptomatic infection, transmission rate  $b1$                                       0.45%        [@Burke2020]
Severe infection transmission rate        $1/DurMildInf - g1$                        6.2%         [p1 = (1/DurMildInf) - g1 = (1/DurMildInf) - FracMild / DurMildInf = 1/7 - 0.566/7 = 0.062]
Critical infection transmission rate      $1/DurHosp\times(\%Crit/(%Severe+\%Crit))$ 1.6%         [p2 = 1/DurHosp * (FracCrit/(FracSevere+FracCrit)) = 1/11 * (0.06/(0.28+0.06)) = 0.01604]
Probability of death, critical infections $1/TimeICUDeath\times CFR/\%Crit$          5.8%         [u = ProbDeath = 1/TimeICUDeath * CFR/FracCrit = 1/7.5 * 0.026/0.06 = 0.05778]

Table 2:  Estimated parameters for COVID-19 clinical progression, and literature sources

The rates of transmission are generally impossible to directly observe or estimate. Instead, these values can be backed out by looking at the early exponential growth rate ($r$) of an epidemic and choosing transmission rates that recreate these observations. The growth of COVID-19 outbreaks has varied a lot between settings and over time. Some values reported in the literature are in Table 3. Real-time automated calculation of growth rates for different countries is available at [CITE]. The default values for the simulation are currently set to match a situation with $r=$ [ADDDD]. As a default we assume that only $\beta_1 > 0$ (e.g. no hospital transmission).

Growth rate $r$   Doubling time  Location        Dates           Source
---------------- --------------  --------------  --------------- -------------------------
0.1               6.9 days        Wuhan           Early January   [@li_early_2020]
0.25              2.8 days        Wuhan           January         [@zhao_analysis_2020]
0.3               2.3 days        Wuhan           January         [@sanche_novel_2020]
0.5               1.4 days        Italy           Feb 24          [@abbott_temporal_2020]
0.17              4.1 days        Italy           Mar 9           [@abbott_temporal_2020]
0.3               2.3 days        Iran            Mar 2           [@abbott_temporal_2020]
0.5               1.4 days        Spain           Feb 29          [@abbott_temporal_2020]
0.2               3.5 days        Spain           Mar 9           [@abbott_temporal_2020]
0.2               3.5 days        France          Mar 9           [@abbott_temporal_2020]
0.2               3.5 days        South Korea     Feb 24          [@abbott_temporal_2020]
0.5               1.4 days        UK              Mar 2           [@abbott_temporal_2020]

Table 3: Observed early epidemic growth rates $r$ across different settings, along with the corresponding doubling times. There are many other settings where growth rates are now close to zero. 

### Hospital capacity parameters

One of the biggest dangers of a widespread COVID-19 epidemic is the strain it could place on hospital resources, since individuals with severe and critical infection require hospital care. The critical stage of infection requires mechanical ventilation, which is ICU-level care. Severe infection can be treated in a regular hospital ward. Individuals with mild infection do not require hospitalization, and could recover at home on their own. However, in many countries these individuals have also been hospitalized, likely as a way to isolate them and reduce transmission, as well as to monitor them for progression to more aggressive disease stages. 

The default hospital capacity parameters are estimated for the US, and are expressed as resources per capita. The available hospital beds (in regular wards or on ICU floors) depends on the total number of beds that exist as well as the occupancy level. During flu season (winter months), occupancy levels are generally higher. We report the number of *available* (e.g. unoccupied) beds of both types (Table 4). When deployed, our model sets these parameters (available hospital beds, available ICU beds based, and bed utilization) at the state-level based on real data from Definitive Healthcare's Foundation's hospital resource database [@def_hc_hosp_beds]. 
Studies in the pandemic preparedness literature have examined how the capacity to delivery mechanical ventilation during a respiratory pathogen outbreak could be expanded beyond the traditional ICU-bed capacity (aka *conventional capacity*) by using stockpiled ventilators, non-specialist hospital staff, and retro-fitting other hospital rooms [@ajao_assessing_2015]. These expanded delivery levels are called *contingency* and *crisis* capacity. 


Quantity                                        Total      Per 1000 ppl    Source
---------------------------------------------   ---------  -------------   -----------------------
Hospital beds                                   900,000     2.8            [@nchs_table_2017]
Occupancy                                       66%                        [@nchs_table_2017]
ICU beds                                        80,000      0.26           [@sccm_2010]
Occupancy                                       68%                        [@sccm_2010]
$\uparrow$ flu season occupancy                  7%                         [@ajao_assessing_2015]
Available hospital beds                         264,000     0.82           From above     
Available ICU beds                              22,000      0.071          From above  
Conventional Mechanical Ventilation Capacity    20,000      0.062          [@ajao_assessing_2015]
Contingency Mechanical Ventilation Capacity     50,000      0.15           [@ajao_assessing_2015]
Critical Mechanical Ventilation Capacity        135,000     0.24           [@ajao_assessing_2015]


Table 4: US hospital capacity. Values are for adults beds only. 

#### Data-Driven Model Parameters
While most model parameters are based on estimates from the literature, some are based on the most empirical data about on the spread of the virus and national resources cited above. Specifically, our model sets the following parameters for a given state based on empirical data: 

Quantity                                        Source
---------------------------------------------   -----------------------
Hospital beds                                   [@def_hc_hosp_beds]
Occupancy                                       [@def_hc_hosp_beds]
ICU beds                                        [@def_hc_hosp_beds]
Occupancy                                       [@def_hc_hosp_beds]
Occupancy from COVID-related cases              [@cdc_nhsn]
Population Size                                 [@state_pops]
First Infection Date                            [@usfacts]
Number of Initially Infected                    [@usfacts]
Country Populations                             [@world_pops]

Additionally, the model takes in a list of non-pharmaceutical interventions to take which directly impact the growth rate. These NPIs are pulled directly from Kaiser Family Foundation's running list of NPIs taken by each state [@kff_npi_data].



### References



