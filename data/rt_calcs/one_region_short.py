#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 24 12:25:11 2020

@author: babraham
"""

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May 20 15:58:30 2020

@author: kafitzgerald

Modified from https://github.com/k-sys/covid-19/blob/master/Realtime%20Rt%20mcmc.ipynb

"""
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import sys
import os
import requests
import theano
import theano.tensor as tt
import pandas as pd
import numpy as np

from datetime import date

import io

from rt_codebase import *

data_dir = os.getcwd()



## Transfer confirmed dates to onset dates
#def confirmed_to_onset(confirmed, p_delay):
#
#    assert not confirmed.isna().any()
#    
#    # Reverse cases so that we convolve into the past
#    convolved = np.convolve(confirmed[::-1].values, p_delay)
#
#    # Calculate the new date range
#    dr = pd.date_range(end=confirmed.index[-1],
#                       periods=len(convolved))
#
#    # Flip the values and assign the date range
#    onset = pd.Series(np.flip(convolved), index=dr)
#    
#    return onset
#
## Adjust for right-censoring
#def adjust_onset_for_right_censorship(onset, p_delay):
#    cumulative_p_delay = p_delay.cumsum()
#    
#    # Calculate the additional ones needed so shapes match
#    ones_needed = len(onset) - len(cumulative_p_delay)
#    padding_shape = (0, ones_needed)
#    
#    # Add ones and flip back
#    cumulative_p_delay = np.pad(
#        cumulative_p_delay,
#        padding_shape,
#        'constant',
#        constant_values=1)
#    cumulative_p_delay = np.flip(cumulative_p_delay)
#    
#    # Adjusts observed onset values to expected terminal onset values
#    adjusted = onset / cumulative_p_delay
#    
#    return adjusted, cumulative_p_delay
#
## Sample the Posterior with PyMC3
#    
#class MCMCModel(object):
#    
#    def __init__(self, region, onset, cumulative_p_delay, window=75):
#        
#        # Just for identification purposes
#        self.region = region
#        
#        # For the model, we'll only look at the last N
#        self.onset = onset.iloc[-window:]
#        self.cumulative_p_delay = cumulative_p_delay[-window:]
#        
#        # Where we store the results
#        self.trace = None
#        self.trace_index = self.onset.index[1:]
#
#    def run(self, chains=1, tune=3000, draws=1000, target_accept=.95):
#
#        with pm.Model() as model:
#
#            #i = pm.DiscreteUniformn(NUMBER OF SAMPLES IN TRACE)
#
#            # Random walk magnitude
#            step_size = pm.HalfNormal('step_size', sigma=.03) #choose ramdomly from all step sizes in previous trace, discrete uniform?
#
#            # Theta random walk
#            # Original code could go negative
#            """
#            theta_raw_init = pm.Normal('theta_raw_init', 0.1, 0.1)
#            theta_raw_steps = pm.Normal('theta_raw_steps', shape=len(self.onset)-2) * step_size
#            theta_raw = tt.concatenate([[theta_raw_init], theta_raw_steps])
#            theta = pm.Deterministic('theta', theta_raw.cumsum())
#            """
#            # Theta random walk
#            # New Version
#            theta_raw_init = pm.Normal('theta_raw_init', 0.1, 0.1) # choose from random samples of starting point
#            theta_raw_steps = pm.Normal('theta_raw_steps', shape=len(self.onset)-2) * step_size
#            theta_raw = tt.concatenate([[theta_raw_init], theta_raw_steps])
#
#            # Let the serial interval be a random variable and calculate r_t
#            # Formerly 6, 1.5
#            # Updated to alpha = m**2/sd**2, beta = m/sd**2
#            # source: https://doi.org/10.1101/2020.02.19.20025452
#            si_mean = 3.96
#            si_sd = 0.215
#            serial_interval = pm.Gamma('serial_interval', alpha=(si_mean**2/si_sd**2), beta=(si_mean/si_sd**2))
#            gamma = 1.0 / serial_interval
#            theta = pm.Deterministic('theta', -gamma+pm.math.abs_(theta_raw.cumsum()+gamma)) #ADDED HERE
#            r_t = pm.Deterministic('r_t', theta/gamma + 1)
#
#            inferred_yesterday = self.onset.values[:-1] / self.cumulative_p_delay[:-1]
#            
#            expected_today = inferred_yesterday * self.cumulative_p_delay[1:] * pm.math.exp(theta)
#
#            # Ensure cases stay above zero for poisson
#            mu = pm.math.maximum(.1, expected_today)
#            observed = self.onset.round().values[1:]
#            cases = pm.Poisson('cases', mu=mu, observed=observed)
#
#            self.trace = pm.sample(
#                chains=chains,
#                tune=tune,
#                draws=draws,
#                target_accept=target_accept)
#            
#            return self
#        
#        
## Run PyMC3
#            
#def df_from_model(model):
#    
#    r_t = model.trace['r_t']
#    mean = np.mean(r_t, axis=0)
#    median = np.median(r_t, axis=0)
#    hpd_90 = pm.stats.hpd(r_t, credible_interval=.9)
#    hpd_50 = pm.stats.hpd(r_t, credible_interval=.5)
#    
#    idx = pd.MultiIndex.from_product([
#            [model.region],
#            model.trace_index
#        ], names=['region', 'date'])
#        
#    df = pd.DataFrame(data=np.c_[mean, median, hpd_90, hpd_50], index=idx,
#                 columns=['mean', 'median', 'lower_90', 'upper_90', 'lower_50','upper_50'])
#    return df
#
#def create_and_run_model(name, state, p_delay):
#    confirmed = state.positive.diff().dropna()
#    onset = confirmed_to_onset(confirmed, p_delay)
#    adjusted, cumulative_p_delay = adjust_onset_for_right_censorship(onset, p_delay)
#    days_from_march1 = (date.today() - date(2020,3,1)).days + 1
#    return MCMCModel(name, onset, cumulative_p_delay, days_from_march1).run()
#
## Load State Data
#def load_state_data(filename):
#    #To use USAFacts instead of covid testing data
#    
#    # read in counts from usafacts and assume total reported positives = case counts
#    #UPDATE DATA FILE WITH MOST RECENT FILE
#    states = (pd.read_csv(filename, parse_dates=['date'])
#               .assign(positive= lambda x: x.case_count))
#    
#    # only run on states with 40 days or more of data
#    state_cts = states.groupby('mState.Providence').count()
#    usable_state_names = set(state_cts.loc[state_cts['positive'] > 40,:].index.values)
#    usable_states = states.loc[states['mState.Providence'].isin(usable_state_names),:]
#    
#    # create hierarchical index with county name and deates, and sort 
#    usable_states = usable_states.set_index(['mState.Providence', 'date']).sort_index()
#    usable_states.head()
#    return(usable_states)
    
#def run_national_model(state_filename, p_delay):
#    states = load_state_data(state_filename)
#    states = states[states['Country.Region']=='United States']
#    # National Statistics
#    nat_cts = states.groupby('date').sum()
#    region = 'United States'
#    nat_model = create_and_run_model(region, nat_cts, p_delay)
#    nat_res = df_from_model(nat_model)
    
#    nat_res.to_csv('{}/data/rt_national.csv'.format(data_dir), index=True)
    
def run_states_model(filename, p_delay, region):
    usable_states = load_state_data(filename)
    
    models = {}

    state = region
    
    dat = usable_states[usable_states.index.get_level_values(0) == state]
    
    try:
        models[state] = create_and_run_model(state, dat.droplevel(0), p_delay)
    except:
        pass
        
    # Check to see if there were divergences
    n_diverging = lambda x: x.trace['diverging'].nonzero()[0].size
    divergences = pd.Series([n_diverging(m) for m in models.values()], index=models.keys())
    has_divergences = divergences.gt(0)
    
    #print('Diverging states:')
    #display(divergences[has_divergences])
    
    # Rerun states with divergences
    for state, n_divergences in divergences[has_divergences].items():
        models[state].run()
    
    results = None

    for state, model in models.items():
    
        try:
            df = df_from_model(model)
    
            if results is None:
                results = df
            else:
                results = pd.concat([results, df], axis=0)
            
        except:
            print('error getting results for {}'.format(state))
            
    results.to_csv('{0}/data/rt_states_{1}.csv'.format(data_dir, state), index=True)
    
    
def run_counties_model(filename, p_delay, region):
    # read in county counts from usafacts and assume total reported positives = case counts
    #UPDATE DATA FILE WITH MOST RECENT FILE
    counties = pd.read_csv(filename, parse_dates=['date']).assign(positive= lambda x: x.case_count)
    
    # only run on counties with 40 days or more of data
    county_cts = counties.groupby('mState.Providence').count()
    usable_county_names = set(county_cts.loc[county_cts['positive'] > 40,:].index.values)
    usable_counties = counties.loc[counties['mState.Providence'].isin(usable_county_names),:]
    
    # create hierarchical index with county name and deates, and sort 
    usable_counties = usable_counties.set_index(['mState.Providence', 'date']).sort_index()
    #usable_counties.head()

    county = region

    dat = usable_counties[usable_counties.index.get_level_values(0) == county]
    
    county_models = {}

    try:
        county_models[county] = create_and_run_model(county, dat.droplevel(0), p_delay)
    except:
        pass
        
    # Check to see if there were divergences
    n_diverging = lambda x: x.trace['diverging'].nonzero()[0].size
    cty_divergences = pd.Series([n_diverging(m) for m in county_models.values()], index=county_models.keys(), dtype='int')
    has_divergences = cty_divergences.gt(0)
    
    #print('Diverging counties:')
    #display(cty_divergences[has_divergences])
    
    # Rerun counties with divergences
    for county, n_divergences in cty_divergences[has_divergences].items():
        county_models[county].run() 
        
    cty_results = None

    for i, (county, model) in enumerate(county_models.items()):
        try:
            df = df_from_model(model)
    
            if cty_results is None:
                cty_results = df
            else:
                cty_results = pd.concat([cty_results, df], axis=0)
        except:
            print('error getting results for {}'.format(county))
    
    county_name = county.replace(',', '').replace(' ','')
    if type(cty_results) != type(None):
        cty_results.to_csv('{0}/data/rt_calcs/data/results/rt_counties_{1}.csv'.format(data_dir, county_name), index=True)
    else:
        print("ERROR: No results for {}".format(region))

if __name__ == '__main__':
    
    print(sys.argv)
    if len(sys.argv) == 2:
        region = sys.argv[1]
    else: 
        raise Exception('Please enter a valid argument')
    
    p_delay = read_patient_data('{}/data/rt_calcs/data/linelist.csv'.format(data_dir))
    
    if region == 'United States':
        run_national_model('{}/data/database/state_inf_stats.csv'.format(data_dir), p_delay)
    elif ',' in region:
        run_counties_model('{}/data/database/county_inf_stats.csv'.format(data_dir), p_delay, region)
    else: 
        run_states_model('{}/data/database/state_inf_stats.csv'.format(data_dir), p_delay, region)

