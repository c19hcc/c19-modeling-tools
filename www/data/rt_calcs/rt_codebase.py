#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Project: C19HCC COVID-19 Modeling Dashboard and Tools
Purpose: App file for R shiny dashboard
Date: June 2020
Developers: Kristin Fitzgerald, Brendan Abraham
Copyright 2020, The MITRE Corporation
Approved for Public Release; Distribution Unlimited. Case Number 20-1521.

Modified from https://github.com/k-sys/covid-19/blob/master/Realtime%20Rt%20mcmc.ipynb
'''
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import os
import pymc3 as pm
import pandas as pd
import numpy as np
import theano.tensor as tt

# turn off theanos compile lock for faster processing
from theano.gof.compilelock import set_lock_status
set_lock_status(False)

from datetime import date

data_dir = os.getcwd()

def read_patient_data(filename = 'data/linelist.csv'):

    # Parse and clean patient data
    # Load the patient CSV
    patients = pd.read_csv(
        filename,
        parse_dates=False,
        usecols=[
            'date_confirmation',
            'date_onset_symptoms'],
        low_memory=False)
    
    patients.columns = ['Onset', 'Confirmed']
    
    # There's an errant reversed date
    patients = patients.replace('01.31.2020', '31.01.2020')
    
    # Only keep if both values are present
    patients = patients.dropna()
    
    # Must have strings that look like individual dates
    # "2020.03.09" is 10 chars long
    is_ten_char = lambda x: x.str.len().eq(10)
    patients = patients[is_ten_char(patients.Confirmed) & 
                        is_ten_char(patients.Onset)]
    
    # Convert both to datetimes
    patients.Confirmed = pd.to_datetime(
        patients.Confirmed, format='%d.%m.%Y')
    patients.Onset = pd.to_datetime(
        patients.Onset, format='%d.%m.%Y')
    
    patients = patients[patients.Confirmed >= patients.Onset]
    
    # Calculate the delta in days between onset and confirmation
    delay = (patients.Confirmed - patients.Onset).dt.days
    
    # Convert samples to an empirical distribution
    p_delay = delay.value_counts().sort_index()
    new_range = np.arange(0, p_delay.index.max()+1)
    p_delay = p_delay.reindex(new_range, fill_value=0)
    p_delay /= p_delay.sum()
    
    return(p_delay)
    

# Transfer confirmed dates to onset dates
def confirmed_to_onset(confirmed, p_delay):

    assert not confirmed.isna().any()
    
    # Reverse cases so that we convolve into the past
    convolved = np.convolve(confirmed[::-1].values, p_delay)

    # Calculate the new date range
    dr = pd.date_range(end=confirmed.index[-1],
                       periods=len(convolved))

    # Flip the values and assign the date range
    onset = pd.Series(np.flip(convolved), index=dr)
    
    return onset

# Adjust for right-censoring
def adjust_onset_for_right_censorship(onset, p_delay):
    cumulative_p_delay = p_delay.cumsum()
    
    # Calculate the additional ones needed so shapes match
    ones_needed = len(onset) - len(cumulative_p_delay)
    padding_shape = (0, ones_needed)
    
    # Add ones and flip back
    cumulative_p_delay = np.pad(
        cumulative_p_delay,
        padding_shape,
        'constant',
        constant_values=1)
    cumulative_p_delay = np.flip(cumulative_p_delay)
    
    # Adjusts observed onset values to expected terminal onset values
    adjusted = onset / cumulative_p_delay
    
    return adjusted, cumulative_p_delay

# Sample the Posterior with PyMC3
    
class MCMCModel(object):
    
    def __init__(self, region, onset, cumulative_p_delay, window=75):
        
        # Just for identification purposes
        self.region = region
        
        # For the model, we'll only look at the last N
        self.onset = onset.iloc[-window:]
        self.cumulative_p_delay = cumulative_p_delay[-window:]
        
        # Where we store the results
        self.trace = None
        self.trace_index = self.onset.index[1:]

    def run(self, chains=1, tune=3000, draws=1000, target_accept=.95):

        with pm.Model() as model:

            #i = pm.DiscreteUniformn(NUMBER OF SAMPLES IN TRACE)

            # Random walk magnitude
            step_size = pm.HalfNormal('step_size', sigma=.03) #choose ramdomly from all step sizes in previous trace, discrete uniform?

            # Theta random walk
            # Original code could go negative
            """
            theta_raw_init = pm.Normal('theta_raw_init', 0.1, 0.1)
            theta_raw_steps = pm.Normal('theta_raw_steps', shape=len(self.onset)-2) * step_size
            theta_raw = tt.concatenate([[theta_raw_init], theta_raw_steps])
            theta = pm.Deterministic('theta', theta_raw.cumsum())
            """
            # Theta random walk
            # New Version
            theta_raw_init = pm.Normal('theta_raw_init', 0.1, 0.1) # choose from random samples of starting point
            theta_raw_steps = pm.Normal('theta_raw_steps', shape=len(self.onset)-2) * step_size
            theta_raw = tt.concatenate([[theta_raw_init], theta_raw_steps])

            # Let the serial interval be a random variable and calculate r_t
            # Formerly 6, 1.5
            # Updated to alpha = m**2/sd**2, beta = m/sd**2
            # source: https://doi.org/10.1101/2020.02.19.20025452
            si_mean = 3.96
            si_sd = 0.215
            serial_interval = pm.Gamma('serial_interval', alpha=(si_mean**2/si_sd**2), beta=(si_mean/si_sd**2))
            gamma = 1.0 / serial_interval
            theta = pm.Deterministic('theta', -gamma+pm.math.abs_(theta_raw.cumsum()+gamma)) #ADDED HERE
            r_t = pm.Deterministic('r_t', theta/gamma + 1)

            inferred_yesterday = self.onset.values[:-1] / self.cumulative_p_delay[:-1]
            
            expected_today = inferred_yesterday * self.cumulative_p_delay[1:] * pm.math.exp(theta)

            # Ensure cases stay above zero for poisson
            mu = pm.math.maximum(.1, expected_today)
            observed = self.onset.round().values[1:]
            cases = pm.Poisson('cases', mu=mu, observed=observed)

            self.trace = pm.sample(
                chains=chains,
                tune=tune,
                draws=draws,
                target_accept=target_accept)
            
            return self
        
        
# Run PyMC3
            
def df_from_model(model):
    
    r_t = model.trace['r_t']
    mean = np.mean(r_t, axis=0)
    median = np.median(r_t, axis=0)
    hpd_90 = pm.stats.hpd(r_t, credible_interval=.9)
    hpd_50 = pm.stats.hpd(r_t, credible_interval=.5)
    
    idx = pd.MultiIndex.from_product([
            [model.region],
            model.trace_index
        ], names=['region', 'date'])
        
    df = pd.DataFrame(data=np.c_[mean, median, hpd_90, hpd_50], index=idx,
                 columns=['mean', 'median', 'lower_90', 'upper_90', 'lower_50','upper_50'])
    return df

def create_and_run_model(name, state, p_delay):
    confirmed = state.positive.diff().dropna()
    onset = confirmed_to_onset(confirmed, p_delay)
    adjusted, cumulative_p_delay = adjust_onset_for_right_censorship(onset, p_delay)
    days_from_march1 = (date.today() - date(2020,3,1)).days + 1
    return MCMCModel(name, onset, cumulative_p_delay, days_from_march1).run()

# Load State Data
def load_state_data(filename):
    #To use USAFacts instead of covid testing data
    
    # read in counts from usafacts and assume total reported positives = case counts
    #UPDATE DATA FILE WITH MOST RECENT FILE
    states = (pd.read_csv(filename, parse_dates=['date'])
               .assign(positive= lambda x: x.case_count))
    
    # only run on states with 40 days or more of data
    state_cts = states.groupby('mState.Providence').count()
    usable_state_names = set(state_cts.loc[state_cts['positive'] > 40,:].index.values)
    usable_states = states.loc[states['mState.Providence'].isin(usable_state_names),:]
    
    # create hierarchical index with county name and deates, and sort 
    usable_states = usable_states.set_index(['mState.Providence', 'date']).sort_index()
    usable_states.head()
    return(usable_states)
    
def run_national_model(state_filename, p_delay):
    states = load_state_data(state_filename)
    states = states[states['Country.Region']=='United States']
    # National Statistics
    nat_cts = states.groupby('date').sum()
    region = 'United States'
    nat_model = create_and_run_model(region, nat_cts, p_delay)
    nat_res = df_from_model(nat_model)
    
    nat_res.to_csv('{}/data/rt_national.csv'.format(data_dir), index=True)
    

# get list of eligible counties based on chosen criteria. Can either    
def get_eligible_counties(cty_data=None, data_fname=None, export=True,\
                          crit_col = 'case_count', cutoff = 50,):
    if cty_data is None:
        if data_fname is None: data_fname = '{}/data/database/county_inf_stats.csv'.format(data_dir)
        print(f'reading data from {data_fname}')
        cty_data =  pd.read_csv(data_fname, parse_dates=['date']).assign(positive= lambda x: x.case_count)
    crit_col = crit_col.lower()
    assert crit_col in ['case_count', 'death_count', 'date']
    county_cts = cty_data.groupby('mState.Providence').agg({'case_count':'max', 'date':len, 'death_count':'max'})
    elig_counties = sorted(list(county_cts[county_cts[crit_col] >= cutoff].index.values))
    if export:
        fname = '/'.join([data_dir, 'data/rt_calcs/county_list_clean.txt'])
        with open(fname, 'w') as out:
            [out.write(c + '\n') for c in elig_counties]
    return set(elig_counties)


#def run_states_model(filename, p_delay, region=None):
#    usable_states = load_state_data(filename)
#    
#    models = {}
#
#    state = region
#
#    dat = usable_states[usable_states.index.get_level_values(0) == state]
#    
#    try:
#        models[state] = create_and_run_model(state, dat.droplevel(0), p_delay)
#    except:
#        pass
#        
#    # Check to see if there were divergences
#    n_diverging = lambda x: x.trace['diverging'].nonzero()[0].size
#    divergences = pd.Series([n_diverging(m) for m in models.values()], index=models.keys())
#    has_divergences = divergences.gt(0)
#    
#    #print('Diverging states:')
#    #display(divergences[has_divergences])
#    
#    # Rerun states with divergences
#    for state, n_divergences in divergences[has_divergences].items():
#        models[state].run()
#    
#    results = None
#
#    for state, model in models.items():
#    
#        try:
#            df = df_from_model(model)
#    
#            if results is None:
#                results = df
#            else:
#                results = pd.concat([results, df], axis=0)
#            
#        except:
#            print('error getting results for {}'.format(state))
#            
#    results.to_csv('{0}/data/rt_states_{1}.csv'.format(data_dir, state), index=True)