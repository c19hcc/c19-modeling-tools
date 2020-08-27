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
import pymc3 as pm
import pandas as pd
import numpy as np
import theano
import theano.tensor as tt

from datetime import date
from datetime import datetime

from IPython.display import clear_output

import io

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

    #prev_param_mat = [si_mean, ss_mean, tmean, cov_mat1, cov_mat2, cov_mat3, cov_mat4, cov_mat5, cov_mat6] (one-dim list)
    def run(self, chains=1, tune=3000, draws=1000, target_accept=.95, prev_param_mat = None): 

        with pm.Model() as model:

            if prev_param_mat is None:
                # Random walk magnitude
                step_size = pm.HalfNormal('step_size', sigma=.03) #choose ramdomly from all step sizes in previous trace, discrete uniform?
                theta_raw_init = pm.Normal('theta_raw_init', 0.1, 0.1) # choose from random samples of starting point
                serial_interval = pm.Gamma('serial_interval', alpha=6, beta=1.5)
            else:
                cov_mat = np.zeros(3,3)
                cov_mat[0,0] = prev_param_mat[3]
                cov_mat[0,1] = prev_param_mat[4]
                cov_mat[0,2] = prev_param_mat[5]
                cov_mat[1,0] = prev_param_mat[4]
                cov_mat[1,1] = prev_param_mat[6]
                cov_mat[1,2] = prev_param_mat[7]
                cov_mat[2,0] = prev_param_mat[5]
                cov_mat[2,1] = prev_param_mat[7]
                cov_mat[2,2] = prev_param_mat[8]

                vec = pm.MvNormal(mu = prev_param_mat[0:3], cov = cov_mat)

                serial_interval = pm.Deterministic('serial_interval', pm.math.maximum(vec[0],0))
                step_size = pm.Deterministic('step_size', pm.math.maximum(vec[1],0))
                theta_raw_init = pm.Deterministic('theta_raw_init', vec[2])


            # Theta random walk
            # New Version
            theta_raw_steps = pm.Normal('theta_raw_steps', shape=len(self.onset)-2) * step_size
            theta_raw = tt.concatenate([[theta_raw_init], theta_raw_steps])

            # Let the serial interval be a random variable and calculate r_t
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

    serial_interval = model.trace.get_values('serial_interval')
    si_mean = np.mean(serial_interval)*np.ones(mean.shape)
    step_size = model.trace.get_values('step_size')
    ss_mean = np.mean(step_size)*np.ones(mean.shape)
    theta = model.trace.get_values('theta')
    tmean = np.mean(theta, axis=0)


    cov_mat1 = np.ones(mean.shape)
    cov_mat2 = np.ones(mean.shape)
    cov_mat3 = np.ones(mean.shape)
    cov_mat4 = np.ones(mean.shape)
    cov_mat5 = np.ones(mean.shape)
    cov_mat6 = np.ones(mean.shape)
    for i in range(len(mean)):
        cov_mat = np.cov(np.vstack((serial_interval, step_size, theta[:,i])))
        cov_mat1[i] = cov_mat[0,0]
        cov_mat2[i] = cov_mat[0,1]
        cov_mat3[i] = cov_mat[0,2]
        cov_mat4[i] = cov_mat[1,1]
        cov_mat5[i] = cov_mat[1,2]
        cov_mat6[i] = cov_mat[2,2]

    idx = pd.MultiIndex.from_product([
            [model.region],
            model.trace_index
        ], names=['region', 'date'])
        
    df = pd.DataFrame(data=np.c_[mean, median, hpd_90, hpd_50, si_mean, ss_mean, tmean, 
        cov_mat1, cov_mat2, cov_mat3, cov_mat4, cov_mat5, cov_mat6], index=idx,
        columns=['mean', 'median', 'lower_90', 'upper_90', 'lower_50','upper_50', 
        'si_mean', 'ss_mean', 'tmean', 'cov_mat1', 'cov_mat2', 'cov_mat3', 'cov_mat4', 'cov_mat5', 'cov_mat6'])

    return df

def create_and_run_model(name, state):
    confirmed = state.positive.diff().dropna()
    onset = confirmed_to_onset(confirmed, p_delay)
    adjusted, cumulative_p_delay = adjust_onset_for_right_censorship(onset, p_delay)
    return MCMCModel(name, onset, cumulative_p_delay).run()

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
    
def run_national_model(state_filename = 'data/state_inf_stats.csv'):
    states = load_state_data(state_filename)
    # National Statistics
    nat_cts = states.groupby('date').sum()
    region = 'United States'
    nat_model = create_and_run_model(region, nat_cts)
    nat_res = df_from_model(nat_model)
    
    nat_res.to_csv('{}/data/rt_calcs/data/rt_national_test.csv'.format(data_dir), index=True)
    
def run_states_model(filename = 'data/state_inf_stats.csv'):
    usable_states = load_state_data(filename)
    
    models = {}
    
    for state, grp in usable_states.groupby('mState.Providence'):
    
        if state in models:
            print(f'Skipping {state}, already in cache')
            continue
    
        print(state)
        try:
            models[state] = create_and_run_model(state, grp.droplevel(0))
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
            
    results.to_csv('{}/data/rt_calcs/data/rt_states.csv'.format(data_dir), index=True)
    
    
def run_counties_model(filename = 'data/county_inf_stats.csv'):
    # read in county counts from usafacts and assume total reported positives = case counts
    #UPDATE DATA FILE WITH MOST RECENT FILE
    counties = (pd.read_csv(filename, parse_dates=['date'])
                .assign(positive= lambda x: x.case_count))
    
    # only run on counties with 40 days or more of data
    county_cts = counties.groupby('mState.Providence').count()
    usable_county_names = set(county_cts.loc[county_cts['positive'] > 40,:].index.values)
    usable_counties = counties.loc[counties['mState.Providence'].isin(usable_county_names),:]
    
    # create hierarchical index with county name and deates, and sort 
    usable_counties = usable_counties.set_index(['mState.Providence', 'date']).sort_index()
    #usable_counties.head()
    
    county_models = {}

    for county, grp in usable_counties.groupby('mState.Providence'):
        
        if county in county_models or "Statewide Unallocated" in county: #or county == "Seneca County, NY"
            print(f'Skipping {county}, already in cache')
            continue
        
        print(county)
        try:
            county_models[county] = create_and_run_model(county, grp.droplevel(0))
        except:
            pass
        
    # Check to see if there were divergences
    n_diverging = lambda x: x.trace['diverging'].nonzero()[0].size
    cty_divergences = pd.Series([n_diverging(m) for m in county_models.values()], index=county_models.keys())
    has_divergences = cty_divergences.gt(0)
    
    #print('Diverging counties:')
    #display(cty_divergences[has_divergences])
    
    # Rerun counties with divergences
    for county, n_divergences in cty_divergences[has_divergences].items():
        county_models[county].run() 
        
    cty_results = None

    for i, (county, model) in enumerate(county_models.items()):
        print('{}: {}'.format(i, county))
        try:
            df = df_from_model(model)
    
            if cty_results is None:
                cty_results = df
            else:
                cty_results = pd.concat([cty_results, df], axis=0)
        except:
            print('error getting results for {}'.format(county))
    
    cty_results.to_csv('{}/data/rt_calcs/data/rt_counties.csv'.format(data_dir), index=True)


if __name__ == '__main__':
    
    if len(sys.argv) == 2:
        if sys.argv[1] in ['US', 'states', 'counties']:
            model_type = sys.argv[1]
        else:
            raise Exception('Please enter a single valid argument: US, states, or counties')
    else: 
        raise Exception('Please enter a single valid argument: US, states, or counties')
    
    p_delay = read_patient_data('{}/data/rt_calcs/data/linelist.csv'.format(data_dir))
    
    if model_type == 'US':
        run_national_model('{}/data/database/state_inf_stats.csv'.format(data_dir))
    elif model_type == 'states':
        run_states_model('{}/data/database/state_inf_stats.csv'.format(data_dir))
    else: 
        run_counties_model('{}/data/database/county_inf_stats.csv'.format(data_dir))

