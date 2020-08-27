#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Project: C19HCC COVID-19 Modeling Dashboard and Tools
Purpose: App file for R shiny dashboard
Date: June 2020
Developers: Kristin Fitzgerald
Copyright 2020, The MITRE Corporation
Approved for Public Release; Distribution Unlimited. Case Number 20-1521.

Modified from https://github.com/k-sys/covid-19/blob/master/Realtime%20Rt%20mcmc.ipynb
'''
import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

import sys
import os
import requests
import pandas as pd
import numpy as np
import theano
import theano.tensor as tt

from datetime import date
from datetime import datetime

from IPython.display import clear_output

import io

data_dir = os.getcwd()



    

    
def run_states_model(filename, p_delay):
    usable_states = load_state_data(filename)
    
    models = {}
    
    for state, grp in usable_states.groupby('mState.Providence'):
    
        if state in models:
            print(f'Skipping {state}, already in cache')
            continue
    
        print(state)
        try:
            models[state] = create_and_run_model(state, grp.droplevel(0), p_delay)
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
        print(state)
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
            
    results.to_csv('{}/data/rt_states.csv'.format(data_dir), index=True)
    
    
def run_counties_model(filename, p_delay):
    # read in county counts from usafacts and assume total reported positives = case counts
    #UPDATE DATA FILE WITH MOST RECENT FILE
    counties = pd.read_csv(filename, parse_dates=['date']).assign(positive= lambda x: x.case_count)
    
    # only run on counties with 50 days or more of data
#    county_cts = counties.groupby('mState.Providence').count()
#    usable_county_names = set(county_cts.loc[county_cts['positive'] > 40,:].index.values)
     usable_county_names = get_eligible_counties(cty_data = counties)
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
            county_models[county] = create_and_run_model(county, grp.droplevel(0), p_delay)
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
    
    cty_results.to_csv('{}/data/rt_counties.csv'.format(data_dir), index=True)


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
        run_national_model('{}/data/database/state_inf_stats.csv'.format(data_dir), p_delay)
    elif model_type == 'states':
        run_states_model('{}/data/database/state_inf_stats.csv'.format(data_dir), p_delay)
    else: 
        run_counties_model('{}/data/database/county_inf_stats.csv'.format(data_dir), p_delay)


        