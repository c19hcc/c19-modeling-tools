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
from one_region_short import run_counties_model, run_national_model, run_states_model
from rt_codebase import read_patient_data
import sys
import os

# script takes two arguments: batch_id (starting county idx, zero-indexed) and batch_size
# the code will then run on all counties in the batch.

if __name__ == '__main__':
    repo_dir = os.getcwd()
    # TODO: write better workaround for this
    if repo_dir.endswith('data/rt_calcs'):
        repo_dir = repo_dir.replace('data/rt_calcs', '')
    elif not repo_dir.endswith('/'):
        repo_dir = repo_dir + '/'
    
    print(sys.argv)
    
    COUNTY_FILE = '{}data/rt_calcs/county_list_clean.txt'.format(repo_dir)

    if len(sys.argv) >= 3:
        job_num = int(sys.argv[1])
        batch_size = int(sys.argv[2])
        if len(sys.argv) == 4:
            COUNTY_FILE = sys.argv[3]
            print('reading counties from {}'.format(COUNTY_FILE))
        county_list = open(COUNTY_FILE, 'r').readlines()
        county_list = [c.strip() for c in county_list]
        start_idx = job_num * batch_size
        stop_idx = start_idx + min((batch_size - 1), (len(county_list)-start_idx))
 
        print('tuning counties in index range {}-{}'.format(start_idx, stop_idx))
    else: 
        raise Exception('Need to provide starting_idx and batch_size as arguments.')
        

    
    p_delay = read_patient_data('{}/data/rt_calcs/data/linelist.csv'.format(repo_dir))
    
    for i in range(start_idx, stop_idx):
        county = county_list[i]
        print('-------{}-------'.format(county))
        run_counties_model('{}/data/database/county_inf_stats.csv'.format(repo_dir), p_delay, county)
    