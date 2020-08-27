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

from rt_codebase import get_eligible_counties
import re
import os

SUBMIT_RT_SCRIPT = 'submit_array_rt.sh'

if __name__ == '__main__':
    repo_dir = os.getcwd()
    # TODO: write better workaround for this
    if repo_dir.endswith('data/rt_calcs'):
        repo_dir = repo_dir.replace('data/rt_calcs', '')
    elif not repo_dir.endswith('/'):
        repo_dir = repo_dir + '/'
    # filename for county time series data (usafacts)
    data_fname = f"{repo_dir}data/database/county_inf_stats.csv"
    
    # only calculate RT for counties with more than 50 cases.
    eligible_counties = get_eligible_counties(data_fname=data_fname, cutoff=50)
    
    # export county list to rt_calc directory for array script
    eligible_counties = sorted(list(eligible_counties))
    output_file = f'{repo_dir}data/rt_calcs/county_list_clean.txt'
    with open(output_file, 'w') as out: out.write('\n'.join(eligible_counties))
    
    # edit array rt script to allocate proper resources
    script_file = f'{repo_dir}{SUBMIT_RT_SCRIPT}'
    stext = open(script_file).read()
    pat = 'array=1\-[0-9]+'
    new_string = 'array=1-{}'.format(len(eligible_counties))
    stext = re.sub(pat, new_string, stext)
    with open(script_file, 'w') as out: out.write(stext)