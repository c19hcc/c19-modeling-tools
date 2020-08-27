#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul  9 14:19:26 2020

@author: babraham
"""
import re
import os

REPO_DIR = re.sub('\/data.*', '', os.getcwd())
LOG_DIR = '{}/logs'.format(REPO_DIR)
BATCH_LOG_DIR = LOG_DIR
RT_DIR = '{}/data/rt_calcs'.format(REPO_DIR)
MISSING_COUNTY_FILE = '{}/missing_counties.txt'.format(RT_DIR)
# get job list from batch log
ftext = open('{}/rt_batch_pipeline.log'.format(BATCH_LOG_DIR), 'r').read()
# only look at last day of logs
ftext = re.split('-+RUNNING ARRAY_RT PIPELINE-+\\n', ftext)[-1]
jobs = [s.strip() for s in re.findall('[0-9]{4}[0-9]+\\n', ftext)]

# try to parse every job log for errors. If no job log, add to list. If errors found, 
# add job to list

cty_list = []
for job in jobs:
    fname = '{}/babraham-RT_Batch_County-{}.out'.format(LOG_DIR, job)
    if os.path.exists(fname):
        jtext = open(fname, 'r').read()
        err_counties = re.findall('ERROR: No results for (.*)\\n', jtext)
        if len(err_counties) > 0:
            print('{} errors found for job {}'.format(len(err_counties), job))
            cty_list = cty_list + err_counties
            
if len(cty_list) > 0:
    # export county list to text file and submit job using this file
    with open(MISSING_COUNTY_FILE, 'w') as out:
        out.write('\n'.join(cty_list))
        
    # submit job
    os.system('sbatch run_county_batch_pipeline.sh {}'.format(MISSING_COUNTY_FILE))
else:
    print('found 0 errors in RT output!')