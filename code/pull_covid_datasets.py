#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Project: C19HCC COVID-19 Modeling Dashboard and Tools
Purpose: App file for R shiny dashboard
Date: June 2020
Developers: Brendan Abraham
Copyright 2020, The MITRE Corporation
Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
'''
from __future__ import unicode_literals
import sys
print(sys.path)
import requests
import json
import pandas as pd
import re
import os
import io
# Tableau data
# url for csv: https://query.data.world/s/p72dj3aso7zcd27mlqedfcqnaeu3nn
#data_dir = '/Users/babraham/Documents/HealthLab/Covid19/npi-modeling-dashboard/data'

#====CAREFUL - MUST RUN SCRIPT FROM WITHIN NPI-MODELING-DASHBOARD REPO (BUT NOT FROM THE CODE DIRECTORY)
data_dir = os.getcwd() + '/data'
mitre_cert = '{}/mitre_cert.crt'.format(data_dir)

cumulative_state_testing_url = 'https://covidtracking.com/api/states'
daily_state_testing_url = 'https://covidtracking.com/api/v1/states/daily.csv'
case_url = 'https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv?_ga=2.111398272.540157555.1584910578-1097424082.1584910578'
death_url = 'https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv?_ga=2.111903104.540157555.1584910578-1097424082.1584910578'

print('DATA DIR: {}'.format(data_dir))

def get_daily_state_testing_data(export=True, prnt=False, cert=None):
    
    state_lat_lons = pd.read_csv('{}/state_lat_lons.csv'.format(data_dir))
    
    try:
        if cert: raw_data = requests.get(daily_state_testing_url, verify=mitre_cert).content
        else: raw_data = requests.get(daily_state_testing_url).content
        testing_df = pd.read_csv(io.StringIO(raw_data.decode('utf8')), parse_dates=['date'])
        
    except:
        print('failed to scrape data from covid tracker, but reading in local backup.')
        testing_df = pd.read_csv('{}/backup_daily_state_testing.csv'.format(data_dir))
        
    testing_df = testing_df.merge(state_lat_lons[['state_abbr','lat','lon']], left_on='state', right_on='state_abbr')
    
    if export:
        # export version to database
        testing_df.to_csv('{}/daily_state_testing.csv'.format(data_dir), index=False)
        # export backup version to data folder
        testing_df.to_csv('{}/backup_daily_state_testing.csv'.format(data_dir), index=False)
        if prnt:
            print('\t Saved testing data to {}/database/daily_state_testing_results.csv'.format(data_dir))
        
    return testing_df

def get_state_testing_data(export=True, prnt=False, cert=None):
    # filling in all na values with 0 for now... will need to double check later
    state_lat_lons = pd.read_csv('{}/state_lat_lons.csv'.format(data_dir))
    try:
        if cert: state_json = json.loads(requests.get(cumulative_state_testing_url, verify=mitre_cert).text)
        else: state_json = json.loads(requests.get(cumulative_state_testing_url).text)
        scd = pd.DataFrame(state_json)
    except:
        print('ERROR: failed to scrape daily resource stats from covidtracking.com.')
        scd = pd.read_csv('data/backup_state_testing_results.csv')
    scd = scd.merge(state_lat_lons[['state_abbr','lat','lon']], left_on='state', right_on='state_abbr')
    scd = scd.fillna(value=0)
    int_cols = ['death', 'negative', 'positive', 'pending', 'total']
    scd[int_cols] = scd[int_cols].astype(int)
    scd['text'] = scd['state'] + ' Testing (as of ' + scd['lastUpdateEt']+ ')<br>'+\
    '<b>Deaths: </b>' + scd['death'].astype(str) + '<br>' +\
    '<b>Total Tests: </b>' + scd['total'].astype(str) + '<br>' +\
    '<b>  Positives: </b>' + scd['positive'].astype(str) + '<br>' +\
    '<b>  Negatives: </b>' + scd['negative'].astype(str) + '<br>' +\
    '<b>  Pending: </b>' + scd['pending'].astype(str) + '<br>'
    if export:
        scd.to_csv('{}/state_testing_results.csv'.format(data_dir))
        if prnt:
            print('\t Saved testing data to {}/state_testing_results.csv'.format(data_dir))
    return scd


def update_county_count_data(output_dir=data_dir, cert=None, prnt=False):
    case_res = requests.get(case_url, verify=cert).text.replace('ï»¿', '')
    with open('{}/usafacts_cases.txt'.format(output_dir), 'w', encoding="utf-8") as out:
        out.write(case_res)
    case_res_df = pd.read_csv('{}/usafacts_cases.txt'.format(output_dir))
    case_res_df = case_res_df.filter(regex='^(?![uU]nnamed.*)', axis=1)
    case_res_df.to_csv('{}/usafacts_cases.txt'.format(output_dir), index=False)
    
    death_res = requests.get(death_url, verify=cert).text.replace('ï»¿', '')
    with open('{}/usafacts_deaths.txt'.format(output_dir), 'w', encoding="utf-8") as out:
        out.write(death_res)
    death_res_df = pd.read_csv('{}/usafacts_deaths.txt'.format(output_dir))
    death_res_df = death_res_df.filter(regex='^(?![uU]nnamed.*)', axis=1)
    death_res_df.to_csv('{}/usafacts_deaths.txt'.format(output_dir), index=False)
    if prnt:
        print('\t Saved case counts to {}/usafacts_cases.txt'.format(output_dir))
        print('\t Saved death counts to {}/usafacts_deaths.txt'.format(output_dir))
        
        
if __name__ == '__main__':
    cert = None
    if len(sys.argv) > 1:
        if sys.argv[1]: cert = mitre_cert
        
    print('Getting CUMULATIVE testing data from covidtracking.com...')
    get_state_testing_data(export=True, prnt=True, cert=cert)
    print('Getting DAILY testing data from covidtracking.com...')
    get_daily_state_testing_data(export=True, prnt=True, cert=cert)
    print('Getting county-level case and count data from USAFACTS...')
    update_county_count_data(prnt=True, cert=cert)
