'''
Project: C19HCC COVID-19 Modeling Dashboard and Tools
Purpose: App file for R shiny dashboard
Date: June 2020
Developers: Steven Guan, Kristin Fitzgerald
Copyright 2020, The MITRE Corporation
Approved for Public Release; Distribution Unlimited. Case Number 20-1521.
'''

import pandas as pd
import numpy as np
from matplotlib import pyplot as plt
from matplotlib.dates import (DAILY, DateFormatter, rrulewrapper, RRuleLocator, drange, num2date)
import datetime
from datetime import timedelta  
from scipy.optimize import curve_fit
import os
import sys

'''
Supporting Functions
'''

def sigmoid(x, L ,x0, k, b):
    y = L / (1 + np.exp(-k*(x-x0)))+b
    return (y)

def quadratic (x, a, b, h, c):
    return(a*((x-h)**2) + b*(x-h) + c)
    
def exponential(x, a, b, h, c):
    return(a*(b ** (x-h)) + c)

def fit(y, X, X_pred, p0_sig=None, p0_quad=None, p0_exp=None,maxfev=50000, ftol=1e-6, tr_solver=None, method='trf'):
    
    ps, _ = curve_fit(sigmoid, X, y, p0=p0_sig, maxfev=maxfev, ftol=ftol, method=method)
    sig = sigmoid(X_pred, *ps)
    
    pq, _ = curve_fit(quadratic, X, y, p0=p0_quad, maxfev=maxfev, ftol=ftol, method=method, bounds=([0, -np.inf, -np.inf, -np.inf], np.inf))
    quad = quadratic(X_pred, *pq)
    
    pe, _ = curve_fit(exponential, X, y, p0=p0_exp, maxfev=maxfev, ftol=ftol, method=method)
    expo = exponential(X_pred, *pe)
    return sig, quad, expo, ps, pq, pe

def predictCases(data, days_predicted = 30, vis=True):
    ##  Input
    #   metric: prediction variable (cases or deaths)
    #   data: confirmed cases and deaths from usafacts
    #   days_predicted: number of days to predict
    #   vis: view graphs

    ##  Output
    #   X: number of days since first case
    #   X_pred: number of days since first case including predicted days
    #   y: number of cases
    #   y_pred: number of cases predicted
    #   dates: actual dates
    #   dates_pred: actual dates including predicted daysill

    # Parameters
    learn_rate = 0.125
    w1=0.5
    w2=0.5
    z1 = w1
    z2 = w2
    learn_period = 14

    county = data['County Name'] + ', ' + data['State'] # County name
    y = np.array(data.to_list()[4:]).astype(np.float) #Number of cases
    y_orig = y
    # Create dates
    start_date = datetime.date(2020,1,22)
    end_date = start_date + timedelta(days=len(y))
    delta = datetime.timedelta(days=1)
    dates = drange(start_date, end_date, delta)
    end_date_pred = start_date + timedelta(days=len(y)+days_predicted)
    dates_pred = drange(start_date, end_date_pred, delta)
    dates_str = [item.strftime('%m/%d/%Y') for item in num2date(dates_pred) ]

    # Need to remove leading 0's and make sure there is enough data to find a meaningful curve fit
    first_case_idx = np.nonzero(y)[0][0] # index of first case
    y_first = y[first_case_idx:] #data since first case
    # Learn from N days
    if len(y_first) < learn_period:
        learn_period = 3
    y_idx_start = len(y_first)-learn_period

    # Initialize variables to sum cumulative observed error for comparing exponential vs quadratic
    quad_pred_err_tot = 0
    exp_pred_err_tot = 0

    # Loop through dates to fit and learn from
    for idx, y_idx in enumerate(np.arange(y_idx_start,len(y_first)+1)):
        y = y_first[:y_idx]
        X = np.linspace(0,len(y)-1,len(y)) #Days
        X_pred = np.linspace(0,len(y)-1+days_predicted,len(y)+days_predicted) #Days
        # Create dates

        if (idx == 0):
            p0_sig = [max(y_first), np.median(X),1,min(y_first)]
            p0_quad = None
            p0_exp = None
        else:
            p0_sig = ps
            p0_quad = pq
            p0_exp = pe

        sig, quad, exp, ps, pq, pe = fit(y, X, X_pred, p0_sig, p0_quad, p0_exp)
        y_pred_quad = w1*quad + w2*sig
        y_pred_exp = z1*exp + z2*sig

        if (y_idx < len(y_first)):
            sig_err = np.sum((y_first[y_idx:] - sig[y_idx:len(y_first)]) ** 2)
            quad_err = np.sum((y_first[y_idx:] - quad[y_idx:len(y_first)]) ** 2)
            exp_err = np.sum((y_first[y_idx:] - exp[y_idx:len(y_first)]) ** 2)

            if (idx > 2):
                quad_pred_err_tot += np.sum((y_first[y_idx:] - y_pred_quad[y_idx:len(y_first)]) ** 2)
                exp_pred_err_tot += np.sum((y_first[y_idx:] - y_pred_exp[y_idx:len(y_first)]) ** 2)

            tot_error = sig_err + quad_err
            w1_new = 1 - (quad_err/tot_error)
            w2_new = 1 - (sig_err/tot_error)

            tot_error = sig_err + exp_err
            z1_new = 1 - (exp_err/tot_error)
            z2_new = 1 - (sig_err/tot_error)

            # Save for plotting
            w1_old = np.copy(w1)
            w2_old = np.copy(w2)
            z1_old = z1
            z2_old = z2

            # Update weights
            w1 = w1 + (w1_new - w1) * learn_rate
            w2 = w2 + (w2_new - w2) * learn_rate
            z1 = z1 + (z1_new - z1) * learn_rate
            z2 = z2 + (z2_new - z2) * learn_rate

        if (vis):
            dates_pred_trunc = dates_pred[first_case_idx:first_case_idx+len(X_pred)]
            # Plot
            fig = plt.figure(figsize=(8, 6))
            ax = fig.add_subplot(1, 1, 1)
            ax.plot_date(dates[first_case_idx:first_case_idx+y_idx], y, color='#e74c3c', ls='solid', marker='o')
            try:
                ax.plot_date(dates[first_case_idx+y_idx:first_case_idx+len(y_first)], y_first[y_idx:], color='#636e72', ls='solid', marker='o')
            except:
                pass
            ax.plot_date(dates_pred_trunc, y_pred_quad, color='#00b894', ls='solid',marker="")
            ax.plot_date(dates_pred_trunc, y_pred_exp, color='#2d3436', ls='solid',marker="")
            ax.plot_date(dates_pred_trunc, sig, color='#0984e3', ls='dashed', marker="")
            ax.plot_date(dates_pred_trunc, quad, color='#6c5ce7', ls='dashed', marker="")
            ax.plot_date(dates_pred_trunc, exp, color='#d63031', ls='dashed', marker="")

            w1r = np.round(w1_old, decimals=2)
            w2r = np.round(w2_old, decimals=2)
            z1r = np.round(z1_old, decimals=2)
            z2r = np.round(z2_old, decimals=2)

            # Format plot
            formatter = DateFormatter('%m/%d')
            rule = rrulewrapper(DAILY, interval=7)
            loc = RRuleLocator(rule)
            ax.xaxis.set_major_locator(loc)
            ax.set_xlabel('Days')
            ax.set_ylabel('Confirmed Cases')
            ax.set_title(county + '-- Q: ' + str(w1r) + ' L1: ' + str(w2r) + ' E: ' + str(z1r) + ' L2: ' + str(z2r))
            ax.xaxis.set_major_formatter(formatter)
            ax.xaxis.set_tick_params(rotation=30, labelsize=8)
            ax.legend(['True [Training]', 'True [Testing]', 'Predicted-Quad.', 'Predicted-Exp.','Logistic', 'Quadratic', 'Exponential'])
            ax.set_ylim([0, np.max(y_first) * 1.5])

    if (quad_pred_err_tot < exp_pred_err_tot):
        y_pred = y_pred_quad
        # growth = 'quadratic'
    else:
        y_pred = y_pred_exp
        # growth = 'exponential'

    pad_width = len(y_orig)- len(y)
    y = np.pad(y, (pad_width,0), 'constant')
    y_pred = np.pad(y_pred, (pad_width,0), 'constant')
    y_pred[y_pred<0] = 0
    pad_width = len(y_pred)- len(y)
    y = np.pad(y, (0,pad_width), 'constant', constant_values=(-1,-1))


    X = np.linspace(0,len(y)-1,len(y)) #Days
    X_pred = np.linspace(0,len(y)-1+days_predicted,len(y)+days_predicted) #Days
    # Create dictionary output
    case_output = {
            # 'X': X,
            # 'X_pred': X_pred,
            'y': y,
            'y_pred': y_pred,
            # 'dates': dates,
            # 'dates_pred':dates_pred,
            'dates_str': dates_str,
            }
    return(case_output)

def predictCasesOld(data, days_predicted = 28, vis=True):
    ##  Input
    #   data: confirmed cases from usafacts
    #   days_predicted: number of days to predict
    #   vis: view graphs
    
    ##  Output
    #   X: number of days since first case
    #   X_pred: number of days since first case including predicted days
    #   y: number of cases
    #   y_pred: number of cases predicted
    #   dates: actual dates
    #   dates_pred: actual dates including predicted daysill 

    # Parameters
    learn_rate = 0.125
    w1=0.5
    w2=0.5
    z1 = w1
    z2 = w2
    learn_period = 14
    
    county = data['County Name'] + ', ' + data['State'] # County name
    y = np.array(data.to_list()[4:]).astype(np.float) #Number of cases
    y_orig = y
    # Create dates
    start_date = datetime.date(2020,1,22)
    end_date = start_date + timedelta(days=len(y))
    delta = datetime.timedelta(days=1)
    dates = drange(start_date, end_date, delta)
    end_date_pred = start_date + timedelta(days=len(y)+days_predicted)
    dates_pred = drange(start_date, end_date_pred, delta)
    dates_str = [item.strftime('%m/%d/%Y') for item in num2date(dates_pred) ]
    
    # Need to remove leading 0's and make sure there is enough data to find a meaningful curve fit
    first_case_idx = np.nonzero(y)[0][0] # index of first case
    y_first = y[first_case_idx:] #data since first case
    # Learn from N days
    if len(y_first) < learn_period: 
        learn_period = 3
    y_idx_start = len(y_first)-learn_period

    # Initialize variables to sum cumulative observed error for comparing exponential vs quadratic
    quad_pred_err_tot = 0
    exp_pred_err_tot = 0

    # Loop through dates to fit and learn from
    for idx, y_idx in enumerate(np.arange(y_idx_start,len(y_first)+1)):
        y = y_first[:y_idx]
        X = np.linspace(0,len(y)-1,len(y)) #Days
        X_pred = np.linspace(0,len(y)-1+days_predicted,len(y)+days_predicted) #Days
        # Create dates

        if (idx == 0):
            p0_sig = [max(y_first), np.median(X),1,min(y_first)]
            p0_quad = None
            p0_exp = None
        else:
            p0_sig = ps
            p0_quad = pq
            p0_exp = pe
   
        sig, quad, exp, ps, pq, pe = fit(y, X, X_pred, p0_sig, p0_quad, p0_exp)
        y_pred_quad = w1*quad + w2*sig
        y_pred_exp = z1*exp + z2*sig
                
        if (y_idx < len(y_first)):
            sig_err = np.sum((y_first[y_idx:] - sig[y_idx:len(y_first)]) ** 2)
            quad_err = np.sum((y_first[y_idx:] - quad[y_idx:len(y_first)]) ** 2)  
            exp_err = np.sum((y_first[y_idx:] - exp[y_idx:len(y_first)]) ** 2)
            
            if (idx > 2):
                quad_pred_err_tot += np.sum((y_first[y_idx:] - y_pred_quad[y_idx:len(y_first)]) ** 2)
                exp_pred_err_tot += np.sum((y_first[y_idx:] - y_pred_exp[y_idx:len(y_first)]) ** 2)
            
            tot_error = sig_err + quad_err
            w1_new = 1 - (quad_err/tot_error) 
            w2_new = 1 - (sig_err/tot_error)            
            
            tot_error = sig_err + exp_err
            z1_new = 1 - (exp_err/tot_error) 
            z2_new = 1 - (sig_err/tot_error)            
                        
            # Save for plotting
            w1_old = np.copy(w1)
            w2_old = np.copy(w2)
            z1_old = z1
            z2_old = z2
            
            # Update weights
            w1 = w1 + (w1_new - w1) * learn_rate
            w2 = w2 + (w2_new - w2) * learn_rate
            z1 = z1 + (z1_new - z1) * learn_rate
            z2 = z2 + (z2_new - z2) * learn_rate        
    
        if (vis):   
            dates_pred_trunc = dates_pred[first_case_idx:first_case_idx+len(X_pred)]
            # Plot
            fig = plt.figure(figsize=(8, 6))
            ax = fig.add_subplot(1, 1, 1)
            ax.plot_date(dates[first_case_idx:first_case_idx+y_idx], y, color='#e74c3c', ls='solid', marker='o')
            try:
                ax.plot_date(dates[first_case_idx+y_idx:first_case_idx+len(y_first)], y_first[y_idx:], color='#636e72', ls='solid', marker='o')
            except:
                pass
            ax.plot_date(dates_pred_trunc, y_pred_quad, color='#00b894', ls='solid',marker="")
            ax.plot_date(dates_pred_trunc, y_pred_exp, color='#2d3436', ls='solid',marker="")
            ax.plot_date(dates_pred_trunc, sig, color='#0984e3', ls='dashed', marker="")
            ax.plot_date(dates_pred_trunc, quad, color='#6c5ce7', ls='dashed', marker="")
            ax.plot_date(dates_pred_trunc, exp, color='#d63031', ls='dashed', marker="")

            w1r = np.round(w1_old, decimals=2)
            w2r = np.round(w2_old, decimals=2)                       
            z1r = np.round(z1_old, decimals=2)
            z2r = np.round(z2_old, decimals=2)                       
             
            # Format plot
            formatter = DateFormatter('%m/%d')
            rule = rrulewrapper(DAILY, interval=7)
            loc = RRuleLocator(rule)
            ax.xaxis.set_major_locator(loc)
            ax.set_xlabel('Days')
            ax.set_ylabel('Confirmed Cases')
            ax.set_title(county + '-- Q: ' + str(w1r) + ' L1: ' + str(w2r) + ' E: ' + str(z1r) + ' L2: ' + str(z2r))
            ax.xaxis.set_major_formatter(formatter)
            ax.xaxis.set_tick_params(rotation=30, labelsize=8)
            ax.legend(['True [Training]', 'True [Testing]', 'Predicted-Quad.', 'Predicted-Exp.','Logistic', 'Quadratic', 'Exponential'])
            ax.set_ylim([0, np.max(y_first) * 1.5])    
    
    if (quad_pred_err_tot < exp_pred_err_tot):
        y_pred = y_pred_quad
        # growth = 'quadratic'
    else:
        y_pred = y_pred_exp
        # growth = 'exponential'
        
    pad_width = len(y_orig)- len(y)
    y = np.pad(y, (pad_width,0), 'constant')
    y_pred = np.pad(y_pred, (pad_width,0), 'constant')
    y_pred[y_pred<0] = 0
    pad_width = len(y_pred)- len(y)
    y = np.pad(y, (0,pad_width), 'constant', constant_values=(-1,-1))
    
    
    X = np.linspace(0,len(y)-1,len(y)) #Days
    X_pred = np.linspace(0,len(y)-1+days_predicted,len(y)+days_predicted) #Days
    # Create dictionary output
    case_output = {
            # 'X': X,
            # 'X_pred': X_pred,
            'y': y,
            'y_pred': y_pred,
            # 'dates': dates,
            # 'dates_pred':dates_pred,
            'dates_str': dates_str,
            }
    return(case_output)

def check_sufficient_data(county_data):
    enough_data = True
    y = np.array(county_data.to_list()[4:]).astype(np.float) #Number of cases
    ydif = np.diff(y, prepend=[0])
    if(len(np.nonzero(ydif)[0]) < 10):
        enough_data = False
    return(enough_data)

def read_data(file_path='./data/covid_confirmed_usafacts.csv', county_file_path='./data/county_data.csv'):
    data = pd.read_csv(file_path,delimiter=',',converters={'countyFIPS': str})

    data = data.sort_values(by=data.columns[-1], ascending=False)
    data = data.reset_index(drop=True)
    data['countyFIPS'] = data['countyFIPS'].apply(lambda x: x.zfill(5))
    county_data = pd.read_csv(county_file_path,delimiter=',')
    county_data['GEO_ID'] = county_data['GEO_ID'].str.strip().str[-5:]
        
    return(data, county_data)

def caseDistribution(cases_data, perc_admitted_crit_care=0.25, perc_crit_care_vent=0.60, 
                      LOS_non_crit_care=3, LOS_crit_care=7, LOS_crit_care_vent=11, vis=False):
    ### Input 
    #   perc_admitted_crit_care: Percentage of cases addmitted receiving critical care
    #   perc_crit_care_vent: Percentage of critical care cases requiring ventilator
    #   LOS_non_crit_care: Length of stay for non-critical care patienet
    #   LOS_crit_care: Length of stay for critical care patient
    #   LOS_crit_care_vent: Length of stay for critical care patient requiring ventilator
    #   vis: view graphs
    
    ### Output
    #   admitted_hospital: number of patients admitted each day into hospital
    #   admitted_non_crit_care: number of patients admitted each day NOT receiving critical care
    #   admitted_crit_care: number of patients admitted receiving critical care
    #   admitted_crit_care_vent: number of patients admitted receiving critical care and require critical care
    #   pop_non_crit_care: number of non-critical care patients each day
    #   pop_crit_care: number of critical care patients each day
    #   pop_crit_care_vent: number of critical care patients requiring a ventilator each day

    #Concatenate known data with predicted data
    
    y = cases_data['y'].to_numpy()
    y_pred = cases_data['y_pred'].to_numpy()
    y = y[y>=0]
    
    data = np.concatenate([y, y_pred[len(y):]])
    data = np.round(data)
    data = np.diff(data, prepend=[0])
    data[data<0] = 0
    
    # Caluclate number of new cases for each day
    admitted_hospital = data
    admitted_crit_care = admitted_hospital*perc_admitted_crit_care
    admitted_crit_care = np.round(admitted_crit_care)
    admitted_non_crit_care = admitted_hospital - admitted_crit_care
    admitted_crit_care_vent = admitted_crit_care*perc_crit_care_vent
    admitted_crit_care_vent = np.round(admitted_crit_care_vent)
    
    # Calculate population of each patient type
    LOS_max = np.max([LOS_non_crit_care, LOS_crit_care, LOS_crit_care_vent])
    pop_non_crit_care = np.zeros(len(cases_data['y_pred']) + LOS_max)
    pop_crit_care = np.zeros(len(cases_data['y_pred']) + LOS_max)
    pop_crit_care_vent  = np.zeros(len(cases_data['y_pred']) + LOS_max)
    for idx, case in enumerate(data):
        pop_non_crit_care[idx:idx+LOS_non_crit_care] += admitted_non_crit_care[idx]
        pop_crit_care[idx:idx+LOS_crit_care] += admitted_crit_care[idx]
        pop_crit_care_vent[idx:idx+LOS_crit_care_vent] += admitted_crit_care_vent[idx]

    # Truncate population arrays to forecasted date length
    pop_non_crit_care = pop_non_crit_care[0:len(data)]
    pop_crit_care = pop_crit_care[0:len(data)]
    pop_crit_care_vent = pop_crit_care_vent[0:len(data)]

    
    case_dist = {
            'admitted_hospital': admitted_hospital,
            'admitted_non_crit_care': admitted_non_crit_care,
            'admitted_crit_care': admitted_crit_care,
            'admitted_crit_care_vent': admitted_crit_care_vent,
            'pop_non_crit_care': pop_non_crit_care,
            'pop_crit_care': pop_crit_care,
            'pop_crit_care_vent': pop_crit_care_vent
            }
    
    for key in case_dist:
        case_dist[key] = case_dist[key].tolist()
    case_dist = pd.DataFrame(case_dist)   
    return(case_dist)


##############################################################################
data, county_data = read_data(file_path='./data/usafacts_cases.txt')

if len(sys.argv) > 1: metric = sys.argv[1].lower()
else: metric = 'cases'

data, county_data = read_data(file_path='./data/usafacts_{}.txt'.format(metric))
data = data[data['countyFIPS'] != '00000']
data = data[data['countyFIPS'] != '00001']

output=[]
vis = False

import time
start = time.time()
    
data_fips = data['countyFIPS'].tolist()
#data_fips = ['36081', '36061', '48023']
#data_fips = ['36047', '17031',  '25017',  '04013',  '53033', '45055']
# data_fips = ['15001']
for idx, code in enumerate(data_fips):
    #print(idx)
    row = data[data['countyFIPS']==code].iloc[0]
    if (check_sufficient_data(row)):
        result = predictCases(row, days_predicted = 30, vis=vis)
        df = pd.DataFrame(result)
        df['countyFIPS'] = row['countyFIPS']
        df['County Name'] = row['County Name']
        df['State'] = row['State']
        df['stateFIPS'] = row['stateFIPS']

        case_distribution = caseDistribution(df)
        df = pd.concat([df, case_distribution], axis = 1)
    else:
        dates_str = result['dates_str']
        y_pred = np.zeros(len(dates_str))
        y = np.array(row.to_list()[4:]).astype(np.float) #Number of cases
        pad_width = len(y_pred)- len(y)
        y = np.pad(y, (0,pad_width), 'constant', constant_values=(-1,-1))
        df = pd.DataFrame({'y': y, 'y_pred': y_pred, 'dates_str': dates_str})
        df['countyFIPS'] = row['countyFIPS']
        df['County Name'] = row['County Name']
        df['State'] = row['State']
        df['stateFIPS'] = row['stateFIPS']

        case_distribution = caseDistribution(df)
        df = pd.concat([df, case_distribution], axis = 1)
    if (idx==0):
        df_agg = df
    else:
        df_agg = pd.concat([df_agg,df])

        

data_fips = pd.unique(data['State']).tolist()

for idx, code in enumerate(data_fips):
    #print(idx)
    temp = data[data['State']==code]
    row = temp.sum()
    row['countyFIPS'] = temp['stateFIPS'].iloc[0]
    row['County Name'] = temp['State'].iloc[0]
    row['State'] = temp['State'].iloc[0]
    row['stateFIPS'] = temp['stateFIPS'].iloc[0]

    if (check_sufficient_data(row)):
        result = predictCases(row, days_predicted = 28, vis=vis)
        df = pd.DataFrame(result)
        df['countyFIPS'] = row['countyFIPS']
        df['County Name'] = row['County Name']
        df['State'] = row['State']
        df['stateFIPS'] = row['stateFIPS']

        case_distribution = caseDistribution(df)
        df = pd.concat([df, case_distribution], axis = 1)

    df_agg = pd.concat([df_agg,df])

end = time.time()
total = end-start
print(total)
df_agg.to_csv('./data/predicted_{}.csv'.format(metric),index=False)






