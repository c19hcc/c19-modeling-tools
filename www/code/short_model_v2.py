'''
Project: C19HCC COVID-19 Modeling Dashboard and Tools
Purpose: App file for R shiny dashboard
Date: June 2020
Developers: Steven Guan
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

# Mathematical functions for fitting data
def sigmoid(x, L ,x0, k, b):
    y = L / (1 + np.exp(-k*(x-x0)))+b
    return (y)

def quadratic (x, a, b, h, c):
    return(a*((x-h)**2) + b*(x-h) + c)
    
def exponential(x, a, b, h, c):
    return(a*(b ** (x-h)) + c)

def moving_average(a, n=3) :
    ret = np.cumsum(a, dtype=float)
    ret[n:] = ret[n:] - ret[:-n]
    return ret[n - 1:] / n

# Curve fitting function 
def fit(y, X, X_pred, p0_sig=None, p0_quad=None, p0_exp=None, method='trf'):
    '''
    Parameters
    ----------
    y : Numpy array
        Historical cumulative COVID-19 cases.
    X : Numpy array
        Days since start of cases
    X_pred : Numpy array
        Days since start of cases used for predicting future cases
    p0_sig : Numpy array, optional
        Initial parameters for fitting the sigmoid function. The default is None.
    p0_quad : Numpy array, optional
        Initial parameters for fitting the quadratic function. The default is None.
    p0_exp : Numpy array, optional
        Initial parameters for fitting the exponential function. The default is None.
    method : String, optional
        Method to use for optimization. The default is 'trf'.

    Returns
    -------
    sig : Numpy array
        Predicted sigmoid curve.
    quad : Numpy array
        Predicted quadratic curve.
    expo : Numpy array
        Predicted exponential curve.
    ps : Numpy array
        Fitted sigmoid function parameters.
    pq : Numpy array
        Fitted quadradic function parameters.
    pe : Numpy array
        Fitted exponential function parameters.

    '''
    
    
    # Error tolerance levels to try
    FTOL = [1e-5, 1e-4, 1e-3, 1e-2, 1e-1] 
   
    # Initialize fitted function parameters
    ps = np.array([0,0,0,0])
    pq = np.array([0,0,0,0])
    pe = np.array([0,1,0,0])
    
    # Decaying exponential function a(1-b)^x for weighted fitting
    sigma = 1*(1-0.02)**np.linspace(0,len(X)-1,len(X))
    
    # Apply moving average to smooth data
    y = np.insert(y,0,1)
    y = np.insert(y,-1,y[-1])
    y = moving_average(y)
    
    # Fit sigmoid function
    for tol in FTOL:
        try:
            ps, _ = curve_fit(sigmoid, X, y, p0=p0_sig, ftol=tol, 
                              method=method, sigma=sigma, 
                              absolute_sigma=False)
            break
        except:
            continue
    # Fit quadratic function        
    for tol in FTOL:
        try:
            pq, _ = curve_fit(quadratic, X, y, p0=p0_quad, ftol=tol, 
                              method=method, 
                              bounds=([0, -np.inf, -np.inf, -np.inf], np.inf), 
                              sigma=sigma, absolute_sigma=False)
            break
        except:
            continue
    
    # Fit exponential function
    for tol in FTOL:
        try:
            pe, _ = curve_fit(exponential, X, y, p0=p0_exp, ftol=tol, 
                              method=method, 
                              sigma=sigma, absolute_sigma=False)
            break
        except:
            continue
        
    sig = sigmoid(X_pred, *ps)
    quad = quadratic(X_pred, *pq)    
    expo = exponential(X_pred, *pe)
    return sig, quad, expo, ps, pq, pe

def predictCases(data, days_predicted = 35, vis=True):
    '''
    

    Parameters
    ----------
    data : Pandas Series
        Contains COVID-19 cases data for a county or state.
    days_predicted : Integer, optional
        Number of days to predict. The default is 35.
    vis : TYPE, optional
        Flag for creating visualization of fitting process. The default is True.

    Returns
    -------
    case_output : Dictionary
        Contains predicted cases

    '''
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
            'y': y,
            'y_pred': y_pred,
            'dates_str': dates_str,
            }
    return(case_output)




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
    data = np.concatenate([cases_data['y'], cases_data['y_pred'][len(cases_data['y']):]])
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

    if (vis):
        # Plot
        fig = plt.figure(figsize=(8, 6))
        ax = fig.add_subplot(1, 1, 1)
        ax.plot_date(cases_data['dates_pred'], pop_non_crit_care, color='#34495e', ls='dashed', marker="")
        ax.plot_date(cases_data['dates_pred'], pop_crit_care, color='#e67e22', ls='dashed', marker="")
        ax.plot_date(cases_data['dates_pred'], pop_crit_care_vent, color='#e74c3c', ls='dashed', marker="")

        # Format plot
        formatter = DateFormatter('%m/%d')
        rule = rrulewrapper(DAILY, interval=7)
        loc = RRuleLocator(rule)
        ax.xaxis.set_major_locator(loc)
        ax.set_xlabel('Date')
        ax.set_ylabel('Cases')
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_tick_params(rotation=30, labelsize=8)
        ax.set_title('COVID Case Distribution')

        ax.legend(['Non-critical care', 'Critical Care', 'Critical Care + Ventilator'])

    case_dist = {
            'admitted_hospital': admitted_hospital,
            'admitted_non_crit_care': admitted_non_crit_care,
            'admitted_crit_care': admitted_crit_care,
            'admitted_crit_care_vent': admitted_crit_care_vent,
            'pop_non_crit_care': pop_non_crit_care,
            'pop_crit_care': pop_crit_care,
            'pop_crit_care_vent': pop_crit_care_vent
            }
    return(case_dist)
    


def calculateTotalPPE(predicted_cases, case_distribution, health_teams, health_teams_ppe, health_teams_shifts, vis=True):
    pop_data = case_distribution['pop_non_crit_care']
    team = health_teams['core_team_non_crit']
    team_shifts = health_teams_shifts['core_shifts_non_crit']
    ppe_non_crit = calculatePPE(pop_data, team, team_shifts, health_teams_ppe)
    
    pop_data = case_distribution['pop_crit_care']
    team = health_teams['core_team_crit']
    team_shifts = health_teams_shifts['core_shifts_crit']
    ppe_crit = calculatePPE(pop_data, team, team_shifts, health_teams_ppe)
    
    pop_data = case_distribution['pop_crit_care_vent']
    team = health_teams['core_team_crit_vent']
    team_shifts = health_teams_shifts['core_shifts_crit_vent']
    ppe_crit_vent = calculatePPE(pop_data, team, team_shifts, health_teams_ppe)
    
    total_ppe = ppe_non_crit.add(ppe_crit.add(ppe_crit_vent))
#    total_ppe = total_ppe.to_dict(orient='list')
    
    if (vis):
        # Plot
        fig = plt.figure(figsize=(8, 6))
        ax = fig.add_subplot(1, 1, 1)
        color = ['#0984e3', '#00b894', '#d63031', '#6c5ce7', '#2d3436']
        legend_labels = []
        for idx, item in enumerate(total_ppe):
            ax.plot_date(predicted_cases['dates_pred'], np.asarray(total_ppe[item]), color=color[idx], ls='dashed', marker="")
            legend_labels.append(item)
        # Format plot
        formatter = DateFormatter('%m/%d')
        rule = rrulewrapper(DAILY, interval=7)
        loc = RRuleLocator(rule)
        ax.xaxis.set_major_locator(loc)
        ax.set_xlabel('Date')
        ax.set_ylabel('PPE Supplies')
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_tick_params(rotation=30, labelsize=8)
        ax.legend(legend_labels)
        ax.set_title('Predicted PPE Demand')
    
    return(total_ppe)


def calculatePPE(data, ppe_set, sets, reuse_policy, reuse=False):
    ppe = {}
    
    for idx, patient_type in enumerate(ppe_set):
        key = 'pop_' + patient_type
        pop = data[key]
        for item in ppe_set[patient_type]:
            reuse_factor  = 1
            if (reuse):
                reuse_factor = reuse_policy[item]
            if idx == 0:
                ppe[item] = np.round(pop * ppe_set[patient_type][item] * sets[patient_type] / reuse_factor)
            else:
                ppe[item] += np.round(pop * ppe_set[patient_type][item] * sets[patient_type]/ reuse_factor)
    
    
    return(ppe)


def model(county_data, parameters, save_file=False, vis=True, error='', reuse=False):
    
    ppe_set = parameters['ppe_set']
    reuse_policy = parameters['reuse']
    low_estimate = parameters['estimates']['low_estimate']
    high_estimate = parameters['estimates']['high_estimate']
    mean_estimate = parameters['estimates']['mean_estimate']
    
    try:
        predicted_cases = predictCases(county_data, days_predicted = 28, vis=vis)
        case_distribution = caseDistribution(predicted_cases, vis=vis)
    except:
        error = 'Fit Failed'
        return(error)

    
    low = pd.DataFrame(calculatePPE(case_distribution, ppe_set, low_estimate, reuse_policy, reuse=reuse))
    high = pd.DataFrame(calculatePPE(case_distribution, ppe_set, high_estimate, reuse_policy, reuse=reuse))
    mean = pd.DataFrame(calculatePPE(case_distribution, ppe_set, mean_estimate, reuse_policy, reuse=reuse))
    low.columns = [str(col) + '_low' for col in low.columns]        
    high.columns = [str(col) + '_high' for col in high.columns]
    mean.columns = [str(col) + '_mean' for col in mean.columns]
    # Convert case_distribution to a pandas dataframe
    for key in case_distribution:
        case_distribution[key] = case_distribution[key].tolist()
    case_distribution = pd.DataFrame(case_distribution)

    # Convert predicted cases to a pandas dataframe 
    length = low.shape[0]
    for key in predicted_cases:
        predicted_cases_data = predicted_cases[key]
        if isinstance(predicted_cases_data, list):
            continue
        else:
            key_length = predicted_cases[key].shape[0]
            if (key_length < length):
                diff = length - key_length
                predicted_cases_data = np.pad(predicted_cases_data, (0, diff), 'constant', constant_values=(-1,-1))
            predicted_cases[key] = predicted_cases_data.tolist()
            
#    with open('../tmp/test_output.txt', 'w') as out: out.write(str(predicted_cases))
    predicted_cases = pd.DataFrame(predicted_cases)
        
    temp = pd.concat([predicted_cases, case_distribution, low, mean, high], axis=1)
    temp['county'] = county_data['County Name']
    temp['state'] = county_data['State']
    temp['countyFIPS'] = county_data['countyFIPS']

    temp =  temp.drop(columns=['X', 'X_pred', 'dates', 'dates_pred'])
    
    # Save results to .csv file
    if (save_file):
        fname = './output/' + county_data['State'] + '-' + county_data['County Name'] + '.csv'
        temp.to_csv(fname,index=False)
    return(temp)


def check_sufficient_data(county_data):
    enough_data = True
    y = np.array(county_data.to_list()[4:]).astype(np.float) #Number of cases
    ydif = np.diff(y, prepend=[0])
    if(len(np.nonzero(ydif)[0]) < 7):
        enough_data = False
    return(enough_data)


def county_predict(data, county_data, parameters, fips=[], vis=False, reuse=False):
    # Input FIPS needs to be a string
    output=[]
        
    data_fips = data['countyFIPS'].tolist()
    for code in fips:
        code = str(code)
        
        if (len(code) == 2):
            # State Prediction
            data_state = data[data['State']==code]
            data_state = data_state.sum()
            data_state['State'] = code
            data_state['County Name'] = code
            data_state['countyFIPS'] = code
            result = model(data_state, parameters, vis=vis, reuse=reuse)

        else:
            # County Predict
            if (code in data_fips):
                county_data = data[data['countyFIPS']==code].iloc[0]
                if (check_sufficient_data(county_data)):
                    result = model(county_data, parameters, vis=vis, reuse=reuse)
                else:
                    result = 'Insufficient data for FIPS: ' + str(code)
            else:
                result = 'FIPS not found: ' + str(code)
        output.append(result)
    return(output)

def read_data(file_path=u'./data/usafacts_cases.txt', county_file_path=u'./data/county_data.csv'):
    data = pd.read_csv(file_path,delimiter=',',converters={'countyFIPS': str})
    data = data[data['countyFIPS'] != '0']
    data = data.sort_values(by=data.columns[-1], ascending=False)
    data = data.reset_index(drop=True)
    data['countyFIPS'] = data['countyFIPS'].apply(lambda x: x.zfill(5))
    county_data = pd.read_csv(county_file_path,delimiter=',')
    county_data['GEO_ID'] = county_data['GEO_ID'].str.strip().str[-5:]
        
    return(data, county_data)


def definePPESet():
    core = {
        'isolation_mask': 1,
        'isolation_gown': 1,
        'n95_respirator': 1,
        'face_shield': 1,
        'goggles': 1,
        'sterile_exam_gloves': 2,
        'non-sterile_exam_gloves': 2,
        'bouffant': 1,
        'shoe_covers': 2,
        }
    
    non_crit = core.copy()
    non_crit['sterile_exam_gloves'] = 0
    crit_care = core.copy()
    crit_care_vent = core.copy()
    
    output = {
        'non_crit_care': non_crit,
        'crit_care': crit_care,
        'crit_care_vent': crit_care_vent,
        }
    
    return(output)
 

def reusePolicy ():
    core = {
        'isolation_mask': 1,
        'isolation_gown': 6,
        'n95_respirator': 12,
        'face_shield': 50,
        'goggles': 50,
        'sterile_exam_gloves': 1,
        'non-sterile_exam_gloves': 1,
        'bouffant': 1,
        'shoe_covers': 1,
        }
    
    return(core)

def setsUsed():
    low_estimate = {
        'non_crit_care': 6,
        'crit_care': 12,
        'crit_care_vent': 12
        }
    
    high_estimate = {
        'non_crit_care': 30,
        'crit_care': 50,
        'crit_care_vent': 50,
        }
    
    mean_estimate = {}
    for key in low_estimate:
        mean_estimate[key] = (low_estimate[key] + high_estimate[key])/2
    
    output = {
        'low_estimate': low_estimate,
        'mean_estimate': mean_estimate,
        'high_estimate': high_estimate
        }
    return(output)


def formatDataForViz(county_output, items):
    
    for county_idx, data in enumerate(county_output): 
        data['date'] = pd.to_datetime(data['dates_str']) - pd.to_timedelta(7, unit='d')
        display_names = {
                'isolation_mask': 'Surg/Proc. Mask',
                'n95_respirator': 'N95 Respirator',
                'isolation_gown': 'Isolation Gown',
                'face_shield': 'Face Shield',
                'goggles': 'Goggles',
                'sterile_exam_gloves': 'Sterile Exam Gloves',
                'non-sterile_exam_gloves': 'Non-sterile Exam Gloves',
                'bouffant': 'Bouffant',
                'shoe_covers': 'Shoe Covers',
                }
            
        # items = list(parameters['ppe_set']['non_crit_care'].keys())
        for idx, item in enumerate(items):
            select = ['date']
            replace = {}
            for estimate in ['low', 'mean', 'high']:
                select.append(item+'_'+estimate)
                replace[item+'_'+estimate] =  estimate
            temp = data[select].rename(columns=replace)
            temp['item'] = display_names[item]
            item
            
            if (idx == 0):
                df = temp
            else:
                df = pd.concat([df, temp])
            
        df_grouped = df.groupby(['item', pd.Grouper(key='date', freq='W-MON')])['low', 'mean', 'high'].sum()
        df_grouped = df_grouped.reset_index().sort_values(['item','date'])
        df_grouped['state'] = data['state'][0]
        df_grouped['county'] = data['county'][0]
        df_grouped['countyFIPS'] = data['countyFIPS'][0]
        
        if(county_idx==0):
            output = df_grouped
        else:
            output = pd.concat([output, df_grouped])
    output = output.reset_index(drop=True)
    output['date'] = output['date'].dt.strftime('%m/%d/%Y')
    return(output)
    
def getCountyData(fips=['06037'], metric='deaths', reuse=False):
    data, county_data = read_data(file_path='./data/usafacts_{}.txt'.format(metric))
#    parameters = {
#        'ppe_set': definePPESet(),
#        'reuse': reusePolicy(),
#        'estimates': setsUsed()
#        }
#    county_output = county_predict(data, county_data, parameters=parameters, fips=fips, vis=False, reuse=reuse)
    
    
    if (isinstance(county_output[0], str)):
        output = 'Insufficient Data'
    else:
        weekly_data = formatDataForViz(county_output, list(parameters['ppe_set']['non_crit_care'].keys()))
        output = {
            'weekly_data': weekly_data,
            'daily_data': county_output[0]
            }
    
    return(output)

def getCountyDataOld(fips=['06037'], reuse=False):
    data, county_data = read_data(file_path='./data/usafacts_cases.txt')
    parameters = {
        'ppe_set': definePPESet(),
        'reuse': reusePolicy(),
        'estimates': setsUsed()
        }
    county_output = county_predict(data, county_data, parameters=parameters, fips=fips, vis=False, reuse=reuse)
    
    if (isinstance(county_output[0], str)):
        output = 'Insufficient Data'
    else:
        weekly_data = formatDataForViz(county_output, list(parameters['ppe_set']['non_crit_care'].keys()))
        output = {
            'weekly_data': weekly_data,
            'daily_data': county_output[0]
            }
    
    return(output)
    
def predictRegionValues(fips_list = ['06037'], metric='deaths', n_days=28):
    data, county_data = read_data(file_path='./data/usafacts_{}.txt'.format(metric))
    res_list = []
    res_df = pd.DataFrame()
    if type(n_days) is list: n_days = int(n_days[0])
    for fip in fips_list:
        if len(fip) == 2:
            temp = data[data['stateFIPS']==int(fip)]
            row = temp.sum()
            row['countyFIPS'] = temp['stateFIPS'].iloc[0]
            row['County Name'] = temp['State'].iloc[0]
            row['State'] = temp['State'].iloc[0]
            row['stateFIPS'] = temp['stateFIPS'].iloc[0]
        else:
            row = data[data['countyFIPS']==fip].iloc[0]
        if len(row) > 0:
            if check_sufficient_data(row):
                df = pd.DataFrame(predictCases(row, vis=False, days_predicted=n_days))
                df['countyFIPS'] = row['countyFIPS']
                df['County Name'] = row['County Name']
                df['State'] = row['State']
                df['stateFIPS'] = row['stateFIPS']
                
                case_distribution = pd.DataFrame(caseDistribution(df))
                df = pd.concat([df, case_distribution], axis = 1)
                res_list.append(df)
            else:
                print('insufficient data for {}'.format(fip))
        else:
            print('no county data for {}'.format(fip))
    res_df = pd.concat(res_list)
    return res_df


'''
Exectue Model
'''    
# This helps R interface to find necessary plugins for reticulate
# os.environ['QT_QPA_PLATFORM_PLUGIN_PATH'] = 'C:/Users/sguan/Anaconda3/envs/pytorch37/Library/plugins/platforms'

#Test
# df = getCountyData(fips=['06037'], reuse=False)
        
# df = getCountyData(fips=['VA'], reuse=False)
