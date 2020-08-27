#!/bin/bash
BASE_DIR=`pwd`
DATA_DIR=$BASE_DIR'/data'
CODE_DIR=$BASE_DIR'/code'

LOG_FILE='/var/log/script_output.log'
#MITRE_CERT_FILE=$DATA_DIR'/mitre_cert.crt'

STATE_TESTING_URL='https://covidtracking.com/api/states'
CASE_URL='https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv?_ga=2.111398272.540157555.1584910578-1097424082.1584910578'
DEATH_URL='https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv?_ga=2.111903104.540157555.1584910578-1097424082.1584910578'

