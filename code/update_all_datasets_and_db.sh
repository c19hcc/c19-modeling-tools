#!/bin/bash
. ./config.sh

# pull covid datasets
echo '1. PULLING IN COVID DATASETS'
$CODE_DIR/pull_covid_datasets.sh

# pull NPI datasets
#echo '2. PULLING IN NPI DATASETS'
#$CODE_DIR/pull_kff_datasets.sh

# build R database
echo '3. BUILDING R DATABASE'
Rscript $CODE_DIR/build_database.R

