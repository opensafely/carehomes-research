# ---
# jupyter:
#   jupytext:
#     cell_metadata_filter: all
#     notebook_metadata_filter: all,-language_info
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.3.3
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# + [markdown]
# # Creating a time-series dataset of covid-related event counts per day
#
#
# ### Events
# Current events extracted:
#
# * suspected COVID as recorded in primary care (including GP consultations, 111 calls)
# * swab tests and test results for SARS-CoV2 from pillar 1 tests (SGSS)
# * admission to ICU
# * In-hospital covid-related deaths (from CPNS)
# * Registered deaths by ICD-10 classification -- covid as underlying cause, covid as contributory cause and non-covid cause of death (from ONS)
#
#
# ### import libraries and data
# The dataset used for this report is `/output/input.csv`, created using the study definition `\analysis\study_definintion.py`. The `input.csv` file is imported as a dataframe called `df` and is not exposed. Instead, a dataset containing the frequency of these events per day within each STP is created. 


# +
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec

date_cols = [
        "primary_care_case",
        "primary_care_suspect_case",
        "first_pos_test_sgss",
        "a_e_consult_date",
        "ons_covid_death_date",
]

# import data
df = pd.read_csv(
    #'../output/simulated_input.csv', #dummy data
    '../output/input.csv', #real data
    
    parse_dates=date_cols,
)


# +
## View dataframe 
#print(df)

## check types
print(df.dtypes)

## check earliest and latest dates
print(df[date_cols].agg(['min', 'max']).transpose())


# +
## Some data tidying

# replace NaN with ""
df[['msoa','stp','region','care_home_type']] = df[['msoa','stp','region','care_home_type']].fillna('')

# derive some values:
# select start and end dates / first at-risk date
start_date = pd.to_datetime("2020-02-01", format='%Y-%m-%d')
df_start_date = df[date_cols].min().min()
df_end_date = df[date_cols].max().max()


# derive some variables:
# start date and cause-specific death deaths
df = df.assign(
    start_date = start_date,
)



# +
# choose only date variables
event_dates = df.filter(items=date_cols)

# Make a dataframe with consecutive dates
consec_dates = pd.DataFrame(
    index=pd.date_range(start=start_date, end=df_end_date, freq="D")
)


# -

# The following function is used to calculate the daily number of events. First events only, no subsequent events (for instance if a patient is admitted to ICU twice only the first admission is observed). 

# +
# this counts the number of people on each date who have experienced a covid-related event

def firsteventcountdata(all_dates, event_dates):

    # initialise dataset
    in_counts = all_dates
    
    for col in event_dates:

        # Creates a series of the entry date of the index event
        in_date = event_dates.loc[:, col]

        in_counts = in_counts.join(
            pd.DataFrame(in_date, columns=[col]).groupby(col)[col].count().to_frame()
        )

    # convert nan to zero
    in_counts = in_counts.fillna(0)
    
    return(in_counts)


# -

var = 'stp'
strata = sorted(df[var].unique())
df[var].value_counts()

# +
# calculate event counts per day for each level of var, and put in one dataset
counts_per_day_list = list()

for strat in strata:
    events_strat = (event_dates[df[var] == strat])
    counts_strat = firsteventcountdata(consec_dates, events_strat)
    counts_strat[var] = strat
    counts_per_day_list.append(counts_strat)

counts_per_day = pd.concat(counts_per_day_list)


# -

# Redact numbers between 1 and 5 inclusive

counts_per_day_redacted = counts_per_day.copy()
counts_per_day_redacted[date_cols] = (counts_per_day_redacted[date_cols]).apply(lambda x: np.where((x<6) & (x>0), np.nan, x))
print(counts_per_day_redacted)

# +
# save to file
counts_per_day_redacted.to_csv(path_or_buf = "../data/event_counts_by_"+var+".csv")

df[var].value_counts().to_csv(path_or_buf = "../data/total_patient_counts_by_ "+var+".csv")
# -










