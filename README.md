# Spatiotemporal risk of infection of care homes during the first wave of the COVID-19 pandemic in the UK

Social care homes form insular communities with low levels of mixing with the general population. However, close proximity between residents and staff combined with the high vulnerability of this group means that one infection can escalate to a devastating outbreak. The aim of this analysis was to investigate the timing of first evidence of introduction of COVID-19 to care homes across England, in order to understand factors associated with earlier introduction and with particular focus on the relationship between introduction risk and the surrounding local epidemic. 

During an epidemic, whether the risk of introduction to care homes depends specifically on local disease burden has implications for the targeting of control efforts. However, it may be that the local burden adds little information above the national epidemic picture in terms of explaining risk to any particular care home - a relationship observed between local incidence and care home introduction may simply reflect a broader, background risk from the national epidemic. We therefore set out to evaluate the associations between national and local levels of COVID-19 incidence and the risk of first introduction to care homes, throughout the first and second epidemic waves in England. 

## Methods
We defined introduction as the first COVID-19 event (clinical diagnosis, positive test result, admission for COVID-19 or COVID-19-related death) among residents of each care home. Local incidence was defined as incidence of probable COVID-19 diagnoses via primary care in the same Middle Super Output Area (MSOA) as the care home. 

A landmarking analysis was then planned to analyse the 14-day risk of a first COVID-19 event in relation to time-varying local incidence. Other characteristics of the home which were considered as potential predictors were overall size, service provided (residential or nursing), proportion of residents with a dementia diagnosis, and deprivation and rural/urban index of the postcode.

## Update August 2021
This study is currently paused due to unresolvable issues with the grouping of individuals into households. 

The planned analysis required aggregation of the available patient-level EHR data into common households, according to a household identifier defined from the grouping of patients under the same address as of February 2020. However, discrepancies arise due to mismatching of patient addresses between linked datasets and larger households (for example care homes) are more vulnerable to such discrepancies. Moreover, the data accessible within OpenSAFELY reflects only one primary care software provider (TPP) and it is not uncommon for larger households to have residents split between different practices and hence providers. 

These are unavoidable limitations inherent to the imperfect linkage and inconsistencies between the patients' health and address data. As a result, we cannot be confident of the total size of the household, whether or not a household is definitely a care home, whether other household characteristics can be identified correctly, or whether all residents of the household are being properly captured.  


* The original study protocol is [here](https://docs.google.com/document/d/1zcOoSIO3yKCTIF9ImmoHuS_S8h9iUUuBbmAB5gzP2hE/edit?usp=sharing)
* An investigation into the household identifier and data disrepancies is [here](https://docs.google.com/document/d/1zHiJULgnrbOUTXU67Lw45Yv22hC5e6Knx5FQWlBgMPc/edit?usp=sharing)
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition.py); this is written in `python`, but non-programmers should be able to understand what is going on there
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).
* Developers and epidemiologists interested in the code should review
[DEVELOPERS.md](./docs/DEVELOPERS.md).



