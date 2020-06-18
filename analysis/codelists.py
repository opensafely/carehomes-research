from datalab_cohorts import (
    codelist,
    codelist_from_csv,
)

covid_codelist = codelist(["U071", "U072"], system="icd10")

creatinine_codes = codelist(["XE2q5"], system="ctv3")
