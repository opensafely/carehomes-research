from cohortextractor import (
    codelist,
    codelist_from_csv,
)

covid_primary_care_probable_case_clinical = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv", 
    system="ctv3", 
    column="CTV3ID",
)

covid_primary_care_probable_case_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    system="ctv3", 
    column="CTV3ID",
)

covid_primary_care_probable_case_seq = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    system="ctv3", 
    column="CTV3ID",
)

covid_codelist = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv", system="icd10", column="icd10_code"
)

ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)

dementia = codelist_from_csv(
    "codelists/opensafely-dementia-complete.csv", system="ctv3", column="code"
)
