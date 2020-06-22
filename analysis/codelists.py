from cohortextractor import (
    codelist,
    codelist_from_csv,
)

covid_codelist = codelist(["U071", "U072"], system="icd10")
