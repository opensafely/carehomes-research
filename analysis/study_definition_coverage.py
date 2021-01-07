  
from cohortextractor import (
    StudyDefinition,
    patients,
    codelist_from_csv,
    codelist,
    filter_codes_by_category,
    combine_codelists
)

## CODE LISTS
# All codelist are held within the codelist/ folder.
from codelists import *

study = StudyDefinition(
    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": "2020-01-01", "latest": "today"},
        "rate": "exponential_increase",
        "incidence" : 0.2
    },
    # This line defines the study population
    population=patients.registered_with_one_practice_between(
        "2020-01-01", "2020-02-01"
    ),

    msoa=patients.household_as_of(
        "2020-02-01",
        returning="MSOA",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.0625, "E02000002": 0.0625,"E02000003": 0.0625, "E02000004": 0.0625,
                                    "E02000005": 0.0625, "E02000006": 0.0625,"E02000007": 0.0625, "E02000008": 0.0625,
                                    "E02000009": 0.0625, "E02000010": 0.0625,"E02000011": 0.0625, "E02000012": 0.0625,
                                    "E02000013": 0.0625, "E02000014": 0.0625,"E02000015": 0.0625, "E02000016": 0.0625}},        },
    ),    
    
    household_id=patients.household_as_of(
        "2020-02-01",
        returning="pseudo_id",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1000, "stddev": 200},
            "incidence": 1,
        },
    ),
    
    household_size=patients.household_as_of(
        "2020-02-01",
        returning="household_size",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 1},
            "incidence": 1,
        },
    ),

)


