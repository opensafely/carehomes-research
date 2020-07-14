  
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
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "exponential_increase",
    },
    # This line defines the study population
    population=patients.registered_with_one_practice_between(
        "2019-02-01", "2020-03-01"
    ),

    # OUTCOMES,
    primary_care_case=patients.with_these_clinical_events(
        covid_primary_care_case,
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        return_expectations={"date": {"earliest" : "2020-03-01",
                                      "latest": "2020-06-30"},
                             "rate" : "exponential_increase"},
    ),
    primary_care_suspect_case=patients.with_these_clinical_events(
        covid_primary_care_suspect_case,
        returning="date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        return_expectations={"date": {"earliest" : "2020-03-01",
                                      "latest": "2020-06-30"},
                             "rate" : "exponential_increase"},
    ),

    ### testing positive (SGSS or primary care)
    #first_pos_test_sgss=patients.with_test_result_in_sgss(
    #    pathogen="SARS-CoV-2",
    #    test_result="positive",
    #    find_first_match_in_period=True,
    #    returning="date",
    #    date_format="YYYY-MM-DD",
    #    return_expectations={"date": {"earliest": "2020-03-01"}},
    #),

    #first_pos_test_primcare=patients.with_these_clinical_events(
    #    covid_pos_primary_care,
    #    on_or_before="today",
    #    return_first_date_in_period=True,
    #    returning="date",
    #    date_format="YYYY-MM-DD",
    #    return_expectations={
    #        "date": {"earliest": "2020-03-01", "latest": "today"}
    #    },
    #),
    ### A&E attendence
    #a_e_consult_date=patients.attended_emergency_care(
    #    between=["2019-03-01", "2020-06-30"],
    #    returning="date_arrived",
    #    date_format="YYYY-MM-DD",
    #    return_expectations={"date": {"earliest": "2020-03-01",
    #                                  "latest": "2020-06-30"},
    #                         "rate": "exponential_increase"},
    #),
    
    ### Covid-related death
    # Registered death, any COVID
    ons_covid_death_date=patients.with_these_codes_on_death_certificate(
        covid_death_codelist,
        on_or_before="2020-06-01",
        match_only_underlying_cause=False,
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={"date": {"earliest": "2020-03-01"}},
    ),   
    
    # Registered death, any COVID as underlying cause
    ons_underlyingcovid_death_date=patients.with_these_codes_on_death_certificate(
        covid_death_codelist,
        on_or_before="2020-06-01",
        match_only_underlying_cause=True,
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={"date": {"earliest": "2020-03-01"}},
    ),

    ## HOUSEHOLD INFORMATION
    # CAREHOME STATUS
    care_home_type=patients.care_home_status_as_of(
        "2020-03-01",
        categorised_as={
            "PC": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='Y'
              AND LocationRequiresNursing='N'
            """,
            "PN": """
              IsPotentialCareHome
              AND LocationDoesNotRequireNursing='N'
              AND LocationRequiresNursing='Y'
            """,
            "PS": "IsPotentialCareHome",
            "U": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"PC": 0.05, "PN": 0.05, "PS": 0.05, "U": 0.85,},},
        },
    ),

    household_id=patients.household_as_of(
        "2020-03-01",
        returning="pseudo_id",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1000, "stddev": 200},
            "incidence": 1,
        },
    ),

    household_size=patients.household_as_of(
        "2020-03-01",
        returning="household_size",
        return_expectations={
            "int": {"distribution": "normal", "mean": 3, "stddev": 1},
            "incidence": 1,
        },
    ),

    # The rest of the lines define the covariates with associated GitHub issues
    # https://github.com/ebmdatalab/tpp-sql-notebook/issues/33
    age=patients.age_as_of(
        "2020-03-01",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),
    # https://github.com/ebmdatalab/tpp-sql-notebook/issues/46
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    # region - one of NHS England 9 regions
    region=patients.registered_practice_as_of(
        "2020-03-01",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and the Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.2,
                    "South East": 0.2,
                },
            },
        },
    ),
    # https://github.com/ebmdatalab/tpp-sql-notebook/issues/54
    stp=patients.registered_practice_as_of(
        "2020-03-01",
        returning="stp_code",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "STP1": 0.1,
                    "STP2": 0.1,
                    "STP3": 0.1,
                    "STP4": 0.1,
                    "STP5": 0.1,
                    "STP6": 0.1,
                    "STP7": 0.1,
                    "STP8": 0.1,
                    "STP9": 0.1,
                    "STP10": 0.1,
                }
            },
        },
    ),
    msoa=patients.registered_practice_as_of(
        "2020-03-01",
        returning="msoa_code",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"MSOA1": 0.5, "MSOA2": 0.5}},
        },
    ),    
    rural_urban=patients.address_as_of(
        "2020-03-01",
        returning="rural_urban_classification",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"rural": 0.1, "urban": 0.9}},
        },
    ),
    # https://github.com/ebmdatalab/tpp-sql-notebook/issues/52
    imd=patients.address_as_of(
        "2020-03-01",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"100": 0.1, "200": 0.2, "300": 0.7}},
        },
    ),    
    ethnicity=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        include_date_of_match=True,
        return_expectations={
            "category": {"ratios": {"1": 0.8, "5": 0.1, "3": 0.1}},
            "incidence": 0.75,
        },
    ),
    dementia=patients.with_these_clinical_events(
        dementia, return_first_date_in_period=True, include_month=True,
    ), 

    ### GP CONSULTATION RATE
    #gp_consult_count=patients.with_gp_consultations(
    #    between=["2020-03-01", "2020-06-30"],
    #    returning="number_of_matches_in_period",
    #    return_expectations={
    #        "int": {"distribution": "normal", "mean": 4, "stddev": 2},
    #        "date": {"earliest": "2020-03-01", "latest": "2020-06-30"},
    #       "incidence": 0.7,
    #    },
    #),
    #has_consultation_history=patients.with_complete_gp_consultation_history_between(
    #    "2020-03-01", "2020-06-30", return_expectations={"incidence": 0.9},
    #),

)
