from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv

from codelists import *

study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },
    population=patients.registered_with_one_practice_between(
        "2019-02-01", "2020-02-01"
    ),
    stp=patients.registered_practice_as_of(
        "2020-03-01",
        returning="stp_code",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"STP1": 0.5, "STP2": 0.5}},
        },
    ),
    qfit=patients.with_these_clinical_events(
        qfit_codes,
        find_last_match_in_period=True,
        between=["2020-04-01", "2021-03-31"],
        returning="numeric_value",
        include_date_of_match=True,
        include_month=True,
        return_expectations={
            "float": {"distribution": "normal", "mean": 8.0, "stddev": 4.0},
            "date": {"earliest": "2020-04-01", "latest": "2021-03-31"},
            "incidence": 0.95,
        },
    ),

)
