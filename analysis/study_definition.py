from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv

from codelists import *

study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },
    population=patients.satisfying(
        """
        has_follow_up AND
        (age >=18 AND age <= 110) AND
        (sex = "M" OR sex = "F")
        """,
        has_follow_up=patients.registered_with_one_practice_between(
            "2020-04-17", "2020-12-31"
        )
    ),
    age=patients.age_as_of(
        "2020-04-17",
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
    stp=patients.registered_practice_as_of(
        "2020-04-17",
        returning="stp_code",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"STP1": 0.5, "STP2": 0.5}},
        },
    ),
    qfit=patients.with_these_clinical_events(
        qfit_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2020-12-31"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "float": {"distribution": "normal", "mean": 8.0, "stddev": 4.0},
            "date": {"earliest": "2020-04-17", "latest": "2020-12-31"},
            "incidence": 0.1,
        },
    ),
    # Two months added onto qFIT date for referrals and clinic appointments
    ft_referral=patients.with_these_clinical_events(
        ft_colorectal_referral_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-02-28"],
        returning="binary_flag",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2020-02-28"},
            "incidence": 0.1,
        },
    ),
    ft_clinic=patients.with_these_clinical_events(
        ft_colorectal_clinic_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-02-28"],
        returning="binary_flag",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2020-02-28"},
            "incidence": 0.1,
        },
    ),
    # Further two months added for colorectal cancer diagnosis
    crc_diagnosis=patients.with_these_clinical_events(
        crc_diagnosis_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="binary_flag",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2020-02-28"},
            "incidence": 0.1,
        },
    ),
)
