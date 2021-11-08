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
            "category": {"ratios": {"E54000005": 0.5, "E54000006": 0.5}},
        },
    ),
    region=patients.registered_practice_as_of(
        "2020-04-17",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"North East": 0.5, "North West": 0.5}},
        },
    ),
    qfit=patients.with_these_clinical_events(
        qfit_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-10-31"],
        returning="numeric_value",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "float": {"distribution": "normal", "mean": 8.0, "stddev": 4.0},
            "date": {"earliest": "2020-04-17", "latest": "2021-10-31"},
            "incidence": 0.1,
        },
    ),
    qfit_comp=patients.comparator_from("qfit", return_expectations={"category":{"ratios":{"<": 0.1, "=": 0.9}}}),
    qfit_ref_lower=patients.reference_range_lower_bound_from(
        "qfit",
        return_expectations={"float": {"distribution": "normal", "mean": 0.0, "stddev": 0.0}}
    ),
    qfit_ref_upper=patients.reference_range_upper_bound_from(
        "qfit",
        return_expectations={"float": {"distribution": "normal", "mean": 10.0, "stddev": 0.0}}
    ),
    fobt=patients.with_these_clinical_events(
        fobt_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2020-12-31"],
        returning="code",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "category": {"ratios": {
                "1015401000000102": 0.05, "104435004": 0.05, "144890002": 0.05, "144891003": 0.05,
                "144892005": 0.05, "144893000": 0.05, "144894006": 0.05, "144895007": 0.05,
                "167665003": 0.05, "167666002": 0.05, "167667006": 0.05, "167668001": 0.05,
                "167669009": 0.05, "167670005": 0.05, "270024004": 0.05, "320531000000106": 0.05,
                "320581000000105": 0.05, "389076003": 0.05, "555901000000100": 0.05, "59614000": 0.05}
            },
            "date": {"earliest": "2020-04-17", "latest": "2020-12-31"},
            "incidence": 0.1,
        },
    ),
    fobt_num=patients.with_these_clinical_events(
        fobt_codes,
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
            "date": {"earliest": "2020-04-17", "latest": "2021-02-28"},
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
            "date": {"earliest": "2020-04-17", "latest": "2021-02-28"},
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
            "date": {"earliest": "2020-04-17", "latest": "2021-04-30"},
            "incidence": 0.1,
        },
    ),

    crc_admission_date=patients.admitted_to_hospital(
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="date_admitted",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-04-30"},
            "incidence": 0.1,
        },
        with_these_primary_diagnoses=crc_diagnosis_icd10_codes
    ),
    
    colonoscopy_primary_care=patients.with_these_clinical_events(
        colonoscopy_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="binary_flag",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-04-30"},
            "incidence": 0.1,
        },
    ),
    
    colonoscopy_sus=patients.admitted_to_hospital(
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="date_admitted",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-04-30"},
            "incidence": 0.1,
        },
        with_these_procedures=opcs4_colonoscopy_codes
    ),

    colonoscopy_sus_diagnosis=patients.admitted_to_hospital(
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="primary_diagnosis",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-04-30"},
            "incidence": 0.1,
            "category": {"ratios": {"123": 0.5, "124": 0.5}}
        },
        with_these_procedures=opcs4_colonoscopy_codes
    ),
    
    flexi_sig_primary_care=patients.with_these_clinical_events(
        flexi_sig_codes,
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="binary_flag",
        include_date_of_match=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-04-30"},
            "incidence": 0.1,
        },
    ),

    flexi_sig_sus=patients.admitted_to_hospital(
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="date_admitted",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-02-28"},
            "incidence": 0.1,
        },
        with_these_procedures=opcs4_flexi_sig_codes
    ),

    flexi_sig_sus_diagnosis=patients.admitted_to_hospital(
        find_first_match_in_period=True,
        between=["2020-04-17", "2021-04-30"],
        returning="primary_diagnosis",
        return_expectations={
            "date": {"earliest": "2020-04-17", "latest": "2021-02-28"},
            "incidence": 0.1,
            "category": {"ratios": {"123": 0.5, "124": 0.5}}
        },
        with_these_procedures=opcs4_flexi_sig_codes
    ),
)
