from cohortextractor import (
    codelist_from_csv,
    codelist,
)

# DEMOGRAPHIC CODELIST
ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)

qfit_codes = codelist_from_csv(
  "codelists/user-NikNakk-quantitative-faecal-immunochemical-test.csv",
  system="snomed",
  column="code"
)

crc_diagnosis_codes = codelist_from_csv(
  "codelists/user-NikNakk-bowel-cancer-diagnosis.csv",
  system="snomed",
  column="code"
)

ft_colorectal_referral_codes = codelist_from_csv(
  "codelists/user-NikNakk-fast-track-colorectal-cancer-referral.csv",
  system="snomed",
  column="code"
)

ft_colorectal_clinic_codes = codelist_from_csv(
  "codelists/user-NikNakk-seen-in-fast-track-colorectal-clinic.csv",
  system="snomed",
  column="code"
)
