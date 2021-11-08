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

crc_diagnosis_icd10_codes = codelist_from_csv(
  "codelists-icd10/icd10-colorectal-cancer.csv",
  system="icd10",
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

fobt_codes = codelist_from_csv(
  "codelists/user-NikNakk-faecal-occult-blood-test.csv",
  system="snomed",
  column="code"
)

flexi_sig_codes = codelist_from_csv(
  "codelists/user-NikNakk-flexible-sigmoidoscopy.csv",
  system="snomed",
  column="code"
)

colonoscopy_codes = codelist_from_csv(
  "codelists/user-NikNakk-colonoscopy.csv",
  system="snomed",
  column="code"
)

opcs4_flexi_sig_codes = codelist_from_csv(
  "codelists-opcs4/opcs4-flexible-sigmoidoscopy.csv",
  system="opcs4",
  column="code"
)

opcs4_colonoscopy_codes = codelist_from_csv(
  "codelists-opcs4/opcs4-colonoscopy.csv",
  system="opcs4",
  column="code"
)
