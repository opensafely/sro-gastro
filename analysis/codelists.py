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
