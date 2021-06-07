# ====================
# Import national data
# ====================

### Demographics
tables <- c("S1810")
national_demographics <- fun_pull_mongo_data(tables, host_name = host_name, "state")

### Community Living
tables <- c("S2601A", "S2602", "S1810")
national_living <- fun_pull_mongo_data(tables, host_name = host_name, "state")

### Community Participation
tables <- c("B18135", "S1811", "S1810") 
national_participation <- fun_pull_mongo_data(tables, host_name = host_name, "state")

### Work/Economic
tables <- c("B18135", "B18140", "C18120", "C18121", "C18130", "S1811", "B25091", "B25070") 
national_economic <- fun_pull_mongo_data(tables, host_name = host_name, "state")
rm(tables)

### ---------------
### Clean data
###

### Human readable tables for use in dashboard
demographics <- national_demographics %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    ABBR = ABBR,
    
    ### Pop pwd
    pop_total = S1810_C01_001_estimate,
    pwd_total = S1810_C02_001_estimate,
    pwd_pct = pwd_total / pop_total,
    
    ### Age
    pop_18_64 = S1810_C01_015_estimate + S1810_C01_016_estimate,
    pwd_18_64 = S1810_C02_015_estimate + S1810_C02_016_estimate,
    pwd_18_64_pct = pwd_18_64 / pop_18_64,
    pop_grtoeq_65 = S1810_C01_017_estimate + S1810_C01_018_estimate,
    pwd_grtoeq_65 = S1810_C02_017_estimate + S1810_C02_018_estimate,
    pwd_grtoeq_65_pct = pwd_grtoeq_65 / pop_grtoeq_65,
    
    ### Race
    pwd_white = S1810_C02_004_estimate,
    pwd_black = S1810_C02_005_estimate,
    pwd_hisp = S1810_C02_012_estimate,
    pwd_white_nonhisp = S1810_C02_011_estimate,
    pwd_other = S1810_C02_006_estimate + S1810_C02_007_estimate + S1810_C02_008_estimate + S1810_C02_009_estimate + S1810_C02_010_estimate,
    pwd_white_pct = pwd_white / pwd_total,
    pwd_black_pct = pwd_black / pwd_total,
    pwd_hisp_pct = pwd_hisp / pwd_total,
    pwd_white_nonhisp_pct = pwd_white_nonhisp / pwd_total,
    pwd_other_pct = pwd_other / pwd_total,
    
    ### Gender
    pop_female = S1810_C01_003_estimate,
    pwd_female = S1810_C02_003_estimate,
    female_pwd_pct = pwd_female / pop_female,
    pwd_female_pct = pwd_female / pwd_total,
    pop_male = S1810_C01_002_estimate,
    pwd_male = S1810_C02_002_estimate,
    male_pwd_pct = pwd_male / pop_male,
    pwd_male_pct = pwd_male / pwd_total,
    
    ### Type of disability
    pwd_hearing = S1810_C02_019_estimate,
    pwd_hearing_pct = pwd_hearing / pwd_total,
    pwd_vision = S1810_C02_029_estimate,
    pwd_vision_pct = pwd_vision / pwd_total,
    pwd_cognitive = S1810_C02_039_estimate,
    pwd_cognitive_pct = pwd_cognitive / pwd_total,
    pwd_ambulatory = S1810_C02_047_estimate,
    pwd_ambulatory_pct = pwd_ambulatory / pwd_total,
    pwd_selfcare = S1810_C02_055_estimate,
    pwd_selfcare_pct = pwd_selfcare / pwd_total,
    pwd_indliving = S1810_C02_063_estimate,
    pwd_indliving_pct = pwd_indliving / pwd_total
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))

community_living <- national_living %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    ABBR = ABBR,
    
    ### Pop pwd
    pop_total = S2601A_C01_001_estimate,
    pwd_pct = S2601A_C01_047_estimate,
    pwd_total = round(pop_total * (pwd_pct/100), 0),
    
    ### Group quarters
    pop_grpquarters = S2601A_C02_001_estimate,
    pwd_grpquarters_pct = S2601A_C02_047_estimate / 100,  # Percentages supplied by ACS are whole numbers
    grpquarters_pct = pop_grpquarters / pop_total,
    pop_grpquarters_institution = S2601A_C03_001_estimate,
    pop_grpquarters_noninstitution = S2601A_C04_001_estimate,
    pwd_grpquarters_institution_pct = ((S2601A_C03_047_estimate / 100) * S2601A_C03_001_estimate) / pwd_total, 
    pwd_grpquarters_institution = round(pop_grpquarters * (pwd_grpquarters_institution_pct/100), 0),
    pwd_grpquarters_noninstitution_pct = S2601A_C04_047_estimate / 100,
    pwd_grpquarters_noninstitution = round(pop_grpquarters * (pwd_grpquarters_noninstitution_pct/100), 0),
    pwd_home_pct = round((pwd_total - pwd_grpquarters_institution - pwd_grpquarters_noninstitution) / pwd_total, 2),
    pwd_grpquarters_other_pct = round((pwd_grpquarters_noninstitution / pwd_total) * 100, 2),
    
    ### Nursing homes
    pop_nursing = S2602_C04_001_estimate,
    pop_18_64 = S2602_C01_047_estimate,
    pwd_18_64_pct = S2602_C01_048_estimate / 100, 
    pwd_18_64 = round(pop_total * (pwd_18_64_pct / 100), 0),
    pop_nursing_18_64_pct = (S2602_C04_006_estimate + S2602_C04_007_estimate + S2602_C04_008_estimate + S2602_C04_009_estimate + S2602_C04_010_estimate) / 100,
    pwd_nursing_18_64_pct = S2602_C04_048_estimate / 100,
    pwd_nursing_18_64 = round(pwd_total * (pwd_nursing_18_64_pct/100), 0) 
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))

community_participation <- national_participation %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    ABBR = ABBR,
    
    ### Health Insurance
    pop_19_64 = B18135_013_estimate,
    
    pwd_19_64 = B18135_014_estimate,
    pwd_19_64_insured = B18135_015_estimate,
    pwd_19_64_insured_private = B18135_016_estimate,
    pwd_19_64_insured_public = B18135_017_estimate,
    pwd_19_64_uninsured = B18135_018_estimate,
    
    pwod_19_64 = B18135_019_estimate,
    pwod_19_64_insured = B18135_020_estimate,
    pwod_19_64_insured_private = B18135_021_estimate,
    pwod_19_64_insured_public = B18135_022_estimate,
    pwod_19_64_uninsured = B18135_023_estimate,
    
    pop_grtoeq_65 = B18135_024_estimate,
    
    pwd_grtoeq_65 = B18135_025_estimate,
    pwd_grtoeq_65_insured = B18135_026_estimate,
    pwd_grtoeq_65_insured_private = B18135_027_estimate,
    pwd_grtoeq_65_insured_public = B18135_028_estimate,
    pwd_grtoeq_65_uninsured = B18135_029_estimate,
    
    pwod_grtoeq_65 = B18135_030_estimate,
    pwod_grtoeq_65_insured = B18135_031_estimate,
    pwod_grtoeq_65_insured_private = B18135_032_estimate,
    pwod_grtoeq_65_insured_public = B18135_033_estimate,
    pwod_grtoeq_65_uninsured = B18135_034_estimate,
    
    pwd_19_64_uninsured_pct = pwd_19_64_uninsured / pwd_19_64,
    pwod_19_64_uninsured_pct = pwod_19_64_uninsured / pwod_19_64 ,
    pwd_grtoeq_65_uninsured_pct = pwd_grtoeq_65_uninsured / pwd_grtoeq_65,
    pwod_groeq_65_uninsured_pct = pwod_grtoeq_65_uninsured / pwod_grtoeq_65,
    pwd_19_64_insured_private_pct = pwd_19_64_insured_private / pwd_19_64,
    pwod_19_64_insured_private_pct = pwod_19_64_insured_private / pwod_19_64,
    pwd_grtoeq_65_insured_private_pct = pwd_grtoeq_65_insured_private / pwd_grtoeq_65,
    pwod_grtoeq_65_insured_private_pct = pwod_grtoeq_65_insured_private / pwod_grtoeq_65,
    
    ### Medicare/Medicaid
    pop_total = S1810_C01_001_estimate,
    pwd_total = S1810_C02_001_estimate,
    pwod_total = pop_total - pwd_total,
    
    ### Transit Usage
    pwd_commute_public_pct = S1811_C02_035_estimate / 100,
    pwod_commute_public_pct = S1811_C03_035_estimate / 100,
    pwd_commute_car_alone_pct = S1811_C02_033_estimate / 100,
    pwod_commute_car_alone_pct = S1811_C03_033_estimate / 100,
    
    ### Educational Attainment
    pwd_lessthan_highschool_pct = S1811_C02_040_estimate / 100,
    pwod_lessthan_highschool_pct = S1811_C03_040_estimate / 100,
    pwd_highschoolequiv_pct = S1811_C02_041_estimate / 100,
    pwod_highschoolequiv_pct = S1811_C03_041_estimate / 100,
    pwd_degree_aa_pct = S1811_C02_042_estimate / 100,
    pwod_degree_aa_pct = S1811_C03_042_estimate / 100, 
    pwd_degree_grtoeq_ba_pct = S1811_C02_043_estimate / 100,
    pwod_degree_grtoeq_ba_pct = S1811_C03_043_estimate / 100
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))

work_economic <- national_economic %>%
  transmute(
    ### ID
    GEOID = GEOID,
    NAME = NAME,
    ABBR = ABBR.x, # for some reason, national_economic doesn't properly join ABBR
    
    ### Employment Status
    pop_total = C18120_001_estimate,
    pop_19_64 = B18135_013_estimate, # Not the same as the instructions spreadsheet; used this instead to keep calculations in same universe
    pwd_19_64 = B18135_014_estimate,
    pwod_19_64 = pop_19_64 - pwd_19_64,
    pwd_employed = C18120_004_estimate,
    pwod_employed = C18120_005_estimate,
    pwd_unemployed = C18120_007_estimate,
    pwod_unemployed = C18120_008_estimate,
    pwd_notlabor = C18120_010_estimate,
    pwod_notlabor = C18120_011_estimate,
    pwd_employed_pct = pwd_employed / pwd_19_64,
    pwod_employed_pct = pwod_employed / pwod_19_64,
    pwd_unemployed_pct = pwd_unemployed / pwd_19_64,
    pwod_unemployed_pct = pwod_unemployed / pwod_19_64,
    pwd_notlabor_pct = pwd_notlabor / pwd_19_64,
    pwod_notlabor_pct = pwod_notlabor / pwod_19_64,
    
    ### Poverty Status
    pop_18_64 = C18130_009_estimate,
    pwd_18_64 = C18130_010_estimate,
    pwod_18_64 = C18130_013_estimate,
    pwd_below_poverty = C18130_011_estimate,
    pwd_atorbelow_poverty = C18130_012_estimate,
    pwod_below_poverty = C18130_014_estimate,
    pwod_atorbelow_poverty = C18130_015_estimate,
    pwd_below_poverty_pct = pwd_below_poverty / pwd_18_64,
    pwod_below_poverty_pct = pwod_below_poverty / pwod_18_64,
    
    ### Affordability
    mortgage_burdened = B25091_008_estimate + B25091_009_estimate + B25091_010_estimate + B25091_011_estimate,
    rent_burdened = B25070_007_estimate + B25070_008_estimate + B25070_009_estimate + B25070_010_estimate,
    
    ### Full/Part Time Workers
    pop_fulltime = C18121_002_estimate,
    pwd_fulltime = C18121_003_estimate,
    pwod_fulltime = C18121_004_estimate,
    pop_not_fulltime = C18121_005_estimate,
    pwd_not_fulltime = C18121_006_estimate,
    pwod_not_fulltime = C18121_007_estimate,
    pop_didnotwork = C18121_008_estimate,
    pwd_didnotwork = C18121_009_estimate,
    pwod_didnotwork = C18121_010_estimate,
    pwd_fulltime_pct = pwd_fulltime / pwd_19_64,
    pwod_fulltime_pct = pwod_fulltime / pwod_19_64,
    pwd_not_fulltime_pct = pwd_not_fulltime / pwd_19_64,
    pwod_not_fulltime_pct = pwod_not_fulltime / pwod_19_64,
    
    ### Income
    pwd_grtoeq_16_med_individual_income = B18140_002_estimate,
    pwod_grtoeq_16_med_individual_income = B18140_005_estimate,
    
    ### Working from Home
    pwd_grtoeq_16_wfh_pct = S1811_C02_038_estimate / 100, # Percentages supplied by ACS are whole numbers
    pwod_grtoeq_16_wfh_pct = S1811_C03_038_estimate / 100
  ) %>%
  mutate(across(.cols = ends_with("pct"),.fns = ~ round(.x * 100, 2)))
