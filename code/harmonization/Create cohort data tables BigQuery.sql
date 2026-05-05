
CREATE or replace TABLE cohorts.alldata1
(
  study STRING,
  patientid STRING,
  visit_yr INT64,
  visitdays INT64,
  form STRING,
  variable STRING,
  value STRING,
  datapoint STRING,
  study_field string,
  cohort STRING,
  cohort_name STRING,
  age_obs INT64
);


CREATE or replace TABLE cohorts.fhs_data
(
  study STRING,
  patientid STRING,
  visit_yr INT64,
  visitdays INT64,
  form STRING,
  variable STRING,
  value STRING,
  datapoint STRING,
  study_field string,
  cohort STRING,
  cohort_name STRING,
  age_obs INT64
);

CREATE or replace TABLE cohorts.aric_data
(
  study STRING,
  patientid STRING,
  visit_yr INT64,
  visitdays INT64,
  form STRING,
  variable STRING,
  value STRING,
  datapoint STRING,
  study_field string,
  cohort STRING,
  cohort_name STRING,
  age_obs INT64
);

CREATE or replace TABLE cohorts.mesa_data
(
  study STRING,
  patientid STRING,
  visit_yr INT64,
  visitdays INT64,
  form STRING,
  variable STRING,
  value STRING,
  datapoint STRING,
  study_field string,
  cohort STRING,
  cohort_name STRING,
  age_obs INT64
);

CREATE or replace TABLE cohorts.chs_data
(
  study STRING,
  patientid STRING,
  visit_yr INT64,
  visitdays INT64,
  form STRING,
  variable STRING,
  value STRING,
  datapoint STRING,
  study_field string,
  cohort STRING,
  cohort_name STRING,
  age_obs INT64
);


CREATE or replace TABLE cohorts.outcomes
(
  study STRING,
  patientid STRING,
  dth_status INT64,
  dth_dt INT64,
  cvdth_status INT64,
  cvdth_dt INT64,
  noncvdth_status INT64,
  noncvdth_dt INT64,
  hosp_status INT64,
  hosp_dt INT64,
  cvhosp_status INT64,
  cvhosp_dt INT64,
  cadhosp_status INT64,
  cadhosp_dt INT64,
  hfhosp_status INT64,
  hfhosp_dt INT64,
  hfhosp_ef_val INT64,
  hfhosp_ef_cat STRING,
  noncvhosp_status INT64,
  noncvhosp_dt INT64
);
