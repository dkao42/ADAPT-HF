
truncate table cohorts.jhs_data;

LOAD DATA INTO cohorts.jhs_data
FROM FILES (
  skip_leading_rows=1,
  format = 'CSV',
  uris = ['gs://master_hdcp_files/jhs_melt_all.parquet']);

delete from cohorts.alldata
where study = "JHS";

insert into `cohorts.alldata`
(study,
  patientid,
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort ,
  cohort_name ,
  age_obs)

  select study,
  cast(patientid as string),
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cast(cohort as string) as cohort,
  cohort_name ,
  age_obs
  from `cohorts.jhs_data`;




TRUNCATE TABLE cohorts.fhs_data;

LOAD DATA INTO cohorts.fhs_data
(study STRING,
patientid INT64,
visit_yr INT64,
visitdays INT64,
form STRING,
variable STRING,
value STRING,
datapoint STRING,
study_field STRING,
cohort INT64,
cohort_name STRING,
age_obs INT64)
FROM FILES (
  skip_leading_rows=1,
  format = 'CSV',
  uris = ['gs://master_hdcp_files/fhs_melt_all.parquet']);

delete from cohorts.alldata
where study = "FHS";

insert into `cohorts.alldata`
(study,
  patientid,
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort ,
  cohort_name ,
  age_obs)

  select study,
  cast(patientid as string),
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cast(cohort as string) as cohort,
  cohort_name ,
  age_obs
  from `cohorts.fhs_data`;




TRUNCATE TABLE cohorts.mesa_data;

LOAD DATA INTO cohorts.mesa_data
(study STRING,
patientid INT64,
visit_yr INT64,
visitdays INT64,
form STRING,
variable STRING,
value STRING,
datapoint STRING,
study_field STRING,
cohort STRING,
cohort_name STRING,
age_obs INT64)
FROM FILES (
  skip_leading_rows=1,
  format = 'CSV',
  uris = ['gs://master_hdcp_files/mesa_melt_all.parquet']);

delete from cohorts.alldata
where study = "MESA";

insert into `cohorts.alldata`
(study,
  patientid,
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort ,
  cohort_name ,
  age_obs)

  select study,
  cast(patientid as string),
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort ,
  cohort_name ,
  age_obs
  from `cohorts.mesa_data`;




TRUNCATE TABLE cohorts.chs_data;

LOAD DATA INTO cohorts.chs_data
(study STRING,
patientid INT64,
visit_yr INT64,
visitdays INT64,
form STRING,
variable STRING,
value STRING,
datapoint STRING,
study_field STRING,
cohort INT64,
cohort_name STRING,
age_obs INT64)
FROM FILES (
  skip_leading_rows=1,
  format = 'CSV',
  uris = ['gs://master_hdcp_files/chs_melt_all.parquet']);


delete from `cohorts.alldata`
where study = "CHS";

insert into `cohorts.alldata`
(study,
  patientid,
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort ,
  cohort_name ,
  age_obs)

  select study,
  cast(patientid as string),
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cast(cohort as string),
  cohort_name ,
  age_obs
  from `cohorts.chs_data`;


TRUNCATE TABLE cohorts.aric_data;

LOAD DATA INTO cohorts.aric_data
(study STRING,
patientid STRING,
visit_yr FLOAT64,
visitdays INT64,
form STRING,
variable STRING,
value STRING,
datapoint STRING,
study_field STRING,
cohort STRING,
cohort_name STRING,
age_obs INT64)
FROM FILES (
  skip_leading_rows=1,
  format = 'CSV',
  uris = ['gs://master_hdcp_files/aric_melt_all.parquet']);

delete from `cohorts.alldata`
where study = "ARIC";

insert into `cohorts.alldata`
(study,
  patientid,
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort,
  cohort_name ,
  age_obs)

  select study,
  cast(patientid as string),
  visit_yr,
  visitdays,
  form,
  variable,
  value,
  datapoint,
  study_field,
  cohort ,
  cohort_name,
  age_obs
  from `cohorts.aric_data`;


create or replace table cohorts.ptlist
as (
  select distinct study, cohort, cohort_name, patientid,visit_yr, visitdays,age_obs, sex
  from `cohorts.cohort_dates`
)