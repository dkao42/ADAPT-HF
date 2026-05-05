

update pipeline.cohort_criteria
set upper_bound = upper_bound + 0.00001
where include_upper = TRUE;

update pipeline.cohort_criteria
set lower_bound = lower_bound - 0.00001
where include_lower = TRUE;

create or replace table cohorts.harmonized_data
cluster by study
AS 
(select distinct 
      j.study,
      patientid,
      cohort,
      cohort_name,
      visit_yr,
      visitdays,
      age_obs,
      phenotype,
      string_value,
      `rank_value`,
      rule_num,
      datapoint,
      brief_name
      from cohorts.alldata as j
      join pipeline.cohort_criteria as c
      using (study_field)
      join pipeline.phenotypes_cohort
      using (phenotype)
      where c.rule_type = 'match' and
      value = c.match_string

      union all

      select distinct 
     j.study,
      patientid,
      cohort,
      cohort_name,
      visit_yr,
      visitdays,
      age_obs,
      phenotype,
      string_value,
      `rank_value`,
      rule_num,
      datapoint,
      brief_name
      from cohorts.alldata as j
      join pipeline.cohort_criteria as c
      using (study_field)
      join pipeline.phenotypes_cohort
      using (phenotype)
      where c.rule_type = 'range' 
      and sex_specific is FALSE 
      and safe_cast(value as numeric) > lower_bound 
      and safe_cast(value as numeric) < upper_bound

      union all

      select distinct 
      j.study,
      patientid,
      cohort,
      cohort_name,
      visit_yr,
      visitdays,
      age_obs,
      phenotype,
      string_value,
      `rank_value`,
      rule_num,
      datapoint,
      brief_name
      from cohorts.alldata as j
      join (select distinct study, patientid, sex from`cohorts.ptlist`) as pl
      using (study,patientid)
     join pipeline.cohort_criteria as c
      using (study_field)
      join pipeline.phenotypes_cohort
      using (phenotype)
       where c.rule_type = 'range' and
      sex_specific = TRUE and
      criteria_sex = sex and
      safe_cast(value as numeric) > lower_bound and
      safe_cast(value as numeric) < upper_bound

      union all

      select distinct 
      j.study,
      patientid,
      cohort,
      cohort_name,
      visit_yr,
      visitdays,
      age_obs,
      phenotype,
      value as string_value,
      safe_cast(value as numeric) as `rank`,
      rule_num,
      datapoint,
      brief_name
      from cohorts.alldata as j
      join pipeline.cohort_criteria as c
      using (study_field)
      join pipeline.phenotypes_cohort
      using (phenotype)
      where c.rule_type = 'value'
      and safe_cast(value as numeric) is not null);
