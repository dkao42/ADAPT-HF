
update pipeline.trial_criteria
set upper_bound = upper_bound + 0.00001
where include_upper = TRUE;

update pipeline.trial_criteria
set lower_bound = lower_bound - 0.00001
where include_lower = TRUE;

create or replace table harmonized_data
cluster by study
as
(select 
      patientid,
      j.study,
      variable,
      value,
      phenotype,
      `rank`,
      string_value,
      studyvisit,
      study_wks,
      rule_num,
      datapoint
      from trials.alldata as j
      join pipeline.trial_criteria as c
      using (study_field)
      where c.rule_type = 'match' and
      value = c.match_string)

union all

#### Apply 'range' rules ####
## Compare value with upper and lower bounds
## For convenience, set lower bound to -1000000 and upper bound to 1000000 where either is not specified.
## THis allows application of range criteria with only a single expression

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
      join pipeline.trial_criteria as c
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
      and safe_cast(value as numeric) is not null

union all

select 
      patientid,
      j.study,
      j.form,
      variable,
      lower,
      value,
      upper,
      phenotype,
      rank,
      string_value,
      studyvisit,
      study_wks,
      rule_num,
      datapoint
      from alldata as j
      join hdcp_trial_criteria as c
      using (study_field)
      where c.rule_type = 'range' and
      sex_specific='no' and
      value > lower and
      value < upper

r[r==-1000000] <- NA
r[r==1000000] <- NA

r_sex <- sqldf("select 
      patientid,
      sex_specific,
      sex,
      j.study,
      j.form,
      variable,
      lower,
      value,
      upper,
      phenotype,
      rank,
      string_value,
      studyvisit,
      study_wks,
      rule_num,
      datapoint
      from alldata as j
      join hdcp_trial_criteria as c
      using (study_field)
      join ptlist using (patientid,study)
      where c.rule_type = 'range' and
      sex_specific='yes' and
      criteria_sex = sex and
      value > lower and
      value < upper",stringsAsFactors = F)

r_sex[r_sex==-1000000] <- NA
r_sex[r_sex==1000000] <- NA

#### Apply identity rules ####
## Pass value through unchanged for rules marked 'value'.  This effectively just assigns a phenotype name.
## Units are not considered here so be careful.

v <- sqldf("select patientid,
      j.study,
      j.form,
      studyvisit,
      study_wks,
      variable,
      phenotype,
      value as string_value,
      rule_num,
      datapoint
      from alldata as j
      join hdcp_trial_criteria as c
      using (study_field)
      where c.rule_type = 'value'
           ",stringsAsFactors = F)

v$rank <-  as.numeric(v$string_value)

## Now assemble everything
## Only some columns will be included in the combined datasets.  However the rule number will be retained for auditing if necessary.

merged_phenotypedata <- rbind(m[,c("study",
                                   "patientid",
                                   "studyvisit",
                                   "study_wks",
                                   "phenotype",
                                   "string_value",
                                   "rank",
                                   "rule_num",
                                   "datapoint")],
                              r[,c("study",
                                   "patientid",
                                   "studyvisit",
                                   "study_wks",
                                   "phenotype",
                                   "string_value",
                                   "rank",
                                   "rule_num",
                                   "datapoint")],
                              r_sex[,c("study",
                                       "patientid",
                                       "studyvisit",
                                       "study_wks",
                                       "phenotype",
                                       "string_value",
                                       "rank",
                                       "rule_num",
                                       "datapoint")],
                              v[,c("study",
                                   "patientid",
                                   "studyvisit",
                                   "study_wks",
                                   "phenotype",
                                   "string_value",
                                   "rank",
                                   "rule_num",
                                   "datapoint")])
