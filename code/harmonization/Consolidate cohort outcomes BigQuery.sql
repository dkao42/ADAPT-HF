truncate table cohorts.outcomes;

insert into cohorts.outcomes (study,
patientid,
dth_status,
dth_dt,
cvdth_status,
cvdth_dt,
noncvdth_status,
noncvdth_dt,
hfhosp_status,
hfhosp_dt,
hfhosp_ef_val,
hfhosp_ef_cat,
cadhosp_status,
cadhosp_dt)

select 
study,
patientid,
dth_status,
dth_dt,
cvdth_status,
cvdth_dt,
noncvdth_status,
noncvdth_dt,
hfhosp_status,
hfhosp_dt,
hfhosp_ef_val,
hfhosp_ef_cat,
cadhosp_status,
cadhosp_dt
from cohorts.aric_outcomes;
