RunControl <- function(ds,set.vax.intro.date, set.form_cont1){
  ds1 <- ds %>%
    arrange(date) %>%
    #create the variables needed for ITS and trend and seasonal adjustments
    mutate( index=row_number(),
            vax.intro.index = which(date ==set.vax.intro.date   ),
            month=as.factor(month(date)),
            Outcome_pre = ifelse(date <set.vax.intro.date, Outcome, NA_real_)) %>%
    #log and scale the covariates
    mutate(log_acm_noJ= log(acm_noJ)
    )
  
  #acm_noresp_nodiar 
  mod1 <- glm.nb(set.form_cont1, data=ds1)
  
  out.list=list(mod1=mod1,'form_its1'=set.form_cont1, 'ds'=ds1)
  
  return(out.list)
}