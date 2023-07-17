RunControl <- function(ds,set.start.date, set.form_cont1){
  ds1 <- ds %>%
    arrange(date) %>%
    #create the variables needed for ITS and trend and seasonal adjustments
    mutate( index=row_number(),
            vax.intro.index = which(date ==set.start.date   ),
            month=as.factor(month(date)),
            Outcome_pre = ifelse(date <set.start.date, Outcome, NA_real_))
    
  
  #acm_noresp_nodiar 
  mod1 <- glm.nb(set.form_cont1, data=ds1)
  
  out.list=list(mod1=mod1,'form_its1'=set.form_cont1, 'ds'=ds1)
  
  return(out.list)
}