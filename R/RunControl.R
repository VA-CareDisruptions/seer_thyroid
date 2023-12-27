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
  model_type='nb'
  
  #If unable to fit a negative binomial model, reverts to a Poisson model
  if( 'th.warn' %in% names(mod1) ){
    mod1 <- glm(set.form_cont1, data=ds1, family='poisson')
    print('Using Poisson model instead')
    model_type='pois'
    
  }
  
  
  out.list=list(mod1=mod1,'form_its1'=set.form_cont1,   'ds'=ds1, 'model_type'=model_type)
  
  return(out.list)
}