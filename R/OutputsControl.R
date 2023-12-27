OutputsControl <- function( fitted.mod=mod3,niter=10000, months.start=0, set.months_until_second_knot=0){
  
  
  model.output= fitted.mod$mod1
  mod.ds=fitted.mod$mod1$model
  form1=fitted.mod$form_its1
  ds=fitted.mod$ds
  Y= ds$Outcome
  
  vax.intro.index = which(ds$date ==set.start.date   )
  eval.index = vax.intro.index + 0
  
  form2 <- as.formula(form1a)
  mod.matrix <- model.matrix(form2, data=ds) 
  
  
  #GENERATE PREDICTIONS
  
  pred.coefs.reg.mean <-
    mvrnorm(n = niter,
            mu = coef(model.output),
            Sigma = vcov(model.output))
  
  if(sum(grepl('offset', colnames(ds)))>0 ){
    logoffset <- ds[, grep('offset', colnames(ds))]
  }else{
    logoffset = rep(0, nrow(ds))
  }
  
  preds.stage1.regmean <-
    as.matrix(exp( mod.matrix %*% t(pred.coefs.reg.mean) + logoffset$log_offset))
  
  N.samples.stage2 = 1
  
  if(fitted.mod$model_type=='nb'){
  preds.stage2 <- rnbinom(n = length(preds.stage1.regmean) * N.samples.stage2,
                          size = model.output$theta, mu = preds.stage1.regmean)
  }else{
    preds.stage2 <- rpois(n = length(preds.stage1.regmean) * N.samples.stage2,
                            lambda = preds.stage1.regmean)
  }
  
  
  preds.stage2 <- matrix(preds.stage2,
                         nrow = nrow(preds.stage1.regmean),
                         ncol = ncol(preds.stage1.regmean) * N.samples.stage2)
  
  preds.q<-t(apply(preds.stage2,1,quantile, probs=c(0.025,0.5,0.975)))%>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median_pred=`50%`, lcl_pred=`2.5%`, ucl_pred=`97.5%`)
  
  
  rr.t <-  apply(preds.stage2,2, function(x) Y/x  )
  
  rr.q.t <- as.data.frame(t(apply(rr.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
  
  eval.period = eval.index:nrow(mod.matrix) #period when evaluate
  
  preds.stage2.regmean.SUM <-    apply(preds.stage2[eval.period ,],2, sum )
  obs.sum.eval <- sum(Y[eval.period])
  
  rr.post <-    obs.sum.eval/preds.stage2.regmean.SUM 
  rr.q.post <- quantile(rr.post, probs = c(0.025, 0.5, 0.975))
  
  preds.stage2.post <- preds.stage2[eval.period,]
  
  prevented.post.t <-    apply(preds.stage2.post,2, function(x){ Z=c(rep(0,(eval.index-1)) , (x -Y[eval.period])   )
  return(Z)
  })
  
  
  #Cumulative cases
  cum.post.t <-  apply(prevented.post.t,2, function(x) cumsum(x)   )
  
  cum.post.t.q <-   as.data.frame(t(apply(cum.post.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
  
  p.rr.trend <- rr.q.t %>% 
    ungroup() %>%
    ggplot( aes( x=date, y=median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon(data=rr.q.t, aes(x=date, ymin=lcl, ymax=ucl), alpha=0.1) +
    ylab('Risk ratio') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=as.numeric(set.start.date), lty=2, col='black')
  
  
  p.cum_prevented <- cum.post.t.q %>% 
    ungroup() %>%
    ggplot( aes( x=date, y=median)) +
    geom_line() +
    geom_line() +
    geom_point() +
    theme_classic() +
    geom_ribbon(data=cum.post.t.q, aes(x=date, ymin=lcl, ymax=ucl), alpha=0.1) +
    ylab('Cases not diagnosed') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=(as.numeric(set.start.date)-15), lty=2, col='black')+
    xlim(min=as.Date('2020-02-01'), NA)
  
  
  all.preds <- preds.q %>%
    cbind.data.frame('outcome'=Y)
  
  #Obs vs expected
  p.preds <- all.preds %>%
    ggplot( aes( x=date, y=median_pred)) +
    geom_ribbon(data=all.preds, aes(x=date, ymin=lcl_pred, ymax=ucl_pred), alpha=0.1) +
    geom_line() +
    geom_point(data=all.preds, aes(x=date, y=outcome), color='red', alpha=0.3, size=1) +
    geom_line(data=all.preds, aes(x=date, y=outcome), color='red', alpha=0.3) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA) +
    geom_vline(xintercept=as.numeric(set.start.date), lty=2, col='black')
  
  #logged Obs vs expected
  p.preds.log <- all.preds %>%
    ggplot( aes( x=date, y=log(median_pred))) +
    geom_ribbon(data=all.preds, aes(x=date, ymin=log(lcl_pred), ymax=log(ucl_pred)), alpha=0.1) +
    geom_line() +
    geom_point(data=all.preds, aes(x=date, y=log(outcome+0.5)), color='red', alpha=0.3, size=1) +
    geom_line(data=all.preds, aes(x=date, y=log(outcome+0.5)), color='red', alpha=0.3) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA) +
    geom_vline(xintercept=as.numeric(set.start.date), lty=2, col='black')

    
  agg.pred <- all.preds %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    summarize( across(c(median_pred,lcl_pred, ucl_pred,  outcome), sum )) 
  
  p.preds.agg <- agg.pred %>%
    ggplot( aes( x=year, y=median_pred)) +
    geom_ribbon(data=agg.pred, aes(x=year, ymin=lcl_pred, ymax=ucl_pred), alpha=0.1) +
    geom_line() +
    geom_point(data=agg.pred, aes(x=year, y=outcome), color='red', alpha=0.3) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA)+
    geom_vline(xintercept=as.numeric(year(set.start.date)), lty=2, col='black')
  
  rr.out <- list('rr.q.post' = rr.q.post, 
                 'aic1'=AIC(model.output),'outcome'=mod1$y,
                 'all.preds'=all.preds, 
                 'rr.q.t'=rr.q.t, 'dates'=ds$date, 'p.rr.trend'=p.rr.trend,
                 'p.preds.agg'=p.preds.agg, 'p.preds'=p.preds, 
                 'p.cum_prevented'=p.cum_prevented,
                 'p.preds.log'=p.preds.log,
                 'cum.post.t.q'=cum.post.t.q,
                 'cum.post.t.q.last'=as.matrix(cum.post.t.q[nrow(cum.post.t.q),c(1:3)])
                 )
  
  
  return(rr.out)
}