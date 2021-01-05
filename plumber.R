# plumber.R

#* @preempt __first__
#* @get /
function(req, res) {
  res$status <- 302
  res$setHeader("Location", "./__docs__/")
  res$body <- "Redirecting..."
  res
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot the line of best fit for data
#* @serializer png
#* @post /plot
function(ind_results){
  
  mod1 <- lm(time ~ nitch, ind_results)
  
  ind_results_pred <- ind_results %>% 
    add_predictions(mod1)
  
  x <- ggplot(ind_results_pred,aes(nitch,time)) + 
    geom_point(aes(y = time)) +
    geom_line(aes(y = pred), color = "red")
  
  print(x)
}

#* Return the gamma distribution parameters for a set of data
#* @param data a first object
#* @post /fit
function(data){
  vd_gamma <- fitdist(data, "gamma",lower=c(0,0))
  
  result <- list(
    "gmean" = vd_gamma$estimate[["shape"]]/vd_gamma$estimate[["rate"]],
    "gof" = gofstat(vd_gamma)$ks[["1-mle-gamma"]]
  )
  return(result)
}
