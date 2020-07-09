library(rstan)
library(loo)
library(caret)
library(ModelMetrics)
library(bayesplot)
library(ggmcmc)
library(cutpointr)

# Convert Hb from g / L to mmol / L
to_mmol_per_litre <- function(Hb) {
  return(Hb * 0.01551 * 4)
}

probability_of_deferral <- function(v, threshold) {
  return(mean(v < threshold))  
}

# deferral score
get_scores <- function(fit, cutoff, norm_mean, norm_sd) {
  pars <- rstan::extract(fit, pars = c("y_pred"))
  y_pred <- pars$y_pred
  
  #y_pred <- apply(y_pred, MARGIN = 2, FUN = denormalize, smallm.stan$original_Hb)
  #print(y_pred)
  #normalised_threshold <- (cutoff - norm_mean) / norm_sd
  normalised_threshold <- normalize_vector(cutoff, norm_mean, norm_sd)
  predicted_probabilities <- apply(y_pred, MARGIN = 2, FUN = probability_of_deferral, threshold = normalised_threshold)
  return(predicted_probabilities) 
}

create_roc <- function(labels, scores) {
  
# create_roc <- function(fit, labels, cutoff, norm_mean, norm_sd) {
#   scores <- get_scores(fit, cutoff, norm_mean, norm_sd)

  pROC_obj <- pROC::roc(response = labels,
                        predictor = scores,
                        #smoothed = TRUE,
                        auc = TRUE,
                        legacy.axes = TRUE,   # x-axis is False positive rate instead of specificity
                        xlab = "False Positive Rate", ylab = "True Positive Rate",
                        #percent = TRUE,
                        # arguments for ci
                        #ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                        # arguments for plot
                        plot=TRUE, 
                        main="Receiver operating characteric",
                        #auc.polygon=TRUE, 
                        max.auc.polygon=TRUE, 
                        #grid=TRUE,
                        print.auc=TRUE 
                        #show.thres=FALSE
                        )
  return(pROC_obj)
}

create_precision_recall <- function(labels, scores) {
  # create_precision_recall <- function(fit, labels, cutoff, norm_mean, norm_sd) {
  #   scores <- get_scores(fit, cutoff, norm_mean, norm_sd)
  pr <- PRROC::pr.curve(scores.class0=scores, weights.class0=labels, curve=TRUE, rand.compute=TRUE)

  return(pr)
}

generate_my_breaks <- function(step) {
  # Break between limits at every position that is multiple of 'step' 
  my_breaks <- function(limits) {
    #step <- 0.2
    m <- limits %/% step
    m <- ifelse(m < 0, m+1, m)
    m <- m*step
    return(seq(m[1], m[2], step))
  }
  return(my_breaks)
}

validate_fit <- function(fit, original_Hb, orig_labels, params, pnames = NULL, male = TRUE, metric = "mean", 
                         cat.plot = TRUE,
                         Hb_cutoff_male = 135, Hb_cutoff_female = 125,
                         use_optimal_cutoff=FALSE) {
  Hb_cutoff <- ifelse(male, Hb_cutoff_male, Hb_cutoff_female)
  
  # Posterior effect sizes
  x <- as.matrix(fit, pars = params)
  if (is.null(pnames)) {
    posterior.plot <- bayesplot::mcmc_intervals(x)
  } else {
    posterior.plot <- bayesplot::mcmc_intervals(x) + scale_y_discrete(labels = pnames)
  }
  posterior.plot <- posterior.plot + scale_x_continuous(breaks = generate_my_breaks(0.2)) +
    labs(title="Effects sizes of variables on Hb prediction",
         x="Regression coefficient")
  
  # Caterpillar plot
  if (cat.plot) {
      transformed <- ggmcmc::ggs(fit, inc_warmup = T)
      cat.plot <- ggplot(filter(transformed, Parameter == params),aes(x=Iteration,y=value, col=as.factor(Chain)))+
          geom_line()+ geom_vline(xintercept = 2000)+
          facet_grid(Parameter ~ .,scale='free_y',switch = 'y')+
          labs(title="Caterpillar Plots", col= "Chains")
  }
  else {
      cat.plot <- NULL
  }
      
  #ll <- extract_log_lik(fit)
  #loo1 <- loo(ll, save_psis = TRUE)

  #loo.plot <- plot(loo1)
  
  # Observed vs predicted scatter plot
  pars <- rstan::extract(fit, pars = c("y_pred"))
  y_pred <- pars$y_pred
  
  if (metric == "mean") {
    y_pred <- colMeans(y_pred)
  } else if (metric == "quantile") {
    y_pred <- apply(y_pred, 2, quantile, 0.05)
  }
  
  Hb_predictions <- denormalize(y_pred, original_Hb)
#  if (male == TRUE) {
  pred_labels <- ifelse(Hb_predictions < Hb_cutoff, 1, 0)
#  } else {
#    pred_labels <- ifelse(Hb_predictions < Hb_cutoff_female, 1, 0)
#  }
  
  #pred_df <- cbind(Hb = Hb_predictions, rep("prediction",length(Hb_predictions)))
  #orig_df <- cbind(Hb = original_Hb, rep("original", length(original_Hb)))
  
  #comp_df <- as.data.frame(cbind(prediction = Hb_predictions, actual = original_Hb, deferral = as.factor(orig_labels)))
  # The above does not work, because cbind creates a matrix which has type double, therefore the factor is coerced to double.
  # The result is that 0 is converted to 1 and 1 is converted to 2, which is not what we want. It is simpler to use tibble.
  comp_df <- tibble(prediction = Hb_predictions, actual = original_Hb, deferral = as.factor(orig_labels))
  
  xymin <- min(min(comp_df$prediction), min(comp_df$actual))
  xymax <- max(max(comp_df$prediction), max(comp_df$actual))

  comp_plot <- ggplot(comp_df, aes(x = actual, y=prediction, color = deferral)) +
    geom_point() +
    #xlim(xymin,xymax) + ylim(xymin,xymax) +
    scale_x_continuous(breaks = generate_my_breaks(20), limits=c(xymin,xymax)) +
    scale_y_continuous(breaks = generate_my_breaks(20), limits=c(xymin,xymax)) +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "observed", y = "predicted", colour = "status") +
    scale_colour_discrete(labels=c("accepted", "deferred")) +
    geom_vline(xintercept = Hb_cutoff, linetype = "dashed") +
    geom_hline(yintercept = Hb_cutoff, linetype = "dashed") +
    ggtitle("Observed vs predicted Hb-values")
  
  # Observed vs standard deviation scatter plot
  sd_df <- as.data.frame(cbind(sds = apply(pars$y_pred, 2, FUN = sd), actual = original_Hb, deferral = orig_labels))
  sd_df$deferral <- as.factor(sd_df$deferral)
  sd_plot <- ggplot(sd_df, aes(x = actual, y=sds, color = deferral)) + geom_point() +
    labs(x = "observed", y = "sds", colour = "status") +
    scale_colour_discrete(labels=c("accepted", "deferred"))


  
  # Confusion matrix
  conf.matrix <- caret::confusionMatrix(factor(pred_labels, levels=c(0,1), labels=c("no", "yes")), 
                                        factor(orig_labels, levels=c(0,1), labels=c("no", "yes")), 
                                        dnn=c("Actual deferral", "Predicted deferral"))
  # "Optimal" confusion matrix
  if (use_optimal_cutoff) {
    cp <- cutpointr::cutpointr(Hb_predictions, test_labels, 
                    direction="<=",   # Smaller values mean positive class 
                    method = maximize_metric, metric = sum_sens_spec)
    optimal_cutoff <- cp$optimal_cutpoint
    comp_plot <- comp_plot +
      geom_hline(yintercept = optimal_cutoff, linetype = "dashed", color="green")
    optimal_pred_labels <- ifelse(Hb_predictions < optimal_cutoff, 1, 0)
    optimal.conf.matrix <- caret::confusionMatrix(factor(optimal_pred_labels, levels=c(0,1), labels=c("no", "yes")), 
                                                  factor(orig_labels, levels=c(0,1), labels=c("no", "yes")), 
                                                  dnn=c("Actual deferral", "Predicted deferral"))
  } else {
    optimal.conf.matrix <- NULL
  }
    
  # Errors
  mae  <- mae(original_Hb, Hb_predictions)
  rmse <- rmse(original_Hb, Hb_predictions)

  original_Hb2 <- to_mmol_per_litre(original_Hb)
  Hb_predictions2 <- to_mmol_per_litre(Hb_predictions)
  mae2  <- mae(original_Hb2, Hb_predictions2)
  rmse2 <- rmse(original_Hb2, Hb_predictions2)
  
  
  return(list(posterior.plot = posterior.plot,
              cat.plot = cat.plot,
              #loo = loo1,
              #loo.plot = loo.plot,
              conf.matrix = conf.matrix,
              optimal.conf.matrix = optimal.conf.matrix,
              pred_labels = pred_labels,
              mae = mae,
              rmse = rmse,
              mae2 = mae2,
              rmse2 = rmse2,
              comp_df = comp_df,
              Hb_predictions = Hb_predictions,
              comp_plot = comp_plot,
              sd_plot = sd_plot))
}
