library(rstan)
library(loo)
library(caret)
library(ModelMetrics)
library(bayesplot)
library(ggmcmc)
library(cutpointr)
library(pROC)

source("helper_functions.R")

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

create_roc_new <- function(labels, scores) {
  
  roc <- pROC::roc(response = labels,
                        predictor = scores,
                        #smoothed = TRUE,
                        auc = TRUE,
                        legacy.axes = TRUE,   # x-axis is False positive rate instead of specificity
                        xlab = "False Positive Rate", ylab = "True Positive Rate",
                        #percent = TRUE,
                        # arguments for ci
                        #ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                        # arguments for plot
                        plot=FALSE, #plot=TRUE, 
                        main="Receiver operating characteric",
                        #auc.polygon=TRUE, 
                        max.auc.polygon=TRUE, 
                        #grid=TRUE,
                        print.auc=TRUE 
                        #show.thres=FALSE
  )
  AUC <- roc$auc
  title <- "Receiver operating characteric"
  #title <- sprintf("Receiver operating characteric (AUC=%.3f)", AUC)
  roc_plot <- ggroc(roc, legacy.axes=TRUE) +
    geom_abline(aes(intercept=0, slope=1), color="lightgray", alpha=0.5) +
    annotate(geom="text", label=sprintf("AUC: %.3f", AUC), x=0.5, y=0.125) +
    labs(title=title, x = "False positive rate", y = "True positive rate")
  return(list(roc_plot=roc_plot, roc=roc, roc_auc=AUC))
}




create_precision_recall <- function(labels, scores) {
  # create_precision_recall <- function(fit, labels, cutoff, norm_mean, norm_sd) {
  #   scores <- get_scores(fit, cutoff, norm_mean, norm_sd)
  pr <- PRROC::pr.curve(scores.class0=scores, weights.class0=labels, curve=TRUE, rand.compute=TRUE)

  return(pr)
}

create_precision_recall_new <- function(labels, scores) {
  pr <- PRROC::pr.curve(scores.class0=scores, weights.class0=labels, curve=TRUE, rand.compute=TRUE)
  AUPR <- pr$auc.davis.goadrich
  #title <- sprintf("Precision-recall (AUC=%.3f)", AUPR) 
  title <- "Precision-recall"
  pr_plot <- ggplot(data.frame(pr$curve),aes(x=X1,y=X2)) +
    geom_line() +
    annotate(geom="text", label=sprintf("AUC: %.3f", AUPR), x=0.5, y=0.125) +
    scale_y_continuous(limits=c(0.0, 1.0)) +
    labs(x="Recall",y="Precision", title=title)
  return(list(pr_plot=pr_plot, pr=pr, pr_auc=AUPR))
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

create_confusion_matrix_plot <- function(orig_labels, pred_labels) {
  conf.matrix <- caret::confusionMatrix(factor(pred_labels, levels=c(0,1), labels=c("no", "yes")), 
                                        factor(orig_labels, levels=c(0,1), labels=c("no", "yes")), 
                                        dnn=c("Observed deferral", "Predicted deferral"))
  plot <- plot_confusion_matrix(conf.matrix$table, "Confusion matrix")
  return(plot)
}

create_scatter_plot <- function(df, threshold) {
  xymin <- min(min(df$predicted), min(df$observed))
  xymax <- max(max(df$predicted), max(df$observed))

  scatter_plot <- ggplot(df, aes(x = observed, y=predicted, color = factor(as.integer(deferral)))) +
    geom_point() +
    #xlim(xymin,xymax) + ylim(xymin,xymax) +
    scale_x_continuous(breaks = generate_my_breaks(20), limits=c(xymin,xymax)) +
    scale_y_continuous(breaks = generate_my_breaks(20), limits=c(xymin,xymax)) +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "Observed", y = "Predicted", colour = "Status") +
    scale_colour_discrete(labels=c("Accepted", "Deferred")) +
    geom_smooth(mapping=aes(x = observed, y=predicted), colour="black", show.legend = FALSE) +
    geom_vline(xintercept = threshold, linetype = "dashed") +
    geom_hline(yintercept = threshold, linetype = "dashed") +
    ggtitle("Observed vs predicted Hb-values")
  return(scatter_plot)
}

create_forest_plot_old <- function(x, pnames) {
  if (is.null(pnames)) {
    posterior.plot <- bayesplot::mcmc_intervals(x)
  } else {
    posterior.plot <- bayesplot::mcmc_intervals(x) + scale_y_discrete(labels = pnames)
  }
  posterior.plot <- posterior.plot + scale_x_continuous(breaks = generate_my_breaks(0.2)) +
    labs(title="Effects sizes of variables on Hb prediction",
         x="Regression coefficient")
  return(posterior.plot)
}

create_forest_plot <- function(posterior, variables) {
  for (i in seq_along(posterior)) {
    #cat(sprintf("Column %i\n", i))
    v <- posterior[[i]]
    ci_hdi <- bayestestR::ci(v, method = "HDI", ci=0.95)
    #str(ci_hdi)
    L <- list(names = variables[[i]], mean = mean(v), low = ci_hdi$CI_low, high = ci_hdi$CI_high)
    if (i == 1) {
      result <- data.frame(L, stringsAsFactors = FALSE)
    } else {
      result <- rbind(result, L)
    }
    #print(L)
  }
  result <- as_tibble(result)
  str(result)
  cis <- result
  result <- DescTools::Rev(result, margin=1)   # Reverse the order of rows so they appear in correct order in the forest plot
  #print(head(result))
  plot <- ggplot() +     
    geom_vline(aes(xintercept=0), color="lightgray") +
    geom_pointrange(aes(y=factor(result$names, levels=result$names), x=result$mean, xmin=result$low, xmax=result$high), 
                    color="blue", fill="green", position=position_dodge2(width=1, padding=0.9)) +
    labs(title="Effects sizes of variables on Hb prediction (95% CI)",
         x="Regression coefficient", y="") +
    theme_classic()
  return(list(plot=plot, cis=cis))
}

# Creates Confusion matrix, ROC, and Precision-recall plots, and puts them side-by-side.
# Input should be a tibble (or data.frame) that contains the following columns:
# - deferral, boolean or integer (FALSE==0==accepted, TRUE==1==deferred), true labels
# - predicted_labels, same definition as for above
# - scores, double, higher score means that deferral is more likely
# Returns the combined plot object.
# If filename if given, then the combined plot is saved to that file.
# BUG: The plot may look good when saved to a file, but possibly not when the plot object is shown on screen.
create_performance_plots <- function(df, 
                                     filename=NULL,
                                     width = 180  # width of the combined figure in millimetres
                                     ) {
  roc <- create_roc_new(df$deferral, df$scores)
  pr <- create_precision_recall_new(df$deferral, df$scores)
  cm <- create_confusion_matrix_plot(df$deferral, df$predicted_labels)
  use_cowplot <- FALSE
  
  if (use_cowplot) {
    performances <- cowplot::plot_grid(cm, roc$roc_plot, pr$pr_plot, labels = c('A', 'B', 'C'), label_size = 12, nrow=1, scale=0.7)
    if (!is.null(filename)) {
      cowplot::save_plot(filename, performances, ncol = 1, base_asp = 3.0, base_width = width / 25.4, base_height = NULL)
    }
  } else {
    performances <- gridExtra::grid.arrange(cm, roc$roc_plot, pr$pr_plot, nrow = 1, respect=TRUE)   # Combine the plots
    if (!is.null(filename)) {
      ggsave(filename=filename, performances, width = width, units="mm", dpi=600, scale=1.0)
    }
  }
  performances
}

validate_fit <- function(fit, original_Hb, orig_labels, Hb_cutoff, scores, params, pnames = NULL, metric = "mean", 
                         cat.plot = TRUE,
                         use_optimal_cutoff=FALSE) {
  #message("here1")
  # Posterior effect sizes
  #x <- as.matrix(fit, pars = params)
  #message("here2")
  #message(paste0(params, collapse=""))
  #posterior.plot <- create_forest_plot_old(x, pnames)
  samples <- as_tibble(rstan::extract(fit, params))
  posterior.plot <- create_forest_plot(samples, pnames)
  #message("here3")
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
  #message("here4")
  #ll <- extract_log_lik(fit)
  #loo1 <- loo(ll, save_psis = TRUE)

  #loo.plot <- plot(loo1)
  
  # Observed vs predicted scatter plot
  pars <- rstan::extract(fit, pars = c("y_pred"))
  y_pred <- pars$y_pred  # The rows are iterations and columns correspond to donors
  message("here5")
  if (metric == "mean") {
    y_pred <- colMeans(y_pred)
  } else if (metric == "quantile") {
    y_pred <- apply(y_pred, 2, quantile, 0.05)
  }
  sds <- apply(pars$y_pred, 2, FUN = sd)
  
  Hb_predictions <- denormalize(y_pred, original_Hb)
  pred_labels <- ifelse(Hb_predictions < Hb_cutoff, 1, 0)

  
  df <- tibble(predicted = Hb_predictions, sds=sds, observed = original_Hb, predicted_labels=pred_labels, deferral = orig_labels, scores=scores)
  
  scatter_plot <- create_scatter_plot(df, Hb_cutoff)
  
  # Observed vs standard deviation scatter plot
  if (FALSE) {
    #sd_df <- tibble(sds = sds, observed = original_Hb, deferral = as.factor(orig_labels))
    sd_plot <- ggplot(df, aes(x = observed, y=sds, color = factor(as.integer(deferral)))) + 
      geom_point() +
      labs(x = "Observed", y = "SDs", colour = "Status") +
      scale_colour_discrete(labels=c("Accepted", "Deferred"))
  } else {
    sd_plot <- NULL
  }
  
  roc <- create_roc_new(df$deferral, df$scores)
  pr <- create_precision_recall_new(df$deferral, df$scores)
  
  # Confusion matrix
  conf.matrix.plot <- create_confusion_matrix_plot(df$deferral, df$predicted_labels)
  # "Optimal" confusion matrix
  if (use_optimal_cutoff) {
    cp <- cutpointr::cutpointr(df$predicted, df$deferral, 
                    direction="<=",   # Smaller values mean positive class 
                    method = maximize_metric, metric = sum_sens_spec)
    optimal_cutoff <- cp$optimal_cutpoint
    scatter_plot <- scatter_plot +
      geom_hline(yintercept = optimal_cutoff, linetype = "dashed", color="green")
    optimal_pred_labels <- ifelse(Hb_predictions < optimal_cutoff, 1, 0)
    optimal.conf.matrix.plot <- create_confusion_matrix_plot(df$deferral, optimal_pred_labels)
  } else {
    optimal.conf.matrix.plot <- NULL
  }
    
  # Errors
  mae  <- mae(original_Hb, Hb_predictions)
  rmse <- rmse(original_Hb, Hb_predictions)

  original_Hb2 <- to_mmol_per_litre(original_Hb)
  Hb_predictions2 <- to_mmol_per_litre(Hb_predictions)
  mae2  <- mae(original_Hb2, Hb_predictions2)
  rmse2 <- rmse(original_Hb2, Hb_predictions2)
  
  
  return(c(list(posterior.plot = posterior.plot,
              cat.plot = cat.plot,
              #loo = loo1,
              #loo.plot = loo.plot,
              conf.matrix.plot = conf.matrix.plot,
              optimal.conf.matrix.plot = optimal.conf.matrix.plot,
              pred_labels = pred_labels,
              Hb_predictions = Hb_predictions,
              stan_variable_names = params,
              pretty_variable_names = pnames,
              mae = mae,
              rmse = rmse,
              mae2 = mae2,
              rmse2 = rmse2,
              scatter_plot = scatter_plot,
              comp_df = df,
              sd_plot = sd_plot,
              samples = samples), 
           roc, 
           pr))
}
