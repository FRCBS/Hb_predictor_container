library(rstan)
#library(loo)
library(caret)
#library(ModelMetrics)
#library(bayesplot)
library(ggmcmc)
library(cutpointr)
library(pROC)
library(boot)
library(progress)  # For progress bar

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



create_roc_new <- function(labels, scores, boot.n=2000) {
  message("Computing the ROC curve")
  tryCatch(error = function(cnd) {
    t <- table(labels, useNA = "always")
    #if (is.null(names(t))) {
      mynames <- c("Accepted (0)", "Deferred (1)")
    #} else mynames <- names(t)
    s <- paste(sprintf("%s: %i", mynames, t), collapse=", ")   # Show the distribution of factor levels in the error message
    cnd$message <- paste("\nThe distribution of response levels is:", s, cnd$message, sep="\n")
    stop(cnd)
  },
           roc <- pROC::roc(response = labels,
                            predictor = scores,
                            #smoothed = TRUE,
                            auc = TRUE,
                            legacy.axes = TRUE,   # x-axis is False positive rate instead of specificity
                            xlab = "False Positive Rate", ylab = "True Positive Rate",
                            #percent = TRUE,
                            # arguments for ci
                            ci=TRUE, 
                            conf.level=0.95, 
                            boot.stratified=TRUE,
                            boot.n=boot.n,
                            # arguments for plot
                            plot=FALSE, #plot=TRUE, 
                            main="Receiver operating characteric",
                            #auc.polygon=TRUE, 
                            max.auc.polygon=TRUE, 
                            #grid=TRUE,
                            print.auc=TRUE 
                            #show.thres=FALSE
           )
  )
  AUC <- roc$auc
  #title <- "Receiver operating characteristic"
  title <- "ROC"
  #title <- sprintf("Receiver operating characteristicc (AUC=%.3f)", AUC)
  c <- as.numeric(roc$ci)
  ci <- tibble("AUROC value"=c[2], "AUROC low"=c[1], "AUROC high"=c[3])
  roc_plot <- ggroc(roc, legacy.axes=TRUE) +
    geom_abline(aes(intercept=0, slope=1), color="lightgray") +
    annotate(geom="text", label=sprintf("AUROC: %.2f (%.2f–%.2f)", c[2], c[1], c[3]), x=0.5, y=0.125) +
    labs(title=title, x = "False positive rate", y = "True positive rate")
  return(list(roc_plot=roc_plot, roc=roc, roc_auc=AUC, roc_ci=ci))
}







# It seems that the number of replicates must be at least as high as the number of rows in the dataframe
# https://stat.ethz.ch/pipermail/r-help/2011-February/269006.html
# boot.n is the number of bootstrap replications, if null use as many replications as there are rows in the dataframe
precision_recall_ci <- function(df, method="norm", boot.n=NULL) {
  
  get_aupr <- function(df, indices) {
    df2 <- df[indices,]
    pb$tick()  # update progress bar
    aupr <- PRROC::pr.curve(scores.class0=df2$scores, weights.class0=df2$labels)$auc.davis.goadrich
    return(aupr)
  }
    
  df <- df %>% select(labels, scores)
  if (is.null(boot.n)) {
    boot.n <- nrow(df)
  }
  
  pb <- progress::progress_bar$new(total = boot.n+1)# init progress bar
  pb$tick(0)
  #p <- progress_estimated(n+1)  # init progress bar
  b <- boot(df, statistic = get_aupr, R=boot.n, sim="ordinary", stype="i", strata=df$labels, parallel="multicore")#, ncpus=1)
  ret <- tryCatch(
    error = function(cnd) return(-1),
    {
      result <- boot.ci(b, conf=0.95, type=method)
      var <- recode(method, "norm"="normal", "perc"="percent", "stud"="student")  # The name of the output field is stupidly sometimes not the same as the parameter name
      ci <- if (method=="norm") result[[var]][2:3] else result[[var]][4:5]
      NULL
    })
  if (!is.null(ret) && ret == -1) {
    ci <- C(NA, NA)
  }
  return(list(ci=ci, result=result))
}
  
create_precision_recall_new <- function(labels, scores, method="norm", boot.n=2000) {
  debug <- TRUE
  message("Computing the precision-recall curve")
  pr_model     <- PRROC::pr.curve(scores.class0=scores, weights.class0=labels, curve=TRUE, rand.compute=TRUE)
  if (debug) message("hep1")
  points <- data.frame(pr_model$curve)
  if (debug) message("hep2")
  AUPR <- pr_model$auc.davis.goadrich
  df <- tibble(labels=labels, scores=scores)
  if (debug) message("hep3")
  prci <- precision_recall_ci(df, method=method, boot.n=boot.n)$ci
  if (debug) message("tassa1")
  title <- "Precision-recall"
  m <- mean(labels)
  if (debug) message("tassa2")
  pr_plot <- ggplot(points, aes(x=X1,y=X2)) +
    geom_hline(aes(yintercept=m), color="lightgray") +   # theoretical PR curve of random classifier
    annotate(geom="text", label=sprintf("y=%.2f", m), x=0.25, y=m, vjust=-1) +
    geom_line() +
    annotate(geom="text", label=sprintf("AUPR: %.2f (%.2f–%.2f)", AUPR, prci[1], prci[2]), x=0.5, y=0.875) +
    scale_y_continuous(limits=c(0.0, 1.0)) +
    labs(x="Recall",y="Precision", title=title)
  if (debug) message("tassa3")
  ci <- tibble("AUPR value"=AUPR, "AUPR low"=prci[1], "AUPR high"=prci[2])
  if (debug) message("tassa4")
  return(list(pr_plot=pr_plot, pr=points, pr_auc=AUPR, pr_ci=ci))
}

# Computes the F1 score.
# The input dataframe 'df' should have two columns:
# 'deferral': factor with levels "Accepted", "Deferred", where "Deferred" is the positive class.
# 'scores': a numerical vector. Score (e.g. probability) of deferral.
get_f1 <- function(df, threshold = 0.5) {
  pred_class <- factor( ifelse(df$scores >= threshold, "Deferred", "Accepted"), levels=c("Accepted", "Deferred"))
  obs_class <- factor(ifelse(df$deferral == 1, "Deferred", "Accepted"), levels=c("Accepted", "Deferred"))
  cm <- caret::confusionMatrix(reference = obs_class, data = pred_class, positive = "Deferred", mode = "prec_recall")
  f1 <- cm$byClass["F1"]
  return(f1)   # Returns a single value
}

get_f1_ci <- function(df, method="norm", boot.n=2000) {
  #message("Computing the F1 score")
  f1_helper <- function(df, indices) {
    df2 <- df[indices,]
    pb$tick()  # update progress bar
    f1 <- get_f1(df2)
    return(f1)
  }
  #message("moi1")
  df <- df %>% select(deferral, scores)
  #message("moi2")
  if (is.null(boot.n)) {
    boot.n <- nrow(df)
  }
  #message("moi3")
  pb <- progress::progress_bar$new(total = boot.n+1)# init progress bar
  #message("moi4")
  pb$tick(0)
  #message("moi5")
  #p <- progress_estimated(n+1)  # init progress bar
  b <- boot(df, statistic = f1_helper, R=boot.n, sim="ordinary", stype="i", strata=df$deferral, parallel="multicore")
  #message("moi6")
  error_code <- tryCatch(
    error = function(cnd) -1 # return exit code
    ,
    {
      #message("moi7")
      result <- boot.ci(b, conf=0.95, type=method)
      #message("moi8")
      var <- recode(method, "norm"="normal", "perc"="percent", "stud"="student")  # The name of the output field is stupidly sometimes not the same as the parameter name
      #message("moi9")
      cis <- if (method=="norm") result[[var]][2:3] else result[[var]][4:5]
      #message("moi99")
      ci <- tibble("F1 value"=result$t0, "F1 low"=cis[1], "F1 high"=cis[2])
      print(ci)
    })
  #message("moi999")
  if (!is.null(error_code) && error_code == -1) {
    f1 <- get_f1(df)
    ci <- tibble("F1 value"=f1, "F1 low"=f1, "F1 high"=f1)
  }
  #message("moi10")
  return(list(ci=ci))
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
    theme(legend.position = "bottom") +
    ggtitle("Observed vs predicted Hb-values")
  return(scatter_plot)
}



# create_forest_plot_old <- function(x, pnames) {
#   if (is.null(pnames)) {
#     posterior.plot <- bayesplot::mcmc_intervals(x)
#   } else {
#     posterior.plot <- bayesplot::mcmc_intervals(x) + scale_y_discrete(labels = pnames)
#   }
#   posterior.plot <- posterior.plot + scale_x_continuous(breaks = generate_my_breaks(0.2)) +
#     labs(title="Effects sizes of variables on Hb prediction",
#          x="Regression coefficient")
#   return(posterior.plot)
# }

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
    labs(title="Effects sizes of variables on Hb prediction",
         x="Regression coefficient", y="") +
    theme_classic()
  return(list(plot=plot, cis=cis))
}



# Creates ROC, and Precision-recall plots, and puts them side-by-side.
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
  # If the below base_size change is made, then the text "Receiver operating characteristic" fits in the title
  # of figure of width 60 mm. But it won't change confusion matrix, since that explicitly sets theme_bw.
  #theme_set(  # Set a current theme
  #  theme_gray(base_size = 7.5)
  #)

  roc <- create_roc_new(df$deferral, df$scores)
  pr <- create_precision_recall_new(df$deferral, df$scores)
  #cm <- create_confusion_matrix_plot(df$deferral, df$predicted_labels)
  use_cowplot <- TRUE
  
  if (use_cowplot) {
    performances <- cowplot::plot_grid(roc$roc_plot, pr$pr_plot, labels = c('A', 'B'), label_size = 12, nrow=1, scale=1.0)
    if (!is.null(filename)) {
      # JT: I added device=cairo_pdf below so that special characters (en dash, in this case) get converted properly
      cowplot::save_plot(filename, performances, ncol = 1, base_asp = 2.0, base_width = width / 25.4, base_height = NULL, device=cairo_pdf)
    }
  } else {
    performances <- gridExtra::grid.arrange(roc$roc_plot, pr$pr_plot, nrow = 1, respect=TRUE)   # Combine the plots
    if (!is.null(filename)) {
      ggsave(filename=filename, performances, width = width, units="mm", dpi=600, scale=1.0)
    }
  }
  performances
}

# Creates scatter and confusion plots, and puts them side-by-side.
# Input should be a tibble (or data.frame) that contains the following columns:
# - deferral, boolean or integer (FALSE==0==accepted, TRUE==1==deferred), true labels
# - predicted_labels, same definition as for above
# - scores, double, higher score means that deferral is more likely
# Returns the combined plot object.
# If filename if given, then the combined plot is saved to that file.
# BUG: The plot may look good when saved to a file, but possibly not when the plot object is shown on screen.
create_scatter_confusion_plots <- function(df, Hb_cutoff,
                                     filename=NULL,
                                     width = 180  # width of the combined figure in millimetres
) {

  scatter_plot <- create_scatter_plot(df, Hb_cutoff) + coord_fixed() + theme(plot.margin = unit(c(5.5, 5.5, 0, 0), "pt"))
  cm <- create_confusion_matrix_plot(df$deferral, df$predicted_labels)
  use_cowplot <- TRUE
  
  if (use_cowplot) {
    scatter_confusion <- cowplot::plot_grid(scatter_plot, cm, labels = c('A', 'B'), label_size = 12, nrow=1, scale=1.0, axis="tb", align="h")
    if (!is.null(filename)) {
      #cowplot::save_plot(filename, scatter_confusion, ncol = 1, base_asp = 2.0, base_width = width / 25.4, base_height = NULL)
      cowplot::save_plot(filename, scatter_confusion, ncol = 2, base_asp = 1.0, base_width = width / 25.4 / 2, base_height = NULL)
    }
  } else {
    scatter_confusion <- gridExtra::grid.arrange(scatter_plot, cm, nrow = 1, respect=TRUE)   # Combine the plots
    if (!is.null(filename)) {
      ggsave(filename=filename, scatter_confusion, width = width, units="mm", dpi=600, scale=1.0)
    }
  }
  scatter_confusion
}


validate_fit <- function(fit, original_Hb, orig_labels, Hb_cutoff, scores, params, pnames = NULL, metric = "mean", 
                         cat.plot = TRUE,
                         use_optimal_cutoff=FALSE) {
  # Posterior effect sizes
  #x <- as.matrix(fit, pars = params)
  #posterior.plot <- create_forest_plot_old(x, pnames)
  samples <- as_tibble(rstan::extract(fit, params))
  posterior.plot <- create_forest_plot(samples, pnames)

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
  y_pred <- pars$y_pred  # The rows are iterations and columns correspond to donors
  
  if (metric == "mean") {
    y_pred <- colMeans(y_pred)
  } else if (metric == "quantile") {
    y_pred <- apply(y_pred, 2, quantile, 0.05)
  }
  sds <- apply(pars$y_pred, 2, FUN = sd)
  
  Hb_predictions <- denormalize(y_pred, original_Hb)
  pred_labels <- ifelse(Hb_predictions < Hb_cutoff, 1, 0)

  
  df <- tibble(predicted = Hb_predictions, sds=sds, observed = original_Hb, 
               predicted_labels=pred_labels, deferral = orig_labels, scores=scores)
  
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
  mae  <- ModelMetrics::mae(df$observed, df$predicted)
  rmse <- ModelMetrics::rmse(df$observed, df$predicted)

  original_Hb2 <- to_mmol_per_litre(df$observed)
  Hb_predictions2 <- to_mmol_per_litre(df$predicted)
  mae2  <- ModelMetrics::mae(original_Hb2, Hb_predictions2)
  rmse2 <- ModelMetrics::rmse(original_Hb2, Hb_predictions2)
  
  # F1 score and confidence intervals
  f1_ci <- get_f1_ci(df)
  

  
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
              samples = samples,
              f1_ci=f1_ci), 
           roc, 
           pr))
}
