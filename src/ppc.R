

posterior_check <- function(fit, y_real) {
  y_pred <- extract(fit, pars = c("y_pred"))$y_pred
  pred_means <- colMeans(y_pred)
  
  deltas <- y_real - pred_means
  
  delta_idx <- as_tibble(cbind(idx = 1:length(deltas), deltas = deltas))
  best <- head(delta_idx[order(abs(delta_idx$deltas)),]$idx, 10)
  most_under <- head(delta_idx[order(delta_idx$deltas, decreasing = TRUE),]$idx, 10) 
  most_over <- head(delta_idx[order(delta_idx$deltas),]$idx, 10)
  
  best_preds <- as.tibble(y_pred[,best])
  best_preds <- gather(best_preds, "V")
  best_preds$V <- factor(best_preds$V, levels = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"))
  vline.dat <- tibble(V = levels(best_preds$V), vl = y_real[best])
  p1 <- ggplot(best_preds, aes(x = value)) + 
    geom_histogram() + 
    geom_vline(data = vline.dat, mapping = aes(xintercept = vl), colour = "red") +  
    facet_wrap(~ V)
  
  over_preds <- as.tibble(y_pred[,most_over])
  over_preds <- gather(over_preds, "V")
  over_preds$V <- factor(over_preds$V, levels = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"))
  vline.dat <- tibble(V = levels(over_preds$V), vl = y_real[most_over])
  p2 <- ggplot(over_preds, aes(x = value)) + 
    geom_histogram() + 
    geom_vline(data = vline.dat, mapping = aes(xintercept = vl), col = "blue") +  facet_wrap(~ V)
  
  under_preds <- as.tibble(y_pred[,most_under])
  under_preds <- gather(under_preds, "V")
  under_preds$V <- factor(under_preds$V, levels = c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10"))
  vline.dat <- tibble(V = levels(under_preds$V), vl = y_real[most_under])
  
  p3 <- ggplot(under_preds, aes(x = value)) + 
    geom_histogram() + 
    geom_vline(data = vline.dat, mapping = aes(xintercept = vl), col = "orange") +  facet_wrap(~ V)
  
  comp_df <- tibble(x = c(y_real, pred_means),
                    cat = c(rep("actual", length(y_real)), rep("prediction", length(pred_means))))
  
  comp_plot <- ggplot(comp_df, aes(x = x)) + 
                geom_histogram(data=subset(comp_df,cat=='actual'),aes(fill=cat),alpha=0.5)+
                geom_histogram(data=subset(comp_df,cat=='prediction'),aes(fill=cat),alpha=0.5)+
                scale_fill_manual(name="category", values=c("orange","blue"),labels=c("actual","prediction"))
  
  return(list(p1 = p1,
              p2 = p2,
              p3 = p3,
              comp_plot = comp_plot))
}