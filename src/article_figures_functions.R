#all_ids <- c(ids, dummy_ids)

get_name_of_data <- function(s) {
  s <- str_split_fixed(s, "-", 3)[,1]  # Extract the name of the data set
  # Note that recode stupidly uses order "old"="new"
  s <- recode(s, "progesa" = "eProgesa", "finngen" = "Biobank", "findonor" = "FinDonor")  # Prettify
  return(s)
} 

# Show AUROC, AUPR, F1, E6, and E12 as forest plot
create_performance_forest_plot <- function(df) {
  new_names <- c(`€/donation, 6 month deferral` = "E6", `€/donation, 12 month deferral` = "E12")
  #    df <- read_csv("~/FRCBS/results-for-eba-paper/raw-data-2020-11-30.csv")
  #    df <- read_csv("~/FRCBS/results-for-eba-paper/raw-data-2020-12-01.csv")
  
  df <- df %>% 
    filter(variable %in% c("AUROC", "AUPR", "F1", "E6", "E12", "threshold6", "threshold12")) %>%
    pivot_wider(names_from=c("type"), values_from=value) %>%
    mutate(low = ifelse(is.na(low), value, low),     # To make the plot work, copy low=high=value for thresholds
           high = ifelse(is.na(high), value, high))
  
  df <- df %>%
    mutate(Id=factor(Id, levels=rev(all_ids)),
           sex=factor(str_split_fixed(Id, "-", 3)[,2], levels=c("female", "both", "male")),
           model=case_when(variable == "threshold6" ~ "t6",
                           variable == "threshold12" ~ "t12",
                           TRUE ~ str_split_fixed(Id, "-", 3)[,3]),
           model = factor(model, levels=c("lmm", "dlmm", "dt", "rf", "baseline", "t6", "t12")),
           variable = recode(variable, "threshold6" = "Threshold", "threshold12" = "Threshold"),
           variable = factor(variable, levels=c("AUROC", "AUPR", "F1", "E6", "E12", "Threshold")),
           variable = fct_recode(variable, !!!new_names))
  
  
  
  
  # Below we use a dummy table and geom_blank to have the same xlimits for E6 and E12 panels
  xrange <- as.numeric(df %>% filter(variable %in% names(new_names)) %>% summarise(low=min(low), high=max(high))) # range of x-axis for E6 and E12
  xrange <- rep(xrange, 2)
  mytype <- factor(rep(c("E6", "E12"), each=2), levels=c("AUROC", "AUPR", "F1", "E6", "E12"))
  dummy <- tibble(Id="progesa-female-lmm", value=xrange, low=xrange, high=xrange, variable=mytype) # Dummy table to use the same x-axis limits for E6 and E12
  
  dummy <- dummy %>%
    mutate(variable = fct_recode(variable, !!!new_names))
  #print(dummy)
  #print(df)
  
  #key_size <- grid::convertX(theme_get()$legend.key.size, "mm", valueOnly=TRUE)
  #print(key_size)
  key_size <- 1.8   # This results in box of size 6 mm
  #key_size <- unit(6.096, "mm")
  #key_size <- unit(1.2, "lines")
  #grid::convertX(grid::unit(1.2, "lines"), "mm")
  g <- df %>% ggplot(aes(y=Id, x=value, xmin=low, xmax=high)) + 
    ggstance::geom_pointrangeh(aes(colour=sex, shape=model, fill=model)) + 
    #geom_pointrange(aes(colour=sex, fill=sex, shape=model), key_glyph = draw_key_rect) + 
    labs(x="Value", y="Data and model", colour="Sex", shape="Model", fill="Deferral length") + 
    scale_y_discrete(labels=get_name_of_data) +
    scale_colour_discrete(breaks = c("male", "female", "both"), labels=c("Male", "Female", "Both")) +
    scale_fill_manual(breaks=c("t6", "t12"), labels=c("Six months", "Twelve months"), values=1:7) + #breaks = new_names) +
    scale_shape_manual(breaks=c("lmm", "dlmm", "dt", "rf", "baseline"),
                       labels=c("Linear mixed model", "Dynamic linear mixed model", "Decision tree", "Random forest", "Baseline"),
                       values=c("lmm"="square", "dlmm"="circle", "dt"="triangle", "rf"="diamond", "baseline"="plus", "t6"="cross", "t12"="asterisk"))+
                       #values=c(15, 16, 17, 18, 3, 4, 8)) +  # These correspond to the below shapes. The below does not work for some reason!
                       #values=c("square", "circle", "triangle", "diamond", "plus", "cross", "asterisk")) +
    # scale_fill_manual(breaks=c("lmm", "dlmm", "dt", "rf"), 
    #                    labels=c("Linear mixed model", "Dynamic linear mixed model", "Decision tree", "Random forest"),
    #                    values=c("square", "circle", "triangle", "diamond", "cross", "asterisk")) +
    #scale_colour_manual(values = c("female" = 1, "male" = 2, "both" = 3)) +
    guides(colour = guide_legend(override.aes = list(shape = 15, linetype=0, size=key_size)),
           shape = guide_legend(override.aes = list(shape = c("square", "circle", "triangle", "diamond", "plus"), linetype=0)),
           fill = guide_legend(override.aes = list(shape = c("cross", "asterisk"), linetype=0))) +
    #guides(colour = guide_legend(override.aes = list(shape = 15, linetype=0))) +
    #theme(legend.key.size = theme_get()$legend.key.size) +
    geom_blank(data=dummy) +
    facet_wrap("variable", scales="free_x") +
    #theme(legend.position = "right", legend.direction = "vertical") 
    theme(legend.position = "bottom", legend.direction = "vertical")
  return(list(g=g, df=df))
}

