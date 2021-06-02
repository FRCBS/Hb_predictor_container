# Variables that are in use

descript <- tibble(Variable = c("donor", "Hb", "days_to_previous_fb", "age", "previous_Hb_def", 
                                "year", "warm_season", "consecutive_deferrals", "recent_donations",
                                "recent_deferrals", "hour", 
                                "previous_Hb", "Hb_first", "Hb_deferral"), 
                   Pretty = c("Donor ID", "Hemoglobin", "Days to previous full blood donation", "Age", "Previous Hb deferral", 
                              "Year", "Warm season", "Consecutive deferrals", "Recent donations", 
                              "Recent deferrals", "Hour", 
                              "Previous Hb", "First Hb", "Hb deferral"),
                   Type = c("Factor", "numeric", "numeric (int)", "numeric", "boolean",
                            "numeric (int)", "boolean", "numeric (int)", "numeric (int)", "numeric (int)", "numeric",
                            "numeric", "numeric", "boolean"),
                   Explanation = c("Donor identifier",
                                   "Amount of Hemoglobin",
                                   "Time (in days) between Hb measurement and previous full blood donation event",
                                   "Age of donor",
                                   "Indicates whether the donor was deferred from blood donation due to low hemoglobin at previous donation event",
                                   "Year of donation",
                                   "True if donation was given in April-September",
                                   "Number of times the donor has been deferred due to low hemoglobin since last succesful whole blood donation",
                                   "Number of donations in the last two years",
                                   "Number of deferrals due to low hemoglobin in the last two years",
                                   "Time of day when donation was given as hours (e.g. 13:45 = 13.75)",
                                   "Hb value at previous measurement (dynamic linear mixed model)",
                                   "Hb value at first donation of this donor (linear mixed model)",
                                   "Deferred based on low hemoglogin")
)

#tibble_row(Variable="one_deferral", Pretty="At least one deferral", Type="numeric (int)", Explanation="At least one deferral")

donor_descript <- tibble(
  Variable    = c("smoking", "height", "weight", "RNF43_mutant", "prs", "FERRITIN_FIRST", "FERRITIN_LAST", "one_deferral"),
  Pretty      = c("Smoking", "Height", "Weight", "RNF43", "Polygenic score", "First ferritin", "Last ferritin", "At least one deferral"),
  Type        = c("boolean", "numeric", "numeric", "boolean", "numeric", "numeric", "numeric", "numeric (int)"),
  Explanation = c("Does the person smoke", "Height of the donor", "Weight of the donor", 
                  "Mutation at RNF43 gene in chromosome 17 position 58358769", "Polygenic risk score for hemoglobin", 
                  "First measured ferritin value", "Last measured ferritin value", "At least one deferral")
)

# FinDonor donation specific:
descript.fd <- tibble(Variable = c("Eryt", "HKR", "Leuk", "Trom", "MCH", "MCHC", "MCV", "RDW", "CRP", "Ferritin", "TransferrinR"),
                      Pretty = c("Erythrocyte", "HKR", "Leukocyte", "Trombocyte", "MCH", "MCHC", "MCV", "RDW", "CRP", "Ferritin", "Transferrin receptor"),
                      Type = c("Numeric", "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",  "Numeric",
                               "Numeric"),
                      Description = c("Amount of erythrocytes, red blood cells [E12/l]",
                                      "Hematocrit (HTC) the volume percentage of RBCs in whole blood [%]",
                                      "Leukocytes, white blood cell counts [E9/l] (thousand cells per microliter)",
                                      "Thrombocytes/platelets [E9/l]",
                                      "Mean corpuscular hemoglobin, average mass of hemoglobin per red blood cell. Diminished in hypochromic anemias. [pg]",
                                      "Mean corpuscular hemoglobin concentration, average mass of hemoglobin in a litre of RBCs. [g/l]",
                                      "Mean Corpuscular Volume, the average volume of RBCs. [fl]",
                                      "Red blood cell distribution width, is a measure of the range of variation of RBC volume. [%]",
                                      "C-reactive protein, measure of inflammation. Its circulating concentrations rise in response to inflammation. [mg/l]",
                                      "An intracellular protein that stores iron and releases it in controlled fashion. Acts as a buffer against iron deficiency and iron overload. Plasma ferritin is an indirect marker of the total iron strorage. [µg/l]",
                                      "Transferrin receptor (TfR) is a carrier protein for transferrin. [mg/l]"))
# FinDonor donor specific
descript.fd.consts <- tibble(Variable=c("height", "weight", "smoking_status", "physical_condition", "meat_amount", "sleep_quality", "iron_supplement"),
                             Pretty=c("Height", "Weight", "Smoking status", "Physical condition", "Meat amount", "Sleep quality", "Iron supplement"),
                             Type=c("Numeric", "Numeric", "Boolean", "Numeric (int)", "Numeric (int)", "Numeric (int)", "Numeric (int)"),
                             Description=c("Height", "Weight", "Smoking status: no=0, sometimes or daily = 1", 
                                           "Physical condition: very bad=0, bad=1, satisfactory=2, rather good=3, good=4, excellent=5",
                                           "How often meat: less than one weekly=1, 1-3 per week=2, 4-6 per week=3, daily=4, several daily=5", 
                                           "How often do you feel like you've slept enough: never=0, rarely = 1, mostly=2, always or almost = 3",
                                           "How many iron table were eaten"))

# Converts column names of a dataframe 'df' to pretty format using dataframe 'description'.
to_pretty <- function(df, description) {
  old_names <- colnames(df)
  conversion <- description %>% filter(Variable %in% all_of(old_names)) %>% select(Pretty, Variable) %>% deframe()
  #print(conversion)
    #message(sprintf("descript names: %s\n", paste(description$Variable, collapse=" ")))
  #message(sprintf("old_names: %s\n", paste(old_names, collapse=" ")))
  #message(sprintf("new_names: %s\n", paste(new_names, collapse=" ")))
  df %>% rename(!!!conversion)
}

summary_plotter <- function(df, variable_descriptions, color) {
  g <- df %>%
    mutate(across(where(is.logical), as.integer)) %>%
    keep(is.numeric) %>%
    to_pretty(variable_descriptions) %>%
    gather() %>%
    mutate(key = factor(key, levels=variable_descriptions$Pretty))  %>% # Don't sort alphabetically
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(fill = color)
  return(g)
}

double_summary_plotter <- function(male_df, female_df, variable_descriptions, freqpoly = FALSE) {
  df <- bind_rows(male=male_df, female=female_df, .id="Sex")
  #print(df, 5)
  df <- df %>%
    mutate(across(where(is.logical), as.integer)) %>%
    #keep(is.numeric(col) %>%
    select(where(is.numeric) | Sex) %>%
    to_pretty(variable_descriptions) %>%
    #gather() %>%
    pivot_longer(!Sex) %>%
    mutate(name = factor(name, levels=variable_descriptions$Pretty))  # Don't sort alphabetically
  if (freqpoly) {
    g <- df %>%
    ggplot(aes(value, color=Sex)) +
    facet_wrap(~ name, scales = "free") +
    geom_freqpoly()
  } else {
    g <- df %>%
      ggplot(aes(value, fill=Sex)) +
      facet_wrap(~ name, scales = "free") +
      geom_histogram(position="dodge") 
  }

  return(g)
}

time_series_length_plotter <- function(df, color) {
  n <- 20  # These will be shown individually, whereas longer time series will be pooled together
  levels <- c(1:n, "Longer")
  df2 <- df %>% 
    count(donor, name="Length") %>% 
    mutate(Length=factor(ifelse(Length <= n, Length, "Longer"), levels=levels)) %>% 
    count(Length, name="Count") #%>%
    #mutate(Length=fct_reorder(levels))
  g <- df2 %>% ggplot(aes(x = Length, y=Count)) + 
    geom_bar(stat="identity", fill=color) +
    labs(x="Time series length", y="Number of donors")
  g
}



