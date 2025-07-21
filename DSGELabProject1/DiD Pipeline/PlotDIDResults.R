library(ggplot2)
library(gridExtra)
library(dplyr)
library(fixest)

# Function to create comprehensive GLM model visualization (Odds Ratios)
create_model_visualization <- function(model, df_model, outdir) {
    
    # Get model coefficients using fixest methods
    coef_table <- summary(model)$coeftable
    
    # Define reference values for predictions
    ref_age_2023 <- median(df_model$AGE_IN_2023, na.rm = TRUE)
    ref_age_event <- median(df_model$AGE_AT_EVENT, na.rm = TRUE)
    ref_event_year <- median(df_model$EVENT_YEAR, na.rm = TRUE)
    ref_sex <- levels(df_model$SEX)[1]  # "Male" is reference
    
    # Get all specialties
    specialties <- levels(df_model$SPECIALTY)
    
    # ============================================================================
    # PLOT 1: SPECIALTY BASELINE ODDS RATIOS (vs reference specialty)
    # ============================================================================
    
    # Extract specialty main effects
    specialty_coefs <- data.frame(
        SPECIALTY = specialties,
        baseline_coef = 0,  # Log-odds coefficient (reference = 0)
        baseline_se = 0,
        baseline_pval = NA,
        stringsAsFactors = FALSE
    )
    
    # Fill in coefficients for non-reference specialties
    for (i in 2:length(specialties)) {
        spec_name <- paste0("SPECIALTY", specialties[i])
        if (spec_name %in% rownames(coef_table)) {
            specialty_coefs$baseline_coef[i] <- coef_table[spec_name, "Estimate"]
            specialty_coefs$baseline_se[i] <- coef_table[spec_name, "Std. Error"]
            specialty_coefs$baseline_pval[i] <- coef_table[spec_name, "Pr(>|t|)"]
        }
    }
    
    # Convert log-odds to odds ratios and compute 95% CI
    specialty_coefs$OR <- exp(specialty_coefs$baseline_coef)
    specialty_coefs$OR_lower <- exp(specialty_coefs$baseline_coef - 1.96 * specialty_coefs$baseline_se)
    specialty_coefs$OR_upper <- exp(specialty_coefs$baseline_coef + 1.96 * specialty_coefs$baseline_se)
    
    # For reference specialty, set OR = 1 and no confidence interval
    specialty_coefs$OR[1] <- 1
    specialty_coefs$OR_lower[1] <- 1
    specialty_coefs$OR_upper[1] <- 1

    p1 <- ggplot(specialty_coefs, aes(x = reorder(SPECIALTY, OR), y = OR)) +
        geom_col(aes(fill = ifelse(is.na(baseline_pval), FALSE, baseline_pval < 0.05)), 
                 alpha = 0.7, width = 0.6) +
        geom_errorbar(aes(ymin = OR_lower, ymax = OR_upper), 
                     width = 0.3, color = "black") +
        geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
        scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70"),
                         name = "Significant", labels = c("No", "Yes")) +
        scale_y_continuous(trans = "log10", 
                          breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                          labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10")) +
        labs(title = "A) Baseline Odds Ratios by Specialty",
             subtitle = paste0("Reference: ", specialties[1], " (OR = 1.0)"),
             x = "Specialty", 
             y = "Odds Ratio (log scale)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              panel.grid.minor = element_blank()) +
        coord_flip()
    
    # ============================================================================
    # PLOT 2: PERIOD EFFECTS BY SPECIALTY (change from before to after)
    # ============================================================================
    
    # Calculate period effects for each specialty
    period_effects <- data.frame(
        SPECIALTY = specialties,
        period_effect = 0,  # Log-odds
        period_se = 0,
        period_pval = NA,
        stringsAsFactors = FALSE
    )
    
    # Reference specialty gets the main period effect
    if ("PERIODAFTER" %in% rownames(coef_table)) {
        period_effects$period_effect[1] <- coef_table["PERIODAFTER", "Estimate"]
        period_effects$period_se[1] <- coef_table["PERIODAFTER", "Std. Error"]
        period_effects$period_pval[1] <- coef_table["PERIODAFTER", "Pr(>|t|)"]
    }
    
    # Other specialties get main effect + interaction
    for (i in 2:length(specialties)) {
        interaction_name <- paste0("PERIODAFTER:SPECIALTY", specialties[i])
        main_effect <- ifelse("PERIODAFTER" %in% rownames(coef_table), 
                             coef_table["PERIODAFTER", "Estimate"], 0)
        
        if (interaction_name %in% rownames(coef_table)) {
            period_effects$period_effect[i] <- main_effect + coef_table[interaction_name, "Estimate"]
            period_effects$period_se[i] <- coef_table[interaction_name, "Std. Error"]
            period_effects$period_pval[i] <- coef_table[interaction_name, "Pr(>|t|)"]
        } else {
            period_effects$period_effect[i] <- main_effect
            period_effects$period_se[i] <- ifelse("PERIODAFTER" %in% rownames(coef_table),
                                                 coef_table["PERIODAFTER", "Std. Error"], 0)
            period_effects$period_pval[i] <- ifelse("PERIODAFTER" %in% rownames(coef_table),
                                                   coef_table["PERIODAFTER", "Pr(>|t|)"], NA)
        }
    }
    
    # Convert to odds ratios
    period_effects$OR <- exp(period_effects$period_effect)
    period_effects$OR_lower <- exp(period_effects$period_effect - 1.96 * period_effects$period_se)
    period_effects$OR_upper <- exp(period_effects$period_effect + 1.96 * period_effects$period_se)

    p2 <- ggplot(period_effects, aes(x = reorder(SPECIALTY, OR), y = OR)) +
        geom_col(aes(fill = ifelse(is.na(period_pval), FALSE, period_pval < 0.05)), 
                 alpha = 0.7, width = 0.6) +
        geom_errorbar(aes(ymin = OR_lower, ymax = OR_upper), 
                     width = 0.3, color = "black") +
        geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
        scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70"),
                         name = "Significant", labels = c("No", "Yes")) +
        scale_y_continuous(trans = "log10",
                          breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                          labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10")) +
        labs(title = "B) Period Effects by Specialty",
             subtitle = "Change from BEFORE to AFTER period (Odds Ratio)",
             x = "Specialty", 
             y = "Period Effect OR (log scale)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              panel.grid.minor = element_blank()) +
        coord_flip()
    
    # ============================================================================
    # PLOT 3: AGE, EVENT_YEAR AND SEX BASELINE EFFECTS (main effects)
    # ============================================================================
    
    # Get baseline age, event year, and sex effects
    age_sex_baseline_data <- data.frame(
        Variable = character(0),
        Coefficient = numeric(0),
        SE = numeric(0),
        P_value = numeric(0),
        stringsAsFactors = FALSE
    )
    
    # Check for age baseline effects
    age_vars <- c("AGE_IN_2023", "AGE_AT_EVENT")
    age_labels <- c("Age in 2023", "Age at Event")
    
    for (i in 1:length(age_vars)) {
        if (age_vars[i] %in% rownames(coef_table)) {
            age_sex_baseline_data <- rbind(age_sex_baseline_data, 
                                         data.frame(
                                             Variable = age_labels[i],
                                             Coefficient = coef_table[age_vars[i], "Estimate"],
                                             SE = coef_table[age_vars[i], "Std. Error"],
                                             P_value = coef_table[age_vars[i], "Pr(>|t|)"],
                                             stringsAsFactors = FALSE
                                         ))
        }
    }
    
    # Check for event year baseline effect
    if ("EVENT_YEAR" %in% rownames(coef_table)) {
        age_sex_baseline_data <- rbind(age_sex_baseline_data,
                                     data.frame(
                                         Variable = "Event Year",
                                         Coefficient = coef_table["EVENT_YEAR", "Estimate"],
                                         SE = coef_table["EVENT_YEAR", "Std. Error"],
                                         P_value = coef_table["EVENT_YEAR", "Pr(>|t|)"],
                                         stringsAsFactors = FALSE
                                     ))
    }
    
    # Check for sex baseline effect (Female vs Male reference)
    if ("SEXFemale" %in% rownames(coef_table)) {
        age_sex_baseline_data <- rbind(age_sex_baseline_data,
                                     data.frame(
                                         Variable = "Sex (Female vs Male)",
                                         Coefficient = coef_table["SEXFemale", "Estimate"],
                                         SE = coef_table["SEXFemale", "Std. Error"],
                                         P_value = coef_table["SEXFemale", "Pr(>|t|)"],
                                         stringsAsFactors = FALSE
                                     ))
    }
    
    if (nrow(age_sex_baseline_data) > 0) {
        # Convert to odds ratios
        age_sex_baseline_data$OR <- exp(age_sex_baseline_data$Coefficient)
        age_sex_baseline_data$OR_lower <- exp(age_sex_baseline_data$Coefficient - 1.96 * age_sex_baseline_data$SE)
        age_sex_baseline_data$OR_upper <- exp(age_sex_baseline_data$Coefficient + 1.96 * age_sex_baseline_data$SE)
        
        p3 <- ggplot(age_sex_baseline_data, aes(x = reorder(Variable, OR), y = OR)) +
            geom_col(aes(fill = P_value < 0.05), alpha = 0.7, width = 0.6) +
            geom_errorbar(aes(ymin = OR_lower, ymax = OR_upper), 
                         width = 0.3, color = "black") +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
            scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "gray70"),
                             name = "Significant", labels = c("No", "Yes")) +
            scale_y_continuous(trans = "log10",
                              breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                              labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10")) +
            labs(title = "C) Age, Event Year and Sex Baseline Effects",
                 subtitle = "Main effects on outcome (Odds Ratio)",
                 x = "Variable", 
                 y = "Baseline Effect OR (log scale)") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank()) +
            coord_flip()
    } else {
        p3 <- ggplot() + 
            annotate("text", x = 0, y = 0, label = "No significant\nAge, Event Year or Sex baseline effects", 
                    size = 6, color = "gray50") +
            labs(title = "C) Age, Event Year and Sex Baseline Effects") +
            theme_void()
    }
    
    # ============================================================================
    # PLOT 4: AGE, EVENT_YEAR AND SEX INTERACTIONS WITH PERIOD
    # ============================================================================
    
    # Prepare interaction data
    interaction_data <- data.frame(
        Variable = character(0),
        Coefficient = numeric(0),
        SE = numeric(0),
        P_value = numeric(0),
        stringsAsFactors = FALSE
    )
    
    # Check for age interactions
    age_interactions <- c("PERIODAFTER:AGE_IN_2023", "PERIODAFTER:AGE_AT_EVENT")
    age_int_labels <- c("Age in 2023 × Period", "Age at Event × Period")
    
    for (i in 1:length(age_interactions)) {
        if (age_interactions[i] %in% rownames(coef_table)) {
            interaction_data <- rbind(interaction_data,
                                    data.frame(
                                        Variable = age_int_labels[i],
                                        Coefficient = coef_table[age_interactions[i], "Estimate"],
                                        SE = coef_table[age_interactions[i], "Std. Error"],
                                        P_value = coef_table[age_interactions[i], "Pr(>|t|)"],
                                        stringsAsFactors = FALSE
                                    ))
        }
    }
    
    # Check for event year interaction
    if ("PERIODAFTER:EVENT_YEAR" %in% rownames(coef_table)) {
        interaction_data <- rbind(interaction_data,
                                data.frame(
                                    Variable = "Event Year × Period",
                                    Coefficient = coef_table["PERIODAFTER:EVENT_YEAR", "Estimate"],
                                    SE = coef_table["PERIODAFTER:EVENT_YEAR", "Std. Error"],
                                    P_value = coef_table["PERIODAFTER:EVENT_YEAR", "Pr(>|t|)"],
                                    stringsAsFactors = FALSE
                                ))
    }
    
    # Check for sex interaction
    if ("PERIODAFTER:SEXFemale" %in% rownames(coef_table)) {
        interaction_data <- rbind(interaction_data,
                                data.frame(
                                    Variable = "Sex (Female) × Period",
                                    Coefficient = coef_table["PERIODAFTER:SEXFemale", "Estimate"],
                                    SE = coef_table["PERIODAFTER:SEXFemale", "Std. Error"],
                                    P_value = coef_table["PERIODAFTER:SEXFemale", "Pr(>|t|)"],
                                    stringsAsFactors = FALSE
                                ))
    }
    
    if (nrow(interaction_data) > 0) {
        # Convert to odds ratios
        interaction_data$OR <- exp(interaction_data$Coefficient)
        interaction_data$OR_lower <- exp(interaction_data$Coefficient - 1.96 * interaction_data$SE)
        interaction_data$OR_upper <- exp(interaction_data$Coefficient + 1.96 * interaction_data$SE)

        p4 <- ggplot(interaction_data, aes(x = reorder(Variable, OR), y = OR)) +
            geom_col(aes(fill = P_value < 0.05), alpha = 0.7, width = 0.6) +
            geom_errorbar(aes(ymin = OR_lower, ymax = OR_upper), 
                         width = 0.3, color = "black") +
            geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
            scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "gray70"),
                             name = "Significant", labels = c("No", "Yes")) +
            scale_y_continuous(trans = "log10",
                              breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                              labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10")) +
            labs(title = "D) Variable × Period Interactions",
                 subtitle = "How variables modify the period effect (Odds Ratio)",
                 x = "Interaction Term", 
                 y = "Interaction Effect OR (log scale)") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank()) +
            coord_flip()
    } else {
        p4 <- ggplot() + 
            annotate("text", x = 0, y = 0, label = "No significant\nVariable × Period interactions", 
                    size = 6, color = "gray50") +
            labs(title = "D) Variable × Period Interactions") +
            theme_void()
    }
    
    # ============================================================================
    # COMBINE ALL PLOTS
    # ============================================================================
    
    # Arrange plots in a 2x2 grid
    combined_plot <- grid.arrange(
        p1, p2, p3, p4, 
        ncol = 2, nrow = 2, 
        top = paste0("GLM Model Results (Odds Ratios)\n", "Reference Specialty: '", specialties[1], "', Reference Sex: Male")
    )

    # Return the individual plots and combined plot object
    return(list(
        baseline = p1, 
        period = p2, 
        age_sex_baseline = p3, 
        age_sex_interactions = p4, 
        combined = combined_plot,
        data = list(
            specialty_coefs = specialty_coefs,
            period_effects = period_effects,
            baseline_effects = if(exists("age_sex_baseline_data")) age_sex_baseline_data else NULL,
            interaction_effects = if(exists("interaction_data")) interaction_data else NULL
        )
    ))
}