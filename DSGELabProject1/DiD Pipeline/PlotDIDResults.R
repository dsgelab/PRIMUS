library(ggplot2)
library(gridExtra)
library(dplyr)
library(fixest)

# Function to create comprehensive model visualization
create_model_visualization <- function(model, df_model, outdir) {
    
    # Get model coefficients using fixest methods
    coef_table <- summary(model)$coeftable
    
    # Define reference values for predictions
    ref_age_2023 <- median(df_model$AGE_IN_2023, na.rm = TRUE)
    ref_age_event <- median(df_model$AGE_AT_EVENT, na.rm = TRUE)
    ref_sex <- levels(df_model$SEX)[1]  # "Male" is reference
    
    # Get all specialties
    specialties <- levels(df_model$SPECIALTY)
    
    # ============================================================================
    # PLOT 1: SPECIALTY BASELINE DIFFERENCES (vs reference specialty)
    # ============================================================================
    
    # Extract specialty main effects
    specialty_coefs <- data.frame(
        SPECIALTY = specialties,
        baseline_coef = 0,  # Start with reference
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
    
    # Add significance stars
    specialty_coefs$sig_star <- ifelse(specialty_coefs$baseline_pval < 0.001, "***",
                                      ifelse(specialty_coefs$baseline_pval < 0.01, "**",
                                            ifelse(specialty_coefs$baseline_pval < 0.05, "*", "")))
    
    p1 <- ggplot(specialty_coefs, aes(x = reorder(SPECIALTY, baseline_coef), y = baseline_coef)) +
        geom_col(aes(fill = baseline_pval < 0.05), alpha = 0.7) +
        geom_errorbar(aes(ymin = baseline_coef - 1.96*baseline_se, 
                         ymax = baseline_coef + 1.96*baseline_se), 
                     width = 0.3) +
        geom_text(aes(y = baseline_coef + sign(baseline_coef)*0.1, label = sig_star), 
                 size = 4, vjust = -0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
        labs(title = "A) Baseline Differences by Specialty",
             subtitle = paste0("Reference: ", specialties[1], " (shown as 0)"),
             x = "Specialty", y = "Baseline Difference") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        coord_flip()
    
    # ============================================================================
    # PLOT 2: PERIOD EFFECTS BY SPECIALTY (change from before to after)
    # ============================================================================
    
    # Calculate period effects for each specialty
    period_effects <- data.frame(
        SPECIALTY = specialties,
        period_effect = 0,
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
        interaction_name <- paste0("SPECIALTY", specialties[i], ":PERIODAFTER")
        if (interaction_name %in% rownames(coef_table)) {
            period_effects$period_effect[i] <- coef_table["PERIODAFTER", "Estimate"] + 
                                             coef_table[interaction_name, "Estimate"]
            period_effects$period_se[i] <- coef_table[interaction_name, "Std. Error"]
            period_effects$period_pval[i] <- coef_table[interaction_name, "Pr(>|t|)"]
        } else {
            period_effects$period_effect[i] <- coef_table["PERIODAFTER", "Estimate"]
            period_effects$period_se[i] <- coef_table["PERIODAFTER", "Std. Error"]
            period_effects$period_pval[i] <- NA
        }
    }
    
    period_effects$sig_star <- ifelse(period_effects$period_pval < 0.001, "***",
                                     ifelse(period_effects$period_pval < 0.01, "**",
                                           ifelse(period_effects$period_pval < 0.05, "*", "")))
    
    p2 <- ggplot(period_effects, aes(x = reorder(SPECIALTY, period_effect), y = period_effect)) +
        geom_col(aes(fill = period_pval < 0.05), alpha = 0.7) +
        geom_errorbar(aes(ymin = period_effect - 1.96*period_se, 
                         ymax = period_effect + 1.96*period_se), 
                     width = 0.3) +
        geom_text(aes(y = period_effect + sign(period_effect)*0.1, label = sig_star), 
                 size = 4, vjust = -0.5) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray70")) +
        labs(title = "B) Period Effects by Specialty",
             subtitle = "Change from BEFORE to AFTER period",
             x = "Specialty", y = "Period Effect (After - Before)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        coord_flip()
    
    # ============================================================================
    # PLOT 3: AGE AND SEX BASELINE EFFECTS (main effects)
    # ============================================================================
    
    # Get baseline age and sex effects
    age_sex_baseline_data <- data.frame(
        Variable = character(0),
        Coefficient = numeric(0),
        SE = numeric(0),
        P_value = numeric(0),
        stringsAsFactors = FALSE
    )
    
    # Check for age baseline effects
    age_vars <- c("AGE_IN_2023", "AGE_AT_EVENT")
    for (age_var in age_vars) {
        if (age_var %in% rownames(coef_table)) {
            age_sex_baseline_data <- rbind(age_sex_baseline_data, 
                                         data.frame(
                                             Variable = ifelse(age_var == "AGE_IN_2023", "Age in 2023", "Age at Event"),
                                             Coefficient = coef_table[age_var, "Estimate"],
                                             SE = coef_table[age_var, "Std. Error"],
                                             P_value = coef_table[age_var, "Pr(>|t|)"],
                                             stringsAsFactors = FALSE
                                         ))
        }
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
        age_sex_baseline_data$sig_star <- ifelse(age_sex_baseline_data$P_value < 0.001, "***",
                                               ifelse(age_sex_baseline_data$P_value < 0.01, "**",
                                                     ifelse(age_sex_baseline_data$P_value < 0.05, "*", "")))
        
        p3 <- ggplot(age_sex_baseline_data, aes(x = Variable, y = Coefficient)) +
            geom_col(aes(fill = P_value < 0.05), alpha = 0.7) +
            geom_errorbar(aes(ymin = Coefficient - 1.96*SE, 
                             ymax = Coefficient + 1.96*SE), 
                         width = 0.3) +
            geom_text(aes(y = Coefficient + sign(Coefficient)*max(SE, na.rm = TRUE)*0.5, 
                         label = sig_star), 
                     size = 4, vjust = -0.5) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            scale_fill_manual(values = c("TRUE" = "purple", "FALSE" = "gray70")) +
            labs(title = "C) Age and Sex Baseline Effects",
                 subtitle = "Main effects of age and sex on outcome",
                 x = "Variable", y = "Baseline Effect") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")
    } else {
        p3 <- ggplot() + 
            annotate("text", x = 0, y = 0, label = "No significant\nAge or Sex baseline effects", 
                    size = 6, color = "gray50") +
            labs(title = "C) Age and Sex Baseline Effects") +
            theme_void()
    }
    
    # ============================================================================
    # PLOT 4: AGE AND SEX INTERACTIONS WITH PERIOD (combined)
    # ============================================================================
    
    # Check if age interactions exist
    age_interactions <- c("AGE_IN_2023:PERIODAFTER", "AGE_AT_EVENT:PERIODAFTER")
    age_int_data <- data.frame(
        Variable = c("Age in 2023", "Age at Event"),
        Coefficient = NA,
        SE = NA,
        P_value = NA,
        stringsAsFactors = FALSE
    )
    
    for (i in 1:length(age_interactions)) {
        if (age_interactions[i] %in% rownames(coef_table)) {
            age_int_data$Coefficient[i] <- coef_table[age_interactions[i], "Estimate"]
            age_int_data$SE[i] <- coef_table[age_interactions[i], "Std. Error"]
            age_int_data$P_value[i] <- coef_table[age_interactions[i], "Pr(>|t|)"]
        }
    }
    
    # Get sex interaction data
    sex_int_coef <- NA
    sex_int_se <- NA
    sex_int_pval <- NA
    
    if ("SEXFemale:PERIODAFTER" %in% rownames(coef_table)) {
        sex_int_coef <- coef_table["SEXFemale:PERIODAFTER", "Estimate"]
        sex_int_se <- coef_table["SEXFemale:PERIODAFTER", "Std. Error"]
        sex_int_pval <- coef_table["SEXFemale:PERIODAFTER", "Pr(>|t|)"]
    }
    
    # Combine age and sex data
    combined_int_data <- rbind(
        age_int_data[!is.na(age_int_data$Coefficient), ],
        if (!is.na(sex_int_coef)) {
            data.frame(
                Variable = "Sex (Female vs Male)",
                Coefficient = sex_int_coef,
                SE = sex_int_se,
                P_value = sex_int_pval,
                stringsAsFactors = FALSE
            )
        } else {
            data.frame(Variable = character(0), Coefficient = numeric(0), 
                      SE = numeric(0), P_value = numeric(0), 
                      stringsAsFactors = FALSE)
        }
    )
    
    if (nrow(combined_int_data) > 0) {
        combined_int_data$sig_star <- ifelse(combined_int_data$P_value < 0.001, "***",
                                           ifelse(combined_int_data$P_value < 0.01, "**",
                                                 ifelse(combined_int_data$P_value < 0.05, "*", "")))
        
        p4 <- ggplot(combined_int_data, aes(x = Variable, y = Coefficient)) +
            geom_col(aes(fill = P_value < 0.05), alpha = 0.7) +
            geom_errorbar(aes(ymin = Coefficient - 1.96*SE, 
                             ymax = Coefficient + 1.96*SE), 
                         width = 0.3) +
            geom_text(aes(y = Coefficient + sign(Coefficient)*max(SE, na.rm = TRUE)*0.5, 
                         label = sig_star), 
                     size = 4, vjust = -0.5) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "gray70")) +
            labs(title = "D) Age and Sex Interactions with Period",
                 subtitle = "How age and sex modify the period effect",
                 x = "Variable", y = "Interaction Coefficient") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "none")
    } else {
        # Create placeholder if no interactions
        p4 <- ggplot() + 
            annotate("text", x = 0, y = 0, label = "No significant\nAge or Sex interactions", 
                    size = 6, color = "gray50") +
            labs(title = "D) Age and Sex Interactions with Period") +
            theme_void()
    }
    
    # ============================================================================
    # COMBINE ALL PLOTS
    # ============================================================================
    
    # Arrange plots in a 2x2 grid
    combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2, top = paste0("Comprehensive Model Results\n","Reference Specialty: '", specialties[1], "', Reference Sex: Male"))
    
    # Return the individual plots and combined plot object for potential separate use
    return(list(baseline = p1, period = p2, age_sex_baseline = p3, age_sex_interactions = p4, combined = combined_plot))
}