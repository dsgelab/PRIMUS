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
    ref_sex <- "Male"  
    ref_specialty <- ""  # No specialty is reference
    
    # Get all specialties
    specialties <- levels(df_model$SPECIALTY)
    
    # ============================================================================
    # PLOT 1: SPECIALTY BASELINE ODDS RATIOS 
    # ============================================================================
    
    specialty_coefs <- data.frame(
        SPECIALTY = specialties,
        baseline_coef = 0,  
        baseline_se = 0,
        baseline_pval = NA,
        stringsAsFactors = FALSE
    )
    
    # Fill in coefficients for non-reference specialties
    ref_index <- which(specialties == ref_specialty)
    for (i in seq_along(specialties)) {
        if (i != ref_index) {
            spec_name <- paste0("SPECIALTY", specialties[i])
            if (spec_name %in% rownames(coef_table)) {
                specialty_coefs$baseline_coef[i] <- coef_table[spec_name, "Estimate"]
                specialty_coefs$baseline_se[i] <- coef_table[spec_name, "Std. Error"]
                specialty_coefs$baseline_pval[i] <- coef_table[spec_name, "Pr(>|t|)"]
            }
        }
    }
    
    # Convert log-odds to odds ratios and compute 95% CI
    specialty_coefs$OR <- exp(specialty_coefs$baseline_coef)
    specialty_coefs$OR_lower <- exp(specialty_coefs$baseline_coef - 1.96 * specialty_coefs$baseline_se)
    specialty_coefs$OR_upper <- exp(specialty_coefs$baseline_coef + 1.96 * specialty_coefs$baseline_se)
    
    # For reference specialty, set OR = 1 and no confidence interval
    specialty_coefs$OR[ref_index] <- 1
    specialty_coefs$OR_lower[ref_index] <- 1
    specialty_coefs$OR_upper[ref_index] <- 1

    p1 <- ggplot(specialty_coefs, aes(x = reorder(SPECIALTY, OR))) +
        geom_col(aes(y = OR - 1, fill = ifelse(is.na(baseline_pval), FALSE, baseline_pval < 0.05)), 
                 alpha = 0.75, width = 0.5) +
        geom_errorbar(aes(ymin = OR_lower - 1, ymax = OR_upper - 1), width = 0.3, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
        scale_y_continuous(labels = function(x) x + 1, name = "Odds Ratio",
                           expand = c(0, 0),
                           limits = function(x) {
                               max_abs <- max(abs(x), na.rm = TRUE)
                               max_abs <- min(max_abs, 10)  # Cap at 10
                               c(-max_abs, max_abs)
                           }) +
        scale_fill_manual(
            values = c("FALSE" = "gray70", "TRUE" = "steelblue"), 
            name = "Significant", 
            labels = c("FALSE" = "No", "TRUE" = "Yes"),            
            drop = FALSE,
            breaks = c("FALSE", "TRUE")
        ) +
        labs(title = "A) Baseline Odds Ratios by Specialty",
             subtitle = paste0("Reference: No specialty"),
             x = "Specialty", 
             y = "Odds Ratio") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              panel.grid.minor = element_blank()) +
        coord_flip()
    
    # ============================================================================
    # PLOT 2: PERIOD EFFECTS BY SPECIALTY 
    # ============================================================================
    
    period_effects <- data.frame(
        SPECIALTY = specialties,
        period_effect = 0, 
        period_se = 0,
        period_pval = NA,
        stringsAsFactors = FALSE
    )
    
    # Reference specialty gets the main period effect
    ref_index <- which(specialties == ref_specialty)
    if ("PERIODAFTER" %in% rownames(coef_table)) {
        period_effects$period_effect[ref_index] <- coef_table["PERIODAFTER", "Estimate"]
        period_effects$period_se[ref_index] <- coef_table["PERIODAFTER", "Std. Error"]
        period_effects$period_pval[ref_index] <- coef_table["PERIODAFTER", "Pr(>|t|)"]
    }
    
    # Other specialties get main effect + interaction
    for (i in seq_along(specialties)) {
        if (i != ref_index) {

            interaction_name <- paste0("PERIODAFTER:SPECIALTY", specialties[i])
            main_effect <- ifelse("PERIODAFTER" %in% rownames(coef_table), coef_table["PERIODAFTER", "Estimate"], 0)
            
            if (interaction_name %in% rownames(coef_table)) {
                period_effects$period_effect[i] <- main_effect + coef_table[interaction_name, "Estimate"]
                period_effects$period_se[i] <- coef_table[interaction_name, "Std. Error"]
                period_effects$period_pval[i] <- coef_table[interaction_name, "Pr(>|t|)"]
            } 

            else {
                period_effects$period_effect[i] <- main_effect
                period_effects$period_se[i] <- ifelse("PERIODAFTER" %in% rownames(coef_table),coef_table["PERIODAFTER", "Std. Error"], 0)
                period_effects$period_pval[i] <- ifelse("PERIODAFTER" %in% rownames(coef_table),coef_table["PERIODAFTER", "Pr(>|t|)"], NA)
            }
        }
    }
    
    
    # Convert to odds ratios
    period_effects$OR <- exp(period_effects$period_effect)
    period_effects$OR_lower <- exp(period_effects$period_effect - 1.96 * period_effects$period_se)
    period_effects$OR_upper <- exp(period_effects$period_effect + 1.96 * period_effects$period_se)

    p2 <- ggplot(period_effects, aes(x = reorder(SPECIALTY, OR))) +
        geom_col(aes(y = OR - 1, fill = ifelse(is.na(period_pval), FALSE, period_pval < 0.05)), alpha = 0.75, width = 0.5) +
        geom_errorbar(aes(ymin = OR_lower - 1, ymax = OR_upper - 1), width = 0.3, color = "black") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
        scale_y_continuous(labels = function(x) x + 1, name = "Odds Ratio",
                           expand = c(0, 0),
                           limits = function(x) {
                               max_abs <- max(abs(x), na.rm = TRUE)
                               max_abs <- min(max_abs, 10)  # Cap at 10
                               c(-max_abs, max_abs)
                           }) +
        scale_fill_manual(
            values = c("FALSE" = "gray70", "TRUE" = "darkgreen"), 
            name = "Significant", 
            labels = c("FALSE" = "No", "TRUE" = "Yes"),            
            drop = FALSE,
            breaks = c("FALSE", "TRUE")
        ) +
        labs(title = "B) Specialty × Period Interactions",
             subtitle = "Change from BEFORE to AFTER Event for Each Specialty",
             x = "Specialty", 
             y = "Odds Ratio") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              panel.grid.minor = element_blank()) +
        coord_flip()
    
    # ============================================================================
    # PLOT 3: AGE AND SEX BASELINE EFFECTS
    # ============================================================================
    
    # Get baseline age, event year, and sex effects
    age_sex_baseline_data <- data.frame(
        Variable = character(0),
        Coefficient = numeric(0),
        SE = numeric(0),
        P_value = numeric(0),
        stringsAsFactors = FALSE
    )
    
    # Check for age baseline effects (including quadratic terms)
    age_vars <- c("AGE_IN_2023", "AGE_AT_EVENT", "I(AGE_IN_2023^2)", "I(AGE_AT_EVENT^2)")
    age_labels <- c("Age in 2023", "Age at Event", "Age in 2023 (Quadratic)", "Age at Event (Quadratic)")  
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

    
    # Check for sex baseline effect
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
        
        p3 <- ggplot(age_sex_baseline_data, aes(x = reorder(Variable, OR))) +
            geom_col(aes(y = OR - 1, fill = P_value < 0.05), alpha = 0.75, width = 0.5) +
            geom_errorbar(aes(ymin = OR_lower - 1, ymax = OR_upper - 1), width = 0.3, color = "black") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
            scale_y_continuous(labels = function(x) x + 1, name = "Odds Ratio",
                               expand = c(0, 0),
                               limits = function(x) {
                                   max_abs <- max(abs(x), na.rm = TRUE)
                                   max_abs <- min(max_abs, 10)  # Cap at 10
                                   c(-max_abs, max_abs)
                               }) +
            scale_fill_manual(
                values = c("FALSE" = "gray70", "TRUE" = "purple"), 
                name = "Significant", 
                labels = c("FALSE" = "No", "TRUE" = "Yes"),            
                drop = FALSE,
                breaks = c("FALSE", "TRUE")
            ) +
            labs(title = "C) Age and Sex Baseline Effects",
                 x = "Variable",
                 y = "Odds Ratio") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank()) +
            coord_flip()
    } else {
        p3 <- ggplot() + 
            annotate("text", x = 0, y = 0, label = "No significant\nAge or Sex baseline effects", size = 6, color = "gray50") +
            labs(title = "C) Age and Sex Baseline Effects") +
            theme_void()
    }
    
    # ============================================================================
    # PLOT 4: AGE AND SEX INTERACTIONS WITH PERIOD
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

        p4 <- ggplot(interaction_data, aes(x = reorder(Variable, OR))) +
            geom_col(aes(y = OR - 1, fill = P_value < 0.05), alpha = 0.75, width = 0.5) +
            geom_errorbar(aes(ymin = OR_lower - 1, ymax = OR_upper - 1), width = 0.3, color = "black") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
            scale_y_continuous(labels = function(x) x + 1, name = "Odds Ratio",
                               expand = c(0, 0),
                               limits = function(x) {
                                   max_abs <- max(abs(x), na.rm = TRUE)
                                   max_abs <- min(max_abs, 10)  # Cap at 10
                                   c(-max_abs, max_abs)
                               }) +
            scale_fill_manual(
                values = c("FALSE" = "gray70", "TRUE" = "orange"), 
                name = "Significant", 
                labels = c("FALSE" = "No", "TRUE" = "Yes"),            
                drop = FALSE,
                breaks = c("FALSE", "TRUE")
            ) +
            labs(title = "D) Age and Sex × Period Interactions",
                 subtitle = "Change from BEFORE to AFTER Event for Age and Sex",
                 x = "Interaction Term", 
                 y = "Odds Ratio") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.position = "bottom",
                  panel.grid.minor = element_blank()) +
            coord_flip()
    } else {
        p4 <- ggplot() + 
            annotate("text", x = 0, y = 0, label = "No significant\nAge and Sex × Period interactions", size = 6, color = "gray50") +
            labs(title = "D) Age and Sex × Period Interactions") +
            theme_void()
    }
    
    # ============================================================================
    # COMBINE ALL PLOTS
    # ============================================================================

    # Arrange plots in a 2x2 grid
    combined_plot <- arrangeGrob(
        p1, p2, p3, p4, 
        ncol = 2, nrow = 2, 
        top = paste0(
            "GLM Model Results (Odds Ratios)\n",
            "Reference Specialty: No specialty",
            ", Reference Sex: Male",
            ", Reference Age in 2023: ", ref_age_2023, 
            ", Reference Age at Event: ", ref_age_event
        )
    )

    # Return the individual plots and combined plot object
    return(list(
        baseline = p1, 
        period = p2, 
        age_sex_baseline = p3, 
        age_sex_interactions = p4, 
        combined = combined_plot
    ))
}