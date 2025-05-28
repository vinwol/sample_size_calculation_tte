library(shiny)
library(rpact)

ui <- fluidPage(
  tags$head(tags$style(
    shiny::HTML(
      "
      .custom-title {
        font-size: 36px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 20px;
      }
      .styled-text {
        font-size: 16px;
        border: 2px solid blue;
        background-color: #f0f8ff;
        padding: 10px;
      }
      .disclaimer-text {
        font-size: 14px;
        color: black;
        text-align: center;
        margin-bottom: 20px;
        border: 1px solid red;
        padding: 10px;
        background-color: #FFCCFF;
      }
      .disclaimer-title {
        font-size: 18px;
        font-weight: bold;
      }
      .sidebar {
        margin-top: 25px; 
      }
      .main-panel {
        margin-top: 0px; 
      }
      .flex-container {
        display: flex;
        align-items: flex-start;
      }
       .reduced-space {
        margin-bottom: -10px; 
        font-size: 12px;
       }
       .reduced-space-paragraph {
        margin-bottom: 0px; 
        font-size: 12px;
       }
      .bordered-section {
        border: 1px solid #4169E1; 
        padding: 10px;
        margin-bottom: 10px;
        background-color: #F0F8FF;
      }
      .small-h4 {
        font-size: 14px; /* Smaller font size for h4 tags */
      }
       .reduced-vertical-space {
        margin-top: -2px; 
        padding-top: 0px; 
       }
       .short-input {
        width: 120px; 
      }
    "
    )
  )),
  titlePanel(
    tags$div("Sample Size Estimator for Time to Event Endpoint", class = "custom-title")
  ),
  div(
    class = "flex-container",
    sidebarPanel(
      class = "sidebar",
      div(tags$h4("Input:", class = "reduced-vertical-space"),),
      div(
        tags$h4("Number of Stages (kMax):", class = "small-h4"),
        tags$p(
          "kMax represents the total number of times data will be analyzed during the trial.
                 It represents the total number of analyses (interim and final) to be conducted during the trial.
                 For example, kMax = 3 means two interim analyses and one final analysis.",
          class = "reduced-space"
        ),
        numericInput("kMax", "", value = 1, min = 1),
      ),
      div(
        tags$h4("Significance Level Alpha (%):", class = "small-h4"),
        tags$p(
          "Alpha is the probability of making a Type I error, which occurs when the null 
          hypothesis is incorrectly rejected when it is actually true.",
          class = "reduced-space"
        ),
        numericInput(
          "alpha",
          "",
          value = 5,
          min = 0,
          max = 100,
          step = 0.1
        )
      ),
      div(
        tags$h4("Power (%):", class = "small-h4"),
        tags$p(
          "Power is the probability of correctly rejecting the null hypothesis when 
          it is false (i.e. probability of detecting a true treatment effect).",
          class = "reduced-space"
        ),
        numericInput(
          "power",
          "",
          value = 80,
          min = 0,
          max = 100,
          step = 1
        )
      ),
      div(
        tags$h4("Sidedness (1 or 2):", class = "small-h4"),
        tags$p(
          "Sidedness refers to whether the statistical test is one-sided or two-sided.",
          class = "reduced-space-paragraph"
        ),
        tags$p(
          shiny::HTML(
            "<b>One-sided test:</b> Tests for an effect in only one direction 
            (whether the treatment is better than the control)."
          ),
          class = "reduced-space-paragraph"
        ),
        tags$p(
          shiny::HTML(
            "<b>Two-sided test:</b> Tests for an effect in both directions 
            (whether the treatment is either better or worse than the control)."
          ),
          class = "reduced-space"
        ),
        numericInput(
          "sided",
          "",
          value = 2,
          min = 1,
          max = 2,
          step = 1
        )
      ),
      div(
        tags$h4("Median Survival Time in Control Arm (months):", class = "small-h4"),
        tags$p(
          "Time at which 50% of the study population in the control arm has 
          experienced the event of interest (e.g., death).",
          class = "reduced-space"
        ),
        numericInput("median_surv_control", "", value = 8, min = 0)
      ),
      div(
        tags$h4("Hazard Ratio:", class = "small-h4"),
        tags$p(
          "Measure to compare the hazard of an event occurring at any given point 
          in time between control and treatment group.",
          class = "reduced-space"
        ),
        numericInput(
          "HR",
          "",
          value = 0.62,
          min = 0,
          max = 10,
          step = 0.01
        )
      ),
      div(
        tags$h4("Accrual Time (months):", class = "small-h4"),
        tags$p(
          "Period during which participants are being enrolled into the clinical trial.",
          class = "reduced-space"
        ),
        numericInput("accrual_time", "", value = 24, min = 0)
      ),
      div(
        tags$h4("Follow-Up Time (months):", class = "small-h4"),
        tags$p(
          "Period during which participants are monitored after enrollment to 
          observe the occurrence of the event of interest (e.g., death, relapse).",
          class = "reduced-space"
        ),
        numericInput("follow_up_time", "", value = 24, min = 0)
      ),
      div(
        tags$h4("Dropout Rate for Control Arm (in % per year):", class = "small-h4"),
        tags$p(
          "Proportion of participants who leave the control arm before it is 
          completed or before the event of interest occurs.",
          class = "reduced-space"
        ),
        numericInput(
          "dropout_rate_control",
          "",
          value = 10,
          min = 0,
          max = 100,
          step = 1
        )
      ),
      div(
        tags$h4("Dropout Rate for Treatment Arm (in % per year):", class = "small-h4"),
        tags$p(
          "Proportion of participants who leave the treatment arm before it is completed 
          or before the event of interest occurs.",
          class = "reduced-space"
        ),
        numericInput(
          "dropout_rate_treatment",
          "",
          value = 10,
          min = 0,
          max = 100,
          step = 1
        )
      )
    ),
    mainPanel(class = "main-panel",
              h3("Results"),
              htmlOutput("results"))
  )
)

server <- function(input, output) {
  # Reactive expression to calculate the design and sample size.
  result <- reactive({
    median_surv_control <- input$median_surv_control
    lambda_control <- log(2) / median_surv_control
    accrual_time <- input$accrual_time
    follow_up_time <- input$follow_up_time
    design <- rpact::getDesignGroupSequential(
      kMax = input$kMax,
      alpha = round(input$alpha / 100, 3),
      sided = input$sided,
      beta = 1 - round(input$power / 100, 3)
    )
    rpact::getSampleSizeSurvival(
      design,
      lambda2 = lambda_control,
      hazardRatio = input$HR,
      accrualTime = accrual_time,
      followUpTime = follow_up_time,
      dropoutRate1 = input$dropout_rate_treatment / 100,
      dropoutRate2 = input$dropout_rate_control / 100
    )
  })
  
  output$results <- renderPrint({
    res <- result()
    lambda_control <- log(2) / as.numeric(input$median_surv_control)
    # Subjects in treatment arm.
    maxNumberOfSubjects1 <- ceiling(res$numberOfSubjects1[input$kMax])
    # Subjects in control arm.
    maxNumberOfSubjects2 <- ceiling(res$numberOfSubjects2[input$kMax])
    maxNumberOfSubjects <- maxNumberOfSubjects1 + maxNumberOfSubjects2
    probEvent <- rpact::getEventProbabilities(
        time = (input$accrual_time + input$follow_up_time),
        lambda2 = lambda_control,
        hazardRatio = input$HR,
        dropoutRate1 = input$dropout_rate_treatment / 100,
        dropoutRate2 = input$dropout_rate_control / 100,
        accrualTime = input$accrual_time,
        maxNumberOfSubjects = maxNumberOfSubjects
      )
    # Overall events.
    expected_events_overall <-
      ceiling(probEvent$overallEventProbabilities * maxNumberOfSubjects)
    # Expected events in treatment arm.
    expected_events_treatment_arm <-
      ceiling(probEvent$eventProbabilities1 * res$numberOfSubjects1)
    # Expected events in control arm.
    expected_events_control_arm <-
      ceiling(probEvent$eventProbabilities2 * res$numberOfSubjects2)
    expected_events <-
      ceiling(probEvent$eventProbabilities1 * res$numberOfSubjects1[input$kMax]) +
      ceiling(probEvent$eventProbabilities2 * res$numberOfSubjects2[input$kMax])
    if (expected_events_overall != expected_events) {
      expected_events_overall <- expected_events
    }
    critical_value_lower <- NULL
    critical_value_upper <- NULL
    critical_value <- NULL
    mdd_val <- NULL
    if (input$sided == 2) {
        critical_value_lower <- round(res$criticalValuesEffectScaleLower, 3)
        critical_value_upper <- round(res$criticalValuesEffectScaleUpper, 3)
        mdd_val <- critical_value_upper[input$kMax]
    } else {
        critical_value <- round(res$criticalValuesEffectScale, 3)
        mdd_val <- critical_value[input$kMax]
    }
    
    # Results:
    HTML(
      paste0(
        "<div class='bordered-section'><b>Subjects</b><br>",
        "Total Number of Subjects at the predicted time of the final analysis: ",
        maxNumberOfSubjects,
        "<br>",
        if (input$kMax <= 1) {
          paste0(
            "Number of Subjects in Treatment Group: ",
            paste(ceiling(res$numberOfSubjects1), collapse = ", "),
            "<br>",
            "Number of Subjects in Control Group: ",
            paste(ceiling(res$numberOfSubjects2), collapse = ", "),
            "<br>"
          )
        } else {
          paste0(
            "Number of Subjects in Treatment Group at the different stages: ",
            paste(ceiling(res$numberOfSubjects1), collapse = ", "),
            "<br>",
            "Number of Subjects in Control Group at the different stages: ",
            paste(ceiling(res$numberOfSubjects2), collapse = ", "),
            "<br>"
          )
        },
        "</div><div class='bordered-section'><b>Events</b><br>",
        "Number of total expected Events at the predicted time of the final analysis: ",
        expected_events_overall,
        "<br>",
        if (input$kMax <= 1) {
          paste0(
            "Number of expected Events in Control Group: ",
            paste(expected_events_control_arm, collapse = ", "),
            "<br>",
            "Number of expected Events in Treatment Group: ",
            paste(expected_events_treatment_arm, collapse = ", "),
            "<br>"
          )
        } else {
          paste0(
            "Number of expected Events in Control Group at the different stages: ",
            paste(expected_events_control_arm, collapse = ", "),
            "<br>",
            "Number of expected Events in Treatment Group at the different stages: ",
            paste(expected_events_treatment_arm, collapse = ", "),
            "<br>"
          )
        },
        "</div><div class='bordered-section'><b>Event Rates</b><br>",
        "Event Rate in Control Group: ",
        round(lambda_control, 3),
        "<br>",
        "Event Rate in Treatment Group: ",
        round(lambda_control * as.numeric(input$HR), 3),
        "</div><div class='bordered-section'><b>MDD (Minimal Detectable Difference)</b><br>",
        "The critical values are thresholds for statistical significance describing 
        the boundaries within which the null hypothesis could not be rejected.<br>",
        if (input$sided <= 1) {
            if (input$kMax <= 1) {
                paste0(
                    "Critical value (treatment effect scale): ",
                    paste(critical_value, collapse = ", "),
                    "<br>"
                )
            } else {
                paste0(
                    "Critical value (treatment effect scale) at the different stages: ",
                    paste(critical_value, collapse = ", "),
                    "<br>"
                )
            }
        } else {
            if (input$kMax <= 1) {
              paste0(
                "Lower critical value (treatment effect scale): ",
                paste(critical_value_lower, collapse = ", "),
                "<br>",
                "Upper critical value (treatment effect scale): ",
                paste(critical_value_upper, collapse = ", "),
                "<br>"
              )
            } else {
              paste0(
                "Lower critical values (treatment effect scale) at the different stages: ",
                paste(critical_value_lower, collapse = ", "),
                "<br>",
                "Upper critical value (treatment effect scale) at the different stages: ",
                paste(critical_value_upper, collapse = ", "),
                "<br>"
              )
            }
        },
        "</div><div class='bordered-section'><b>Analysis Time</b><br>",
        if (input$kMax <= 1) {
          paste0("Analysis Time (months): ",
                 paste(ceiling(res$analysisTime), collapse = ", "),
                 "<br>")
        } else {
          paste0(
            "Analysis Time (months) for the different stages: ",
            paste(ceiling(res$analysisTime), collapse = ", "),
            "<br>"
          )
        },
        "</div><div class='bordered-section'><b>Duration</b><br>",
        "Total Study Duration (months): ",
        ceiling(res$maxStudyDuration),
        "</div><div class='bordered-section'><b>Summary</b><br>",
        "With the specified HR of <b>", 
        input$HR,
        "</b>, the significance level (alpha) defined at <b>",
        input$alpha,
        "%</b> and the desired power set to <b>", 
        input$power,
        "%</b>, we need approximately <b>",
        expected_events_overall,
        "</b> events and thus a sample size of <b>",
        maxNumberOfSubjects,
        "</b> patients, considering an accrual time of <b>",
        input$accrual_time,
        "</b> months, a follow-up time of <b>",
        input$follow_up_time,
        "</b> months, and a total study duration of <b>",
        ceiling(res$maxStudyDuration),
        "</b> months.<br>
        The critical value of <b>",
        mdd_val,
        "</b> represents the estimated minimal detectable difference (MDD), 
        i.e. the smallest effect size that can be considered statistically 
        significant at the final analysis, indicating a beneficial effect of the 
        treatment.</div>"
      )
    )
  })
}

shinyApp(ui = ui, server = server)
