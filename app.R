#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(readr)
library(scales)
library(stringr)

#library(rsconnect)
#rsconnect::deployApp("~/Desktop/NOAA SBB Project/NOAA SBB Project ShinyApp/NOAA_SBB_Shiny_working")


# ---------- Set WD ----------
#setwd("~/NOAA_SBB_Shiny_working")

# ---------- Data ----------
crew <- read_csv("CrewSurvey.csv", show_col_types = FALSE)

landings <- read_csv("NMFS_landings.csv", show_col_types = FALSE)

performance <- read_csv("SSB_PerformanceMeasures.csv", show_col_types = FALSE)
if ("SPECIES" %in% names(performance) && !"FISHERY" %in% names(performance)) {
  performance <- performance %>% rename(FISHERY = SPECIES)
}

reference <- read_csv("ReferencePoints.csv", show_col_types = FALSE)

# Standardize YEAR fields across datasets.
crew <- crew %>% mutate(YEAR = suppressWarnings(as.integer(YEAR)))
landings <- landings %>% mutate(YEAR = suppressWarnings(as.integer(YEAR)))
performance <- performance %>% mutate(YEAR = suppressWarnings(as.integer(YEAR)))
reference <- reference %>% mutate(YEAR = suppressWarnings(as.integer(YEAR)))

# Numeric cleanup for columns stored with commas in CSV.
landings <- landings %>% mutate(
  `Landings pounds`=parse_number(as.character(`Landings pounds`)),
  `Landings metric tons`=parse_number(as.character(`Landings metric tons`)),
  `Landings dollars`=parse_number(as.character(`Landings dollars`))
)
reference <- reference %>% mutate(`Limit metric tons`=parse_number(as.character(`Limit metric tons`)))

# Normalize the shared fishery key without forcing row-level joins.
normalize_fishery <- function(x) {
  y <- str_to_upper(str_squish(as.character(x)))
  y <- case_when(
    y == "MONKFISH" ~ "MONKFISH",
    y %in% c("SCALLOP", "SEA SCALLOP", "SCALLOP, SEA") ~ "SCALLOP, SEA",
    TRUE ~ y
  )
  y
}

crew <- crew %>% mutate(FISHERY_KEY=normalize_fishery(FISHERY))
landings <- landings %>% mutate(FISHERY_KEY=normalize_fishery(FISHERY))
performance <- performance %>% mutate(FISHERY_KEY=normalize_fishery(FISHERY))
reference <- reference %>% mutate(FISHERY_KEY=normalize_fishery(FISHERY))

fishery_labels <- bind_rows(
  crew %>% transmute(FISHERY_KEY, label=FISHERY),
  landings %>% transmute(FISHERY_KEY, label=FISHERY),
  performance %>% transmute(FISHERY_KEY, label=FISHERY),
  reference %>% transmute(FISHERY_KEY, label=FISHERY)) %>% filter(!is.na(FISHERY_KEY), FISHERY_KEY!="") %>%
  group_by(FISHERY_KEY) %>% summarise(label=first(label), .groups="drop")

# Prefer human-readable crew labels for the two cross-dataset fisheries.
fishery_labels <- fishery_labels %>% mutate(label=case_when(
  FISHERY_KEY=="MONKFISH" ~ "Monkfish",
  FISHERY_KEY=="SCALLOP, SEA" ~ "Sea Scallop",
  TRUE ~ as.character(label)
))

fishery_choices <- setNames(fishery_labels$FISHERY_KEY, fishery_labels$label)
fishery_choices <- fishery_choices[order(names(fishery_choices))]

# Shared annual key. YEAR is now available in the crew survey and can be
# compared directly with annual records in the other three datasets.
all_years <- sort(unique(c(
  suppressWarnings(as.integer(crew$YEAR)),
  suppressWarnings(as.integer(landings$YEAR)),
  suppressWarnings(as.integer(performance$YEAR)),
  suppressWarnings(as.integer(reference$YEAR))
)))

all_years <- all_years[!is.na(all_years)]

pretty_names <- c(
  YEAR="Year", SURVEY_WAVE="Survey wave", FISHERY="Fishery", FISHERY_4_CAT="Fishery (4-category)", PRIMARY_PORT="Primary port",
  AGE_CATEGORY="Age category", AGE="Age", EDUCATION_COMBINED="Education", RACE="Race", RACE_BINARY="Race (binary)",
  HISPANIC="Hispanic", INCOME="Income", INCOME_5_CAT="Income category", HEALTH_INSURANCE="Health insurance",
  HEALTH_INSURANCE_BINARY="Health insurance (binary)", MARITAL_STATUS="Marital status", MARITAL_STATUS_BINARY="Marital status (binary)",
  PLACE_OF_BIRTH="Place of birth", PRIMARY_LANGUAGE="Primary language", FAMILY_INVLOVED_IN_COMMERCIAL_FISHING="Family involved in fishing",
  GENERATION_IN_COMMERCIAL_FISHING_CATEGORICAL="Fishing generation", YEARS_IN_COMMERCIAL_FISHING="Years in fishing",
  YEARS_IN_COMMERCIAL_FISHING_CATEGORICAL="Years in fishing category", NUMBER_OWNERS_CATEGORICAL="Number of owners",
  FIRST_CREW_POSITION_COMMERCIAL_FISHING="First crew position", YEARS_ON_CURRENT_VESSEL="Years on current vessel",
  YEARS_ON_CURRENT_VESSEL_CATEGORICAL="Years on current vessel category", PATH_TO_EMPLOYMENT_ON_CURRENT_VESSEL="Path to current employment",
  DIFFICULTY_FINDING_EMPLOYMENT="Difficulty finding employment", TRIP_DURATION_DAYS="Trip duration (days)",
  TRIP_DURATION_DAYS_CATEGORICAL="Trip duration category", HOURS_WORKED_PER_DAY="Hours worked per day",
  HOURS_WORKED_PER_DAY_CATEGORICAL="Hours worked category", AVERAGE_CREW_SIZE="Average crew size",
  AVERAGE_CREW_SIZE_CATEGORICAL="Crew-size category", OWNER_OPERATOR_STATUS="Owner/operator status",
  POSITION_ON_CURRENT_VESSEL="Position on current vessel", POSITION_ON_VESSEL_CONDENSED="Vessel position (condensed)",
  PAYMENT_SYSTEM="Payment system", PAYMENT_SYSTEM_CONDENSED="Payment system (condensed)", BOAT_PERCENT_SHARE="Boat percent share",
  CREW_PERCENT_SHARE="Crew percent share", BASIC="Basic index", SOCPSY="Social/psychological index", SELFACT="Self-actualization index", REZIP="REZIP"
)

label_for <- function(x) if (x %in% names(pretty_names)) unname(pretty_names[[x]]) else gsub("_"," ",x)

cat_vars <- names(crew)[vapply(crew, function(x) is.character(x) || dplyr::n_distinct(x,na.rm=TRUE)<=15, logical(1))]
cat_vars <- setdiff(cat_vars,c(".RESPONDENT_ID","FISHERY_KEY"))
cat_choices <- setNames(cat_vars, vapply(cat_vars,label_for,character(1))); cat_choices <- cat_choices[order(names(cat_choices))]

satisfaction_vars <- c(
  YOUR_ACTUAL_EARNINGS_NO_DK="Actual earnings", PREDICTABILITY_OF_EARNINGS_NO_DK="Predictability of earnings",
  JOB_SAFETY_NO_DK="Job safety", TIME_AWAY_NO_DK="Time away", PHYSICAL_FATIGUE_NO_DK="Physical fatigue",
  HEALTHFULNESS_NO_DK="Healthfulness", ADVENTURE_NO_DK="Adventure", CHALLENGE_NO_DK="Challenge", OWN_BOSS_NO_DK="Being own boss"
)

sat_choices <- setNames(names(satisfaction_vars),unname(satisfaction_vars))
all_choice <- function(x) c("All"="__ALL__",x)

ui <- page_sidebar(
  title="Integrated Fisheries Explorer",
  theme=bs_theme(version=5,bootswatch="flatly"),
  sidebar=sidebar(
    width=330,
    h5("Shared filters"),
    selectInput(
      "fishery",
      "Fishery",
      choices = all_choice(fishery_choices),
      selected = "__ALL__"
    ),
    selectInput(
      "shared_year",
      "Year",
      choices = all_choice(setNames(as.character(all_years), as.character(all_years))),
      selected = "__ALL__"
    ),
    helpText(
      "Fishery and Year are shared keys across datasets. The app filters each dataset synchronously without performing row-level many-to-many joins."
    )
  ),
  navset_card_tab(
    nav_panel(
      "Integrated profile",
      card(
        card_header("Trends"),
        selectizeInput(
          "profile_summary_vars",
          "Variables to display",
          choices = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select one or more variables"
          )
        ),
        checkboxInput(
          "profile_summary_standardize",
          "Standardize variables for comparison (z-scores)",
          value = FALSE
        ),
        plotOutput(
          "profile_plot",
          height = "650px"
        )
      )
    ),
    nav_panel("Crew cross-tab",
              layout_columns(
                card(card_header("Variables"),
                     selectInput("row_var","Row variable",choices=cat_choices,selected="AGE_CATEGORY"),
                     selectInput("col_var","Column variable",choices=cat_choices,selected="HEALTH_INSURANCE_BINARY"),
                     radioButtons("tab_display","Display",choices=c("Count"="count","Row %"="row","Column %"="col","Total %"="total"),selected="count",inline=TRUE),
                     downloadButton("download_crosstab","Download table")
                ),
                card(card_header("Association test"),uiOutput("test_summary")), col_widths=c(7,5)
              ),
              card(card_header("Cross-tabulation"),DTOutput("crosstab")),
              card(card_header("Cross-tab chart"),plotOutput("crosstab_plot",height="480px"))
    ),
    nav_panel("Crew satisfaction",
              layout_columns(
                card(card_header("Satisfaction measure"),
                     selectInput("sat_var","Measure",choices=sat_choices,selected="JOB_SAFETY_NO_DK"),
                     selectInput("sat_group","Compare by",choices=c("Year"="YEAR","Survey wave"="SURVEY_WAVE","Fishery"="FISHERY","Age category"="AGE_CATEGORY","Education"="EDUCATION_COMBINED","Health insurance"="HEALTH_INSURANCE_BINARY","Owner/operator status"="OWNER_OPERATOR_STATUS","Vessel position"="POSITION_ON_VESSEL_CONDENSED"),selected="SURVEY_WAVE"),
                     downloadButton("download_satisfaction","Download summary")
                ),
                value_box("Valid N",textOutput("sat_n"),showcase="N"),
                value_box("Mean score",textOutput("sat_mean"),showcase="x̄"),
                value_box("% satisfied",textOutput("sat_pct"),showcase="%"), col_widths=c(5,2,2,3)
              ),
              card(card_header("Satisfaction by selected group"),DTOutput("sat_table")),
              card(card_header("Mean satisfaction score"),plotOutput("sat_plot",height="480px"))
    ),
    nav_panel("NMFS landings",
              layout_columns(
                card(card_header("Landings filters"),
                     selectInput("landing_state","State",choices=all_choice(sort(unique(na.omit(landings$State)))),selected="__ALL__"),
                     helpText("When a shared Year is selected in the sidebar, it overrides this landings year range."),
                     sliderInput("landing_year","Year range",min=min(landings$YEAR,na.rm=TRUE),max=max(landings$YEAR,na.rm=TRUE),value=range(landings$YEAR,na.rm=TRUE),sep="")
                ),
                card(card_header("Summary"),uiOutput("landings_summary")), col_widths=c(5,7)
              ),
              card(card_header("Commercial landings (metric tons)"),plotOutput("landings_plot",height="480px")),
              card(card_header("Landings records"),DTOutput("landings_table"))
    ),
    nav_panel("Performance measures",
              card(card_header("Fishery performance measures"),DTOutput("performance_table")),
              card(card_header("Performance trend"),
                   selectInput("performance_metric","Measure",choices=c("Vessels","Fishing trips","FMP revenue","Non-FMP revenue","Total revenue per trip"),selected="FMP revenue"),
                   plotOutput("performance_plot",height="480px")
              )
    ),
    nav_panel("Reference points",
              card(card_header("Reference points"),DTOutput("reference_table")),
              card(card_header("Reference point trend"),plotOutput("reference_plot",height="480px"))
    ),
  )
)

server <- function(input,output,session) {
  fishery_selected <- reactive(if (input$fishery=="__ALL__") NULL else input$fishery)
  year_selected <- reactive(if (input$shared_year=="__ALL__") NULL else as.integer(input$shared_year))
  
  # ==========================================================
  # INTEGRATED PROFILE — TRENDS VARIABLE SELECTOR
  # ==========================================================
  
  observe({
    
    # Keep only substantive NMFS landings measures.
    landings_vars <- intersect(
      c(
        "Landings pounds",
        "Landings metric tons",
        "Landings dollars"
      ),
      names(landings)
    )
    
    # Numeric variables from performance and reference-point datasets.
    performance_vars <- names(performance)[vapply(performance, is.numeric, logical(1))]
    reference_vars <- names(reference)[vapply(reference, is.numeric, logical(1))]
    
    performance_vars <- setdiff(performance_vars, "YEAR")
    reference_vars <- setdiff(reference_vars, "YEAR")
    
    choices <- c(
      setNames(
        paste0("landings::", landings_vars),
        paste0("NMFS Landings — ", landings_vars)
      ),
      setNames(
        paste0("performance::", performance_vars),
        paste0("Performance — ", performance_vars)
      ),
      setNames(
        paste0("reference::", reference_vars),
        paste0("Reference points — ", reference_vars)
      )
    )
    
    default_vars <- character(0)
    
    if ("Landings metric tons" %in% landings_vars) {
      default_vars <- c(default_vars, "landings::Landings metric tons")
    }
    
    if ("FMP revenue" %in% performance_vars) {
      default_vars <- c(default_vars, "performance::FMP revenue")
    }
    
    if (length(default_vars) == 0 && length(choices) > 0) {
      default_vars <- unname(choices[1])
    }
    
    updateSelectizeInput(
      session,
      "profile_summary_vars",
      choices = choices,
      selected = default_vars,
      server = TRUE
    )
  })
  
  crew_filtered <- reactive({
    x <- crew
    
    if (!is.null(fishery_selected())) {
      x <- x %>%
        filter(FISHERY_KEY == fishery_selected())
    }
    
    if (!is.null(year_selected())) {
      x <- x %>%
        filter(YEAR == year_selected())
    }
    
    x
  })
  
  landings_filtered <- reactive({
    x <- landings
    if (!is.null(fishery_selected())) x <- x %>% filter(FISHERY_KEY==fishery_selected())
    if (!is.null(year_selected())) x <- x %>% filter(as.integer(YEAR)==year_selected())
    if (input$landing_state!="__ALL__") x <- x %>% filter(State==input$landing_state)
    if (is.null(year_selected())) x <- x %>% filter(YEAR>=input$landing_year[1],YEAR<=input$landing_year[2])
    x
  })
  
  performance_filtered <- reactive({
    x <- performance
    if (!is.null(fishery_selected())) x <- x %>% filter(FISHERY_KEY==fishery_selected())
    if (!is.null(year_selected())) x <- x %>% filter(as.integer(YEAR)==year_selected())
    x
  })
  
  reference_filtered <- reactive({
    x <- reference
    if (!is.null(fishery_selected())) x <- x %>% filter(FISHERY_KEY==fishery_selected())
    if (!is.null(year_selected())) x <- x %>% filter(as.integer(YEAR)==year_selected())
    x
  })
  
  output$profile_plot <- renderPlot({
    req(!is.null(fishery_selected()))
    
    selected_vars <- input$profile_summary_vars
    validate(need(length(selected_vars) > 0, "Select at least one variable to display."))
    
    fish <- fishery_selected()
    plot_data <- list()
    
    for (v in selected_vars) {
      
      # ------------------------------------------------------
      # NMFS LANDINGS
      # ------------------------------------------------------
      if (grepl("^landings::", v)) {
        var_name <- sub("^landings::", "", v)
        
        if (var_name %in% names(landings)) {
          z <- landings %>%
            filter(FISHERY_KEY == fish) %>%
            mutate(
              .VALUE = suppressWarnings(as.numeric(.data[[var_name]]))
            ) %>%
            group_by(YEAR) %>%
            summarise(
              Value = sum(.VALUE, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              Variable = var_name,
              Indicator = NA_character_,
              Source = "NMFS Landings"
            ) %>%
            filter(!is.na(YEAR), !is.na(Value))
          
          plot_data[[length(plot_data) + 1]] <- z
        }
      }
      
      # ------------------------------------------------------
      # PERFORMANCE MEASURES
      # ------------------------------------------------------
      if (grepl("^performance::", v)) {
        var_name <- sub("^performance::", "", v)
        
        if (var_name %in% names(performance)) {
          z <- performance %>%
            filter(FISHERY_KEY == fish) %>%
            transmute(
              YEAR = YEAR,
              Value = suppressWarnings(as.numeric(.data[[var_name]])),
              Variable = var_name,
              Indicator = NA_character_,
              Source = "Performance"
            ) %>%
            filter(!is.na(YEAR), !is.na(Value))
          
          plot_data[[length(plot_data) + 1]] <- z
        }
      }
      
      # ------------------------------------------------------
      # REFERENCE POINTS
      # ------------------------------------------------------
      if (grepl("^reference::", v)) {
        var_name <- sub("^reference::", "", v)
        
        if (var_name %in% names(reference)) {
          z <- reference %>%
            filter(FISHERY_KEY == fish) %>%
            transmute(
              YEAR = YEAR,
              Value = suppressWarnings(as.numeric(.data[[var_name]])),
              Variable = var_name,
              Indicator = if ("Indicator" %in% names(reference)) as.character(Indicator) else NA_character_,
              Source = "Reference points"
            ) %>%
            filter(!is.na(YEAR), !is.na(Value))
          
          plot_data[[length(plot_data) + 1]] <- z
        }
      }
    }
    
    z <- bind_rows(plot_data)
    
    validate(
      need(
        nrow(z) > 0,
        "No data are available for the selected variables and fishery."
      )
    )
    
    z <- z %>%
      mutate(
        Series = if_else(
          Source == "Reference points" & !is.na(Indicator) & Indicator != "",
          paste(Source, Indicator, Variable, sep = " — "),
          paste(Source, Variable, sep = " — ")
        )
      )
    
    if (isTRUE(input$profile_summary_standardize)) {
      z <- z %>%
        group_by(Series) %>%
        mutate(
          Value_plot = if (
            sum(!is.na(Value)) > 1 &&
            stats::sd(Value, na.rm = TRUE) > 0
          ) {
            as.numeric(scale(Value))
          } else {
            Value
          }
        ) %>%
        ungroup()
      
      y_label <- "Standardized value (z-score)"
    } else {
      z <- z %>% mutate(Value_plot = Value)
      y_label <- "Value"
    }
    
    ggplot(
      z,
      aes(
        x = YEAR,
        y = Value_plot,
        group = Series,
        linetype = Series,
        shape = Series
      )
    ) +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2.5, na.rm = TRUE) +
      theme_minimal(base_size = 13) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Trends",
        subtitle = fish,
        x = "Year",
        y = y_label,
        linetype = NULL,
        shape = NULL
      ) +
      guides(
        linetype = guide_legend(nrow = 3, byrow = TRUE),
        shape = guide_legend(nrow = 3, byrow = TRUE)
      ) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.width = grid::unit(1.2, "cm"),
        legend.spacing.x = grid::unit(0.25, "cm"),
        plot.margin = margin(10, 10, 10, 10)
      )
  }, width = 1000, height = 650, res = 96)
  
  crosstab_data <- reactive({
    x <- crew_filtered()
    rv <- input$row_var
    cv <- input$col_var
    
    x <- x %>%
      transmute(
        YEAR = YEAR,
        Row = .data[[rv]],
        Column = .data[[cv]]
      ) %>%
      filter(
        !is.na(Row),
        !is.na(Column),
        Row != "",
        Column != ""
      )
    
    if (is.null(year_selected())) {
      
      x <- x %>%
        filter(!is.na(YEAR))
      
      tab <- x %>%
        count(
          YEAR,
          Row,
          Column,
          name = "n"
        )
      
      if (input$tab_display == "row") {
        
        tab <- tab %>%
          group_by(YEAR, Row) %>%
          mutate(Value = n / sum(n)) %>%
          ungroup()
        
      } else if (input$tab_display == "col") {
        
        tab <- tab %>%
          group_by(YEAR, Column) %>%
          mutate(Value = n / sum(n)) %>%
          ungroup()
        
      } else if (input$tab_display == "total") {
        
        tab <- tab %>%
          group_by(YEAR) %>%
          mutate(Value = n / sum(n)) %>%
          ungroup()
        
      } else {
        
        tab <- tab %>%
          mutate(Value = n)
      }
      
    } else {
      
      tab <- x %>%
        count(
          Row,
          Column,
          name = "n"
        )
      
      if (input$tab_display == "row") {
        
        tab <- tab %>%
          group_by(Row) %>%
          mutate(Value = n / sum(n)) %>%
          ungroup()
        
      } else if (input$tab_display == "col") {
        
        tab <- tab %>%
          group_by(Column) %>%
          mutate(Value = n / sum(n)) %>%
          ungroup()
        
      } else if (input$tab_display == "total") {
        
        tab <- tab %>%
          mutate(Value = n / sum(n))
        
      } else {
        
        tab <- tab %>%
          mutate(Value = n)
      }
    }
    
    tab
  })
  
  crosstab_wide <- reactive({
    
    z <- crosstab_data()
    
    if (is.null(year_selected())) {
      
      z <- z %>%
        select(
          YEAR,
          Row,
          Column,
          Value
        ) %>%
        pivot_wider(
          names_from = Column,
          values_from = Value,
          values_fill = 0
        )
      
    } else {
      
      z <- z %>%
        select(
          Row,
          Column,
          Value
        ) %>%
        pivot_wider(
          names_from = Column,
          values_from = Value,
          values_fill = 0
        )
    }
    
    if (input$tab_display != "count") {
      
      if (is.null(year_selected())) {
        
        z <- z %>%
          mutate(
            across(
              -c(YEAR, Row),
              ~ round(.x * 100, 1)
            )
          )
        
      } else {
        
        z <- z %>%
          mutate(
            across(
              -Row,
              ~ round(.x * 100, 1)
            )
          )
      }
    }
    
    z
  })
  output$crosstab <- renderDT(datatable(crosstab_wide(),options=list(scrollX=TRUE,pageLength=20),rownames=FALSE))
  output$crosstab_plot <- renderPlot({
    
    z <- crosstab_data()
    
    validate(
      need(
        nrow(z) > 0,
        "No data for this selection."
      )
    )
    
    p <- ggplot(
      z,
      aes(
        x = factor(Row),
        y = Value,
        fill = factor(Column)
      )
    ) +
      geom_col(
        position =
          if (input$tab_display == "count")
            "dodge"
        else
          "stack"
      ) +
      theme_minimal(base_size = 12) +
      labs(
        x = label_for(input$row_var),
        y =
          if (input$tab_display == "count")
            "Count"
        else
          "Percent",
        fill = label_for(input$col_var)
      )
    
    # When all survey waves are selected,
    # display a separate panel for each YEAR
    if (is.null(year_selected())) {
      
      p <- p +
        facet_wrap(
          ~ YEAR
        )
    }
    
    # Percentage values are stored internally as proportions
    if (input$tab_display != "count") {
      
      p <- p +
        scale_y_continuous(
          labels = scales::percent_format(
            accuracy = 1
          )
        )
    }
    
    p
  })
  
  output$test_summary <- renderUI({
    x <- crew_filtered(); rv<-input$row_var; cv<-input$col_var
    x <- x %>% filter(!is.na(.data[[rv]]),!is.na(.data[[cv]]))
    tab <- table(x[[rv]],x[[cv]])
    if (nrow(tab)<2 || ncol(tab)<2) return(p("Chi-square test requires at least two categories in each variable."))
    tst <- suppressWarnings(chisq.test(tab)); sparse <- mean(tst$expected<5)
    tagList(h4(paste0("χ² = ",round(unname(tst$statistic),2))),p(paste0("df = ",unname(tst$parameter))),p(paste0("p = ",format.pval(tst$p.value,digits=3,eps=.001))),p(paste0(round(100*sparse,1),"% of expected cells < 5")))
  })
  
  output$download_crosstab <- downloadHandler(filename=function() "crew_crosstab.csv",content=function(file) write_csv(crosstab_wide(),file))
  
  sat_summary <- reactive({
    x<-crew_filtered(); sv<-input$sat_var; gv<-input$sat_group
    x %>% group_by(Group=.data[[gv]]) %>% summarise(N=sum(!is.na(.data[[sv]])),Mean=mean(.data[[sv]],na.rm=TRUE),Median=median(.data[[sv]],na.rm=TRUE),`% satisfied`=100*mean(.data[[sv]]>=4,na.rm=TRUE),.groups="drop")
  })
  
  output$sat_n <- renderText(sum(!is.na(crew_filtered()[[input$sat_var]])))
  output$sat_mean <- renderText(round(mean(crew_filtered()[[input$sat_var]],na.rm=TRUE),2))
  output$sat_pct <- renderText(paste0(round(100*mean(crew_filtered()[[input$sat_var]]>=4,na.rm=TRUE),1),"%"))
  output$sat_table <- renderDT(datatable(sat_summary(),options=list(pageLength=20),rownames=FALSE))
  output$sat_plot <- renderPlot({z<-sat_summary(); ggplot(z,aes(x=reorder(as.character(Group),Mean),y=Mean))+geom_col()+coord_flip()+theme_minimal(base_size=12)+labs(x=label_for(input$sat_group),y="Mean score")+ylim(0,5)})
  output$download_satisfaction <- downloadHandler(filename=function() "crew_satisfaction_summary.csv",content=function(file) write_csv(sat_summary(),file))
  
  output$landings_summary <- renderUI({
    x<-landings_filtered(); tagList(
      p(strong(paste("Records:",comma(nrow(x))))),
      p(paste("Metric tons:",comma(round(sum(x$`Landings metric tons`,na.rm=TRUE))))),
      p(paste("Landings value:",dollar(sum(x$`Landings dollars`,na.rm=TRUE),accuracy=1))),
      p(paste("Years:",if(nrow(x)>0) paste(range(x$YEAR,na.rm=TRUE),collapse="–") else "—"))
    )
  })
  
  output$landings_plot <- renderPlot({
    z<-landings_filtered() %>% group_by(YEAR) %>% summarise(MetricTons=sum(`Landings metric tons`,na.rm=TRUE),.groups="drop")
    validate(need(nrow(z)>0,"No landings data for this selection."))
    ggplot(z,aes(YEAR,MetricTons))+geom_line(linewidth=1)+geom_point()+theme_minimal(base_size=12)+scale_y_continuous(labels=comma)+labs(y="Landings (metric tons)")
  })
  
  output$landings_table <- renderDT(datatable(landings_filtered(),options=list(pageLength=20,scrollX=TRUE),rownames=FALSE))
  
  output$performance_table <- renderDT(datatable(performance_filtered(),options=list(pageLength=20,scrollX=TRUE),rownames=FALSE))
  output$performance_plot <- renderPlot({
    z<-performance_filtered(); validate(need(nrow(z)>0,"No performance data for this fishery.")); m<-input$performance_metric
    ggplot(z,aes(x=YEAR,y=.data[[m]],group=FISHERY_KEY))+geom_line(linewidth=1)+geom_point()+theme_minimal(base_size=12)+scale_y_continuous(labels=comma)+labs(x="YEAR",y=m)
  })
  
  output$reference_table <- renderDT(datatable(reference_filtered(),options=list(pageLength=20,scrollX=TRUE),rownames=FALSE))
  output$reference_plot <- renderPlot({
    z<-reference_filtered(); validate(need(nrow(z)>0,"No reference-point data for this fishery."))
    ggplot(z,aes(x=YEAR,y=`Limit metric tons`,group=Indicator,linetype=Indicator))+geom_line(linewidth=1)+geom_point()+theme_minimal(base_size=12)+scale_y_continuous(labels=comma)+labs(x="YEAR",y="Limit (metric tons)")
  })
}

shinyApp(ui = ui, server = server)
