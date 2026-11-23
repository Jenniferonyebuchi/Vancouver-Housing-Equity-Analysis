library(shiny)
library(dplyr)
library(plotly)
library(readr)

source("scripts/prepare_vancouver_housing_data.R")
prepare_vancouver_housing_data()

df <- read_csv("data/vancouver_housing_tidy.csv", show_col_types = FALSE)

reference_years <- c(2001, 2006, 2011, 2016, 2021)

reference_record <- function(target_year, target_group, target_status = "Total", target_tenure = "Total") {
  record <- df %>%
    filter(
      .data$year == .env$target_year,
      .data$visible_minority_status == .env$target_group,
      .data$core_housing_need_status == .env$target_status,
      .data$tenure == .env$target_tenure
    )

  if (nrow(record) != 1) {
    stop(
      paste("Expected one reference record for", target_group, target_year, target_status, target_tenure),
      call. = FALSE
    )
  }

  record
}

reference_value <- function(
  target_year,
  target_group,
  target_status = "Total",
  target_tenure = "Total",
  field = "household_count"
) {
  record <- reference_record(target_year, target_group, target_status, target_tenure)
  value <- record[[field]][[1]]

  if (is.na(value)) {
    stop(
      paste("Missing", field, "for", target_group, target_year, target_status, target_tenure),
      call. = FALSE
    )
  }

  as.numeric(value)
}

reference_count <- function(target_year, target_group, target_status = "Total", target_tenure = "Total") {
  reference_value(target_year, target_group, target_status, target_tenure)
}

reference_series <- function(group, status = "Total", tenure = "Total", field = "household_count") {
  vapply(reference_years, function(year) {
    reference_value(year, group, status, tenure, field)
  }, numeric(1))
}

format_percentage <- function(value) {
  paste0(formatC(value, format = "f", digits = 1), "%")
}

core_need_share <- function(group) {
  vapply(reference_years, function(year) {
    total <- reference_count(year, group)
    if (total <= 0) {
      stop("Total household counts must be greater than zero.", call. = FALSE)
    }
    100 * reference_count(year, group, "In core housing need") / total
  }, numeric(1))
}

black_core_need_share <- core_need_share("Black")
chinese_core_need_share <- core_need_share("Chinese")
black_not_need_share <- 100 * reference_series("Black", "Not in core housing need") /
  reference_series("Black")
chinese_not_need_share <- 100 * reference_series("Chinese", "Not in core housing need") /
  reference_series("Chinese")
black_previous_change <- c(NA_real_, diff(black_core_need_share))
chinese_previous_change <- c(NA_real_, diff(chinese_core_need_share))
black_change_since_2001 <- c(NA_real_, black_core_need_share[-1] - black_core_need_share[1])
chinese_change_since_2001 <- c(NA_real_, chinese_core_need_share[-1] - chinese_core_need_share[1])

table5_display <- data.frame(
  Year = reference_years,
  `Black in core housing need (%)` = format_percentage(black_core_need_share),
  `Chinese in core housing need (%)` = format_percentage(chinese_core_need_share),
  `Black not in core housing need (%)` = format_percentage(black_not_need_share),
  `Chinese not in core housing need (%)` = format_percentage(chinese_not_need_share),
  check.names = FALSE
)

format_count <- function(value) {
  formatC(value, format = "f", digits = 0, big.mark = ",")
}

format_currency <- function(value) {
  paste0("$", formatC(value, format = "f", digits = 0, big.mark = ","))
}

reference_categories <- list(
  "Total households" = list(status = "Total", tenure = "Total"),
  "Owner households" = list(status = "Total", tenure = "Owner"),
  "Renter households" = list(status = "Total", tenure = "Renter"),
  "In core housing need" = list(status = "In core housing need", tenure = "Total"),
  "Not in core housing need" = list(status = "Not in core housing need", tenure = "Total")
)

build_reference_table <- function(field, formatter) {
  table <- data.frame(Year = reference_years)

  for (category in names(reference_categories)) {
    for (group in c("Black", "Chinese")) {
      selection <- reference_categories[[category]]
      table[[paste(category, group, sep = " - ")]] <- formatter(
        reference_series(group, selection$status, selection$tenure, field)
      )
    }
  }

  table
}

table1_display <- build_reference_table("household_count", format_count)
table2_display <- build_reference_table("avg_household_income", format_currency)
table3_display <- build_reference_table("avg_monthly_shelter_cost", format_currency)
table4_display <- build_reference_table("avg_stir_pct", format_percentage)

format_percentage_point_change <- function(value) {
  if (is.na(value)) {
    return("n/a")
  }
  paste0(if (value > 0) "+" else "", formatC(value, format = "f", digits = 1))
}

format_percentage_point_changes <- function(values) {
  vapply(values, format_percentage_point_change, character(1))
}

table6_display <- data.frame(
  Year = reference_years,
  `Black share in need (%)` = format_percentage(black_core_need_share),
  `Chinese share in need (%)` = format_percentage(chinese_core_need_share),
  `Black change vs previous census (pp)` = format_percentage_point_changes(black_previous_change),
  `Chinese change vs previous census (pp)` = format_percentage_point_changes(chinese_previous_change),
  `Black change since 2001 (pp)` = format_percentage_point_changes(black_change_since_2001),
  `Chinese change since 2001 (pp)` = format_percentage_point_changes(chinese_change_since_2001),
  check.names = FALSE
)

table7_share <- function(group, tenure) {
  vapply(reference_years, function(year) {
    total <- reference_count(year, group)
    if (total <= 0) {
      stop("Total household counts must be greater than zero.", call. = FALSE)
    }
    100 * reference_count(year, group, target_tenure = tenure) / total
  }, numeric(1))
}

black_owner_share <- table7_share("Black", "Owner")
chinese_owner_share <- table7_share("Chinese", "Owner")
black_renter_share <- table7_share("Black", "Renter")
chinese_renter_share <- table7_share("Chinese", "Renter")

table7_display <- data.frame(
  Year = reference_years,
  `Black owners (%)` = format_percentage(black_owner_share),
  `Chinese owners (%)` = format_percentage(chinese_owner_share),
  `Black renters (%)` = format_percentage(black_renter_share),
  `Chinese renters (%)` = format_percentage(chinese_renter_share),
  check.names = FALSE
)

core_need_trend <- data.frame(
  year = rep(reference_years, times = 2),
  group = rep(c("Black", "Chinese"), each = length(reference_years)),
  share = c(black_core_need_share, chinese_core_need_share)
) %>%
  mutate(tooltip = paste0(group, "<br>", year, ": ", format_percentage(share)))

all_groups <- c(
  "Black",
  "South Asian",
  "Chinese",
  "Filipino",
  "Arab",
  "Latin American",
  "Southeast Asian",
  "West Asian",
  "Korean",
  "Japanese",
  "Visible minority total",
  "Not a visible minority"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #f2f6fa; color: #26384a; font-family: Arial, sans-serif; }
      .container-fluid { max-width: 1440px; padding: 24px clamp(16px, 4vw, 48px) 36px; }
      .project-hero {
        background: linear-gradient(120deg, #102a43 0%, #173e63 62%, #1d6670 100%);
        border-radius: 20px;
        box-shadow: 0 16px 38px rgba(16, 42, 67, 0.16);
        color: #fff;
        margin-bottom: 22px;
        padding: clamp(28px, 5vw, 52px);
      }
      .project-kicker { color: #b9e8e4; font-size: 12px; font-weight: 700; letter-spacing: .12em; margin-bottom: 12px; text-transform: uppercase; }
      .project-hero h1 { font-size: clamp(30px, 4.2vw, 48px); font-weight: 700; line-height: 1.08; margin: 0 0 16px; }
      .project-hero p { color: #e7f0f7; font-size: 17px; line-height: 1.65; margin: 0; max-width: 850px; }
      .project-hero p + p { margin-top: 12px; }
      .project-tags { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 22px; }
      .project-tag { background: rgba(255,255,255,.09); border: 1px solid rgba(255,255,255,.24); border-radius: 999px; color: #fff; font-size: 12px; padding: 7px 11px; }
      .well { background: #fff; border: 1px solid #dce5ee; border-radius: 16px; box-shadow: 0 8px 24px rgba(16, 42, 67, .05); padding: 22px; }
      .sidebarPanel h3 { color: #173451; font-size: 20px; font-weight: 700; margin: 0 0 8px; }
      .sidebarPanel .help-block { color: #5e7082; line-height: 1.5; }
      .control-label { color: #27384d; font-weight: 600; }
      .shiny-input-container { margin-bottom: 12px; }
      .checkbox, .radio { margin-bottom: 7px; margin-top: 7px; }
      .plot-card { background: #fff; border: 1px solid #dce5ee; border-radius: 16px; box-shadow: 0 8px 24px rgba(16, 42, 67, .05); padding: 18px; }
      .chart-heading h3 { color: #173451; font-size: 20px; font-weight: 700; margin: 0 0 6px; }
      .chart-heading p { color: #5e7082; line-height: 1.5; margin: 0 0 10px; }
      #comparisonPlot { max-height: 760px; min-height: 420px; }
      .interpretation-card, .interpretation-empty { background: #fff; border: 1px solid #dce5ee; border-radius: 16px; margin-top: 16px; padding: 20px; }
      .interpretation-card { border-left: 4px solid #2e86de; }
      .interpretation-card h3, .interpretation-empty h3 { color: #173451; font-size: 19px; font-weight: 700; margin: 0 0 8px; }
      .interpretation-card p, .interpretation-empty p { color: #465b70; line-height: 1.6; margin-bottom: 8px; }
      .interpretation-card .interpretation-note { color: #65798c; font-size: 13px; margin-bottom: 0; }
      .project-tabs { margin-top: 20px; }
      .nav-tabs { border-bottom: 1px solid #dce5ee; }
      .nav-tabs > li > a { color: #43576b; font-weight: 600; }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus { background: #fff; border-color: #dce5ee #dce5ee #fff; color: #173e63; }
      .tab-content { padding-top: 18px; }
      .guide-content { margin: 0 auto; max-width: 1120px; }
      .guide-heading { color: #173451; font-size: 30px; font-weight: 700; margin: 0 0 8px; }
      .guide-lead { color: #5e7082; font-size: 16px; line-height: 1.65; margin: 0 0 20px; }
      .guide-section { background: #fff; border: 1px solid #dce5ee; border-radius: 16px; box-shadow: 0 8px 24px rgba(16, 42, 67, .04); margin-bottom: 16px; padding: 24px; }
      .guide-section h2 { color: #173451; font-size: 21px; font-weight: 700; margin: 0 0 12px; }
      .guide-section h3 { color: #294d6b; font-size: 16px; font-weight: 700; margin: 0 0 7px; }
      .guide-section p, .guide-section li { color: #465b70; line-height: 1.65; }
      .guide-section p:last-child { margin-bottom: 0; }
      .guide-grid { display: grid; gap: 12px; grid-template-columns: repeat(auto-fit, minmax(220px, 1fr)); }
      .guide-card { background: #f5f8fb; border: 1px solid #e1e9f0; border-radius: 12px; padding: 16px; }
      .guide-card p { margin: 0; }
      .guide-card h3 { margin-top: 0; }
      .guide-list { margin-bottom: 0; padding-left: 20px; }
      .guide-list li + li { margin-top: 7px; }
      .table-responsive { border: 0; margin-bottom: 12px; }
      .table-responsive .table { background: #fff; margin: 0; width: 100%; }
      .table-responsive .table th { background: #edf3f8; color: #173451; white-space: normal; }
      .table-responsive .table td { white-space: nowrap; }
      #coreNeedTrendPlot { min-height: 340px; }
      .guide-callout { background: #eef7f6; border-left: 4px solid #2a9d8f; border-radius: 8px; color: #35566a; line-height: 1.6; margin-top: 14px; padding: 13px 16px; }
      .guide-callout p { margin: 0; }
      .footer-note { color: #66788a; font-size: 12px; margin-top: 18px; text-align: center; }
      @media (max-width: 767px) {
        .container-fluid { padding: 16px 12px 24px; }
        .project-hero { border-radius: 16px; padding: 24px 20px; }
        .project-hero p { font-size: 15px; }
        .plot-card { padding: 12px; }
        .well { padding: 18px; }
        .guide-section { padding: 18px; }
        .guide-heading { font-size: 25px; }
      }
    "))
  ),
  div(
    class = "project-hero",
    div(class = "project-kicker", "Vancouver housing | 2001-2021"),
    h1("Vancouver Housing Equity Dashboard"),
    p(
      "Vancouver has long faced intense pressure on housing affordability, and the data used in this project suggests that these pressures are not distributed evenly across communities. The dashboard is designed to make visible who is carrying the heaviest burden in terms of shelter costs, household income, and core housing need."
    ),
    p(
      "The goal is not to reduce a complex issue to a single story, but to show where the data points suggest structural inequities in the housing market. In particular, the findings are consistent with the view that Black households in Vancouver experience significant marginalization within the housing system."
    ),
    p(
      "Explore how household income, monthly shelter costs, and shelter-cost-to-income ratios vary across visible-minority groups and core housing need status in Vancouver over time. This dashboard supports descriptive comparison; it does not establish the causes of observed differences."
    ),
    div(
      class = "project-tags",
      span(class = "project-tag", "Interactive data story"),
      span(class = "project-tag", "Income and shelter costs"),
      span(class = "project-tag", "Core housing need")
    )
  ),
  tabsetPanel(
    id = "project-tabs",
    selected = "dashboard",
    type = "tabs",
    tabPanel(
      title = "Explore dashboard",
      value = "dashboard",
      sidebarLayout(
        sidebarPanel(
          h3("Explore the data"),
          p(
            "Choose a year, one or more groups, an indicator, and a tenure. The chart compares average values for households in and not in core housing need.",
            class = "help-block"
          ),
          checkboxGroupInput(
            inputId = "groups",
            label = "Select groups to compare",
            choices = all_groups,
            selected = c("Black", "South Asian", "Chinese", "Filipino", "West Asian", "Not a visible minority")
          ),
          selectInput(
            inputId = "year",
            label = "Choose year",
            choices = sort(unique(df$year)),
            selected = 2021
          ),
          radioButtons(
            inputId = "metric",
            label = "Choose metric",
            choices = c(
              "Average Shelter-Cost-to-Income Ratio (%)" = "avg_stir_pct",
              "Average monthly shelter cost ($)" = "avg_monthly_shelter_cost",
              "Average household income ($)" = "avg_household_income"
            ),
            selected = "avg_stir_pct"
          ),
          radioButtons(
            inputId = "tenure",
            label = "Tenure",
            choices = c("Total", "Owner", "Renter"),
            selected = "Total"
          )
        ),
        mainPanel(
          div(class = "plot-card",
            div(
              class = "chart-heading",
              h3("Compare housing indicators"),
              p("Hover over bars to see the selected group, housing-need status, and value.")
            ),
            plotlyOutput("comparisonPlot", height = "70vh")
          ),
          uiOutput("interpretationPanel"),
          div(class = "footer-note", "Built by Jennifer Ezinne Onyebuchi")
        )
      )
    ),
    tabPanel(
      title = "Project guide",
      value = "guide",
      div(
        class = "guide-content",
        h2(class = "guide-heading", "About this project"),
        p(
          "This project examines how housing affordability and core housing need vary across visible minority groups in Vancouver, with a specific focus on how Black households compare with other groups across time. The work is built around a Shiny dashboard that makes the story accessible to a broad audience through clear visuals and interactive comparisons.",
          class = "guide-lead"
        ),
        div(
          class = "guide-section",
          h2("Research background and objective"),
          p(
            "Vancouver faces significant housing-affordability pressures, and the project asks how those pressures appear across household groups in the available data. The dashboard compares reported household income, monthly shelter costs, shelter-cost-to-income ratios, and core housing need across visible-minority categories and census years, with particular interest in how Black households compare with other groups."
          ),
          p(
            "The objective is to make differences visible and support careful, data-informed discussion. These descriptive comparisons do not identify why differences occur and should not be read as causal evidence."
          )
        ),
        div(
          class = "guide-section",
          h2("Data source and dataset"),
          p(
            "The dataset was obtained through the Canada Mortgage and Housing Corporation (CMHC) table, ",
            tags$a(
              "Household Income and Costs of Visible Minority Groups",
              href = "https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/household-characteristics/household-income-shelter-cost-to-income-ratio-visible-minority-groups-core-need-status",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            ". The packaged workbook identifies Statistics Canada census data as the underlying source. The local source workbook is ",
            tags$code("household-income-costs-visible-minority-groups-2001-2021-en.xlsx"),
            ". The preparation script reads the 2001, 2006, 2011, 2016, and 2021 sheets and retains Vancouver geography rows."
          ),
          div(
            class = "guide-grid",
            div(
              class = "guide-card",
              h3("Coverage"),
              p("The cleaned file contains 675 summarized records across five census years, with Vancouver as the geography.")
            ),
            div(
              class = "guide-card",
              h3("Dimensions"),
              p("Year, geography, core-housing-need status, tenure, and visible-minority status of the primary household maintainer.")
            ),
            div(
              class = "guide-card",
              h3("Measures"),
              p("Household count, average household income, average monthly shelter cost, and average shelter-cost-to-income ratio (STIR).")
            )
          ),
          div(
            class = "guide-callout",
            p("The dataset contains group-level summaries, not records for individual households. Values shown in the chart are the source-provided measures; the app does not recompute the averages.")
          )
        ),
        div(
          class = "guide-section",
          h2("Key findings | 2021 Black households"),
          uiOutput("keyFindingsContent"),
          div(
            class = "guide-callout",
            p("These figures describe the source’s reported group-level counts and averages. They do not explain why the observed differences occur.")
          )
        ),
        div(
          class = "guide-section",
          h2("Housing concepts"),
          div(
            class = "guide-grid",
            div(
              class = "guide-card",
              h3("Core housing need"),
              p("CMHC defines a household as being in core housing need when its housing fails at least one of the adequacy, suitability, or affordability standards and it cannot afford an alternative acceptable unit in its local market. The affordability test uses a threshold of 30% or more of before-tax household income for the median rent of that alternative. The workbook notes an exception for non-family households with a maintainer aged 15 to 29 who is attending school.")
            ),
            div(
              class = "guide-card",
              h3("Households examined"),
              p("The source assesses only private, non-farm, non-band, non-reserve households with income greater than zero and a shelter-cost-to-income ratio below 100%. The app compares the source’s in-need and not-in-need categories.")
            ),
            div(
              class = "guide-card",
              h3("Shelter-cost-to-income ratio"),
              p("STIR is the source-reported percentage measure relating shelter costs to income. A higher percentage means shelter costs represent a larger share relative to income. This dashboard displays the supplied average STIR.")
            ),
            div(
              class = "guide-card",
              h3("Shelter costs and tenure"),
              p("CMHC describes shelter cost as average monthly household shelter expenses, including relevant housing payments and utilities. Tenure distinguishes owner, renter, and total households; the workbook notes that tenure is based on whether any household member owns the dwelling or the household rents it.")
            )
          ),
          div(
            class = "guide-callout",
            p("Visible-minority status in this dataset refers to the status of the primary household maintainer, not a classification of every household member.")
          )
        ),
        div(
          class = "guide-section",
          h2("How to use the dashboard"),
          tags$ol(
            class = "guide-list",
            tags$li("Open the Explore dashboard tab."),
            tags$li("Select one or more groups, then choose a year, metric, and tenure."),
            tags$li("Compare the paired bars for households in and not in core housing need."),
            tags$li("Hover over a bar to see its group, status, and value.")
          ),
          p(
            "The current group selector includes named groups, Visible minority total, and Not a visible minority. Some aggregate categories present in the source file are not offered as selectable groups."
          )
        ),
        div(
          class = "guide-section",
          h2("Limitations and considerations"),
          tags$ul(
            class = "guide-list",
            tags$li("The results are descriptive and do not establish causes of housing differences."),
            tags$li("The measures summarize groups; averages do not show the range of household experiences within each group."),
            tags$li("Core housing need applies the workbook’s eligibility rules, so it does not describe every household."),
            tags$li("The source cautions that census geography boundaries and names may change across censuses; interpret comparisons over time with care."),
            tags$li("The chart offers a subset of the source’s visible-minority categories and aggregate groups."),
            tags$li("The dashboard presents reported values without uncertainty intervals.")
          )
        ),
        div(
          class = "guide-section",
          h2("Author and technical contribution"),
          p(strong("Project author: "), "Jennifer Ezinne Onyebuchi.")
        )
      )
    ),
    tabPanel(
      title = "Reference tables",
      value = "tables",
      div(
        class = "guide-content",
        h2(class = "guide-heading", "Reference tables"),
        p(
          "These seven tables show Black and Chinese household counts and housing measures from 2001 to 2021. Tables 1–4 show the source counts and averages; Tables 5–7 show shares and changes calculated from the counts. Percentages and percentage-point changes are displayed to one decimal place.",
          class = "guide-lead"
        ),
        div(
          class = "guide-section",
          h2("Table 1. Household count"),
          div(
            class = "table-responsive",
            tableOutput("table1")
          ),
          p(
            "Table 1 counts households, not individual people. In 2021, the table includes 13,975 Black households and 175,355 Chinese households. The other columns break the counts down by tenure and core-housing-need status."
          ),
          div(
            class = "guide-callout",
            p("Owner and renter counts may not add exactly to the total because the published counts are rounded. For example, in 2001 there were 2,245 Black owner households and 3,765 renter households (6,010 combined), compared with 6,015 total Black households.")
          )
        ),
        div(
          class = "guide-section",
          h2("Table 2. Average household income ($)"),
          div(
            class = "table-responsive",
            tableOutput("table2")
          ),
          p(
            "This table shows average household income for each group, tenure, and core-housing-need category. In 2021, the total-tenure averages were $96,500 for Black households and $111,700 for Chinese households. Among households in core housing need, the averages were $43,640 and $40,440; among those not in need, they were $113,200 and $130,000."
          ),
          p("These are group averages, not the income of every household.")
        ),
        div(
          class = "guide-section",
          h2("Table 3. Average monthly shelter cost ($)"),
          div(
            class = "table-responsive",
            tableOutput("table3")
          ),
          p(
            "This table shows the average monthly shelter cost for each group and category. In 2021, total-tenure averages were $1,694 for Black households and $1,776 for Chinese households. For households in core housing need, the averages were $1,470 and $1,672; for those not in need, they were $1,764 and $1,804."
          ),
          p("Shelter cost by itself does not show how affordable housing is for a household; income also matters.")
        ),
        div(
          class = "guide-section",
          h2("Table 4. Average shelter-cost-to-income ratio (STIR, %)"),
          div(
            class = "table-responsive",
            tableOutput("table4")
          ),
          p(
            "STIR expresses shelter cost in relation to household income. A higher percentage means shelter costs take up a larger share of income on average. In 2021, the average STIR for households in core housing need was 43.9% for Black households and 51.2% for Chinese households; for households not in need, it was 21.3% and 19.9%."
          ),
          p("These are averages for groups; they do not describe every household’s costs or income.")
        ),
        div(
          class = "guide-section",
          h2("Table 5. Share in and not in core housing need"),
          div(
            class = "table-responsive",
            tableOutput("table5")
          ),
          p(
            "Each percentage is calculated within that group’s total households; it is not the group’s share of all Vancouver households. In 2021, 24.0% of Black households and 20.4% of Chinese households were in core housing need. The corresponding not-in-need shares were 76.0% and 79.6%."
          ),
          div(
            class = "guide-callout",
            p("The published counts are rounded, so the in-need and not-in-need shares may not add to exactly 100%. For example, among Black households in 2006, 26.8% + 73.1% = 99.9%.")
          )
        ),
        div(
          class = "guide-section",
          h2("Table 6. Change over time in the share in core housing need"),
          div(
            class = "table-responsive",
            tableOutput("table6")
          ),
          p(
            "Table 6 shows the share of each group’s households that were in core housing need. “Change vs previous census” compares each year with the census before it; “change since 2001” compares it with 2001. A percentage-point change is the difference between two percentages."
          ),
          p(
            "For Black households, the share rose from 24.9% in 2001 to 27.9% in 2016, then fell by 3.9 percentage points to 24.0% in 2021—0.9 points below 2001. For Chinese households, it rose to 24.0% in 2006 and then declined to 20.4% in 2021, 1.8 points below 2001. The Black share was higher than the Chinese share in all five census years."
          ),
          div(
            class = "guide-callout",
            p("Changes are calculated from the underlying shares before rounding them for display. As a result, a displayed change may differ slightly from subtracting the one-decimal percentages in the table.")
          ),
          div(
            class = "plot-card",
            div(
              class = "chart-heading",
              h3("Trend in core housing need"),
              p("The line chart shows the share of households in core housing need for each group and census year.")
            ),
            plotlyOutput("coreNeedTrendPlot", height = "360px")
          )
        ),
        div(
          class = "guide-section",
          h2("Table 7. Share of each group that owns vs rents"),
          div(
            class = "table-responsive",
            tableOutput("table7")
          ),
          p(
            "Black households were mostly renters in every year. The owner share peaked at 42.8% in 2011, then fell to 31.2% by 2021. Among Chinese households, about four in five were owners; the owner share increased from 77.5% in 2001 to 81.7% in 2021, reaching 82.0% in 2016."
          ),
          div(
            class = "guide-callout",
            p("Owner and renter percentages may add up to slightly more or less than 100% because the published counts are rounded. For example, for Black households in 2011, 42.8% + 57.3% = 100.1%.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_df <- reactive({
    df %>%
      filter(year == input$year) %>%
      filter(visible_minority_status %in% input$groups) %>%
      filter(tenure == input$tenure) %>%
      filter(core_housing_need_status %in% c("In core housing need", "Not in core housing need"))
  })

  output$comparisonPlot <- renderPlotly({
    validate(need(
      length(input$groups) > 0,
      "Select at least one group to display the chart."
    ))
    req(nrow(filtered_df()) > 0)

    plot_width <- session$clientData$output_comparisonPlot_width
    is_narrow <- !is.null(plot_width) && plot_width < 600

    metric_label <- switch(
      input$metric,
      "avg_stir_pct" = "Average Shelter-Cost-to-Income Ratio (%)",
      "avg_monthly_shelter_cost" = "Average Monthly Shelter Cost ($)",
      "avg_household_income" = "Average Household Income ($)"
    )

    plot_df <- filtered_df() %>%
      mutate(
        core_housing_need_status = recode(
          core_housing_need_status,
          "In core housing need" = "In core housing need",
          "Not in core housing need" = "Not in core housing need"
        ),
        tooltip = paste0(
          "Group: ", visible_minority_status, "<br>",
          "Status: ", core_housing_need_status, "<br>",
          metric_label, ": ", round(.data[[input$metric]], 1)
        )
      )

    plot_ly(
      data = plot_df,
      x = ~visible_minority_status,
      y = ~.data[[input$metric]],
      type = "bar",
      color = ~core_housing_need_status,
      colors = c("#2E86DE", "#2A9D8F"),
      hoverinfo = "text",
      text = ~tooltip,
      marker = list(line = list(color = "rgba(0,0,0,0.15)", width = 1.5))
    ) %>%
      layout(
        barmode = "group",
        title = list(
          text = if (is_narrow) {
            paste("Housing need:", input$year, "|", input$tenure)
          } else {
            paste("Housing need by visible minority group in", input$year)
          },
          font = list(size = if (is_narrow) 18 else 28, color = "#1f2d3d"),
          x = if (is_narrow) 0.02 else 0.5,
          xanchor = if (is_narrow) "left" else "center"
        ),
        annotations = if (is_narrow) {
          list()
        } else {
          list(
            list(
              text = paste("Tenure:", input$tenure),
              x = 0.5,
              y = 1.05,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 14, color = "#46586e")
            )
          )
        },
        xaxis = list(
          title = list(text = "Visible minority group", font = list(size = 15, color = "#24384d")),
          tickfont = list(size = if (is_narrow) 10 else 12, color = "#24384d"),
          tickangle = if (is_narrow) -45 else -20,
          categoryorder = "array",
          categoryarray = sort(unique(plot_df$visible_minority_status)),
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = metric_label, font = list(size = 15, color = "#24384d")),
          tickfont = list(size = 12, color = "#24384d"),
          gridcolor = "#dfe7f1"
        ),
        legend = if (is_narrow) {
          list(
            title = list(text = ""),
            font = list(size = 10, color = "#24384d"),
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = 1.12,
            yanchor = "bottom"
          )
        } else {
          list(
            title = list(text = "Core housing need", font = list(size = 14, color = "#24384d")),
            font = list(size = 12, color = "#24384d"),
            orientation = "v",
            x = 1.02,
            y = 1
          )
        },
        bargap = 0.35,
        margin = if (is_narrow) {
          list(b = 180, l = 50, r = 20, t = 145)
        } else {
          list(b = 150, l = 70, r = 60, t = 90)
        },
        paper_bgcolor = "#f3f6fb",
        plot_bgcolor = "#f3f6fb"
      )
  })

  output$interpretationPanel <- renderUI({
    rows <- filtered_df()

    if (length(input$groups) == 0 || nrow(rows) == 0) {
      return(div(
        class = "interpretation-empty",
        h3("No groups selected"),
        p("Select at least one group to see a filter-specific interpretation.")
      ))
    }

    metric_column <- input$metric
    metric_label <- switch(
      metric_column,
      "avg_stir_pct" = "average shelter-cost-to-income ratio",
      "avg_monthly_shelter_cost" = "average monthly shelter cost",
      "avg_household_income" = "average household income"
    )
    group_comparison <- rows %>%
      group_by(visible_minority_status) %>%
      summarise(
        in_need = first(.data[[metric_column]][core_housing_need_status == "In core housing need"]),
        not_in_need = first(.data[[metric_column]][core_housing_need_status == "Not in core housing need"]),
        .groups = "drop"
      ) %>%
      filter(!is.na(in_need), !is.na(not_in_need)) %>%
      mutate(absolute_gap = abs(in_need - not_in_need))

    if (nrow(group_comparison) == 0) {
      return(div(
        class = "interpretation-empty",
        h3("No paired values for this selection"),
        p("The selected filters do not contain both core-housing-need statuses to compare.")
      ))
    }

    largest_gap <- slice_max(group_comparison, absolute_gap, n = 1, with_ties = TRUE)
    in_need_value <- largest_gap$in_need[[1]]
    not_in_need_value <- largest_gap$not_in_need[[1]]
    direction <- if (in_need_value > not_in_need_value) {
      "higher"
    } else if (in_need_value < not_in_need_value) {
      "lower"
    } else {
      "the same"
    }
    format_value <- function(value) {
      if (metric_column == "avg_stir_pct") {
        paste0(formatC(value, format = "f", digits = 1), "%")
      } else {
        paste0("$", formatC(value, format = "f", digits = 0, big.mark = ","))
      }
    }
    gap_value <- if (metric_column == "avg_stir_pct") {
      paste0(formatC(largest_gap$absolute_gap[[1]], format = "f", digits = 1), " percentage points")
    } else {
      paste0("$", formatC(largest_gap$absolute_gap[[1]], format = "f", digits = 0, big.mark = ","))
    }

    div(
      class = "interpretation-card",
      h3("What the current selection shows"),
      p(
        paste0(
          "For ", input$year, " (", input$tenure, " tenure), a selected group with the largest absolute difference in reported ",
          metric_label, " is ", largest_gap$visible_minority_status[[1]],
          ": ",
          format_value(in_need_value), " for households in core housing need and ",
          format_value(not_in_need_value), " for households not in core housing need. ",
          "The in-need value is ", direction, " by ", gap_value, "."
        )
      ),
      p(
        class = "interpretation-note",
        "This is a descriptive comparison of source-reported averages for the current filters; it does not explain the cause of the difference."
      )
    )
  })

  output$keyFindingsContent <- renderUI({
    black_2021 <- df %>%
      filter(
        year == 2021,
        visible_minority_status == "Black",
        tenure == "Total",
        core_housing_need_status %in% c(
          "Total",
          "In core housing need",
          "Not in core housing need"
        )
      )
    expected_statuses <- c(
      "Total",
      "In core housing need",
      "Not in core housing need"
    )

    if (
      nrow(black_2021) != length(expected_statuses) ||
      anyDuplicated(black_2021$core_housing_need_status) > 0 ||
      !setequal(black_2021$core_housing_need_status, expected_statuses)
    ) {
      stop("Expected one 2021 Black Total-tenure record for each housing-need status.", call. = FALSE)
    }

    total_row <- filter(black_2021, core_housing_need_status == "Total")
    in_need_row <- filter(black_2021, core_housing_need_status == "In core housing need")
    not_in_need_row <- filter(black_2021, core_housing_need_status == "Not in core housing need")
    total_count <- total_row$household_count[[1]]
    in_need_count <- in_need_row$household_count[[1]]
    not_in_need_count <- not_in_need_row$household_count[[1]]

    if (total_count <= 0 || total_count != in_need_count + not_in_need_count) {
      stop("The 2021 Black household counts do not reconcile to the Total-tenure count.", call. = FALSE)
    }

    in_need_share <- 100 * in_need_count / total_count
    stir_difference <- in_need_row$avg_stir_pct[[1]] - not_in_need_row$avg_stir_pct[[1]]
    income_difference <- in_need_row$avg_household_income[[1]] - not_in_need_row$avg_household_income[[1]]
    stir_direction <- if (stir_difference > 0) "higher" else if (stir_difference < 0) "lower" else "the same"
    income_direction <- if (income_difference > 0) "higher" else if (income_difference < 0) "lower" else "the same"

    div(
      class = "guide-grid",
      div(
        class = "guide-card",
        h3("Households in core housing need"),
        p(
          paste0(
            formatC(in_need_count, format = "f", digits = 0, big.mark = ","),
            " of ",
            formatC(total_count, format = "f", digits = 0, big.mark = ","),
            " Black households in the 2021 Total-tenure rows were in core housing need (",
            formatC(in_need_share, format = "f", digits = 1),
            "%)."
          )
        )
      ),
      div(
        class = "guide-card",
        h3("Average shelter-cost-to-income ratio"),
        p(
          paste0(
            formatC(in_need_row$avg_stir_pct[[1]], format = "f", digits = 1),
            "% for households in core housing need versus ",
            formatC(not_in_need_row$avg_stir_pct[[1]], format = "f", digits = 1),
            "% for those not in core housing need; the in-need average is ",
            stir_direction,
            " by ",
            formatC(abs(stir_difference), format = "f", digits = 1),
            " percentage points."
          )
        )
      ),
      div(
        class = "guide-card",
        h3("Average household income"),
        p(
          paste0(
            "$",
            formatC(in_need_row$avg_household_income[[1]], format = "f", digits = 0, big.mark = ","),
            " for households in core housing need versus $",
            formatC(not_in_need_row$avg_household_income[[1]], format = "f", digits = 0, big.mark = ","),
            " for those not in core housing need; the in-need average is ",
            income_direction,
            " by $",
            formatC(abs(income_difference), format = "f", digits = 0, big.mark = ","),
            "."
          )
        )
      )
    )
  })

  output$table1 <- renderTable({
    table1_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$table2 <- renderTable({
    table2_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$table3 <- renderTable({
    table3_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$table4 <- renderTable({
    table4_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$table5 <- renderTable({
    table5_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$table6 <- renderTable({
    table6_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$table7 <- renderTable({
    table7_display
  }, rownames = FALSE, striped = TRUE, hover = TRUE)

  output$coreNeedTrendPlot <- renderPlotly({
    plot_ly(
      data = core_need_trend,
      x = ~year,
      y = ~share,
      type = "scatter",
      mode = "lines+markers",
      color = ~group,
      colors = c("#2E86DE", "#2A9D8F"),
      text = ~tooltip,
      hoverinfo = "text",
      line = list(width = 3),
      marker = list(size = 8)
    ) %>%
      layout(
        xaxis = list(
          title = "Census year",
          tickmode = "array",
          tickvals = reference_years,
          gridcolor = "#dfe7f1"
        ),
        yaxis = list(
          title = "Households in core housing need (%)",
          range = c(0, 35),
          ticksuffix = "%",
          gridcolor = "#dfe7f1"
        ),
        legend = list(
          title = list(text = "Household group"),
          orientation = "h",
          x = 0,
          y = 1.12
        ),
        paper_bgcolor = "#fff",
        plot_bgcolor = "#fff",
        margin = list(b = 55, l = 65, r = 20, t = 35)
      )
  })
}

shinyApp(ui, server)
