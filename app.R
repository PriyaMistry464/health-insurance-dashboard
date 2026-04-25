options(expressions = 5e5)
options(encoding = "UTF-8")

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Load modules
source("global.R")
source("modules/actuarial_engine.R")
source("modules/ui_components.R")

# ══════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  title = "Health Insurance Plan Intelligence",
  
  # ── Global styles ──────────────────────────────────────────────────────
  tags$head(tags$style(HTML("
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
           background: #eeece8; color: #1a1a18; font-size:14px; }
    .main-wrap { max-width: 1080px; margin: 0 auto; padding: 1.5rem 1rem; }
    .top-bar { display:flex; justify-content:space-between; align-items:flex-start;
               border-bottom:0.5px solid rgba(0,0,0,0.12);
               padding-bottom:1rem; margin-bottom:1.25rem; }
    .top-bar h1 { font-size:18px; font-weight:500; margin:0; }
    .top-bar .sub { font-size:12px; color:#888; margin-top:4px; }
    .sidebar { background:#f5f5f4; border:0.5px solid rgba(0,0,0,0.1);
               border-radius:12px; padding:1rem; }
    .sidebar label { font-size:11px; color:#888; }
    .method-note { font-size:10px; color:#aaa; line-height:1.7;
                   border-top:0.5px solid rgba(0,0,0,0.1);
                   padding-top:10px; margin-top:1rem; }
    .shiny-input-container { margin-bottom:12px; }
    .btn-analyze { width:100%; background:#378ADD; color:white;
                   border:none; border-radius:8px; padding:9px;
                   font-size:13px; font-weight:500; cursor:pointer; }
    .btn-analyze:hover { opacity:0.9; }
  "))),
  
  div(class = "main-wrap",
      
      # ── Top bar ──────────────────────────────────────────────────────────
      div(class = "top-bar",
          div(
            tags$h1("Health insurance plan intelligence"),
            div(class = "sub",
                "Actuarial cost modeling · Monte Carlo simulation · GLM-based expected costs")
          ),
          div(style = "font-size:10px; color:#aaa; text-align:right; line-height:1.8;",
              "Frequency × Severity model", tags$br(),
              "Poisson λ · Gamma severity · VaR 95", tags$br(),
              "10,000 Monte Carlo simulations")
      ),
      
      # ── Layout ───────────────────────────────────────────────────────────
      fluidRow(
        
        # Sidebar
        column(3,
               div(class = "sidebar",
                   div(style = "font-size:10px; font-weight:500; color:#aaa;
                       text-transform:uppercase; letter-spacing:0.07em;
                       margin-bottom:12px;",
                       "Your profile"),
                   
                   numericInput("age", "Age", value = 34, min = 18, max = 64),
                   
                   sliderInput("visits", "Visits per year",
                               min = 1, max = 30, value = 6, step = 1),
                   
                   selectInput("health", "Health status",
                               choices = c("Excellent" = "excellent",
                                           "Good"      = "good",
                                           "Fair"      = "fair",
                                           "Poor"      = "poor"),
                               selected = "good"),
                   
                   selectInput("chronic", "Chronic condition",
                               choices = c("None"         = "none",
                                           "Diabetes"     = "diabetes",
                                           "Hypertension" = "hypertension",
                                           "Asthma"       = "asthma",
                                           "Heart disease"= "heart"),
                               selected = "none"),
                   
                   selectInput("income", "Annual income (for HSA)",
                               choices = c("Under $50k"     = "40000",
                                           "$50k - $100k"   = "75000",
                                           "$100k - $150k"  = "130000",
                                           "Over $150k"     = "200000"),
                               selected = "75000"),
                   
                   selectInput("risk", "Risk tolerance",
                               choices = c("Low — prefer certainty" = "low",
                                           "Moderate"               = "moderate",
                                           "High — OK with risk"    = "high"),
                               selected = "moderate"),
                   
                   tags$hr(style = "border-top:0.5px solid rgba(0,0,0,0.1); margin:12px 0;"),
                   
                   actionButton("analyze", "Analyze plans",
                                class = "btn-analyze")
               )
        ),
        
        # Main panel
        column(9,
               
               # Metric strip
               fluidRow(
                 column(3, metric_box("Recommended plan",   "m_rec",  "Lowest expected total")),
                 column(3, metric_box("Expected annual cost","m_exp",  "Premium + E[OOP]")),
                 column(3, metric_box("VaR 95th percentile","m_var",  "Worst-case annual cost")),
                 column(3, metric_box("vs. naive cheapest", "m_save", "Total cost saving"))
               ),
               
               tags$br(),
               
               # Plan comparison cards
               uiOutput("plan_cards"),
               
               tags$br(),
               
               # Charts row
               fluidRow(
                 column(6,
                        div(style = "background:white; border-radius:12px;
                         border:0.5px solid rgba(0,0,0,0.1); padding:12px;",
                            section_header("Cost distribution — Monte Carlo"),
                            plotOutput("dist_chart", height = "210px")
                        )
                 ),
                 column(6,
                        div(style = "background:white; border-radius:12px;
                         border:0.5px solid rgba(0,0,0,0.1); padding:12px;",
                            section_header("Break-even — cumulative cost by month"),
                            plotOutput("be_chart", height = "210px")
                        )
                 )
               ),
               
               tags$br(),
               
               # Scenario analysis
               div(style = "background:white; border-radius:12px;
                     border:0.5px solid rgba(0,0,0,0.1); padding:12px;",
                   section_header("Scenario analysis — what if..."),
                   uiOutput("scenario_panel")
               ),
               
               # Methodology note
               div(class = "method-note",
                   tags$strong("Methodology. "),
                   "Expected OOP modeled as E[OOP] = Σ(claim severity × frequency) subject to
           deductible → coinsurance → OOP-max waterfall. Claim frequency follows
           Poisson(λ) calibrated to age, health status, and chronic condition load.
           Claim severity follows Gamma(α=2, β) where β is calibrated to mean severity
           for the profile. VaR = 95th percentile of 10,000 simulated annual totals.
           HDHP adjusted cost nets the HSA tax shield = min(deductible, $3,850) × marginal rate.
           Break-even = cumulative (monthly premium + expected monthly OOP) over 12 months."
               )
        )
      )
  )
)


# ══════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  # ── Reactive: run full analysis when button clicked ────────────────────
  # isolate() inside eventReactive means it only runs on button click,
  # not on every input change.
  
  analysis <- eventReactive(input$analyze, {
    req(input$age, input$visits, input$health, input$chronic, input$income)
    
    params     <- get_actuarial_params(input$age, input$visits,
                                       input$health, input$chronic)
    tax_rate   <- get_marginal_rate(as.numeric(input$income))
    
    # Simulate all plans
    results <- lapply(PLANS, function(plan) {
      sims  <- simulate_plan(plan, params$lambda, params$alpha,
                             params$beta, n_sims = 1000)
      stats <- simulation_summary(sims, plan, tax_rate)
      list(plan = plan, stats = stats)
    })
    
    list(results = results, params = params, tax_rate = tax_rate)
  }, ignoreNULL = FALSE)   # run once on startup with defaults
  
  
  # ── Helper: find recommended plan ─────────────────────────────────────
  recommended_id <- reactive({
    res <- analysis()$results
    adj_means <- sapply(res, function(r) r$stats$adj_mean)
    names(which.min(adj_means))
  })
  
  
  # ── Metric strip outputs ───────────────────────────────────────────────
  output$m_rec <- renderText({
    rec <- analysis()$results[[recommended_id()]]
    rec$plan$name
  })
  
  output$m_exp <- renderText({
    rec <- analysis()$results[[recommended_id()]]
    paste0("$", format(round(rec$stats$adj_mean), big.mark = ","))
  })
  
  output$m_var <- renderText({
    rec <- analysis()$results[[recommended_id()]]
    paste0("$", format(round(rec$stats$var_95), big.mark = ","))
  })
  
  output$m_save <- renderText({
    res      <- analysis()$results
    premiums <- sapply(res, function(r) r$plan$premium)
    cheapest_id <- names(which.min(premiums))
    rec_id      <- recommended_id()
    
    saving <- res[[cheapest_id]]$stats$adj_mean - res[[rec_id]]$stats$adj_mean
    sign   <- if (saving >= 0) "−" else "+"
    paste0(sign, "$", format(round(abs(saving)), big.mark = ","))
  })
  
  
  # ── Plan comparison cards ──────────────────────────────────────────────
  output$plan_cards <- renderUI({
    res      <- analysis()$results
    rec_id   <- recommended_id()
    premiums <- sapply(res, function(r) r$plan$premium)
    var95s   <- sapply(res, function(r) r$stats$var_95)
    
    min_premium_id <- names(which.min(premiums))
    max_var_id     <- names(which.max(var95s))
    
    cards <- lapply(names(res), function(pid) {
      r <- res[[pid]]
      column(3,
             plan_card(
               plan              = r$plan,
               stats             = r$stats,
               is_best           = (pid == rec_id),
               is_cheapest_premium = (pid == min_premium_id && pid != rec_id),
               is_riskiest       = (pid == max_var_id && pid != rec_id)
             )
      )
    })
    
    do.call(fluidRow, cards)
  })
  
  
  # ── Distribution chart ─────────────────────────────────────────────────
  # Overlapping density curves from simulation results.
  # ggplot2 geom_density on the raw simulation vectors.
  
  output$dist_chart <- renderPlot({
    res <- analysis()$results
    
    # Stack all simulations into one long data frame
    sim_df <- bind_rows(lapply(names(res), function(pid) {
      data.frame(
        plan = res[[pid]]$plan$name,
        cost = res[[pid]]$stats$sims
      )
    }))
    
    ggplot(sim_df, aes(x = cost, fill = plan, color = plan)) +
      geom_density(alpha = 0.18, linewidth = 0.8) +
      scale_x_continuous(labels = dollar_format(scale = 0.001, suffix = "k"),
                         expand = c(0.01, 0)) +
      scale_y_continuous(labels = NULL) +
      scale_fill_manual(values  = PLAN_COLORS) +
      scale_color_manual(values = PLAN_COLORS) +
      labs(x = "Annual cost", y = "Density", color = NULL, fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position  = "top",
        legend.key.size  = unit(0.5, "cm"),
        legend.text      = element_text(size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x      = element_text(size = 9),
        plot.background  = element_rect(fill = "white", color = NA)
      )
  }, bg = "white")
  
  
  # ── Break-even chart ───────────────────────────────────────────────────
  # Cumulative (premium + E[OOP]) over 12 months for each plan.
  
  output$be_chart <- renderPlot({
    res <- analysis()$results
    
    be_df <- bind_rows(lapply(names(res), function(pid) {
      r             <- res[[pid]]
      monthly_total <- (r$plan$premium + r$stats$exp_oop) / 12
      data.frame(
        plan  = r$plan$name,
        month = 0:12,
        cost  = monthly_total * 0:12
      )
    }))
    
    ggplot(be_df, aes(x = month, y = cost, color = plan)) +
      geom_line(linewidth = 1) +
      scale_x_continuous(breaks = seq(0, 12, 2)) +
      scale_y_continuous(labels = dollar_format(scale = 0.001, suffix = "k")) +
      scale_color_manual(values = PLAN_COLORS) +
      labs(x = "Month", y = "Cumulative cost", color = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position  = "top",
        legend.key.size  = unit(0.5, "cm"),
        legend.text      = element_text(size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text        = element_text(size = 9),
        plot.background  = element_rect(fill = "white", color = NA)
      )
  }, bg = "white")
  
  
  # ── Scenario panel ─────────────────────────────────────────────────────
  
  # Track which scenario is active
  active_scenario <- reactiveVal("moderate")
  
  output$scenario_panel <- renderUI({
    sc_buttons <- lapply(seq_len(nrow(SCENARIOS)), function(i) {
      sc  <- SCENARIOS[i, ]
      is_active <- sc$id == active_scenario()
      bg  <- if (is_active) "#E6F1FB" else "white"
      bdr <- if (is_active) "2px solid #378ADD" else "0.5px solid rgba(0,0,0,0.1)"
      col <- if (is_active) "#0C447C" else "#1a1a18"
      
      column(2,
             div(
               style = paste0(
                 "background:", bg, "; border:", bdr, "; border-radius:8px;",
                 "padding:8px 10px; cursor:pointer; margin-bottom:8px;"
               ),
               onclick = paste0('Shiny.setInputValue("scenario_click","', sc$id,
                                '", {priority:"event"})'),
               div(style = paste0("font-size:11px; font-weight:500; color:", col, ";
                              margin-bottom:2px;"), sc$label),
               div(style = "font-size:10px; color:#aaa;", sc$description)
             )
      )
    })
    
    # Cost bars for active scenario
    res      <- analysis()$results
    params   <- analysis()$params
    tax_rate <- analysis()$tax_rate
    sc       <- SCENARIOS[SCENARIOS$id == active_scenario(), ]
    
    sc_costs <- sapply(names(res), function(pid) {
      scenario_cost(res[[pid]]$plan, params$lambda, sc, tax_rate)
    })
    
    min_cost <- min(sc_costs)
    max_cost <- max(sc_costs)
    winner   <- names(res)[which.min(sc_costs)]
    
    bars <- lapply(names(res), function(pid) {
      r    <- res[[pid]]
      cost <- sc_costs[pid]
      bar_pct <- if (max_cost == min_cost) 50 else
        round(10 + (cost - min_cost) / (max_cost - min_cost) * 80)
      
      fluidRow(
        column(2, div(style = "font-size:11px; color:#888; text-align:right;
                                padding-right:8px; line-height:28px;",
                      gsub(" Silver| Bronze| Gold| \\+ HSA", "", r$plan$name))),
        column(8,
               div(style = "background:#f5f5f4; border-radius:4px;
                       height:8px; margin-top:10px; overflow:hidden;",
                   div(style = paste0(
                     "width:", bar_pct, "%; height:100%; border-radius:4px;",
                     "background:", PLAN_COLORS[r$plan$name], ";"
                   ))
               )
        ),
        column(2, div(style = "font-size:11px; font-weight:500; line-height:28px;",
                      paste0("$", format(round(cost), big.mark = ","))))
      )
    })
    
    winner_note <- div(
      style = "font-size:10px; color:#aaa; margin-top:8px;",
      "Best plan for this scenario: ",
      tags$strong(res[[winner]]$plan$name)
    )
    
    tagList(
      div(style = "margin-bottom:10px;", do.call(fluidRow, sc_buttons)),
      div(style = "font-size:11px; color:#888; margin-bottom:8px;",
          "Total cost in this scenario (premium + out-of-pocket):"),
      tagList(bars),
      winner_note
    )
  })
  
  # Update active scenario on card click
  observeEvent(input$scenario_click, {
    active_scenario(input$scenario_click)
  })
  
}

# ══════════════════════════════════════════════════════════════════════════
# RUN
# ══════════════════════════════════════════════════════════════════════════

shinyApp(ui = ui, server = server)