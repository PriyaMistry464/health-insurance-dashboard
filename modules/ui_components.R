# ── Metric box ─────────────────────────────────────────────────────────────
# Renders a single summary metric tile.

metric_box <- function(label, value_id, subtitle) {
  div(
    style = paste(
      "background:#f5f5f4; border-radius:8px; padding:10px 14px;",
      "border:0.5px solid rgba(0,0,0,0.1);"
    ),
    div(style = "font-size:11px; color:#888; margin-bottom:4px;", label),
    div(style = "font-size:20px; font-weight:500;", textOutput(value_id, inline = TRUE)),
    div(style = "font-size:10px; color:#aaa; margin-top:3px;", subtitle)
  )
}


# ── Section header ─────────────────────────────────────────────────────────

section_header <- function(title) {
  div(
    style = paste(
      "font-size:10px; font-weight:500; color:#999;",
      "text-transform:uppercase; letter-spacing:0.07em;",
      "border-bottom:0.5px solid rgba(0,0,0,0.1);",
      "padding-bottom:7px; margin-bottom:10px;"
    ),
    title
  )
}


# ── Plan card ──────────────────────────────────────────────────────────────
# Renders one plan's summary card given pre-computed result stats.

plan_card <- function(plan, stats, is_best, is_cheapest_premium, is_riskiest) {
  
  badge_html <- if (is_best) {
    tags$span(
      style = "background:#E6F1FB; color:#0C447C; font-size:10px;
               padding:2px 8px; border-radius:6px; margin-bottom:6px;
               display:inline-block;",
      "Best fit"
    )
  } else if (is_cheapest_premium) {
    tags$span(
      style = "background:#EAF3DE; color:#27500A; font-size:10px;
               padding:2px 8px; border-radius:6px; margin-bottom:6px;
               display:inline-block;",
      "Lowest premium"
    )
  } else if (is_riskiest) {
    tags$span(
      style = "background:#FAEEDA; color:#633806; font-size:10px;
               padding:2px 8px; border-radius:6px; margin-bottom:6px;
               display:inline-block;",
      "Highest risk"
    )
  } else {
    tags$span(style = "display:inline-block; height:22px;", "")
  }
  
  border_style <- if (is_best) {
    "border:2px solid #378ADD;"
  } else {
    "border:0.5px solid rgba(0,0,0,0.1);"
  }
  
  hsa_row <- if (plan$type == "HDHP" && stats$hsa_adj > 0) {
    tags$div(
      style = "display:flex; justify-content:space-between;
               font-size:11px; padding:2px 0; color:#888;",
      tags$span("HSA tax shield"),
      tags$span(style = "font-weight:500; color:#3B6D11;",
                paste0("-$", format(round(stats$hsa_adj), big.mark=",")))
    )
  } else NULL
  
  div(
    style = paste(
      "background:white; border-radius:10px; padding:10px 12px;",
      border_style
    ),
    badge_html,
    div(style = "font-size:12px; font-weight:500; margin-bottom:8px;", plan$name),
    div(style = "display:flex; justify-content:space-between; font-size:11px; padding:2px 0; color:#888;",
        span("Annual premium"), span(style="font-weight:500;color:#1a1a18;", paste0("$", format(plan$premium, big.mark=",")))),
    div(style = "display:flex; justify-content:space-between; font-size:11px; padding:2px 0; color:#888;",
        span("E[OOP]"), span(style="font-weight:500;color:#1a1a18;", paste0("$", format(round(stats$exp_oop), big.mark=",")))),
    hsa_row,
    div(style = "display:flex; justify-content:space-between; font-size:11px; padding:2px 0; color:#888;",
        span("VaR 95%"), span(style="font-weight:500;color:#E24B4A;", paste0("$", format(round(stats$var_95), big.mark=",")))),
    div(
      style = "display:flex; justify-content:space-between; font-size:12px;
               font-weight:500; padding-top:6px; margin-top:5px;
               border-top:0.5px solid rgba(0,0,0,0.1);",
      span("Adj. total"),
      span(paste0("$", format(round(stats$adj_mean), big.mark=",")))
    )
  )
}

