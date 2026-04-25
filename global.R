# ── Plan definitions ───────────────────────────────────────────────────────
# Each plan mirrors a real ACA metallic tier structure.
# Extend this list to add more plans.

PLANS <- list(
  hmo = list(
    id = "hmo", name = "HMO Bronze", type = "HMO",
    premium = 2100, deductible = 5000, oop_max = 7500,
    copay = 30, coinsurance = 0.20, network = "Restricted"
  ),
  ppo = list(
    id = "ppo", name = "PPO Silver", type = "PPO",
    premium = 3240, deductible = 1500, oop_max = 6000,
    copay = 40, coinsurance = 0.20, network = "Broad"
  ),
  hdhp = list(
    id = "hdhp", name = "HDHP + HSA", type = "HDHP",
    premium = 1680, deductible = 3000, oop_max = 8000,
    copay = 0, coinsurance = 0.30, network = "Broad"
  ),
  epo = list(
    id = "epo", name = "EPO Gold", type = "EPO",
    premium = 4560, deductible = 500, oop_max = 4000,
    copay = 35, coinsurance = 0.15, network = "Moderate"
  )
)

# ── Profile calibration multipliers ───────────────────────────────────────

HEALTH_MULT <- c(excellent = 0.60, good = 1.00, fair = 1.50, poor = 2.20)

CHRONIC_SEVERITY_MULT <- c(
  none = 1.0, diabetes = 1.8, hypertension = 1.4,
  asthma = 1.3, heart = 2.5
)

CHRONIC_EXTRA_VISITS <- c(
  none = 0, diabetes = 6, hypertension = 4, asthma = 3, heart = 8
)

# ── Scenario definitions ───────────────────────────────────────────────────

SCENARIOS <- data.frame(
  id          = c("healthy","moderate","er","surgery","hospital","chronic_flare"),
  label       = c("Healthy year","Moderate use","ER visit",
                  "Outpatient surgery","Hospitalization","Chronic flare"),
  description = c("No major events","Few specialist visits","One emergency event",
                  "Planned procedure","3-day inpatient stay","Extended specialist care"),
  visit_mult  = c(0.5, 1.0, 1.0, 1.5, 1.5, 2.5),
  severity    = c(300, 700, 4500, 9000, 18000, 1200),
  stringsAsFactors = FALSE
)

# ── Colors — one per plan, used consistently across all charts ─────────────

PLAN_COLORS <- c(
  "HMO Bronze"  = "#378ADD",
  "PPO Silver"  = "#1D9E75",
  "HDHP + HSA"  = "#EF9F27",
  "EPO Gold"    = "#D85A30"
)

# ── Tax rate lookup ────────────────────────────────────────────────────────

get_marginal_rate <- function(income) {
  dplyr::case_when(
    income < 50000  ~ 0.12,
    income < 100000 ~ 0.22,
    income < 150000 ~ 0.24,
    TRUE            ~ 0.32
  )
}