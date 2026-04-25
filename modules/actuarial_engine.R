# ── OOP waterfall ──────────────────────────────────────────────────────────
# Applies the standard insurance cost-sharing structure:
#   1. Claims hit the deductible first  (member pays 100%)
#   2. Remaining cost splits by coinsurance + copay
#   3. Total OOP is capped at the OOP maximum
#
# Args:
#   plan   : one plan from PLANS list
#   claims : numeric vector of individual claim amounts ($)
#
# Returns: total out-of-pocket for the year

compute_oop <- function(plan, claims) {
  total_oop <- 0
  deductible_remaining <- plan$deductible
  
  for (claim in claims) {
    remaining <- claim
    
    # Step 1 — deductible
    if (deductible_remaining > 0) {
      to_ded <- min(remaining, deductible_remaining)
      total_oop <- total_oop + to_ded
      deductible_remaining <- deductible_remaining - to_ded
      remaining <- remaining - to_ded
    }
    
    # Step 2 — coinsurance + copay
    if (remaining > 0) {
      total_oop <- total_oop + remaining * plan$coinsurance + plan$copay
    }
  }
  
  # Step 3 — OOP cap
  return(min(total_oop, plan$oop_max))
}


# ── Profile → actuarial parameters ────────────────────────────────────────
# Converts user inputs into Poisson lambda and Gamma parameters.
#
# Frequency model:  visits ~ Poisson(lambda)
#   lambda = (base_visits + chronic_extra) x health_mult x age_mult
#
# Severity model:   claim_amount ~ Gamma(shape=alpha, scale=beta)
#   mean_severity = $650 x health_mult x chronic_severity_mult x age_mult
#   alpha = 2  (shape — controls skew)
#   beta  = mean_severity / alpha

get_actuarial_params <- function(age, visits, health, chronic) {
  age_mult     <- 1 + max(0, age - 30) * 0.02
  lambda       <- (visits + CHRONIC_EXTRA_VISITS[chronic]) *
    HEALTH_MULT[health] * age_mult
  
  mean_sev     <- 650 * HEALTH_MULT[health] *
    CHRONIC_SEVERITY_MULT[chronic] * age_mult
  alpha        <- 2.0
  beta         <- mean_sev / alpha   # scale parameter: E[Gamma] = alpha x beta
  
  list(lambda = lambda, alpha = alpha, beta = beta)
}


# ── Monte Carlo simulation ─────────────────────────────────────────────────
# Runs n_sims independent policy years for one plan.
# Each year: sample visit count from Poisson, sample claim amounts
# from Gamma, apply OOP waterfall, add premium.
#
# Returns a numeric vector of length n_sims (sorted total annual costs).

simulate_plan <- function(plan, lambda, alpha, beta, n_sims = 10000) {
  results <- numeric(n_sims)
  
  for (i in seq_len(n_sims)) {
    n_visits <- rpois(1, lambda)
    
    claims <- if (n_visits > 0) {
      rgamma(n_visits, shape = alpha, scale = beta)
    } else {
      numeric(0)
    }
    
    oop <- compute_oop(plan, claims)
    results[i] <- plan$premium + oop
  }
  
  sort(results)
}


# ── Summary stats from simulation ─────────────────────────────────────────

simulation_summary <- function(sims, plan, tax_rate) {
  mean_cost  <- mean(sims)
  var_95     <- quantile(sims, 0.95)
  exp_oop    <- mean_cost - plan$premium
  
  # HSA tax shield for HDHP plans
  # IRS 2024 self-only HSA limit: $3,850
  hsa_adj <- if (plan$type == "HDHP") {
    min(plan$deductible, 3850) * tax_rate
  } else {
    0
  }
  
  list(
    mean_cost  = mean_cost,
    var_95     = as.numeric(var_95),
    exp_oop    = exp_oop,
    hsa_adj    = hsa_adj,
    adj_mean   = mean_cost - hsa_adj,
    sims       = sims
  )
}


# ── Scenario analysis ──────────────────────────────────────────────────────
# Deterministic cost: fixed severity × adjusted visits through OOP waterfall.
# No randomness — useful for answering "what if I get hospitalized?"

scenario_cost <- function(plan, lambda, scenario, tax_rate) {
  n_visits <- max(1, round(lambda * scenario$visit_mult))
  claims   <- rep(scenario$severity, n_visits)
  oop      <- compute_oop(plan, claims)
  gross    <- plan$premium + oop
  
  hsa_adj <- if (plan$type == "HDHP") {
    min(plan$deductible, 3850) * tax_rate
  } else {
    0
  }
  
  gross - hsa_adj
}