# Health Insurance Plan Intelligence

An R Shiny dashboard that helps you pick the right ACA health insurance plan using actuarial cost modeling and Monte Carlo simulation — not just the cheapest premium.

## What it does

You enter your health profile (age, expected visits, health status, chronic conditions, income, risk tolerance) and the app runs 1,000 simulated policy years for each plan to estimate your true annual cost. It then recommends the plan with the lowest **expected total cost** (premium + out-of-pocket), not just the lowest premium.

### Plans compared

| Plan | Type | Annual Premium | Deductible | OOP Max |
|------|------|---------------|------------|---------|
| HMO Bronze | HMO | $2,100 | $5,000 | $7,500 |
| PPO Silver | PPO | $3,240 | $1,500 | $6,000 |
| HDHP + HSA | HDHP | $1,680 | $3,000 | $8,000 |
| EPO Gold | EPO | $4,560 | $500 | $4,000 |

## Features

- **Plan recommendation** — identifies the lowest expected-cost plan for your specific profile
- **Cost distribution chart** — overlapping density curves from Monte Carlo simulation showing the full cost range for each plan
- **Break-even chart** — cumulative monthly cost over 12 months so you can see when a higher-premium plan pays off
- **Scenario analysis** — deterministic cost comparison across 6 scenarios: healthy year, moderate use, ER visit, outpatient surgery, hospitalization, and chronic flare
- **HSA tax shield** — HDHP adjusted cost nets the HSA tax benefit based on your income bracket
- **VaR 95** — worst-case annual cost at the 95th percentile so you can plan for bad years

## How the model works

**Claim frequency** follows a Poisson distribution calibrated to your age, visit count, health status, and chronic condition. **Claim severity** follows a Gamma distribution (shape α = 2) with mean calibrated to your profile.

Each simulation year: sample visit count → sample claim amounts → apply the cost-sharing waterfall (deductible → coinsurance + copay → OOP cap) → add annual premium. The HDHP plan nets the HSA tax shield: `min(deductible, $3,850) × marginal tax rate`.

VaR is the 95th percentile of 1,000 simulated annual totals.

## Setup

**Requirements:** R 4.1+

Install dependencies:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "scales"))
```

**Run the app:**

```r
shiny::runApp("path/to/health_insurance_dashboard")
```

Or open `app.R` in RStudio and click **Run App**.

## Project structure

```
health_insurance_dashboard/
├── app.R                     # UI + server
├── global.R                  # Plan definitions, scenario configs, constants
└── modules/
    ├── actuarial_engine.R    # OOP waterfall, Monte Carlo simulation, scenario costs
    └── ui_components.R       # Reusable UI helpers (metric boxes, plan cards)
```
