# Readme

Last updated: 2026-03-04

## 1) Project Scope

This repository supports:
- Main manuscript analyses for Chinese and international university AI policy data.
- Supplemental table generation (S1-S5).
- Intercoder reliability analyses (China and international).
- Source-traceable coding via archived policy snapshots and markdown extracts.

## 2) Directory Overview

- `00_source_material/`: original policy files and markdown policy texts.
- `10_structured_tables/`: legacy/staging tables (including old Excel files).
- `20_coding/`: canonical coding datasets used by current analysis scripts.
- `30_analysis/main/`: main analysis scripts and outputs.
- `30_analysis/reliability/`: reliability scripts and outputs.

## 3) Canonical Data Files for Final Analysis

Use these files for current/final analyses.

### Main analysis inputs

- `20_coding/21_store/S1_china_analysis.csv` (N = 128)
  - China analytic master file.
  - Core keys/variables: `id`, `Inst_type`, `check_framework(0/1)`, `tool_specified(0/1)`, `linked_to_pass(0/1)`, `threshold`, `policy_summary`, `policy_scope`, `snapshot_filename`.

- `20_coding/21_store/S2_international_analysis.csv` (N = 88)
  - International analytic master file.
  - Core keys/variables: `id`, `country_region`, `rank_usnews_2025`, `D1_tech_limit(0/1)`, `D2_proactive(0/1)`, `D3_responsibility(0/1)`, `D4_ai_as_aid(0/1)`, `maturity_score`, `snapshot_filename`.

### Reliability inputs

- `20_coding/23_results/S1_China_25_Results_Code.csv` (China 25-case reliability subset)
- `20_coding/23_results/S1_china_irr_base.csv` (China base reliability frame)
- `20_coding/23_results/S2_international_irr.csv` (international coder-pair frame)

## 4) CSV vs Excel: Which One to Use

For all scripted analyses, use the CSV files in `20_coding/`.

Do not use `10_structured_tables/*.xlsx` as analysis inputs unless you are explicitly rebuilding the pipeline from legacy staging files. The current scripts do not use those Excel files.

## 5) Main Analysis Pipeline

Run:

```bash
Rscript 30_analysis/main/scripts/run_all_main_analyses.R
```

This pipeline runs the following scripts:

1. `F1_analysis.R`
2. `F1_effect_sizes.R`
3. `SM_threshold_inventory.R`
4. `SM_threshold_supp_table.R`
5. `SM_safeguards_coding.R`
6. `SM_safeguards_supp_table.R`
7. `SM_disclosure_unreliability_addon_table.R`
8. `SM_china_policy_subtypes.R`
9. `SM_china_subtypes_by_insttype_table.R`
10. `SM_international_policy_signals_coding.R`
11. `F2_analysis.R`
12. `F3_sensitivity.R`
13. `Fig1v4.R`

Execution status log:
- `30_analysis/main/results/run_all_main_analyses_status.csv`

## 6) Main Outputs and Manuscript/Supplement Mapping

### Main text figures and core summaries

- Fig. 1 export:
  - `30_analysis/main/results/fig1new.pdf`
  - `30_analysis/main/results/fig1new.png`
  - `30_analysis/main/results/fig1new.svg`

- F1 summary stats:
  - `30_analysis/main/results/F1_china_summary.csv`
  - `30_analysis/main/results/F1_china_headline_rate_cis.csv`
  - `30_analysis/main/results/F1_china_rank_stats.csv`
  - `30_analysis/main/results/F1_china_enforcement_regimes.csv`
  - `30_analysis/main/results/F1_china_enforcement_depth_probabilities.csv`
  - `30_analysis/main/results/F1_china_by_institution_type.csv`
  - `30_analysis/main/results/F1_china_type_adoption_probabilities.csv`
  - `30_analysis/main/results/F1_china_type_effect_sizes.csv`

- F2 summary stats:
  - `30_analysis/main/results/F2_international_dimension_prevalence.csv`
  - `30_analysis/main/results/F2_international_country_dimension_matrix.csv`
  - `30_analysis/main/results/F2_international_country_focus.csv`
  - `30_analysis/main/results/F2_international_maturity_distribution.csv`
  - `30_analysis/main/results/F2_international_maturity_regression.csv`

- F3 sensitivity:
  - `30_analysis/main/results/F3_sensitivity_scenarios.csv`
  - `30_analysis/main/results/F3_boundary_ambiguous_cases.csv`
  - `30_analysis/main/results/F3_logistic_coefficients.csv`
  - `30_analysis/main/results/F3_logistic_model_diagnostics.csv`

- Fig. 1 implementation evidence bundles:
  - `30_analysis/main/results/SM_Fig1_29_universities_with_evidence.csv`
  - `30_analysis/main/results/SM_Fig1_29_universities_with_evidence.md`
  - `30_analysis/main/results/SM_Fig1_29_universities_with_exact_policy_language.csv`
  - `30_analysis/main/results/SM_Fig1_29_universities_with_exact_policy_language.md`
  - `30_analysis/main/results/SM_check_framework_implementation_language_29.csv`
  - `30_analysis/main/results/SM_check_framework_implementation_language_29.md`
  - `30_analysis/main/results/SM_check_framework_implementation_signal_flags_29.csv`

### Supplemental outputs

- Fig. S1:
  - `30_analysis/main/results/F1_effectsizes_dotwhisker.pdf`
  - `30_analysis/main/results/F1_effectsizes_dotwhisker.png`
  - `30_analysis/main/results/F1_effectsizes_dotwhisker.svg`

- Table S1 (China policy subtypes by institution type):
  - `30_analysis/main/results/SM_china_policy_subtypes_by_insttype_table.csv`
  - `30_analysis/main/results/SM_china_policy_subtypes_by_insttype_table.md`

- Table S2 (numeric AIGC threshold rules):
  - `30_analysis/main/results/SM_china_aigc_threshold_numeric_rules_table.csv`
  - `30_analysis/main/results/SM_china_aigc_threshold_numeric_rules_table.md`
  - Supporting distribution: `30_analysis/main/results/SM_china_aigc_threshold_distribution.csv`
  - Full inventory: `30_analysis/main/results/SM_china_aigc_threshold_rule_inventory_full.csv`

- Table S3 (procedural safeguards):
  - `30_analysis/main/results/SM_china_safeguards_table_S3.csv`
  - `30_analysis/main/results/SM_china_safeguards_table_S3.md`
  - Coding/evidence support:
    - `30_analysis/main/results/SM_china_safeguards_coding.csv`
    - `30_analysis/main/results/SM_china_safeguards_evidence.csv`
    - `30_analysis/main/results/SM_china_safeguards_qc_discordance.csv`

- Table S4 (D1-D4 contrast across international and China subsets):
  - `30_analysis/main/results/SM_china_disclosure_unreliability_addon_table.csv`
  - `30_analysis/main/results/SM_china_disclosure_unreliability_addon_table.md`
  - China D1/D2 harmonized adjudication file:
    - `30_analysis/main/results/SM_china_d1d2_harmonized_adjudication_37.csv`

- Table S5 (international operationalization signals):
  - `30_analysis/main/results/SM_international_policy_signals_table.csv`
  - `30_analysis/main/results/SM_international_policy_signals_table.md`
  - Coding/evidence support:
    - `30_analysis/main/results/SM_international_policy_signals_coding.csv`
    - `30_analysis/main/results/SM_international_policy_signals_evidence.csv`
    - `30_analysis/main/results/SM_international_policy_signals_summary.csv`
    - `30_analysis/main/results/SM_international_policy_signals_summary.md`

- China policy subtype support files:
  - `30_analysis/main/results/SM_china_policy_subtypes.csv`
  - `30_analysis/main/results/SM_china_policy_subtypes_counts.csv`
  - `30_analysis/main/results/SM_china_nonadopter_policy_validation.csv`

## 7) Reliability Pipeline

Run:

```bash
Rscript 30_analysis/reliability/scripts/S1_IRR_analysis.R
Rscript 30_analysis/reliability/scripts/S2_IRR_analysis.R
```

Reliability outputs:
- `30_analysis/reliability/outputs/S1_IRR_Aligned_25.csv`
- `30_analysis/reliability/outputs/S1_IRR_Disagreements_25.csv`
- `30_analysis/reliability/outputs/S1_IRR_Summary_25.csv`
- `30_analysis/reliability/outputs/S2_IRR_Disagreements_Review.csv`
- `30_analysis/reliability/outputs/S2_IRR_Summary_Table.csv`

## 8) Important Conventions

- Binary variables are encoded as 0/1.
- `id` is the join key within each dataset family.
