# NFL Matchup Email Generator

This directory contains generated matchup emails and the documentation for the automated
email generator that powers them.

## Overview

The generator reads canonical data from two repository CSVs and produces a fully
populated, conversion-oriented email for any NFL matchup:

| Data source | Used for |
|-------------|----------|
| `NFL_Odds/Data/spreads_odds.csv` | Line, price, edge, best book, cover probability |
| `Week 1 model pred_updated.csv` | QB EPA, offensive/defensive efficiency metrics |

No external API calls, HTML parsing, or LLM usage — everything is derived
deterministically from the CSVs.

---

## Quick Start

### Prerequisites

- Python 3.8+
- Jinja2 (`pip install Jinja2`)

### Generate a single matchup email

```bash
python scripts/generate_matchup_email.py --game NE@SEA
```

Output is written to `docs/emails/NE_at_SEA_week1.md`.

### Generate all Week 1 matchup emails

```bash
python scripts/generate_matchup_email.py --all --week 1
```

This generates one file per game (16 files for a full week).

### Print to stdout instead of writing a file

```bash
python scripts/generate_matchup_email.py --game NE@SEA --stdout
```

### Specify a custom output directory

```bash
python scripts/generate_matchup_email.py --game NE@SEA --output /path/to/output
```

### Full CLI reference

```
usage: generate_matchup_email.py [-h] (--game AWAY@HOME | --all)
                                  [--week WEEK] [--output DIR] [--stdout]

arguments:
  --game AWAY@HOME   Generate email for a specific game (e.g., NE@SEA)
  --all              Generate emails for all games in the specified week
  --week WEEK        Week number to filter on (default: auto-detect)
  --output DIR       Output directory (default: docs/emails)
  --stdout           Print output to stdout instead of writing files
```

---

## Email Sections

Every generated email contains the following sections, all populated from CSV data:

| Section | Content |
|---------|---------|
| **Subject** | Data-driven subject line emphasizing the key angle (QB edge, market gap, etc.) |
| **Preview text** | One-sentence teaser with the core edge metric |
| **Hook** | 2–3 sentences capturing the market-vs-model story |
| **Model spotlight card** | Structured table: game, kickoff, market line, model line, best available, cover probability, edge, QB assumptions |
| **Why the model sees it this way** | QB comparison, offensive efficiency, defensive matchup, market gap analysis |
| **Matchup bullets** | Three concise data-driven bullets |
| **Subscriber CTA** | Conversion-oriented call to action linking to the full edge board |

---

## File Structure

```
scripts/
└── generate_matchup_email.py   # Generator script

templates/
└── matchup_email.md.j2         # Jinja2 email template

docs/emails/
├── README.md                   # This file
└── NE_at_SEA_week1.md          # Example generated output (NE@SEA, Week 1)
```

---

## Matchup Object Fields

The generator builds a matchup object containing:

| Field | Source | Description |
|-------|--------|-------------|
| `week` | spreads_odds.csv | Week number |
| `game` | spreads_odds.csv | Game ID (e.g. `NE@SEA`) |
| `away_team` | spreads_odds.csv | Away team abbreviation |
| `home_team` | spreads_odds.csv | Home team abbreviation |
| `kickoff_date` | spreads_odds.csv | Kickoff date (ISO format) |
| `kickoff_time` | spreads_odds.csv | Kickoff time (EST) |
| `featured_side` | computed | Team with the highest model edge |
| `market_line` | spreads_odds.csv | Market spread for the featured side |
| `model_line` | spreads_odds.csv | Model's projected spread for the featured side |
| `best_line` | spreads_odds.csv | Best available line across books |
| `market_price` | spreads_odds.csv | Market vig/price |
| `best_price` | spreads_odds.csv | Best available price across books |
| `cover_probability` | spreads_odds.csv | Probability of covering the market line |
| `edge` | spreads_odds.csv | Model edge vs market (market line) |
| `best_book` | spreads_odds.csv | Book offering the best line |
| `away_qb` | model pred CSV | Away team QB name |
| `home_qb` | model pred CSV | Home team QB name |
| `qb_epa_career` | model pred CSV | Career EPA for each QB |
| `qb_epa_last10` | model pred CSV | EPA over last 10 games for each QB |
| `off_ep_season` | model pred CSV | Offensive expected points above baseline |
| `def_ep_season` | model pred CSV | Defensive expected points above baseline |
| `off_success_rate` | model pred CSV | Offensive success rate |
| `def_success_rate` | model pred CSV | Defensive success rate |

---

## Template Customisation

The email template lives in `templates/matchup_email.md.j2` and uses
[Jinja2](https://jinja.palletsprojects.com/) syntax. Edit this file to change
the layout, add new sections, or adjust formatting without touching the
generator logic.

---

## Adding a New Week

When a new model prediction CSV is added to the repository:

1. Update the `MODEL_CSV` constant in `scripts/generate_matchup_email.py`
   to point to the new file.
2. Ensure the new week's data appears in `NFL_Odds/Data/spreads_odds.csv`.
3. Run:

```bash
python scripts/generate_matchup_email.py --all --week <N>
```

---

## Example Output

See [`NE_at_SEA_week1.md`](NE_at_SEA_week1.md) for a fully generated example
email for the Patriots vs. Seahawks Week 1 matchup.
