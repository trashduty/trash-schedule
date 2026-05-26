#!/usr/bin/env python3
"""Matchup email generator for NFL games.

Generates a conversion-oriented email for any NFL matchup using:
  - NFL_Odds/Data/spreads_odds.csv  (line / price / book / edge data)
  - Week 1 model pred_updated.csv   (QB and model efficiency metrics)

Usage::

    # Generate email for a specific game
    python scripts/generate_matchup_email.py --game NE@SEA

    # Specify week and output directory
    python scripts/generate_matchup_email.py --game NE@SEA --week 1 --output docs/emails

    # Generate all matchups for a week
    python scripts/generate_matchup_email.py --all --week 1 --output docs/emails

    # Print to stdout instead of writing files
    python scripts/generate_matchup_email.py --game NE@SEA --stdout
"""

import argparse
import csv
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Optional

try:
    from jinja2 import Environment, FileSystemLoader
except ImportError:
    print("Jinja2 is required: pip install Jinja2", file=sys.stderr)
    sys.exit(1)

# ---------------------------------------------------------------------------
# Repository file paths
# ---------------------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parent.parent
SPREADS_CSV = REPO_ROOT / "NFL_Odds" / "Data" / "spreads_odds.csv"
MODEL_CSV = REPO_ROOT / "Week 1 model pred_updated.csv"
TEMPLATE_DIR = REPO_ROOT / "templates"
DEFAULT_OUTPUT_DIR = REPO_ROOT / "docs" / "emails"

# ---------------------------------------------------------------------------
# NFL team full names
# ---------------------------------------------------------------------------
NFL_TEAMS: dict = {
    "ARI": "Arizona Cardinals",
    "ATL": "Atlanta Falcons",
    "BAL": "Baltimore Ravens",
    "BUF": "Buffalo Bills",
    "CAR": "Carolina Panthers",
    "CHI": "Chicago Bears",
    "CIN": "Cincinnati Bengals",
    "CLE": "Cleveland Browns",
    "DAL": "Dallas Cowboys",
    "DEN": "Denver Broncos",
    "DET": "Detroit Lions",
    "GB":  "Green Bay Packers",
    "HOU": "Houston Texans",
    "IND": "Indianapolis Colts",
    "JAX": "Jacksonville Jaguars",
    "KC":  "Kansas City Chiefs",
    # The spreads CSV uses "LA" for the Rams; "LAR" is the standard abbreviation.
    # Both are kept so the generator handles either spelling from any data source.
    "LA":  "Los Angeles Rams",
    "LAC": "Los Angeles Chargers",
    "LAR": "Los Angeles Rams",
    "LV":  "Las Vegas Raiders",
    "MIA": "Miami Dolphins",
    "MIN": "Minnesota Vikings",
    "NE":  "New England Patriots",
    "NO":  "New Orleans Saints",
    "NYG": "New York Giants",
    "NYJ": "New York Jets",
    "PHI": "Philadelphia Eagles",
    "PIT": "Pittsburgh Steelers",
    "SEA": "Seattle Seahawks",
    "SF":  "San Francisco 49ers",
    "TB":  "Tampa Bay Buccaneers",
    "TEN": "Tennessee Titans",
    "WAS": "Washington Commanders",
}


def team_name(abbr: str) -> str:
    """Return the full team name for an abbreviation, or the abbreviation if unknown."""
    return NFL_TEAMS.get(abbr.upper(), abbr)


# ---------------------------------------------------------------------------
# Column normalisation for the model prediction CSV
# The BOM (\ufeff) is stripped by csv.DictReader when encoding='utf-8-sig'.
# Fallback aliases handle any future header drift in re-exported CSVs.
# ---------------------------------------------------------------------------
MODEL_FIELD_ALIASES: dict = {
    "week":              ["week"],
    "team":              ["Team", "team"],
    "model_prediction":  ["Model Prediction", "model_prediction"],
    "spread":            ["Spread", "spread"],
    "cover_probability": ["Cover Probability (%)", "cover_probability"],
    "edge":              ["Edge", "edge"],
    "off_ep_season":     ["Offensive Expected Points (Season)", "off_ep_season"],
    "def_ep_season":     ["Defensive Expected Points (Season)", "def_ep_season"],
    "off_success_rate":  ["Offensive Success Rate (%)", "off_success_rate"],
    "def_success_rate":  ["Defensive Success Rate (%)", "def_success_rate"],
    "qb_epa_career":     ["QB Expected Points Added (Career)", "qb_epa_career"],
    "qb_epa_last10":     ["QB Expected Points Added (Last 10 games)", "qb_epa_last10"],
    "proe":              ["PROE", "proe"],
    "qb_name":           ["Qbname", "QB Name", "qb_name"],
}


def _model_get(row: dict, field_key: str, default: str = "") -> str:
    """Return the value for *field_key* from *row* using known column name aliases."""
    for alias in MODEL_FIELD_ALIASES.get(field_key, [field_key]):
        if alias in row:
            return row[alias]
    return default


# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

def fmt_line(value: float) -> str:
    """Format a spread line with an explicit sign (e.g. +4.5, -7, PK)."""
    if abs(value) < 0.01:
        return "PK"
    whole = int(value)
    if value == whole:
        return f"{'+' if value > 0 else ''}{whole}"
    return f"{'+' if value > 0 else ''}{value}"


def fmt_price(value: int) -> str:
    """Format an American odds price (e.g. -115, +100)."""
    return f"+{value}" if value > 0 else str(value)


def fmt_pct(value: float) -> str:
    """Format a [0,1] probability as a percentage string (e.g. 61.9%)."""
    return f"{value * 100:.1f}%"


def fmt_edge(value: float) -> str:
    """Format edge as a signed percentage string (e.g. +10.7%, -4.3%)."""
    return f"{'+' if value >= 0 else ''}{value * 100:.1f}%"


def fmt_date(date_str: str) -> str:
    """Format an ISO date string to a readable form (e.g. September 9th, 2026)."""
    try:
        dt = datetime.strptime(date_str.strip(), "%Y-%m-%d")
        day = dt.day
        suffix = (
            "th" if 11 <= day <= 13
            else {1: "st", 2: "nd", 3: "rd"}.get(day % 10, "th")
        )
        return dt.strftime(f"%B {day}{suffix}, %Y")
    except (ValueError, AttributeError):
        return date_str


def parse_float(s: str) -> float:
    try:
        return float(str(s).strip().rstrip("%"))
    except (ValueError, AttributeError):
        return 0.0


def parse_int(s: str) -> int:
    try:
        return int(float(str(s).strip()))
    except (ValueError, AttributeError):
        return 0


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class TeamStats:
    """Merged line/price and model data for one team in a matchup."""
    team: str
    # From spreads_odds.csv
    model_prediction: float     # Model's projected spread for this team
    market_line: float          # Market spread for this team
    market_price: int           # Market vig/price
    cover_probability: float    # Probability of covering the market line
    edge: float                 # Model edge vs market (market line)
    best_book: str
    best_line: float
    best_price: int
    best_cover_probability: float
    best_edge: float
    # From model prediction CSV
    qb_name: str = ""
    qb_epa_career: float = 0.0
    qb_epa_last10: float = 0.0
    off_ep_season: float = 0.0
    def_ep_season: float = 0.0
    off_success_rate: float = 0.0
    def_success_rate: float = 0.0
    proe: float = 0.0


@dataclass
class MatchupData:
    """Combined matchup object with both teams' stats and game metadata."""
    week: int
    game: str
    away_team: str
    home_team: str
    kickoff_date: str
    kickoff_time: str
    away: TeamStats
    home: TeamStats

    @property
    def featured(self) -> TeamStats:
        """The side the model favors most (highest best_edge)."""
        return self.away if self.away.best_edge >= self.home.best_edge else self.home

    @property
    def featured_side(self) -> str:
        return self.featured.team

    @property
    def opponent_side(self) -> str:
        return self.home.team if self.featured_side == self.away_team else self.away.team

    @property
    def opponent(self) -> TeamStats:
        return self.home if self.featured_side == self.away_team else self.away

    @property
    def is_featured_underdog(self) -> bool:
        return self.featured.market_line > 0

    @property
    def is_featured_road(self) -> bool:
        return self.featured_side == self.away_team

    @property
    def market_gap(self) -> float:
        """
        Points of gap between market line and model line for the featured side.
        Positive = getting more cushion than the model needs (underdog context).
        """
        return self.featured.market_line - self.featured.model_prediction

    @property
    def featured_qb(self) -> str:
        return self.away.qb_name if self.featured_side == self.away_team else self.home.qb_name

    @property
    def opponent_qb(self) -> str:
        return self.home.qb_name if self.featured_side == self.away_team else self.away.qb_name

    @property
    def featured_qb_epa_l10(self) -> float:
        return self.away.qb_epa_last10 if self.featured_side == self.away_team else self.home.qb_epa_last10

    @property
    def opponent_qb_epa_l10(self) -> float:
        return self.home.qb_epa_last10 if self.featured_side == self.away_team else self.away.qb_epa_last10

    @property
    def qb_epa_advantage(self) -> float:
        """Positive = featured QB has higher recent EPA than opponent QB."""
        return self.featured_qb_epa_l10 - self.opponent_qb_epa_l10


# ---------------------------------------------------------------------------
# CSV loaders
# ---------------------------------------------------------------------------

def load_spreads(path: Path, game: str, week: Optional[int] = None) -> dict:
    """
    Return {team_abbr: row_dict} for the given game from spreads_odds.csv.
    Optionally filter by week number.
    """
    results: dict = {}
    with open(path, newline="", encoding="utf-8-sig") as fh:
        for row in csv.DictReader(fh):
            if row.get("game", "").strip().upper() != game.upper():
                continue
            if week is not None and str(row.get("week", "")).strip() != str(week):
                continue
            results[row["team"].strip().upper()] = row
    return results


def load_model(path: Path, teams: list, week: Optional[int] = None) -> dict:
    """
    Return {team_abbr: row_dict} for the given teams from the model CSV.
    Optionally filter by week number.
    """
    team_set = {t.upper() for t in teams}
    results: dict = {}
    with open(path, newline="", encoding="utf-8-sig") as fh:
        for row in csv.DictReader(fh):
            t = _model_get(row, "team").strip().upper()
            if t not in team_set:
                continue
            if week is not None and _model_get(row, "week").strip() != str(week):
                continue
            results[t] = row
    return results


def build_team_stats(spread_row: dict, model_row: Optional[dict], team: str) -> TeamStats:
    """Merge spread and model data into a TeamStats object."""
    mr = model_row or {}
    return TeamStats(
        team=team,
        model_prediction=parse_float(spread_row.get("model_prediction", "0")),
        market_line=parse_float(spread_row.get("market_line", "0")),
        market_price=parse_int(spread_row.get("market_price", "0")),
        cover_probability=parse_float(spread_row.get("cover_probability", "0")),
        edge=parse_float(spread_row.get("edge", "0")),
        best_book=spread_row.get("best_book", "").strip(),
        best_line=parse_float(spread_row.get("best_line", "0")),
        best_price=parse_int(spread_row.get("best_price", "0")),
        best_cover_probability=parse_float(spread_row.get("best_cover_probability", "0")),
        best_edge=parse_float(spread_row.get("best_edge", "0")),
        qb_name=_model_get(mr, "qb_name"),
        qb_epa_career=parse_float(_model_get(mr, "qb_epa_career")),
        qb_epa_last10=parse_float(_model_get(mr, "qb_epa_last10")),
        off_ep_season=parse_float(_model_get(mr, "off_ep_season")),
        def_ep_season=parse_float(_model_get(mr, "def_ep_season")),
        off_success_rate=parse_float(_model_get(mr, "off_success_rate")),
        def_success_rate=parse_float(_model_get(mr, "def_success_rate")),
        proe=parse_float(_model_get(mr, "proe")),
    )


def build_matchup(game: str, week: Optional[int] = None) -> MatchupData:
    """Build a MatchupData object for the given game string (e.g. 'NE@SEA')."""
    if "@" not in game:
        raise ValueError(f"Game must be in AWAY@HOME format, got: {game!r}")

    away_team, home_team = [t.strip().upper() for t in game.split("@", 1)]
    game_key = f"{away_team}@{home_team}"

    spreads = load_spreads(SPREADS_CSV, game_key, week)
    if not spreads:
        raise ValueError(
            f"Game '{game_key}' not found in {SPREADS_CSV}"
            + (f" for week {week}" if week else "")
        )

    model_rows = load_model(MODEL_CSV, [away_team, home_team], week)

    sample = next(iter(spreads.values()))
    kickoff_date = sample.get("game_date_est", "").strip()
    kickoff_time = sample.get("game_time_est", "").strip()
    actual_week = int(sample.get("week", week or 1))

    return MatchupData(
        week=actual_week,
        game=game_key,
        away_team=away_team,
        home_team=home_team,
        kickoff_date=kickoff_date,
        kickoff_time=kickoff_time,
        away=build_team_stats(spreads[away_team], model_rows.get(away_team), away_team),
        home=build_team_stats(spreads[home_team], model_rows.get(home_team), home_team),
    )


# ---------------------------------------------------------------------------
# Narrative generation  (deterministic, data-driven)
# ---------------------------------------------------------------------------

def generate_subject(m: MatchupData) -> str:
    f = m.featured
    gap = m.market_gap
    qb_adv = m.qb_epa_advantage

    if m.is_featured_underdog and gap >= 2.0 and qb_adv > 3.0:
        return (
            f"{m.featured_qb}'s {team_name(m.featured_side)} Are Getting "
            f"{fmt_line(f.market_line)} \u2014 Our Model Only Needs {fmt_line(f.model_prediction)}"
        )
    if m.is_featured_underdog and gap >= 1.5:
        return (
            f"Week {m.week} Value: {team_name(m.featured_side)} "
            f"{fmt_line(f.market_line)} Carries a {fmt_pct(f.best_cover_probability)} Cover Probability"
        )
    if m.is_featured_underdog:
        return (
            f"Our Model Gives {team_name(m.featured_side)} a "
            f"{fmt_pct(f.cover_probability)} Chance to Cover {fmt_line(f.market_line)}"
        )
    return (
        f"{team_name(m.featured_side)} {fmt_line(f.market_line)} \u2014 "
        f"Model Grades This at {fmt_pct(f.best_cover_probability)} Cover Probability"
    )


def generate_preview_text(m: MatchupData) -> str:
    gap = m.market_gap
    return (
        f"The model found a {gap:.1f}-point gap between the market and reality "
        f"in Week {m.week}'s {team_name(m.away_team)} vs. {team_name(m.home_team)} \u2014 "
        f"here\u2019s what the data says."
    )


def generate_hook(m: MatchupData) -> str:
    f = m.featured
    gap = m.market_gap

    if m.is_featured_road and m.is_featured_underdog:
        opener = (
            f"The market has {team_name(m.home_team)} as a "
            f"{abs(f.market_line):.1f}-point home favorite for Week {m.week}. "
            f"Our model sees a {abs(f.model_prediction):.1f}-point game \u2014 a "
            f"{gap:.1f}-point discrepancy that translates directly into a "
            f"{fmt_pct(f.best_cover_probability)} cover probability for the road "
            f"{team_name(m.featured_side)}."
        )
    elif m.is_featured_underdog:
        opener = (
            f"The market has {team_name(m.opponent_side)} as a "
            f"{abs(m.opponent.market_line):.1f}-point favorite. "
            f"Our model disagrees \u2014 it grades this as a near-"
            f"{abs(f.model_prediction):.1f}-point game "
            f"with a {fmt_pct(f.best_cover_probability)} cover probability for "
            f"{team_name(m.featured_side)}."
        )
    else:
        opener = (
            f"The market is asking bettors to lay {abs(f.market_line):.1f} points with "
            f"{team_name(m.featured_side)}. Our model agrees \u2014 it grades "
            f"{team_name(m.featured_side)} at {fmt_pct(f.best_cover_probability)} "
            f"cover probability, and the best number available ({fmt_line(f.best_line)}) "
            f"is at {f.best_book}."
        )

    if m.qb_epa_advantage > 2.0:
        qb_section = (
            f" The QB edge reinforces the model: {m.featured_qb} has posted "
            f"+{m.featured_qb_epa_l10:.2f} EPA over his last 10 games vs. "
            f"{m.opponent_qb}\u2019s +{m.opponent_qb_epa_l10:.2f}."
        )
    elif m.qb_epa_advantage < -2.0:
        qb_section = (
            f" This is a model lean despite the QB edge going the other way \u2014 "
            f"{m.featured_qb} sits at +{m.featured_qb_epa_l10:.2f} EPA (last 10) "
            f"vs. {m.opponent_qb}\u2019s +{m.opponent_qb_epa_l10:.2f}."
        )
    else:
        qb_section = ""

    return opener + qb_section


def generate_why_model(m: MatchupData) -> str:
    f = m.featured
    opp = m.opponent
    gap = m.market_gap
    lines = []

    # QB comparison
    if m.qb_epa_advantage >= 0:
        lines.append(
            f"**Quarterback edge:** {m.featured_qb} carries a career EPA of "
            f"+{f.qb_epa_career:.2f} and is trending upward with "
            f"+{f.qb_epa_last10:.2f} EPA over the last 10 games. "
            f"{m.opponent_qb} trails at +{opp.qb_epa_last10:.2f} over the same window."
        )
    else:
        lines.append(
            f"**Quarterback context:** {m.featured_qb} is at +{f.qb_epa_last10:.2f} EPA "
            f"(last 10 games). Despite {m.opponent_qb}\u2019s edge at "
            f"+{opp.qb_epa_last10:.2f}, the model\u2019s other inputs still produce a "
            f"{fmt_pct(f.best_cover_probability)} cover probability."
        )

    # Offensive efficiency
    off_comparison = "above" if f.off_ep_season > opp.off_ep_season else "below"
    lines.append(
        f"**Offensive efficiency:** {team_name(m.featured_side)} posts "
        f"+{f.off_ep_season:.2f} offensive expected points above baseline (season), "
        f"{off_comparison} {team_name(m.opponent_side)}\u2019s +{opp.off_ep_season:.2f}. "
        f"Success rate stands at {f.off_success_rate * 100:.1f}%."
    )

    # Defensive matchup (opponent's defense)
    if opp.def_success_rate > 0.54:
        def_comment = (
            "above league average \u2014 creating a permissive environment "
            "for the featured offense"
        )
    else:
        def_comment = (
            "below league average \u2014 meaning the featured offense must earn every yard"
        )
    lines.append(
        f"**Defensive matchup:** The opposing defense allows a "
        f"{opp.def_success_rate * 100:.1f}% success rate, {def_comment}."
    )

    # Market gap
    if abs(gap) >= 1.5:
        lines.append(
            f"**The gap:** The market sets the line at {fmt_line(f.market_line)} while "
            f"the model projects {fmt_line(f.model_prediction)} \u2014 a {gap:.1f}-point "
            f"discrepancy. At {fmt_price(f.best_price)} at {f.best_book}, "
            f"you\u2019re pricing in that gap with favorable juice."
        )

    return "\n\n".join(lines)


def generate_bullets(m: MatchupData) -> list:
    f = m.featured
    opp = m.opponent
    gap = m.market_gap
    bullets = []

    # Bullet 1: QB / performance advantage
    qb_adv_abs = abs(m.qb_epa_advantage)
    if m.qb_epa_advantage >= 0:
        bullets.append(
            f"**QB advantage:** {m.featured_qb} has posted +{f.qb_epa_last10:.2f} EPA "
            f"over the last 10 games; {m.opponent_qb} is at +{opp.qb_epa_last10:.2f}. "
            f"That {qb_adv_abs:.2f}-point EPA gap is a signal the market consistently "
            f"underweights in opening lines."
        )
    else:
        bullets.append(
            f"**Model vs. QB market:** Despite {m.opponent_qb}\u2019s EPA edge "
            f"(+{opp.qb_epa_last10:.2f} vs. +{f.qb_epa_last10:.2f} last 10 games), "
            f"the model\u2019s efficiency metrics still favor {team_name(m.featured_side)} "
            f"to cover at {fmt_pct(f.best_cover_probability)}."
        )

    # Bullet 2: Line value
    if m.is_featured_underdog and gap >= 1.0:
        bullets.append(
            f"**Line value:** The market prices {team_name(m.featured_side)} at "
            f"{fmt_line(f.market_line)}, but the model only needs {fmt_line(f.model_prediction)} "
            f"to make this a 50/50 proposition. That {gap:.1f}-point cushion is where the "
            f"{fmt_pct(f.best_cover_probability)} cover probability comes from."
        )
    else:
        bullets.append(
            f"**Line value:** {team_name(m.featured_side)} at {fmt_line(f.market_line)} "
            f"aligns with the model\u2019s {fmt_line(f.model_prediction)} projection. "
            f"The structural edge: {fmt_pct(f.best_cover_probability)} cover probability "
            f"at {fmt_price(f.best_price)} vs. the implied market probability."
        )

    # Bullet 3: Best book / price
    bullets.append(
        f"**Best number:** {f.best_book} is offering {team_name(m.featured_side)} "
        f"{fmt_line(f.best_line)} at {fmt_price(f.best_price)}, "
        f"which translates to a {fmt_edge(f.best_edge)} edge over the implied market "
        f"probability. Shop the number \u2014 even a half-point can be the difference "
        f"between a push and a cover."
    )

    return bullets


def generate_cta(m: MatchupData) -> str:
    return (
        f"This is one of the clearest edges our model identified in Week {m.week}. "
        f"Subscribers get the full ranked edge board for every game \u2014 "
        f"complete with model lines, best books, and cover probabilities for all "
        f"16 matchups.\n\n"
        f"**[Subscribe now to get the complete Week {m.week} edge board \u2192]**"
    )


# ---------------------------------------------------------------------------
# Template rendering
# ---------------------------------------------------------------------------

def render_email(m: MatchupData) -> str:
    """Render the matchup email using the Jinja2 template."""
    env = Environment(
        loader=FileSystemLoader(str(TEMPLATE_DIR)),
        keep_trailing_newline=True,
    )
    template = env.get_template("matchup_email.md.j2")

    f = m.featured
    context = {
        "subject":        generate_subject(m),
        "preview_text":   generate_preview_text(m),
        "hook":           generate_hook(m),
        "why_model":      generate_why_model(m),
        "bullets":        generate_bullets(m),
        "subscriber_cta": generate_cta(m),
        "matchup": {
            "week":                 m.week,
            "game":                 m.game,
            "away_team":            m.away_team,
            "home_team":            m.home_team,
            "away_team_full":       team_name(m.away_team),
            "home_team_full":       team_name(m.home_team),
            "kickoff_date":         m.kickoff_date,
            "kickoff_date_fmt":     fmt_date(m.kickoff_date),
            "kickoff_time":         m.kickoff_time,
            "featured_side":        m.featured_side,
            "featured_side_full":   team_name(m.featured_side),
            "opponent_side":        m.opponent_side,
            "opponent_side_full":   team_name(m.opponent_side),
            "market_line_display":  fmt_line(f.market_line),
            "model_line_display":   fmt_line(f.model_prediction),
            "best_line_display":    fmt_line(f.best_line),
            "market_price_display": fmt_price(f.market_price),
            "best_price_display":   fmt_price(f.best_price),
            "cover_probability_pct":   fmt_pct(f.cover_probability),
            "best_cover_prob_pct":     fmt_pct(f.best_cover_probability),
            "edge_pct":             fmt_edge(f.edge),
            "best_edge_pct":        fmt_edge(f.best_edge),
            "best_book":            f.best_book,
            "away_qb":              m.away.qb_name,
            "home_qb":              m.home.qb_name,
            "market_gap":           f"{m.market_gap:.1f}",
            "is_featured_underdog": m.is_featured_underdog,
            "is_featured_road":     m.is_featured_road,
        },
    }
    return template.render(**context)


# ---------------------------------------------------------------------------
# CLI helpers
# ---------------------------------------------------------------------------

def list_games(week: Optional[int] = None) -> list:
    """Return all unique game strings from the spreads CSV, optionally filtered by week."""
    games: list = []
    seen: set = set()
    with open(SPREADS_CSV, newline="", encoding="utf-8-sig") as fh:
        for row in csv.DictReader(fh):
            if week is not None and str(row.get("week", "")).strip() != str(week):
                continue
            g = row.get("game", "").strip()
            if g and g not in seen:
                seen.add(g)
                games.append(g)
    return games


def main() -> None:
    parser = argparse.ArgumentParser(
        description=(
            "Generate conversion-oriented matchup emails from NFL data CSVs.\n\n"
            "Data sources:\n"
            f"  Spreads : {SPREADS_CSV.relative_to(REPO_ROOT)}\n"
            f"  Model   : {MODEL_CSV.relative_to(REPO_ROOT)}\n"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--game", metavar="AWAY@HOME",
        help="Generate email for a specific game (e.g., NE@SEA)",
    )
    group.add_argument(
        "--all", action="store_true",
        help="Generate emails for all games in the specified week",
    )
    parser.add_argument(
        "--week", type=int, default=None,
        help="Week number to filter on (default: auto-detect from CSV)",
    )
    parser.add_argument(
        "--output", metavar="DIR", default=str(DEFAULT_OUTPUT_DIR),
        help=f"Output directory (default: {DEFAULT_OUTPUT_DIR})",
    )
    parser.add_argument(
        "--stdout", action="store_true",
        help="Print output to stdout instead of writing files",
    )

    args = parser.parse_args()

    output_dir = Path(args.output)
    if not args.stdout:
        output_dir.mkdir(parents=True, exist_ok=True)

    games = [args.game] if args.game else list_games(args.week)

    for game in games:
        try:
            matchup = build_matchup(game, args.week)
        except ValueError as exc:
            print(f"[SKIP] {game}: {exc}", file=sys.stderr)
            continue

        content = render_email(matchup)

        if args.stdout:
            print(content)
            if len(games) > 1:
                print("\n" + "=" * 80 + "\n")
        else:
            safe_game = game.replace("@", "_at_")
            filename = f"{safe_game}_week{matchup.week}.md"
            out_path = output_dir / filename
            out_path.write_text(content, encoding="utf-8")
            print(f"\u2713  {out_path}")


if __name__ == "__main__":
    main()
