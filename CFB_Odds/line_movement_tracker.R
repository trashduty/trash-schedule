library(tidyverse)
library(lubridate)
library(readr)

# Ensure the Reports directory exists
if (!dir.exists("CFB_Odds/Reports")) {
  dir.create("CFB_Odds/Reports", recursive = TRUE)
}

# Read the current odds data
odds_data <- read_csv("CFB_Odds/Data/spreads_odds.csv", show_col_types = FALSE)

# Read or initialize the running log
log_file <- "CFB_Odds/Reports/line_movement_history.csv"

if (file.exists(log_file)) {
  history <- read_csv(log_file, show_col_types = FALSE)
} else {
  history <- tibble(
    week = integer(),
    game = character(),
    team = character(),
    snapshot_date = as.Date(character()),
    snapshot_time = as.POSIXct(character()),
    market_line = numeric(),
    market_price = numeric(),
    commence_time = as.POSIXct(character())
  )
}

# Create today's snapshot with timestamp
today_snapshot <- odds_data %>%
  mutate(
    snapshot_date = as.Date(last_update_api),
    snapshot_time = as.POSIXct(last_update_api)
  ) %>%
  select(
    week,
    game,
    team,
    snapshot_date,
    snapshot_time,
    market_line,
    market_price,
    commence_time
  )

# Append to history (avoiding duplicates from same day)
history <- bind_rows(history, today_snapshot) %>%
  distinct(week, game, team, snapshot_date, .keep_all = TRUE)

# Save updated history
write_csv(history, log_file)

# Generate the line movement report
line_movement_report <- history %>%
  group_by(week, game, team) %>%
  arrange(snapshot_date) %>%
  mutate(
    opening_line = first(market_line),
    opening_date = first(snapshot_date),
    current_line = last(market_line),
    current_date = last(snapshot_date),
    days_elapsed = as.integer(current_date - opening_date),
    line_movement = current_line - opening_line,
    abs_movement = abs(line_movement),
    movement_pct = round((abs_movement / abs(opening_line)) * 100, 1),
    movement_direction = case_when(
      line_movement > 0 ~ "Moved Up",
      line_movement < 0 ~ "Moved Down",
      TRUE ~ "No Change"
    )
  ) %>%
  ungroup() %>%
  select(
    week,
    game,
    team,
    opening_date,
    current_date,
    days_elapsed,
    opening_line,
    current_line,
    line_movement,
    abs_movement,
    movement_direction,
    current_date
  ) %>%
  distinct() %>%
  arrange(week, desc(abs_movement))

# Save report as CSV (sortable in Excel/spreadsheet)
report_file <- "CFB_Odds/Reports/line_movement_report.csv"
write_csv(line_movement_report, report_file)

# Create an HTML report with interactive sorting
create_html_report <- function(report_data) {
  
  # Group by week for better organization
  weeks <- unique(report_data$week)
  
  html_content <- "<!DOCTYPE html>
<html>
<head>
  <meta charset='UTF-8'>
  <title>CFB Line Movement Report</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { 
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      min-height: 100vh;
      padding: 20px;
    }
    .container { max-width: 1400px; margin: 0 auto; }
    .header {
      background: white;
      padding: 20px;
      border-radius: 8px;
      margin-bottom: 20px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }
    .header h1 { color: #333; margin-bottom: 10px; }
    .header p { color: #666; font-size: 14px; }
    .week-section {
      background: white;
      margin-bottom: 20px;
      border-radius: 8px;
      overflow: hidden;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }
    .week-header {
      background: #2c3e50;
      color: white;
      padding: 15px 20px;
      font-size: 18px;
      font-weight: bold;
    }
    table { 
      width: 100%;
      border-collapse: collapse;
    }
    th { 
      background-color: #34495e;
      color: white;
      padding: 12px;
      text-align: left;
      cursor: pointer;
      user-select: none;
      font-weight: 600;
      font-size: 13px;
      border-bottom: 2px solid #2c3e50;
    }
    th:hover { 
      background-color: #2c3e50;
    }
    th::after {
      content: ' ⇅';
      opacity: 0.5;
    }
    td { 
      padding: 12px;
      border-bottom: 1px solid #ecf0f1;
    }
    tr:hover { 
      background-color: #f8f9fa;
    }
    .game-name { font-weight: 500; color: #2c3e50; }
    .team-name { color: #555; padding-left: 20px; font-size: 14px; }
    .positive { color: #27ae60; font-weight: bold; }
    .negative { color: #e74c3c; font-weight: bold; }
    .big-move { background-color: #fff3cd; }
    .huge-move { background-color: #f8d7da; }
    .number { text-align: center; font-family: 'Courier New', monospace; }
    .date { text-align: center; font-size: 13px; color: #666; }
    .legend {
      display: flex;
      gap: 30px;
      margin-top: 15px;
      flex-wrap: wrap;
    }
    .legend-item {
      display: flex;
      align-items: center;
      gap: 8px;
      font-size: 13px;
    }
    .legend-box {
      width: 20px;
      height: 20px;
      border-radius: 3px;
    }
  </style>
  <script>
    function sortTable(table, columnIndex) {
      var rows = Array.from(table.querySelectorAll('tbody tr'));
      var isAscending = table.dataset.sortOrder !== 'asc-' + columnIndex;
      
      rows.sort((a, b) => {
        var aValue = a.children[columnIndex].innerText.trim();
        var bValue = b.children[columnIndex].innerText.trim();
        
        // Try to parse as number
        var aNum = parseFloat(aValue);
        var bNum = parseFloat(bValue);
        
        if (!isNaN(aNum) && !isNaN(bNum)) {
          return isAscending ? aNum - bNum : bNum - aNum;
        }
        
        return isAscending ? 
          aValue.localeCompare(bValue) : 
          bValue.localeCompare(aValue);
      });
      
      rows.forEach(row => table.querySelector('tbody').appendChild(row));
      table.dataset.sortOrder = (isAscending ? 'asc-' : 'desc-') + columnIndex;
    }
  </script>
</head>
<body>
  <div class='container'>
    <div class='header'>
      <h1>📊 CFB Line Movement Tracker</h1>
      <p>Updated: "
  
  html_content <- paste0(html_content, format(Sys.time(), "%A, %B %d, %Y at %I:%M %p %Z"))
  
  html_content <- paste0(html_content, "</p>
      <p>Click column headers to sort | Yellow = 3+ pts movement | Red = 5+ pts movement</p>
      <div class='legend'>
        <div class='legend-item'>
          <div class='legend-box' style='background-color: #fff3cd;'></div>
          <span>3+ point move</span>
        </div>
        <div class='legend-item'>
          <div class='legend-box' style='background-color: #f8d7da;'></div>
          <span>5+ point move (huge)</span>
        </div>
      </div>
    </div>
  ")
  
  # Build table for each week
  for (w in sort(weeks)) {
    week_data <- report_data %>% filter(week == w)
    
    html_content <- paste0(html_content, "
    <div class='week-section'>
      <div class='week-header'>Week ", w, " (", nrow(week_data), " team entries)</div>
      <table data-sort-order=''>
        <thead>
          <tr>
            <th onclick='sortTable(this.closest(\"table\"), 0)'>Game</th>
            <th onclick='sortTable(this.closest(\"table\"), 1)'>Team</th>
            <th onclick='sortTable(this.closest(\"table\"), 2)'>Opening Line</th>
            <th onclick='sortTable(this.closest(\"table\"), 3)'>Current Line</th>
            <th onclick='sortTable(this.closest(\"table\"), 4)'>Movement</th>
            <th onclick='sortTable(this.closest(\"table\"), 5)'>Days</th>
            <th onclick='sortTable(this.closest(\"table\"), 6)'>Direction</th>
            <th onclick='sortTable(this.closest(\"table\"), 7)'>As of</th>
          </tr>
        </thead>
        <tbody>
    ")
    
    # Add rows
    for (i in 1:nrow(week_data)) {
      row <- week_data[i, ]
      
      # Determine styling
      movement_class <- case_when(
        row$line_movement > 0 ~ "positive",
        row$line_movement < 0 ~ "negative",
        TRUE ~ ""
      )
      
      row_class <- case_when(
        abs(row$line_movement) >= 5 ~ " huge-move",
        abs(row$line_movement) >= 3 ~ " big-move",
        TRUE ~ ""
      )
      
      game_display <- if_else(i == 1, row$game, "")
      
      html_content <- paste0(html_content, "
        <tr class='", row_class, "'>
          <td class='game-name'>", game_display, "</td>
          <td class='team-name'>", row$team, "</td>
          <td class='number'>", format(round(row$opening_line, 1), nsmall = 1), "</td>
          <td class='number'>", format(round(row$current_line, 1), nsmall = 1), "</td>
          <td class='number ", movement_class, "'>",
          ifelse(row$line_movement > 0, "+", ""),
          format(round(row$line_movement, 1), nsmall = 1),
          "</td>
          <td class='number'>", row$days_elapsed, "</td>
          <td class='number'>", row$movement_direction, "</td>
          <td class='date'>", format(row$current_date, "%m/%d"), "</td>
        </tr>
      ")
    }
    
    html_content <- paste0(html_content, "
        </tbody>
      </table>
    </div>
    ")
  }
  
  html_content <- paste0(html_content, "
  </div>
</body>
</html>")
  
  write(html_content, "CFB_Odds/Reports/line_movement_report.html")
}

# Generate reports
create_html_report(line_movement_report)

# Print summary
cat("\n=== LINE MOVEMENT REPORT ===\n")
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Records in history:", nrow(history), "\n")
cat("Total games tracked:", n_distinct(line_movement_report$game), "\n")
cat("Weeks active:", paste(sort(unique(line_movement_report$week)), collapse = ", "), "\n\n")

# Show top movers
cat("TOP 10 BIGGEST MOVERS:\n")
print(line_movement_report %>%
  arrange(desc(abs_movement)) %>%
  head(10) %>%
  select(week, game, team, opening_line, current_line, line_movement, days_elapsed))
