# Trash Schedule - CFB Spreads

This repository contains CFB (College Football) spreads data and an interactive web table for viewing the data.

## GitHub Pages Site

The repository includes an interactive HTML table (`index.html`) that displays CFB spreads data with the following features:

- **Live Data**: Automatically fetches data from `cfb model output_new.csv`
- **Sortable Columns**: Click any column header to sort
- **Search**: Filter data in real-time across all columns
- **Color Coding**: Visual indicators for Cover Probability and Edge percentages
- **Auto-Refresh**: Updates data every 5 minutes
- **Mobile Friendly**: Responsive design works on all devices

### Enabling GitHub Pages

To deploy the site:

1. Go to your repository **Settings**
2. Navigate to **Pages** in the left sidebar
3. Under **Source**, select:
   - Branch: `main`
   - Folder: `/ (root)`
4. Click **Save**

The site will be available at: `https://<username>.github.io/trash-schedule/`

### Local Testing

To test the page locally:

```bash
# Start a local HTTP server
python3 -m http.server 8080

# Open in browser
# http://localhost:8080/index.html
```

## Data Files

- `cfb model output_new.csv` - Current CFB spreads data with predictions and edge calculations
- `CFB Team Data Table.csv` - Team data
- Other CSV files for historical data and lookups
