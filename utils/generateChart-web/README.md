# generateChart-web

Small Vue + Vite web app to quickly visualize Ada static analysis benchmark results as scatter plots.

## Installation

From the repository root:

```bash
cd utils/generateChart-web
npm install
```

## Run the dev server

```bash
npm run dev
```

Then open the URL printed in the terminal (by default http://localhost:5173).

## Usage

1. Generate a benchmark result file (for example `results/result.json`) with your usual pipeline.
2. Open the web page while the dev server is running.
3. Click on "Select a JSON file" and choose your `result.json`.
4. Use the controls at the top:
   - **Scope**: global or by rule.
   - **Rule**: if scope = rule, select the rule to display.
   - **Category**: all / small / medium / large.
   - **Metric**: analysis time, overhead, total execution time.
5. The chart displays a log–log scatter plot with one series per tool (AdaControl, Cogralys, GNATcheck 1/32 cores).

This web version focuses on quick visual inspection of point clouds; it does not include the empirical curves or convex hull visualisations.
