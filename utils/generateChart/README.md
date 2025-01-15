# Chart Generator

This project generates scatter plots from result data using matplotlib.

## Setup

1. Create a virtual environment:

```bash
python -m venv venv
```

2. Activate the virtual environment:
   - On Windows:
   ```bash
   .\venv\Scripts\activate
   ```
   - On Unix or MacOS:
   ```bash
   source venv/bin/activate
   ```

3. Install required packages:
```bash
pip install -r requirements.txt
```

## Usage

1. Place your result data in `results/result.json`

2. Run the script:
```bash
python generateChart.py
```

Or with a specific root directory:
```bash
python generateChart.py --root-dir /path/to/directory
```

3. Generated charts will be saved in `results/graphics/` in multiple formats (SVG, EPS, PDF, PNG)

## Data Format

The input JSON file should follow this structure:

```json
{
    "global": {
        "all": {
            "projects": [
                {
                    "scc": {
                        "nbLoC": 1000
                    },
                    "results": {
                        "adactl": {
                            "analysisTime": 1.5,
                            "overheadParsing": 0.2,
                            "overheadPopulating": 0.3,
                            "executionTime": 2.0
                        }
                        // ... other tools
                    }
                }
                // ... other projects
            ]
        },
        "small": { /* ... */ },
        "medium": { /* ... */ },
        "large": { /* ... */ }
    },
    "rules": {
        "rule1": {
            "all": { /* ... */ },
            "small": { /* ... */ },
            "medium": { /* ... */ },
            "large": { /* ... */ }
        }
        // ... other rules
    }
}
```
