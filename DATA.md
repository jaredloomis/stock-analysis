# Market Data Analysis Platform

The purpose of this project is to explore connections in data related to financial markets and assets. To begin down the road to useful market insights, we need (1) data sets and (2) a solid data analysis platform. Free data sets are suprisingly easy to come by, and given we can scrape APIs almost exhaustively before trials run out, gaining access to the premium info is not too expensive anyways.

## Available Data Sources

#### Stock Price Data

**Live ticker data**

- Scrape from any source - google, yahoo, tradingview

**Historical Data**

- Likely need a paid data source
- finnhub

#### Fundamentals Data

**SEC Filings info**

Provides net profit, shareholder equity.

- Official [Financial Statements](https://www.sec.gov/dera/data/financial-statement-data-sets.html) from SEC

#### National / Regional / Industry Data

**GPD Data**

- [World Bank](https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?locations=US)

**Total Market Cap**

- https://ycharts.com/indicators/us_total_market_capitalization (updated fairly often)
- https://siblisresearch.com/data/us-stock-market-value/

**U.S. Bureau of Economic Analysis**

- https://apps.bea.gov/API/signup/index.cfm
- GDP by industry.
- Regional income, employment statistics by county
- Monthly NIPA tables - GDP, national income, corporate profits, govt spending.

#### Social Media Data

In order of preference, we'll try to use:

- Official Libraries
- Community Libraries
- Official API
- Scraping

#### Paid Options

- finnhub or similar APIs

## Entities

#### Stock

Columns:

- Ticker
- Company name

#### StockIndicator

Columns:

- Stock ID
- Indicator Name (ex. `"totalDebt"`)
- Indicator value (ex. `$12.32`)
- Indicator source (ex. `"SEC Filing 2020 March"`)
- Indicated period (start & end dates)

## Data Analysis Platform Use Cases

The Data Analysis Platform (DAP) will provide the ability to execute complex queries on the dataset. Hypothetical queries:

Get all tech stocks with positive assets and

```
SELECT * FROM stocks s WHERE
  totalDebt < totalAssets AND
  industry = 'Technology'
```

```
SELECT * FROM stocks s WHERE
  totalDebt < totalAssets AND
  industry = 'Technology'
```

## Tech Choice

Start with **PostgreSQL**, scale up to Presto / Athena if needed.
