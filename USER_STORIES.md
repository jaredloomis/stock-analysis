# User Stories

## User Story 1: Trading Strategy Tester

As a stock market trader, I'd like to quickly experiment with trading strategies to employ in automated or manual trading.

ex.

A strategy representing "buy a put on AMD at the money, and sell it when AMD's price has dropped 2%."

```
stock=AMD
current_price=price(stock)
limit_percent=2%

buy put(stock) when current_price > price_at(stock, now - )
sell put(stock) when currency_price
```

### Rules and Strategies

**Rules** are the core of the strategy tester. A trading **strategy** can be defined simply as a set of rules. Each rule includes two parts. First, an **action** must be defined. This can be buy, sell, put, or call on a specific asset (perhaps more to come). Second, a **condition** must be described. A rule's condition is a set of requirements which must be met before taking the associated action. For example, a user may define a rule `put 12 SPY when ($elite_sentiment("SPY") - $common_sentiment("SPY")) > 0.3`, meaning "buy puts on SPY whenever the sentiment of the insiders is much greater than public sentiment" (by a certain margin).

#### Indicators

**Ultra-simple**

- Stock value (`value(ticker: string)`)
- Historical value (`value_at(ticker: string, time: Time)`)
- Trailing Limit (`trailing_limit(percent: float): bool`)

**Numeric analysis on market data**

- Linear Regression (`linear_regression(samples: int): float`)
- RSI (`rsi(samples: int): float`)

**Public sentiment**

- "Common class" sentiment (`common_sentiment: float`)
  - Twitter   (if filtered properly, only users w/ relatively low # of followers?) (`twitter_sentiment: float`)
  - FB        (^) (`facebook_sentiment: float`)
  - Instagram (^) (`instagram_sentiment: float`)
  - Reddit (`reddit_sentiment: float`)
  - etc.
- "Elite Class" sentiment (`elite_sentiment: float`)
  - Bloomberg (`bloomburg_sentiment: float`)
  - Forbes (`forbes_sentiment: float`)
  - Entrepreneur (`entrepreneur_com_sentiment: float`)
  - WSJ (`wsj_sentiment: float`)
  - etc.
- Total public sentiment (`sentiment: float`)
  - Total of all previous indicators

**Structural conditions**

- Lending rates (`lending_rate: float`, `recently_changed_lending_rate(by_at_least: float): bool`)
- Presidential approval rating (`presidential_approval_rating: float`)

**Machine Learning**

- Reinforcement learning (each model has a name)
  - Potential Inputs: price history, all other indicators

#### Example Strategy

An example strategy might have the rules:

- `sell AAPL when $value("APPL") - $value_at("APPL", "3 days ago") > 12` (trailing limit)

#### Executing a Strategy

In order to execute a strategy, the user specifies the strategy and provides the date range to run strategy on as an additional input parameter.

Output is:
- Total success of the strategy over the course of the date range.
- Log every time a rule is executed.
- Log every time a rule's sub-condition changes (only if `-v`).

The data fetching subsystem will fill in any missing data in the database, then the analysis subsystem will run the requested analysis by querying the DB directly.

Example:

```
$ <command> --strategy simple.strat --start <date> --end <date>
```

Output:

```
{#for each sample}
{
  "time": "<datetime>",
  "strategy": "simple.strat",
  "rule": "my_rule1",
  "result": false,
  // Optional - more work to implement
  "conditions": [
    "condition": "($elite_sentiment - $common_sentiment) > 0.3",
    "result": false
  ]}
}
{/for}

Profit: -$12.43
```

### User Story 2: Fair Market Value Estimator

Using machine learning
