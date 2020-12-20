# Simulator Design

The goal of the simulator is to provide an API that allow for testing of arbitrary trading strategies, and
general sample analysis. A common use case may be to predict the future direction of an asset given existing samples.
It is an easy-to-use API for traversing large amounts of data provided by the Data Loader.

## Simulation

Any simulation (ie. a trading strategy simulation, or a general analysis) must answer two questions:

- What data should we pull from the Data Loader?
- What do you want to do with the data?

The latter question can be answered by the simulation writer providing a function `Samples -> Actions` which mutably
does whatever analysis is desired, and then returns a set of actions to take (ie. buy, sell, put).

As far as data requests, we should look at a few example use cases:

**Example 1: Simple Trading Strategy**

For the first example, say I have the question

> What would be my estimated return if I were to buy SPY every time its price decreases for 3 days in a row, and sell whenever it goes up for 3 days in a row?

In this case, the data needed from the Data Loader is specific - we need any SPY price data we can get
(maybe with a sane limit on the number of samples).
For the second question, the code may look like:

```python
def simple_trading_strategy_simulation(samples):
    """
        Assuming samples is an array of daily prices with most recent sample at index 0.
    """
    if samples[0].value < samples[1].value < samples[2].value:
        return Buy("SPY")
    elif samples[0].value > samples[1].value > samples[2].value:
        return Sell("SPY")

simulation = Simulation({'price': {'tickers': ["SPY"]}}, simple_trading_strategy_simulation)
simulation.run()
```

This would output something like:

```
SELL SPY 12-12-01 $124
(VERBOSE) running return: $2
SELL SPY 12-13-01 $126
(VERBOSE) running return: $6
BUY  SPY 12-14-01 $123
(VERBOSE) running return: $9
SELL SPY 12-15-01 $121
(VERBOSE) running return: $8
BUY  SPY 12-16-01 $128
(VERBOSE) running return: $12
...
==========
Total return: $17590 over 20 years
```
