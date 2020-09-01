# Data Store Synchronizer Design

This document describes the functionality and implementation of the Data Store Synchonizer (DSS) component.

The role of the DSS is to, given a request for a specific data set, ensure this data is in the database, or signal error.
It is not responsible for serving the data via HTTP or any other method - data will be accessed directly from the data store in the analysis stage.

A data set request, the input parameter, is provided as a **indicator fetch plan**. It is a list of indicator IDs along with any arguments associated with the indicator.
In code, `type DataFetchPlan = [(IndicatorID, IndicatorArgs)]`.

As part of the implementation, we will need a mapping from indicator IDs to indicator commands.
This is probably best implemented as a hand-maintained config json file with a mapping from indicator IDs to commands (shell).

For each dataset requested:

1. Check if data is already present and up-to-date in db.
  - Select items from the data store with matching indicator ID, filtering by items with a start/end date within requested parameters.
2. If not, pull data, and put in db.
  - Run command provided in config file, parse the response.
  - Insert new data (**never overwriting old data**).

We'll use Kotlin just because databases can be annoying because I'm not super familiar with SQL and JVM has great ORMs for hacking.
