SELECT * FROM IndicatorSample
WHERE indicator_id = ?
  AND stock_id = ?
  AND start_time >= ?
  AND end_time < ?;