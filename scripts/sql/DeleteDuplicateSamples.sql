-- Delete all duplicate indicatorsamples
DELETE
FROM  indicatorsample a
USING indicatorsample b
WHERE a.sample_id > b.sample_id
 AND a.indicator_id = b.indicator_id
 AND a.stock_id = b.stock_id
 AND a.start_time = b.start_time
 AND a.end_time = b.end_time
 AND a.indicator_value = b.indicator_value;
