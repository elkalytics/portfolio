-- Attempt at the 'check_phone_format.R' in SQL
-- validate and sanitize phone numbers in the original table
UPDATE my_table
SET phone = regexp_replace(phone, '[^0-9]', '', 'g')
WHERE phone ~ '[^0-9]';

-- format and flag phone numbers in a single query
SELECT t.*, 
       CASE
           WHEN phone ~ '^\\(?[0-9]{3}\\)?[ -]?[0-9]{3}[ -]?[0-9]{4}$'
               THEN concat('(', substr(phone, 1, 3), ')-', substr(phone, 4, 3), '-', substr(phone, 7, 4))
           WHEN phone ~ '^[0-9]{7}$'
               THEN concat(substr(phone, 1, 3), '-', substr(phone, 4, 4))
           ELSE NULL
       END AS formatted_phone,
       CASE
           WHEN phone ~ '^\\(?[0-9]{3}\\)?[ -]?[0-9]{3}[ -]?[0-9]{4}$' OR phone ~ '^[0-9]{7}$'
               THEN 0
           ELSE 1
       END AS number_flag
FROM my_table t;
