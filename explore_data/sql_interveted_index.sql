-- A stored procedure that creates an inverted index for a specified table and column in a PostgreSQL database.

DO $$ 
DECLARE
  exists BOOLEAN;
BEGIN
  -- Prompt user for table and column names
  \echo Enter table name:
  \prompt table_name
  \echo Enter column name:
  \prompt column_name

  -- Check if table exists
  SELECT EXISTS (
    SELECT 1
    FROM information_schema.tables
    WHERE table_schema = 'public'
    AND table_name = :table_name
  ) INTO exists;

  IF NOT exists THEN
    RAISE EXCEPTION 'Table does not exist';
  END IF;

  -- Check if column exists
  SELECT EXISTS (
    SELECT 1
    FROM information_schema.columns
    WHERE table_name = :table_name
    AND column_name = :column_name
  ) INTO exists;

  IF NOT exists THEN
    RAISE EXCEPTION 'Column does not exist';
  END IF;

  -- Create table for documents
  CREATE TABLE documents (
    id SERIAL PRIMARY KEY,
    text VARCHAR(1000)
  );

  -- Insert data from user-specified column into documents table
  EXECUTE format('INSERT INTO documents (text) SELECT %I FROM %I', column_name, table_name);

  -- Create table for inverted index
  CREATE TABLE inverted_index (
    word VARCHAR(50),
    document_id INT,
    frequency INT,
    PRIMARY KEY (word, document_id)
  );

  -- Tokenize and remove stop words
  WITH tokens AS (
    SELECT id, regexp_split_to_table(lower(text), '\W+') AS token
    FROM documents
  ), filtered_tokens AS (
    SELECT id, token
    FROM tokens
    WHERE token NOT IN (
      SELECT word FROM stop_words
    )
  ), vocabulary AS (
    SELECT DISTINCT token
    FROM filtered_tokens
  ), document_frequencies AS (
    SELECT token, COUNT(DISTINCT id) AS df
    FROM filtered_tokens
    GROUP BY token
  ), max_df AS (
    SELECT MAX(df) AS max_df
    FROM document_frequencies
  ), normalized_frequencies AS (
    SELECT id, token, COUNT(*)::FLOAT / MAX(df) AS tf_idf
    FROM filtered_tokens
    JOIN document_frequencies USING (token)
    CROSS JOIN max_df
    GROUP BY id, token
  )
  -- Insert into inverted index
  INSERT INTO inverted_index
  SELECT token, id, tf_idf
  FROM normalized_frequencies;
END $$;