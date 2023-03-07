-- Parameter validation
IF (LEN(@tenant) < 1 OR LEN(@tenant) > 100)
    RAISERROR ('Invalid input: @tenant parameter must be between 1 and 100 characters.', 16, 1)
IF (LEN(@service_name) < 1 OR LEN(@service_name) > 100)
    RAISERROR ('Invalid input: @service_name parameter must be between 1 and 100 characters.', 16, 1)
IF (LEN(@request_type) < 1 OR LEN(@request_type) > 100)
    RAISERROR ('Invalid input: @request_type parameter must be between 1 and 100 characters.', 16, 1)

IF OBJECT_ID('tempdb..#request_parameters_table') IS NULL
    CREATE TABLE #request_parameters_table (
        parameter_name VARCHAR(MAX), 
        parameter_value VARCHAR(MAX)
    )

INSERT INTO #request_parameters_table (parameter_name, parameter_value)
SELECT QUOTENAME(parameter_name, '"'), QUOTENAME(parameter_value, '"')
FROM request_parameters_table

IF NOT EXISTS(SELECT * FROM #request_parameters_table)
    RAISERROR ('Invalid input: request_parameters_table must have at least one row.', 16, 1)

-- Construct the API request
DECLARE @url VARCHAR(MAX) = 'https://wd5-impl-services1.workday.com/ccx/service/' + @tenant + '/' + @service_name + '/v47.0'
DECLARE @request_body NVARCHAR(MAX) = '{ "request": { "type": "' + @request_type + '", "version": "v47.0", "request_parameters": ['

-- Add the request parameters to the request body using a set-based approach
DECLARE @request_params_table TABLE (parameter_name VARCHAR(MAX), parameter_value VARCHAR(MAX))

INSERT INTO @request_params_table (parameter_name, parameter_value)
SELECT parameter_name, parameter_value
FROM #request_parameters_table

SET @request_body = @request_body + (
    SELECT CONCAT(' { "name": ', QUOTENAME(parameter_name, '"'), ', "value": ', QUOTENAME(parameter_value, '"'), ' },')
    FROM @request_params_table
    FOR JSON PATH, WITHOUT_ARRAY_WRAPPER
)

SET @request_body = @request_body + '] } }'

-- Execute the API request and store the JSON response in a variable
DECLARE @json_response NVARCHAR(MAX)
BEGIN TRY
    EXEC sp_executesql N'
    DECLARE @response_table TABLE (response NVARCHAR(MAX))
    DECLARE @http_status INT

    -- Use the sp_OACreate stored procedure to create an HTTP request object
    EXEC sp_OACreate ''MSXML2.XMLHTTP'', @http_request OUT

    -- Send the request
    EXEC sp_OAMethod @http_request, ''Open'', NULL, ''POST'', @url, ''false''
    EXEC sp_OAMethod @http_request, ''setRequestHeader'', NULL, ''Authorization'', ''Bearer '' + @access_token
    EXEC sp_OAMethod @http_request, ''setRequestHeader'', NULL, ''Content-Type'', ''application/json''
    EXEC sp_OAMethod @http_request, ''send'', NULL, @request_body

    -- Check the response status
    EXEC sp_OAGetProperty @http_request, ''status'', @http_status OUT
    IF @http_status <> 200
        RAISERROR (''Workday API returned an error: %d'', 16, 1, @http_status)

    -- Get the response content
    EXEC sp_OAMethod @http_request, ''responseText'', @json_response OUT

    -- Clean up the HTTP request object
    EXEC sp_OADestroy @http_request
    ', N'@url VARCHAR(MAX), @access_token VARCHAR(MAX), @request_body NVARCHAR(MAX), @json_response NVARCHAR(MAX) OUTPUT', @url, @access_token, @request_body, @json_response OUTPUT

    -- Check for other error codes
    IF @http_status = 400
        RAISERROR (''Bad Request: %d'', 16, 1, @http_status)
    ELSE IF @http_status = 401
        RAISERROR (''Unauthorized: %d'', 16, 1, @http_status)
    ELSE IF @http_status = 404
        RAISERROR (''Not Found: %d'', 16, 1, @http_status)

END TRY
BEGIN CATCH
    DECLARE @error_message NVARCHAR(MAX) = ERROR_MESSAGE()
    -- Log the error message to a file or database
    INSERT INTO error_log (message) VALUES (@error_message)
    RAISERROR (@error_message, 16, 1)
END CATCH

SELECT @json_response