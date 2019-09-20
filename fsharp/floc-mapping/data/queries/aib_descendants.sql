


WITH RECURSIVE
temp_table(sai_ref) AS (
    SELECT 'SAI00277997'
        UNION ALL
    SELECT aib_floc.sai_ref
    FROM aib_floc, temp_table
    WHERE aib_floc.parent_ref = temp_table.sai_ref
    )
SELECT 
    floc.sai_ref            AS [Reference],
    floc.short_name         AS [Name],
    floc.common_name        AS [CommonName]
FROM temp_table
JOIN aib_floc    AS floc ON temp_table.sai_ref = floc.sai_ref
ORDER BY floc.common_name
;

SELECT floc.* FROM aib_floc AS floc WHERE floc.sai_ref = 'SAI00277997';