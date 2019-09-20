


WITH RECURSIVE
temp_table(s4_floc) AS (
    SELECT 'ALDWK'
        UNION ALL
    SELECT s4_floc.s4_floc
    FROM s4_floc, temp_table
    WHERE s4_floc.parent_floc = temp_table.s4_floc
    )
SELECT 
    floc.s4_floc            AS [Floc],
    floc.name               AS [Name]
FROM temp_table
JOIN s4_floc    AS floc ON temp_table.s4_floc = floc.s4_floc
ORDER BY floc.s4_floc
;

SELECT floc.* FROM s4_floc AS floc WHERE floc.s4_floc = 'ALDWK';