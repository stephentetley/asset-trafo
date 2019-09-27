
-- Effectively this generates a 'common name' for s4

WITH RECURSIVE
temp_table(floc, parent, short_name) AS (
    SELECT '', 'ALDWK-WWT-PLT-RGM-SYS01', ''
        UNION ALL
    SELECT s4_floc.s4_floc, s4_floc.parent_floc, s4_floc.short_name
    FROM s4_floc, temp_table
    WHERE s4_floc.s4_floc = temp_table.parent
    )
SELECT 
    floc.short_name         AS [Name]
FROM temp_table
JOIN s4_floc    AS floc ON temp_table.floc = floc.s4_floc
ORDER BY floc.s4_floc
;
