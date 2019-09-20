


WITH RECURSIVE
temp_table(sai_ref) AS (
    SELECT 'SAI00277997'
        UNION ALL
    SELECT aib_floc.sai_ref
    FROM aib_floc, temp_table
    WHERE aib_floc.parent_ref = temp_table.sai_ref
    )
SELECT 
    -- temp_table.child_id     AS [AssetId],
    floc.sai_ref         AS [Reference],
    floc.short_name        AS [Name]
    -- asset.asset_common_name AS [CommonName]
FROM temp_table
JOIN aib_floc    AS floc ON temp_table.sai_ref = floc.sai_ref
ORDER BY floc.sai_ref
;

SELECT floc.* FROM aib_floc AS floc WHERE floc.sai_ref = 'SAI00277997';