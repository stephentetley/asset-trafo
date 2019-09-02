WITH RECURSIVE
temp_table(child_id) AS (
    SELECT 826367
        UNION ALL
    SELECT ai_structure_relationships.child_id
    FROM ai_structure_relationships, temp_table
    WHERE ai_structure_relationships.parent_id = temp_table.child_id
    )
SELECT 
    temp_table.child_id AS [ChildId],
    asset.reference AS [Reference],
    asset.asset_name AS [Name],
    asset.asset_common_name AS [CommonName]
FROM temp_table
JOIN ai_asset    AS asset ON temp_table.child_id = asset.ai_asset_id
ORDER BY asset.asset_common_name
;

SELECT asset.* FROM ai_asset AS asset WHERE asset.ai_asset_id = 	826367;