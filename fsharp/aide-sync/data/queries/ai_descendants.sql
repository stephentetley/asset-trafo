WITH RECURSIVE
temp_table(child_id, parent_id, node_level) AS (
    SELECT 826367,0,1
        UNION ALL
    SELECT 
        ai_structure_relationships.child_id,
        ai_structure_relationships.parent_id,
        node_level + 1
    FROM ai_structure_relationships, temp_table
    WHERE ai_structure_relationships.parent_id = temp_table.child_id
    )
SELECT 
    temp_table.child_id AS [AssetId],
    asset.reference AS [Reference],    
    temp_table.node_level  AS [NodeLevel],
    parent_asset.reference AS [ParentReference],
    asset.asset_name AS [Name],
    asset.asset_common_name AS [CommonName]
FROM temp_table
JOIN ai_asset    AS asset ON temp_table.child_id = asset.ai_asset_id
LEFT OUTER JOIN ai_asset    AS parent_asset ON temp_table.parent_id = parent_asset.ai_asset_id
ORDER BY asset.asset_common_name
;

SELECT asset.* FROM ai_asset AS asset WHERE asset.ai_asset_id = 826367;