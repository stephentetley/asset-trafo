WITH RECURSIVE
temp_table(child_id, parent_id) AS (
    SELECT 1934321,0
        UNION ALL
    SELECT 
            aide_structure_relationships.child_id,
            aide_structure_relationships.parent_id
    FROM aide_structure_relationships, temp_table
    WHERE aide_structure_relationships.parent_id = temp_table.child_id
    )
SELECT 
    temp_table.child_id AS [AideAssetId],
    asset.asset_id AS [AssetId],
    asset.reference AS [Reference],
    parent_asset.reference AS [ParentReference],
    asset.asset_name AS [Name],
    asset.asset_common_name AS [CommonName]
FROM temp_table
JOIN aide_asset    AS asset ON temp_table.child_id = asset.aide_asset_id
LEFT OUTER JOIN aide_asset    AS parent_asset ON temp_table.parent_id = parent_asset.aide_asset_id
ORDER BY asset.asset_common_name
;