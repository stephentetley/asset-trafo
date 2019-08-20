-- 

WITH RECURSIVE
  temp_table(ChildId) AS (
      SELECT '2111881'
          UNION ALL
      SELECT aide_structure_relationships.ChildId
      FROM aide_structure_relationships, temp_table
      WHERE aide_structure_relationships.ParentId = temp_table.ChildId
      )
      SELECT * FROM temp_table