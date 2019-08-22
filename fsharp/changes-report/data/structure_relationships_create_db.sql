
-- drop tables
DROP TABLE IF EXISTS ai_structure_relationships;
DROP TABLE IF EXISTS ai_asset_lookups;
DROP TABLE IF EXISTS aide_structure_relationships;
DROP TABLE IF EXISTS aide_asset_lookups;

-- create tables

CREATE TABLE ai_structure_relationships
(
    structure_relationship_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    parent_id BIGINT,
    child_id BIGINT
);

CREATE TABLE ai_asset_lookups
(
    asset_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    reference TEXT,
    asset_common_name TEXT,
    asset_name TEXT,
    asset_type TEXT,
    asset_category TEXT
);

CREATE TABLE aide_structure_relationships
(
    structure_relationship_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    parent_id BIGINT,
    child_id BIGINT
);

CREATE TABLE aide_asset_lookups
(
    aide_asset_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    change_request_id BIGINT,
    asset_id BIGINT,
    reference TEXT,
    asset_common_name TEXT,
    asset_name TEXT,
    asset_type TEXT,
    asset_category TEXT
);
