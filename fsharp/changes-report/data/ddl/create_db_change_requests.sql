
-- drop tables
DROP VIEW IF EXISTS view_scheme_change_requests;
DROP TABLE IF EXISTS work_scheme;
DROP TABLE IF EXISTS change_request;
DROP TABLE IF EXISTS change_request_asset;
DROP TABLE IF EXISTS change_request_attribute;
DROP TABLE IF EXISTS change_request_repeated_attribute;

-- create tables

CREATE TABLE work_scheme
(
    scheme_id BIGINT PRIMARY KEY UNIQUE NOT NULL, 
    scheme_code TEXT,
    scheme_name TEXT,
    description TEXT,
    solution_provider TEXT
);

CREATE TABLE change_request
(
    change_request_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    change_request_time TEXT,
    change_request_type TEXT,
    change_request_status TEXT,
    comments TEXT
);

CREATE TABLE change_request_asset
(
    aide_asset_id BIGINT PRIMARY KEY UNIQUE NOT NULL, 
    change_request_id BIGINT,
    scheme_id BIGINT,
    ai_asset_reference TEXT,
    aide_asset_reference TEXT,
    ai_asset_name TEXT,
    ai_common_name TEXT,
    ai_installed_from_date TEXT,
    ai_manufacturer TEXT,
    ai_model TEXT,
    ai_hierarchy_key TEXT,
    ai_asset_status TEXT,
    ai_location_reference TEXT,
    ai_asset_deleted TINYINT,
    aide_asset_name TEXT,
    aide_common_name TEXT,
    aide_installed_from_date TEXT,
    aide_manufacturer TEXT,
    aide_model TEXT,
    aide_hierarchy_key TEXT,
    aide_asset_status TEXT,
    aide_location_reference TEXT,
    aide_asset_deleted TINYINT
);

CREATE TABLE change_request_attribute
(
    aide_asset_attr_value_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    change_request_id BIGINT,
    asset_reference TEXT,
    asset_name TEXT,
    asset_common_name TEXT,
    attribute_name TEXT,
    ai_value TEXT,
    ai_lookup_value TEXT,
    aide_value TEXT,
    aide_lookup_value TEXT
);

CREATE TABLE change_request_repeated_attribute
(
    aide_asset_attr_repeating_value_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    change_request_id BIGINT,
    asset_reference TEXT,
    asset_name TEXT,
    asset_common_name TEXT,
    attribute_name TEXT,
    attribute_set_name TEXT,
    ai_value TEXT,
    ai_lookup_value TEXT,
    aide_value TEXT,
    aide_lookup_value TEXT
);

CREATE VIEW view_scheme_change_requests 
AS 
SELECT 
        asset.scheme_id             AS [scheme_id],
        asset.change_request_id     AS [change_request_id]
FROM 
    change_request_asset AS asset 
ORDER BY asset.scheme_id, asset.change_request_id
;