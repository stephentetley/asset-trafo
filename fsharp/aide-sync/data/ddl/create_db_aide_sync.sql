
-- drop tables
DROP VIEW IF EXISTS view_scheme_change_requests;
DROP TABLE IF EXISTS work_scheme;
DROP TABLE IF EXISTS change_request;
DROP TABLE IF EXISTS asset;
DROP TABLE IF EXISTS asset_attribute;
DROP TABLE IF EXISTS asset_repeated_attribute;
DROP TABLE IF EXISTS aide_structure_relationships;
DROP TABLE IF EXISTS ai_structure_relationships;


-- create tables

CREATE TABLE work_scheme
(
    scheme_id INTEGER PRIMARY KEY UNIQUE NOT NULL, 
    scheme_code TEXT,
    scheme_name TEXT,
    description TEXT,
    solution_provider TEXT
);

CREATE TABLE change_request
(
    change_request_id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    change_request_time TEXT,
    change_request_type TEXT,
    change_request_status TEXT,
    comments TEXT
);

CREATE TABLE asset
(
    aide_asset_id INTEGER PRIMARY KEY UNIQUE NOT NULL, 
    change_request_id INTEGER,
    scheme_id INTEGER,
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

CREATE TABLE asset_attribute
(
    aide_asset_attr_value_id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    change_request_id INTEGER,
    asset_reference TEXT,
    asset_name TEXT,
    asset_common_name TEXT,
    attribute_name TEXT,
    ai_value TEXT,
    ai_lookup_value TEXT,
    aide_value TEXT,
    aide_lookup_value TEXT
);

CREATE TABLE asset_repeated_attribute
(
    aide_asset_attr_repeating_value_id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    change_request_id INTEGER,
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

CREATE TABLE aide_structure_relationships
(
    structure_relationship_id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    parent_id INTEGER,
    child_id INTEGER
);

CREATE TABLE ai_structure_relationships
(
    structure_relationship_id INTEGER PRIMARY KEY UNIQUE NOT NULL,
    parent_id INTEGER,
    child_id INTEGER
);