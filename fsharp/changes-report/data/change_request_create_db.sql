
CREATE TABLE asset_change
(
    change_request_id BIGINT PRIMARY KEY UNIQUE NOT NULL, 
    request_status TEXT,
    change_request_type TEXT,
    asset_reference TEXT,
    ai_asset_name TEXT,
    ai_common_name TEXT,
    ai_installed_from_date TEXT,
    ai_manufacturer TEXT,
    ai_model TEXT,
    ai_hierarchy_key TEXT,
    ai_asset_status TEXT,
    ai_location_reference TEXT,
    aide_asset_name TEXT,
    aide_common_name TEXT,
    aide_installed_from_date TEXT,
    aide_manufacturer TEXT,
    aide_model TEXT,
    aide_hierarchy_key TEXT,
    aide_asset_status TEXT,
    aide_location_reference TEXT,
    change_request_time TEXT
);

CREATE TABLE attribute_change
(
    attibute_change_id BIGINT PRIMARY KEY UNIQUE NOT NULL,
    change_request_id BIGINT,
    request_status TEXT,
    reference TEXT,
    asset_name TEXT,
    attribute_name TEXT,
    ai_value TEXT,
    ai_lookup_value TEXT,
    ai_lookup_code BIGINT,
    aide__value TEXT,
    aide__lookup_value TEXT,
    aide__lookup_code BIGINT,
    change_request_time TEXT
);