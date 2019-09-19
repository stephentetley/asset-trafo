-- sqlite3 floc_mapping.sqlite < create_db_floc_mapping.sql


-- drop tables
DROP TABLE IF EXISTS aib_floc;
DROP TABLE IF EXISTS aib_equipment;

DROP TABLE IF EXISTS s4_floc;
DROP TABLE IF EXISTS s4_equipment;

DROP TABLE IF EXISTS s4_aib_reference;

CREATE TABLE aib_floc
(
    sai_ref TEXT UNIQUE PRIMARY KEY NOT NULL,
    short_name TEXT,
    short_code TEXT,    -- eg CSO
    category TEXT,      -- eg PLANT ITEM
    asset_type TEXT, 
    parent_ref TEXT    
);


CREATE TABLE aib_equipment 
(
    pli_ref TEXT PRIMARY KEY UNIQUE NOT NULL, 
    short_name TEXT, 
    category TEXT, 
    equipment_type TEXT,     
    parent_ref TEXT NOT NULL
);

CREATE TABLE s4_floc 
(
    s4_floc TEXT PRIMARY KEY NOT NULL UNIQUE, 
    name TEXT NOT NULL,
    category TEXT NOT NULL,
    parent_floc TEXT
);

CREATE TABLE s4_equipment 
(
    s4_ref BIGINT PRIMARY KEY NOT NULL UNIQUE, 
    name TEXT, 
    category TEXT, 
    obj_type TEXT, 
    obj_class TEXT, 
    s4_floc TEXT
);

CREATE TABLE s4_aib_reference 
(
    s4_floc TEXT NOT NULL,
    aib_ref TEXT NOT NULL
);
