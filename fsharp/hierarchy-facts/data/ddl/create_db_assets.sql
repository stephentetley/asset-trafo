
-- drop tables
DROP TABLE IF EXISTS aib_installation;
DROP TABLE IF EXISTS aib_process_group;
DROP TABLE IF EXISTS aib_process;
DROP TABLE IF EXISTS aib_plant;
DROP TABLE IF EXISTS aib_plant_item;
DROP TABLE IF EXISTS aib_equipment;

DROP TABLE IF EXISTS s4_site;
DROP TABLE IF EXISTS s4_function;
DROP TABLE IF EXISTS s4_process_group;
DROP TABLE IF EXISTS s4_process;
DROP TABLE IF EXISTS s4_system;
DROP TABLE IF EXISTS s4_assembly;
DROP TABLE IF EXISTS s4_item;
DROP TABLE IF EXISTS s4_component;
DROP TABLE IF EXISTS s4_equipment;

DROP TABLE IF EXISTS s4_aib_reference;

-- create tables
CREATE TABLE aib_installation 
(
    sai_ref TEXT UNIQUE PRIMARY KEY NOT NULL, 
    common_name TEXT NOT NULL, 
    installation_type TEXT NOT NULL
);

CREATE TABLE aib_process_group 
(
    sai_ref TEXT PRIMARY KEY NOT NULL UNIQUE, 
    asset_name TEXT, 
    asset_type TEXT, 
    parent_ref TEXT NOT NULL
);

CREATE TABLE aib_process 
(
    sai_ref TEXT PRIMARY KEY NOT NULL UNIQUE, 
    asset_name TEXT, asset_type TEXT, 
    parent_ref TEXT NOT NULL
);

CREATE TABLE aib_plant 
(
    sai_ref TEXT PRIMARY KEY UNIQUE NOT NULL, 
    asset_name TEXT, 
    asset_type TEXT, 
    parent_ref TEXT NOT NULL
);

CREATE TABLE aib_plant_item 
(
    sai_ref TEXT PRIMARY KEY UNIQUE NOT NULL, 
    asset_name TEXT, 
    asset_type TEXT, 
    parent_ref TEXT NOT NULL
);

CREATE TABLE aib_equipment 
(
    pli_ref TEXT PRIMARY KEY UNIQUE NOT NULL, 
    equipment_name TEXT, 
    equipment_type TEXT, 
    category TEXT, 
    parent_ref TEXT NOT NULL
);

CREATE TABLE s4_site 
(
    s4_floc TEXT PRIMARY KEY NOT NULL UNIQUE, 
    name TEXT NOT NULL
);

CREATE TABLE s4_function 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    name TEXT NOT NULL,
    parent_floc TEXT
);

CREATE TABLE s4_process_group 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    name TEXT NOT NULL,
    parent_floc TEXT
);


CREATE TABLE s4_process 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    name TEXT NOT NULL,
    parent_floc TEXT
);

CREATE TABLE s4_system 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    name TEXT NOT NULL, 
    parent_floc TEXT
);

CREATE TABLE s4_assembly 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    name TEXT NOT NULL,
    parent_floc TEXT
);

CREATE TABLE s4_item 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    name TEXT NOT NULL,
    parent_floc TEXT
);

CREATE TABLE s4_component 
(
    s4_floc TEXT UNIQUE PRIMARY KEY NOT NULL, 
    name NOT NULL, 
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
