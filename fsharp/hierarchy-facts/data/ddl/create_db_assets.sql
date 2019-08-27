CREATE TABLE aib_process_group (sai_ref TEXT PRIMARY KEY NOT NULL UNIQUE, asset_name TEXT, asset_type TEXT, parent_ref TEXT NOT NULL);
CREATE TABLE aib_plant (sai_ref TEXT PRIMARY KEY UNIQUE NOT NULL, asset_name TEXT, asset_type TEXT, parent_ref TEXT NOT NULL);
CREATE TABLE aib_plant_item (sai_ref TEXT PRIMARY KEY UNIQUE NOT NULL, asset_name TEXT, asset_type TEXT, parent_ref TEXT NOT NULL);
CREATE TABLE aib_process (sai_ref TEXT PRIMARY KEY NOT NULL UNIQUE, asset_name TEXT, asset_type TEXT, parent_ref TEXT NOT NULL);
CREATE TABLE aib_equipment (pli_ref TEXT PRIMARY KEY UNIQUE NOT NULL, equipment_name TEXT, equipment_type TEXT, category TEXT, parent_ref TEXT NOT NULL);
CREATE TABLE aib_installation (sai_ref TEXT UNIQUE PRIMARY KEY NOT NULL, common_name TEXT NOT NULL, installation_type TEXT NOT NULL) WITHOUT ROWID;
CREATE TABLE s4_process_group (s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, name TEXT NOT NULL, aib_ref TEXT, parent_floc TEXT);
CREATE TABLE s4_function (s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, name TEXT NOT NULL, aib_ref TEXT, parent_floc TEXT);
CREATE TABLE s4_site (s4_floc TEXT PRIMARY KEY NOT NULL UNIQUE, name TEXT NOT NULL, aib_ref TEXT);
CREATE TABLE s4_equipment (s4_ref BIGINT PRIMARY KEY NOT NULL UNIQUE, name TEXT, aib_pli_code TEXT NOT NULL, category TEXT, obj_type TEXT, obj_class TEXT, s4_floc TEXT);
CREATE TABLE s4_process (s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, name TEXT NOT NULL, aib_ref TEXT, parent_floc TEXT);
CREATE TABLE s4_system (s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, name TEXT NOT NULL, aib_ref TEXT, parent_floc TEXT);
CREATE TABLE s4_assembly (s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, name TEXT NOT NULL, aib_ref TEXT, parent_floc TEXT);
CREATE TABLE s4_item (s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, name TEXT NOT NULL, aib_ref TEXT, parent_floc TEXT);
CREATE TABLE s4_component (s4_floc TEXT UNIQUE PRIMARY KEY NOT NULL, name NOT NULL, aib_ref TEXT, parent_floc TEXT);
