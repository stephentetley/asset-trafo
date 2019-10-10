-- sqlite3 s4_rulebase_template.sqlite < create_db_s4_rulebase.sql



-- drop indices
DROP INDEX IF EXISTS idx_s4_site_s4_floc;
DROP INDEX IF EXISTS idx_s4_function_s4_floc;
DROP INDEX IF EXISTS idx_s4_process_group_s4_floc;
DROP INDEX IF EXISTS idx_s4_process_s4_floc;
DROP INDEX IF EXISTS idx_s4_system_s4_floc;
DROP INDEX IF EXISTS idx_s4_assembly_s4_floc;
DROP INDEX IF EXISTS idx_s4_item_s4_floc;
DROP INDEX IF EXISTS idx_s4_component_s4_floc;

-- drop tables
DROP TABLE IF EXISTS s4_site;
DROP TABLE IF EXISTS s4_function;
DROP TABLE IF EXISTS s4_process_group;
DROP TABLE IF EXISTS s4_process;
DROP TABLE IF EXISTS s4_system;
DROP TABLE IF EXISTS s4_assembly;
DROP TABLE IF EXISTS s4_item;
DROP TABLE IF EXISTS s4_component;
DROP TABLE IF EXISTS s4_equipment;


CREATE TABLE s4_site 
(
    s4_floc TEXT PRIMARY KEY NOT NULL UNIQUE, 
    site_name TEXT NOT NULL
);

CREATE TABLE s4_function 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    function_name TEXT NOT NULL,
    short_code TEXT,
    object_description TEXT, 
    parent_floc TEXT
);

CREATE TABLE s4_process_group 
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    process_group_name TEXT NOT NULL,
    short_code TEXT,
    object_description TEXT, 
    parent_floc TEXT
);

CREATE TABLE s4_process
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    process_name TEXT NOT NULL,
    short_code TEXT,
    object_description TEXT, 
    parent_floc TEXT
);

CREATE TABLE s4_system
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    system_name TEXT NOT NULL,
    object_code TEXT,
    object_description TEXT, 
    class_code TEXT,
    class_description TEXT, 
    system_code TEXT,
    parent_floc TEXT
);

CREATE TABLE s4_assembly
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    assembly_name TEXT NOT NULL,
    object_code TEXT,
    object_description TEXT, 
    class_code TEXT,
    class_description TEXT, 
    parent_floc TEXT
);

CREATE TABLE s4_item
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    item_name TEXT NOT NULL,
    object_code TEXT,
    object_description TEXT, 
    class_code TEXT,
    class_description TEXT, 
    parent_floc TEXT
);

CREATE TABLE s4_component
(
    s4_floc TEXT PRIMARY KEY UNIQUE NOT NULL, 
    component_name TEXT NOT NULL,
    object_code TEXT,
    object_description TEXT, 
    class_code TEXT,
    class_description TEXT, 
    parent_floc TEXT
);

CREATE TABLE s4_equipment 
(
    s4_equip_ref INTEGER PRIMARY KEY NOT NULL UNIQUE, 
    description TEXT, 
    category TEXT, 
    object_type TEXT, 
    object_class TEXT, 
    s4_floc TEXT NOT NULL
);


-- create indices

CREATE INDEX idx_s4_site_s4_floc
ON s4_site(s4_floc);

CREATE INDEX idx_s4_function_s4_floc
ON s4_function(s4_floc);

CREATE INDEX idx_s4_process_group_s4_floc
ON s4_process_group(s4_floc);

CREATE INDEX idx_s4_process_s4_floc
ON s4_process(s4_floc);

CREATE INDEX idx_s4_system_s4_floc
ON s4_system(s4_floc);

CREATE INDEX idx_s4_assembly_s4_floc
ON s4_assembly(s4_floc);

CREATE INDEX idx_s4_item_s4_floc
ON s4_item(s4_floc);

CREATE INDEX idx_s4_component_s4_floc
ON s4_component(s4_floc);

