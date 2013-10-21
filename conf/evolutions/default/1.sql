# Tasks schema

# --- !Ups

CREATE SEQUENCE version_id_seq;
CREATE TABLE version (
  id bigint not null default nextval('version_id_seq'),
  ts timestamp not null default now()
);

CREATE SEQUENCE domain_id_seq;
CREATE TABLE domain (
  id integer not null default nextval('domain_id_seq'),
  version bigint references version(id),
  name varchar(1024) not null
);
CREATE UNIQUE INDEX domain_name ON domain(name);

CREATE SEQUENCE dimension_id_seq;
CREATE TABLE dimension (
  id integer not null default nextval('dimension_id_seq'),
  domain integer not null,
  name varchar(1024) not null,
  version bigint not null references version(id)
);
CREATE UNIQUE INDEX dimension_name ON dimension(domain, name);

CREATE SEQUENCE dimension_value_seq;
CREATE TABLE dimension_value (
  id integer not null default nextval('dimension_value_seq'),
  dimension integer not null,
  version bigint not null references version(id),
  nr integer not null,
  content varchar(1024) not null
);

CREATE SEQUENCE fact_id_seq;
CREATE TABLE fact (
  id integer not null default nextval('fact_id_seq'),
  name varchar(1024) not null,
  domain integer not null,
  version bigint not null references version(id),
  dataType varchar(100) not null,
  factType varchar(100) not null,
  config text not null
);
CREATE UNIQUE INDEX fact_name ON fact(domain, name);

CREATE SEQUENCE databaseCube_id_seq;
CREATE TABLE databaseCube (
  id integer not null default nextval('databaseCube_id_seq'),
  type varchar(255) not null,
  version bigint not null references version(id)
);

CREATE SEQUENCE databaseCube_dimension_id_seq;
CREATE TABLE databaseCube_dimension (
  id integer not null default nextval('databaseCube_dimension_id_seq'),
  cube integer not null,
  dimension varchar(1024) not null
);




# --- !Downs

DROP SEQUENCE databaseCube_dimension_id_seq;
DROP TABLE databaseCube_dimension;

DROP SEQUENCE databaseCube_id_seq;
DROP TABLE databaseCube;

DROP SEQUENCE fact_id_seq;
DROP INDEX fact_name;
DROP TABLE fact;

DROP SEQUENCE dimension_value_seq;
DROP TABLE dimension_value;

DROP SEQUENCE dimension_id_seq;
DROP INDEX dimension_name;
DROP TABLE dimension;

DROP SEQUENCE domain_id_seq;
DROP INDEX domain_name;
DROP TABLE domain;

DROP SEQUENCE version_id_seq;
DROP TABLE version;
