# Tasks schema

# --- !Ups

CREATE SEQUENCE domain_id_seq;
CREATE TABLE domain (
  id integer not null default nextval('domain_id_seq'),
  name varchar(1024) not null
);

CREATE SEQUENCE dimension_id_seq;
CREATE TABLE dimension (
  id integer not null default nextval('dimension_id_seq'),
  domain integer not null,
  name varchar(1024) not null
);

CREATE SEQUENCE dimension_value_seq;
CREATE TABLE dimension_value (
  id integer not null default nextval('dimension_value_seq'),
  dimension integer not null,
  nr integer not null,
  content varchar(1024) not null
);

CREATE SEQUENCE fact_id_seq;
CREATE TABLE fact (
  id integer not null default nextval('fact_id_seq'),
  name varchar(1024) not null,
  domain integer not null,
  type varchar(100) not null,
  config text not null
);

CREATE SEQUENCE databaseCube_id_seq;
CREATE TABLE databaseCube (
  id integer not null default nextval('databaseCube_id_seq'),
  type varchar(255) not null
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
DROP TABLE fact;

DROP SEQUENCE dimension_value_seq;
DROP TABLE dimension_value;

DROP SEQUENCE dimension_id_seq;
DROP TABLE dimension;

DROP SEQUENCE domain_id_seq;
DROP TABLE domain;