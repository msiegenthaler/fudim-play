# Tasks schema

# --- !Ups

CREATE SEQUENCE dimension_id_seq;
CREATE TABLE dimension (
  id integer not null default nextval('dimension_id_seq'),
  name varchar(1024) not null
);

CREATE TABLE dimension_value (
  dimension integer not null,
  nr integer not null,
  content varchar(1024) not null
);

CREATE SEQUENCE fact_id_seq;
CREATE TABLE fact (
  id integer not null default nextval('fact_id_seq'),
  name varchar(1024) not null
);

CREATE TABLE fact_dimension (
  fact integer not null,
  dimension integer not null
);

CREATE SEQUENCE factStore_id_seq;
CREATE TABLE factStore_value (
  id integer not null default nextval('factStore_id_seq'),
  fact integer not null,
  value varchar(1024) not null
);

CREATE TABLE factStore_dimension (
  id integer not null,
  dimension integer not null,
  value varchar(1024) not null
);

CREATE SEQUENCE databaseCube_id_seq;
CREATE TABLE databaseCube (
  id integer not null default nextval('databaseCube_id_seq'),
  name varchar(1024) not null,
  type varchar(255) not null
);

CREATE TABLE databaseCube_dimension (
  cube integer not null,
  dimension integer not null
);




# --- !Downs

DROP TABLE databaseCube_dimension;

DROP SEQUENCE databaseCube_id_seq;
DROP TABLE databaseCube;

DROP TABLE factStore_dimension;

DROP SEQUENCE factStore_id_seq;
DROP TABLE factStore_value;

DROP TABLE fact_dimension;

DROP SEQUENCE fact_id_seq;
DROP TABLE fact;

DROP TABLE dimension_value;

DROP SEQUENCE dimension_id_seq;
DROP TABLE dimension;
