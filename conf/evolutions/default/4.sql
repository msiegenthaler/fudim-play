# Tasks schema

# --- !Ups

CREATE SEQUENCE factValue_id_seq;
CREATE TABLE factValue (
  id integer not null default nextval('factValue_id_seq'),
  fact integer not null,
  value varchar(1024) not null
);
CREATE TABLE factValue_dimension (
  factValue integer not null,
  dimension varchar(1024) not null,
  value varchar(1024) not null
);

# --- !Downs

DROP SEQUENCE factValue_id_seq;
DROP TABLE factValue;
DROP TABLE factValue_dimension;