# Tasks schema

# --- !Ups

CREATE SEQUENCE factStore_id_seq;
CREATE TABLE factStore_value (
  id integer not null default nextval('factStore_id_seq'),
  fact integer not null,
  value varchar(1024) not null
);
CREATE TABLE factStore_dimension (
  id integer not null,
  dimension varchar(1024) not null,
  value varchar(1024) not null
);

# --- !Downs

DROP SEQUENCE factStore_id_seq;
DROP TABLE factStore_value;
DROP TABLE factStore_dimension;