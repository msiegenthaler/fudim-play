# Tasks schema

# --- !Ups

CREATE SEQUENCE fact_id_seq;
CREATE TABLE fact (
  id integer not null default nextval('fact_id_seq'),
  name varchar(1024) not null
);
CREATE TABLE fact_dimension (
  fact integer not null,
  dimension varchar(1024) not null
);



# --- !Downs

DROP SEQUENCE fact_id_seq;
DROP TABLE fact;
DROP TABLE fact_dimension;