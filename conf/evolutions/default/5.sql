# Tasks schema

# --- !Ups

CREATE SEQUENCE dimension_id_seq;
ALTER TABLE dimension
  ADD COLUMN id integer not null default nextval('dimension_id_seq');

ALTER TABLE dimension_value
  ADD COLUMN dimension_id integer;
UPDATE dimension_value v SET dimension_id = (SELECT d.id FROM dimension d WHERE d.name = v.dimension);
ALTER TABLE dimension_value
  DROP COLUMN dimension;
ALTER TABLE dimension_value
  ADD COLUMN dimension integer not null;
UPDATE dimension_value SET dimension = dimension_id;
ALTER TABLE dimension_value
  DROP COLUMN dimension_id;

# --- !Downs

ALTER TABLE dimension_value
  ADD COLUMN dimension_name integer;
UPDATE dimension_value v SET dimension_name = (SELECT d.name FROM dimension d WHERE d.id = v.dimension);
ALTER TABLE dimension_value
  DROP COLUMN dimension;
ALTER TABLE dimension_value
  ADD COLUMN dimension integer not null;
UPDATE dimension_value SET dimension = dimension_name;
ALTER TABLE dimension_value
  DROP COLUMN dimension_name;

DROP SEQUENCE dimension_id_seq;
ALTER TABLE
  DROP COLUMN id;

