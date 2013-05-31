# Tasks schema

# --- !Ups

CREATE TABLE dimension_value (
		dimension varchar(1024) not null,
		nr integer not null,
		content varchar(1024) not null
);



# --- !Downs

DROP TABLE dimension_value;
