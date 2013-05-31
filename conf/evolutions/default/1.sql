# Tasks schema

# --- !Ups

CREATE TABLE dimension (
		name varchar(1024) not null
);



# --- !Downs

DROP TABLE dimension;
