DROP DATABASE IF EXISTS impatience;
CREATE DATABASE impatience;
\c impatience
CREATE TABLE "progresses" ("id" INT NOT NULL, "completed" INT NOT NULL, "total" INT NOT NULL, PRIMARY KEY("id"));
