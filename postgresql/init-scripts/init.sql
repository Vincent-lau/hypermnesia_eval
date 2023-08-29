create database test;
\c test;
CREATE TABLE usertable (YCSB_KEY VARCHAR(255) PRIMARY KEY not NULL, YCSB_VALUE JSONB not NULL);
GRANT ALL PRIVILEGES ON DATABASE test to postgres;
